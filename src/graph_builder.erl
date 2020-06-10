-module(graph_builder).

-behaviour(gen_server).

%% API
-export([start_link/0, parse_file/2]).

-export([parse_statement/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("types.hrl").

-define(SERVER, ?MODULE).
-define(EXPAND_FLAG, true).
-record(state,{}).

%%%===================================================================
%%% API
%%%===================================================================
parse_file(File, IncludePaths) ->
    gen_server:call(?MODULE, {parse_file, File, IncludePaths}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({parse_file, File, IncludePaths}, _From, State) ->
    {ok, ParsedFile} = epp:parse_file(File, IncludePaths, []),
    Behaviours = [T || {attribute, _, B, T} <- ParsedFile,
                       B == behaviour orelse B == behavior],
    case Behaviours of
        [] ->
            {reply, {error, not_otp}, State};
        [gen_server|_] ->
            {reply, parse_gen_server(ParsedFile), State};
        [gen_fsm|_] ->
            {reply, parse_gen_fsm(ParsedFile), State};
        [gen_statem|_] ->
            {reply, parse_gen_statem(ParsedFile), State};
        [gen_event|_] ->
            {reply, parse_gen_event(ParsedFile), State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_gen_server(_TokenList) ->
    {error, not_supported}.

parse_gen_statem(TokenList) ->
    Type = gen_statem,
    Fun =
        fun({function, _, init, 1, Clauses}, Acc) ->
                parse_function_add_edges(Clauses, init, [async], Type, Acc);
           ({function, _, handle_event = FnName, 4, Clauses}, Acc) ->
                parse_function_add_node_and_edges(Clauses, FnName, [], Type, Acc);
           ({function, _, FnName, 3, Clauses}, Acc) ->
                parse_function_add_node_and_edges(Clauses, FnName, [], Type, Acc);
           (_Other, Acc) ->
                Acc
        end,
    Graph = generic_parse(Fun, TokenList),
    {parsed, Type, Graph}.

parse_gen_fsm(TokenList) ->
    Type = gen_fsm,
    Fun =
        fun({function, _, init, 1, Clauses}, Acc) ->
                parse_function_add_edges(Clauses, init, [async], Type, Acc);
           ({function, _, handle_event, 3, Clauses}, Acc) ->
                parse_function_add_states(Clauses, handle_event, [async, allstate], Type, Acc);
           ({function, _, handle_sync_event, 4, Clauses}, Acc) ->
                parse_function_add_states(Clauses, handle_sync_event, [sync, allstate], Type, Acc);
           ({function, _, handle_info, 3, Clauses}, Acc) ->
                parse_function_add_states(Clauses, handle_info, [async, allstate, info], Type, Acc);
           ({function, _, FnName, 2, Clauses}, Acc) ->
                parse_function_add_node_and_edges(Clauses, FnName, [async], Type, Acc);
           ({function, _, FnName, 3, Clauses}, Acc) ->
                parse_function_add_node_and_edges(Clauses, FnName, [sync], Type, Acc);
           (_Other, Acc) ->
                Acc
        end,
    Graph = generic_parse(Fun, TokenList),
    {parsed, Type, Graph}.

generic_parse(Fun, TokenList) ->
    Graph = digraph:new(),
    {States, Edges, AllStateEdges} = lists:foldl(Fun, {[init, terminate],[],[]}, TokenList),
    {ExpAllStates, Expansion} = case ?EXPAND_FLAG of
                                    true ->
                                        {States, States};
                                    false ->
                                        {["*" | States], ["*"]}
                                end,
    NewAllStateEdges = expand_allstates(AllStateEdges, Expansion),
    lists:foreach(fun(Vertex) ->
                          V = digraph:add_vertex(Graph),
                          digraph:add_vertex(Graph, V, Vertex)
                  end,
                  lists:usort(ExpAllStates)),
    lists:foreach(fun(#edge{vertex1 = From, vertex2 = To, edge_data = Data}) ->
                          digraph:add_edge(Graph,
                                           get_vertex(Graph, From),
                                           get_vertex(Graph, To),
                                           Data);
                     (_) -> ok
                  end,
                  remove_dups(Edges ++ NewAllStateEdges)),
    Graph.

parse_gen_event(_TokenList) ->
    {error, not_supported}.

parse_function_add_states(Clauses, FnName, Options, Type,
                          {OldNodes, OldEdges, OldAllStates}) ->
    case parse_function(Clauses, FnName, Options, Type) of
        {error, _Reason} ->
            {OldNodes, OldEdges, OldAllStates};
        NewEdges ->
            {OldNodes, OldEdges, OldAllStates ++ NewEdges}
    end.

parse_function_add_edges(Clauses, FnName, Options, Type,
                         {OldNodes, OldEdges, OldAllStates}) ->
    case parse_function(Clauses, FnName, Options, Type) of
        {error, _Reason} ->
            {OldNodes, OldEdges, OldAllStates};
        NewEdges ->
            {OldNodes, OldEdges ++ NewEdges, OldAllStates}
    end.
parse_function_add_node_and_edges(Clauses, FnName, Options, Type,
                                  {OldNodes, OldEdges, OldAllStates}) ->
    case parse_function(Clauses, FnName, Options, Type) of
        {error, _Reason} ->
            {OldNodes, OldEdges, OldAllStates};
        NewEdges ->
            {[FnName|OldNodes], OldEdges ++ NewEdges, OldAllStates}
    end.

parse_function(Clauses, FnName, Options, Type) ->
    Edges = lists:map(fun(Clause) ->
                              parse_function_clause(Clause, FnName, Options, Type)
                      end, Clauses),
    FlatEdges = lists:flatten(Edges),
    %% io:format("For function ~p~n", [FnName]),
    %% lists:foreach(fun(E) ->
    %%                       io:format(">>> ~p~n", [E])
    %%               end, FlatEdges),
    case lists:all(fun (El) -> {error, bad_transition} == El end, Edges) of
        false ->
            FlatEdges;
        true ->
            {error, not_a_state}
    end.

parse_function_clause({clause, _Line, Args, Guards, Body},
                      init, _Options, Type) ->
    Fun = fun({ok, NextState, init}) ->
                  PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
                  PrettyBody = erl_pp:exprs(Body),
                  PrettyArgs = erl_pp:exprs(Args),
                  #edge{
                     vertex1 = init,
                     vertex2 = NextState,
                     edge_data =
                         #edge_data{
                            event = "",
                            args = PrettyArgs,
                            pattern = Args,
                            guard = PrettyGuards,
                            code = PrettyBody,
                            attributes = [async]}
                    }
          end,
    map_parse_func(Fun, init, Body, Type);
parse_function_clause({clause, _Line, [Event | Args], Guards, Body},
                      handle_info, Options, gen_fsm) ->
    Fun = fun({ok, NextState, RetType}) ->
                  PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
                  PrettyBody = erl_pp:exprs(Body),
                  PrettyEvent = erl_pp:expr(Event),
                  PrettyArgs = erl_pp:exprs(Args),
                  #edge{
                     vertex1 = handle_info,
                     vertex2 = NextState,
                     edge_data =
                         #edge_data{
                            event = PrettyEvent,
                            args = PrettyArgs,
                            pattern = Args,
                            guard = PrettyGuards,
                            code = PrettyBody,
                            attributes = [RetType|Options]}
                    }
          end,
    map_parse_func(Fun, handle_info, Body, gen_fsm);
parse_function_clause({clause, _Line, [_EventType, Event, State | Args], Guards, Body},
                      handle_event, Options, gen_statem) ->
    PrettyState = parse_state(State),
    Fun = fun({ok, NextState, RetType}) ->
                  PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
                  PrettyBody = erl_pp:exprs(Body),
                  PrettyEvent = erl_pp:expr(Event),
                  PrettyArgs = erl_pp:exprs(Args),
                  #edge{
                     vertex1 = PrettyState,
                     vertex2 = NextState,
                     edge_data =
                         #edge_data{
                            event = PrettyEvent,
                            args = PrettyArgs,
                            pattern = Args,
                            guard = PrettyGuards,
                            code = PrettyBody,
                            attributes = [RetType|Options]}
                    }
          end,
    map_parse_func(Fun, State, Body, gen_statem);
parse_function_clause({clause, _Line, [Event | Args], Guards, Body},
                      FnName, Options, gen_fsm) ->
    Fun = fun({ok, NextState, _}) ->
                  PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
                  PrettyBody = erl_pp:exprs(Body),
                  PrettyEvent = erl_pp:expr(Event),
                  PrettyArgs = erl_pp:exprs(Args),
                  #edge{
                     vertex1 = FnName,
                     vertex2 = NextState,
                     edge_data =
                         #edge_data{
                            event = PrettyEvent,
                            args = PrettyArgs,
                            pattern = Args,
                            guard = PrettyGuards,
                            code = PrettyBody,
                            attributes = Options}
                    }
          end,
    map_parse_func(Fun, FnName, Body, gen_fsm);
parse_function_clause({clause, _Line, [_Call, Event | Args], Guards, Body},
                      FnName, Options, gen_statem) ->
    Fun = fun({ok, NextState, _}) ->
                  PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
                  PrettyBody = erl_pp:exprs(Body),
                  PrettyEvent = erl_pp:expr(Event),
                  PrettyArgs = erl_pp:exprs(Args),
                  #edge{
                     vertex1 = FnName,
                     vertex2 = NextState,
                     edge_data =
                         #edge_data{
                            event = PrettyEvent,
                            args = PrettyArgs,
                            pattern = Args,
                            guard = PrettyGuards,
                            code = PrettyBody,
                            attributes = Options}
                    }
          end,
    map_parse_func(Fun, FnName, Body, gen_statem).

parse_state({atom, _, A}) ->
    A;
parse_state({var, _, _} = V) ->
    V.

map_parse_func(Fun, State, Body, Type) ->
    ParsedBody = lists:map(fun eval_tuple/1, parse_body(Body)),
    EvalFun = case Type of
                  gen_statem ->
                      eval_statem_return(State);
                  gen_fsm ->
                      eval_fsm_return()
              end,
    case lists:filter(fun(E) -> undefined =/= E end,
                      lists:map(EvalFun, ParsedBody)) of
        [] ->
            {error, bad_transition};
        List ->
            lists:map(Fun, List)
    end.

parse_body(Body) ->
    lists:flatmap(fun parse_statement/1, Body).

parse_statement({tuple, Line, Elems}) ->
    [{tuple, Line, Elems}];
parse_statement(Statement) when is_tuple(Statement) ->
    [parse_statement(S) || S <- tuple_to_list(Statement)];
parse_statement(Statement) when is_list(Statement) ->
    [parse_statement(S) || S <- Statement];
parse_statement(_Statement) ->
    [].

eval_fsm_return() ->
    fun({ok, {ok, NextState, _Data}}) ->
            {ok, NextState, init};
       ({ok, {ok, NextState, _Data, _Timeout}}) ->
            {ok, NextState, init};
       ({ok, {stop, _Reason}}) ->
            {ok, terminate, init};
       ({ok, {reply, _Reply, NextState, _Data}}) ->
            {ok, NextState, sync};
       ({ok, {reply, _Reply, NextState, _Data, _Timeout}}) ->
            {ok, NextState, sync};
       ({ok, {stop, _Reason, _Reply, _Data}}) ->
            {ok, terminate, sync};
       ({ok, {next_state, NextState, _Data}}) ->
            {ok, NextState, async};
       ({ok, {next_state, NextState, _Data, _Timeout}}) ->
            {ok, NextState, async};
       ({ok, {stop, _Reason, _Data}}) ->
            {ok, terminate, async};
       (_Other) ->
            undefined
    end.

eval_statem_return(State) ->
    fun({ok, {ok, NextState, _Data}}) ->
            {ok, NextState, init};
       ({ok, {ok, NextState, _Data, _Actions}}) ->
            {ok, NextState, init};
       ({ok, ignore}) ->
            {ok, terminate, init};
       ({ok, stop}) ->
            {ok, terminate, async};
       ({ok, {stop, _Reason}}) ->
            {ok, terminate, async};
       ({ok, {stop, _Reason, _NewData}}) ->
            {ok, terminate, async};
       ({ok, {stop_and_reply, _Reason, _Replies}}) ->
            {ok, terminate, async};
       ({ok, {stop_and_reply, _Reason, _Replies, _NewData}}) ->
            {ok, terminate, async};
       ({ok, {next_state, NextState, _Data}}) ->
            {ok, NextState, async};
       ({ok, {next_state, NextState, _Data, _Actions}}) ->
            {ok, NextState, async};
       ({ok, {keep_state, _NewData}}) ->
            {ok, State, async};
       ({ok, {keep_state, _NewData, _Actions}}) ->
            {ok, State, async};
       ({ok, keep_state_and_data}) ->
            {ok, State, async};
       ({ok, {keep_state_and_data, _Actions}}) ->
            {ok, State, async};
       (_Other) ->
            undefined
    end.

eval_tuple({tuple, Line, Elements})->
    ClearElements = lists:map(fun({atom, RLine, Atom}) ->
                                      {atom, RLine, Atom};
                                 (_Other) ->
                                      {atom, 0, '@var'}
                              end,
                              Elements),
    ClearTuple = {tuple, Line, ClearElements},
    {value, Val, _} = erl_eval:expr(ClearTuple, []),
    {ok, Val};
eval_tuple(_Other)->
    {error, not_a_tuple}.

expand_allstates(Edges, States) ->
    ClearStates = lists:delete(init, lists:delete(terminate, States)),
    lists:foldl(fun(Edge, Acc) ->
                        #edge{vertex1 = FnName,
                              vertex2 = To,
                              edge_data = Data} = Edge,
                        StateIndex = case FnName of
                                         handle_event ->  1;
                                         handle_sync_event -> 2;
                                         handle_info -> 1
                                     end,
                        case To of
                            '@var' ->
                                case lists:nth(StateIndex, Data#edge_data.pattern) of
                                    {atom, _line, StateFrom} ->
                                        Acc ++ lists:map(fun(State) ->
                                                                 #edge{vertex1 = StateFrom,
                                                                       vertex2 = State,
                                                                       edge_data = Data}
                                                         end, ClearStates);
                                    _ ->
                                        Acc ++ lists:map(fun(State) ->
                                                                 #edge{vertex1 = State,
                                                                       vertex2 = State,
                                                                       edge_data = Data}
                                                         end, ClearStates)
                                end;
                            Other ->
                                case lists:nth(StateIndex, Data#edge_data.pattern) of
                                    {atom, _line, StateFrom} ->
                                        Acc ++ [#edge{vertex1 = StateFrom,
                                                      vertex2 = Other,
                                                      edge_data = Data}];
                                    _ ->
                                        Acc ++ lists:map(fun(State) ->
                                                                 #edge{vertex1 = State,
                                                                       vertex2 = Other,
                                                                       edge_data = Data}
                                                         end, ClearStates)
                                end
                        end
                end, [], Edges).

get_vertex(Graph, Label) ->
    Vertices = digraph:vertices(Graph),
    find_label(Label, Vertices, Graph).

find_label(Label, [V|Rest], Graph) ->
    case digraph:vertex(Graph, V) of
        {V, Label} ->
            V;
        _ ->
            find_label(Label, Rest, Graph)
    end;
find_label(_, [], _) ->
    {error, no_label}.

remove_dups([])    -> [];
remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].


abstract_print(Form) ->
    erl_prettypr:format(erl_syntax:form_list([Form])).
