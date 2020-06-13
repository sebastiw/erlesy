-module(erlesy_statem).

-behaviour(erlesy_parser).

-export([parse/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

parse(AbstractForm) ->
    Graph = digraph:new(),
    Edges = parse_functions(AbstractForm),
    add_vertices(Edges, Graph),
    {ok, Graph}.

parse_functions(AbstractForm) ->
    lists:foldl(fun parse_function/2, [], AbstractForm).

parse_function({function, _, init, 1, Clauses}, Acc) ->
    ReturnResults = parse_clauses(Clauses),
    [#edge{vertex1 = init, vertex2 = NextState, edge_data = EdgeData}
     || {NextState, EdgeData} <- ReturnResults] ++ Acc;
parse_function({function, _, handle_event, 4, Clauses}, Acc) ->
    Acc;
parse_function({function, _, FnName, 3, Clauses}, Acc) ->
    Acc;
parse_function(_Other, Acc) ->
    Acc.

parse_clauses(Clauses) ->
    lists:foldl(fun parse_clause/2, [], Clauses).

parse_clause({clause, _Line, Args, Guards, Body}, Acc) ->
    PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
    PrettyBody = erl_pp:exprs(Body),
    PrettyArgs = erl_pp:exprs(Args),
    EdgeData = #edge_data{
                  event = "",
                  args = PrettyArgs,
                  pattern = Args,
                  guard = PrettyGuards,
                  code = PrettyBody,
                  attributes = [async]},
    [R|_B] = lists:reverse(Body),
    case lists:flatten(parse_statement(R)) of
        [] ->
            ?LOG_WARNING(#{could_not_find_next_state => R}),
            Acc;
        ReturnStatements ->
            [{Ret, EdgeData} || Ret <- ReturnStatements] ++ Acc
    end.

parse_clause_var_test() ->
    {description,
     "`init({Res, Data}) -> {ok, Res, Data}.`",
     ?assertMatch([{{'@var', 'Res'}, _}],
                  parse_clauses([{clause,1,
                                  [{tuple,1,[{var,1,'Res'},{var,1,'Data'}]}],
                                  [],
                                  [{tuple,2,
                                    [{atom,2,ok},
                                     {var,2,'Res'},
                                     {var,2,'Data'}]}]}]))}.
parse_clause_atom_test() ->
    {description,
     "`init({Data,Actions}) -> {ok, state_0, Data, Actions}.`",
     ?assertMatch([{state_0, _}],
                  parse_clauses([{clause,1,
                                  [{tuple,1,[{var,1,'Data'},{var,1,'Actions'}]}],
                                  [],
                                  [{tuple,2,
                                    [{atom,2,ok},
                                     {atom,2,state_0},
                                     {var,2,'Data'},
                                     {var,2,'Actions'}]}]}]))}.

%%% @doc
%%%   Pick out all clauses that return a tuple.
%%% @end
parse_statement({'if',_,Clauses}) ->            % Ifs
    [parse_statement(C) || C <- Clauses];
parse_statement({'case',_,_,Clauses}) ->        % Cases
    [parse_statement(C) || C <- Clauses];
parse_statement({match,_,_L,_R}) ->             % Assignment
    [];
parse_statement({call,_,_Fun,_Args}) ->         % Fun call
    [];
parse_statement({clause,_,_Args,_Guards,Body}) -> % Case/if/fun clause
    [B|_] = lists:reverse(Body),
    parse_statement(B);
parse_statement({tuple,_,Elements}) ->          % tuple
    parse_tuple(Elements);
parse_statement({atom,_,ignore}) ->             % ignore
    [terminate];
parse_statement(NotSupported) ->
    ?LOG_INFO(#{not_parsed => NotSupported}),
    [].

parse_tuple([{atom,_,ok},{atom,_,State},_Data|_Actions]) ->
    [State];
parse_tuple([{atom,_,ok},{var,_,VarName},_Data|_Actions]) ->
    [{'@var', VarName}];
parse_tuple([{atom,_,stop},_Reason]) ->
    [terminate];
parse_tuple(_E) ->
    [].

%%% @doc
%%%   For all edges, add the unique set of vertices.
%%% @end
add_vertices(Edges, Graph) ->
    Vs = lists:foldl(fun (#edge{} = E, Acc) ->
                             [E#edge.vertex1, E#edge.vertex2|Acc]
                     end, [], Edges),
    [digraph:add_vertex(Graph, digraph:add_vertex(Graph), V)
     || V <- lists:usort(Vs)].
