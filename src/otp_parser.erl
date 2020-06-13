-module(otp_parser).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_graph/3,
         create_graph/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_graph(FileName, IncludePaths, Mode)->
    create_graph(FileName, IncludePaths, undefined, Mode).

create_graph(FileName, IncludePaths, OutputDir, Mode)->
    gen_server:call(?MODULE, {create, FileName, IncludePaths, OutputDir, Mode}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{}}.

handle_call({create, FileName, IncludePaths, OutputDir, Mode}, _From, State) ->
    OutputFilename = erlesy_formatter:output_filename(Mode, FileName, OutputDir),
    ok = filelib:ensure_dir(OutputFilename),
    {ok, File} = file:open(OutputFilename, [write]),
    case graph_builder:parse_file(FileName, IncludePaths) of
        {parsed, _, Digraph} ->
            file:write(File, erlesy_formatter:format(Mode, filename:rootname(FileName), Digraph)),
            file:close(File),
            {reply, ok, State};
        {error, _} = Err ->
            {reply, Err, State}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
