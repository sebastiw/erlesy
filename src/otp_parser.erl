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

handle_call({create, FileName, IncludePaths, OutputDir, dot}, _From, State) ->
    OutputFilename = output_filename(FileName, OutputDir, dot),
    ok = filelib:ensure_dir(OutputFilename),
    {ok, File} = file:open(OutputFilename, [write]),
    {parsed, _, Digraph} = graph_builder:parse_file(FileName, IncludePaths),
    file:write(File, dot:digraph_to_dot(filename:rootname(FileName), Digraph)), 
    file:close(File),

    {reply, ok, State}.

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
output_filename(FileName, undefined, Mode) ->
    filename:rootname(FileName) ++ output_extension(Mode);
output_filename(FileName, OutputDir, Mode) ->
    filename:join([OutputDir, filename:basename(filename:rootname(FileName)) ++ output_extension(Mode)]).

output_extension(dot) -> ".gv".
