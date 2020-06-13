-module(erlesy_plantuml).

-behaviour(erlesy_formatter).

-include("types.hrl").

%% API
-export([digraph/2]).

digraph(_Name, G) ->
  "@startuml\n"  ++
    edges_to_plantuml(G, digraph:edges(G)) ++
    "@enduml".

edges_to_plantuml(_G, []) ->
  "\n";
edges_to_plantuml(G, [E|T]) ->
  {E, V1, V2, EdgeLabel} = digraph:edge(G, E),
  {_, VLabel1} = digraph:vertex(G, V1),
  {_, VLabel2} = digraph:vertex(G, V2),
  FilteredEvent = erlesy_formatter:filter_newline(lists:flatten(EdgeLabel#edge_data.event)),
  case EdgeLabel#edge_data.guard of
    [] ->
      R= io_lib:format("~s",[FilteredEvent]);
    Guard ->
      R= io_lib:format("~s [~s]",[FilteredEvent, Guard])
  end,

  %EdgePretty=lists:flatten(R),
  case R of
    [[]] ->
      statename(VLabel1) ++ "-->" ++ statename(VLabel2) ++
        "\n" ++
        edges_to_plantuml(G, T);
    _ ->
      statename(VLabel1) ++ "-->" ++ statename(VLabel2) ++
        " : " ++ R ++
        "\n" ++
        edges_to_plantuml(G, T)
  end.

statename(init) ->
  "[*]";
statename(terminate)->
  "[*]";
statename(Other) ->
  atom_to_list(Other).
