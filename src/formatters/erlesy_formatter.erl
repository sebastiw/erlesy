-module(erlesy_formatter).

-callback digraph(Name :: string(), Graph :: digraph:digraph()) -> iodata().

-export([format/3, output_filename/3]).
-export([filter_newline/1]).

format(Mode, Name, Digraph) ->
    (callback(Mode)):digraph(Name, Digraph).

callback(dot) ->
    erlesy_dot;
callback(plantuml) ->
    erlesy_plantuml.

output_filename(Mode, FileName, undefined) ->
    filename:rootname(FileName) ++ output_extension(Mode);
output_filename(Mode, FileName, OutputDir) ->
    filename:join([OutputDir, filename:basename(filename:rootname(FileName)) ++ output_extension(Mode)]).

output_extension(dot) -> ".gv";
output_extension(plantuml) -> ".txt".

filter_newline(List)->
  filter_newline(List, not_found, []).

filter_newline([$\n|T], found, Acc) ->
  filter_newline(T, not_found, Acc ++ [$\\]);
filter_newline([$\n|T], not_found, Acc) ->
  filter_newline(T, not_found, Acc);
filter_newline([$n|T], found, Acc) ->
  filter_newline(T, not_found, Acc);
filter_newline([$n|T], not_found, Acc) ->
  filter_newline(T, not_found, Acc ++ [$n]);
filter_newline([$\\|T], not_found, Acc) ->
  filter_newline(T, found, Acc);
filter_newline([$\\|T], found, Acc) ->
  filter_newline(T, not_found, Acc ++ [$\\]);
filter_newline([H|T], found, Acc) ->
  filter_newline(T, not_found, Acc ++ [$\\, H]);
filter_newline([H|T], not_found, Acc) ->
  filter_newline(T, not_found, Acc ++ [H]);
filter_newline([], _, Acc) ->
  Acc.
