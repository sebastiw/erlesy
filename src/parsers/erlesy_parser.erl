-module(erlesy_parser).

-callback parse(AbstractForm :: erl_syntax:abstract_form()) -> digraph:digraph().

-export([parse/1]).

parse(AbstractForm) ->
    case get_behaviour(AbstractForm) of
        {ok, B} ->
            (callback(B)):parse(AbstractForm);
        {error, _} = Err ->
            Err
    end.

get_behaviour(AbstractForm) ->
    Behaviours = [T || {attribute, _, B, T} <- AbstractForm,
                       B == behaviour orelse B == behavior],
    Parsable = lists:dropwhile(
                 fun (B) ->
                         not lists:member(B, [gen_statem, gen_fsm])
                 end,
                 Behaviours),
    case Parsable of
        [] ->
            {error, no_parsable_behaviour_found};
        [First|_] ->
            {ok, First}
    end.

callback(gen_statem) ->
    erlesy_statem;
callback(gen_fsm) ->
    erlesy_fsm.
