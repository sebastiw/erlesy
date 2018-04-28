-module(erlesy_tests).

-include_lib("eunit/include/eunit.hrl").

create_graph_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        % Gen Fsm
        {"fsm -> dot", fun create_graph_gen_fsm_dot/0},

        % Gen StateM
        {"statem -> dot", fun create_graph_gen_statem_dot/0}
     ]
    }.

create_graph_gen_fsm_dot() ->
    ?assert(ok == otp_parser:create_graph("priv/example_fsm.erl", [], dot)),
    ?assert(ok == wait_unitl_file_complete("priv/example_fsm.gv")),
    {ok, A} = file:read_file("priv/example_fsm_expected.gv"),
    {ok, B} = file:read_file("priv/example_fsm.gv"),
    ?assert(true =:= compare_by_line(A, B)).

create_graph_gen_statem_dot() -> 
    ?assert(ok == otp_parser:create_graph("priv/example_statem.erl", [], dot)), 
    ?assert(ok == wait_unitl_file_complete("priv/example_statem.gv")), 
    {ok, A} = file:read_file("priv/example_statem_expected.gv"),
    {ok, B} = file:read_file("priv/example_statem.gv"),
    ?assert(true =:= compare_by_line(A, B)).

%%------------------------------------------------------------------------------------

setup() ->
    otp_parser_app:start(),
    ?assert(undefined /= whereis(otp_parser) ),
    ?assert(undefined /= whereis(graph_builder) ),
    ?assert(undefined /= whereis(erlesy_fs) ),
    setup.

cleanup(setup) ->
    ok.

wait_unitl_file_complete(ExpectedFile) ->
    wait_loop(ExpectedFile, 100, 20).

wait_loop(_ExpectedFile, _Duration, 0) ->
    {error, not_completed_done_waiting};

wait_loop(ExpectedFile, Duration, Times) when Times > 0 ->
    timer:sleep(Duration),
    case filelib:is_file(ExpectedFile) of
        true ->
            ok;
        false ->
            wait_loop(ExpectedFile, Duration, Times-1)
    end.

%% expects every line in a to be contained in b and vice versa
compare_by_line(Binary_a, Binary_b) -> 
    List_a = [binary_to_list(Bin) || Bin <- binary:split(Binary_a,<<"\n">>,[global])], 
    List_b = [binary_to_list(Bin) || Bin <- binary:split(Binary_b,<<"\n">>,[global])], 
    case length(List_a) =:= length(List_b) of
        true -> list_include(List_a, List_b) and list_include(List_b, List_a);
        false -> false
    end.

list_include([],_) -> true;
list_include([H|T], B) ->
    case lists:member(H, B) of
        true -> list_include(T,B);
        false -> false
    end.

