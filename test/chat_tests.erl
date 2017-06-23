-module(chat_tests).

-include_lib("eunit/include/eunit.hrl").

-define (ALICE, "alice").
-define (BOB, "bob").


simple_test_() ->
    {foreach,
        fun () ->
            {ok, S} = chat_server:start_link(),
            {ok, A} = chat_client:start_link(?ALICE),
            {ok, B} = chat_client:start_link(?BOB),
            [S, A, B]
        end,
        fun(Pids) ->
            [ok = kill(Pid) || Pid <- Pids]
        end,
        [
            fun mandatory/1
        ]
    }.


mandatory([S,A,B]=Pids) ->
    fun() ->
        ?assertEqual(not_connected, chat_client:send(A, ?BOB, "test from alice")),
        ?assertEqual(ok, chat_client:connect(A, chat_server)),
        ?assertEqual(ok, chat_client:connect(B, chat_server)),
        timer:sleep(100),
        ?assertEqual(ok, chat_client:send(A, ?BOB, "test from alice")),
        ?assertEqual(ok, chat_client:send(B, ?ALICE, "test from bob")),
        [?assert(erlang:is_process_alive(P)) || P <- Pids],
        SState = sys:get_state(S, 100),
        Clients = maps:from_list([{A, ?ALICE}, {B, ?BOB}]),
        ?assertMatch(Clients, element(2, SState))
    end.




kill(Pid) ->
    Ref = monitor(process, Pid),
    unlink(Pid),
    exit(Pid, kill),
    receive {'DOWN', Ref, _, _, _} -> ok after 1000 -> error end.
