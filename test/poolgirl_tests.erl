-module(poolgirl_tests).

-include_lib("eunit/include/eunit.hrl").

pool_test_() ->
    {foreach,
        fun() ->
            {ok, _} = application:ensure_all_started(poolgirl),
            error_logger:tty(false)
        end,
        fun(_) ->
            case whereis(poolgirl_test) of
                undefined -> ok;
                Pid -> poolgirl:stop(Pid)
            end,
            error_logger:tty(true)
        end,
        [
            {<<"Basic pool operations">>,
                fun pool_startup/0
            },
            {<<"Pool behaves on worker death">>,
                fun worker_death/0
            },
            {<<"Pool returns status">>,
                fun pool_returns_status/0
            },
            {<<"Multiple Pools">>,
                fun multiple_pools/0
            },
            {<<"Pool worker spin up">>,
                fun pool_worker_spin_up/0
            },
            {<<"Pool worker spin down">>,
                fun pool_worker_spin_down/0
            },
            {<<"Pool only recruits local workers">>,
                fun pool_only_local_workers/0
            },
            {<<"Pool behaves upon worker depletion">>,
                fun pool_worker_depletion/0
            },
            {<<"Proper cleanup on Pool stop">>,
                fun pool_proper_cleanup_on_stop/0
            },
            {<<"Proper cleanup on Pool death">>,
                fun pool_proper_cleanup_on_death/0
            }
        ]
    }.

%% Tell a worker to exit and await its impending doom.
kill_worker(Pid) ->
    erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', _, process, Pid, _} ->
            ok
    end.

pool_startup() ->
    %% Check basic pool operation.
    {ok, Pid} = new_pool(10),
    ?assertEqual(10, length(poolgirl:get_workers(Pid))),
    poolgirl:checkout(Pid),
    ?assertEqual(10, length(poolgirl:get_workers(Pid))),
    ok = pool_call(Pid, stop).

worker_death() ->
    {ok, Pid} = new_pool(5),
    ?assertEqual(5, length(poolgirl:get_workers(Pid))),
    Worker = poolgirl:checkout(Pid),
    kill_worker(Worker),
    %% a little pause to allow the dust to settle after a death
    timer:sleep(1000),
    ?assertEqual(5, length(poolgirl:get_workers(Pid))),
    ok = pool_call(Pid, stop).

pool_returns_status() ->
    {ok, Pool} = new_pool(2),
    ?assertEqual({ready, 2}, poolgirl:status(Pool)),
    ok = pool_call(Pool, stop).

pool_worker_spin_up() ->
    {ok, Pool} = new_pool(2),
    poolgirl:spin(up, Pool, 2),
    ?assertEqual({ready, 4}, poolgirl:status(Pool)),
    ok = pool_call(Pool, stop).

pool_worker_spin_down() ->
    {ok, Pool} = new_pool(4),
    poolgirl:spin(down, Pool, 2),
    ?assertEqual({ready, 2}, poolgirl:status(Pool)),
    ok = pool_call(Pool, stop).

pool_only_local_workers() ->
    {ok, Pool} = new_pool(5),
    Worker = poolgirl:checkout(Pool),
    ?assertEqual(node(), node(Worker)),
    ok = pool_call(Pool, stop).

pool_worker_depletion() ->
    {ok, Pool} = new_pool(5),
    lists:foreach(fun(_E) ->
                    poolgirl:transaction(Pool,
                        fun(Worker) ->
                            gen_server:cast(Worker, {test_crash, atom})
                        end)
                  end, lists:seq(0, 50)),
    ?assertEqual({ready, 5}, poolgirl:status(Pool)),
    ok = pool_call(Pool, stop).

multiple_pools() ->
    {ok, Pool1} = new_pool(poolgirl_test1, 5),
    {ok, Pool2} = new_pool(poolgirl_test2, 5),
    {ok, Pool3} = new_pool(poolgirl_test3, 5),
    lists:foreach(fun(_E) ->
                    poolgirl:transaction(Pool1,
                        fun(Worker) ->
                            gen_server:cast(Worker, {test_crash, atom})
                        end),
                    poolgirl:transaction(Pool2,
                        fun(Worker) ->
                            gen_server:cast(Worker, {test_crash, atom})
                        end),
                    poolgirl:transaction(Pool3,
                        fun(Worker) ->
                            gen_server:cast(Worker, {test_crash, atom})
                        end)
                  end, lists:seq(0, 50)),
    %% a little pause to allow the dust to settle after a death
    timer:sleep(1000),
    ?assertEqual({ready, 5}, poolgirl:status(Pool1)),
    ?assertEqual({ready, 5}, poolgirl:status(Pool2)),
    ?assertEqual({ready, 5}, poolgirl:status(Pool3)),
    ok = pool_call(Pool1, stop),
    ok = pool_call(Pool2, stop),
    ok = pool_call(Pool3, stop).

pool_proper_cleanup_on_stop() ->
    {ok, Pool1} = new_pool(poolgirl_test1, 5),
    ?assertEqual(1, length(supervisor:which_children(poolgirl_app_sup))),
    ok = pool_call(Pool1, stop),
    %% a little pause to allow the dust to settle after a death
    timer:sleep(500),
    ?assertEqual(0, length(supervisor:which_children(poolgirl_app_sup))).

pool_proper_cleanup_on_death() ->
    {ok, _Pool1} = new_pool(poolgirl_test1, 5),
    ?assertEqual(1, length(supervisor:which_children(poolgirl_app_sup))),
    exit(whereis(poolgirl_test1), kill),
    %% a little pause to allow the dust to settle after a death
    timer:sleep(500),
    ?assertEqual(0, length(supervisor:which_children(poolgirl_app_sup))).

%%
%% Internal
%%

new_pool(Size) ->
    new_pool(poolgirl_test, Size).

new_pool(Name, Size) ->
    poolgirl:start_link([{name, {local, Name}},
                         {worker_module, poolgirl_test_worker},
                         {size, Size}]),
    timer:sleep(500),
    {ok, Name}.

pool_call(ServerRef, Request) ->
    gen_server:call(ServerRef, Request).
