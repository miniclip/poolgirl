-module(poolgirl_pg_tests).

-include_lib("eunit/include/eunit.hrl").

membership_test() ->
    Name = pg_membership,
    ok = poolgirl_pg:create(Name),
    ?assertEqual({error, {no_process, Name}}, poolgirl_pg:get(Name)),
    [A, B, C] = Pids = spawn_idle(3),
    lists:foreach(fun(P) -> ok = poolgirl_pg:join(Name, P) end, Pids),
    ?assertEqual(Pids, poolgirl_pg:get_members(Name)),
    ok = poolgirl_pg:leave(Name, B),
    ?assertEqual([A, C], poolgirl_pg:get_members(Name)),
    ok = poolgirl_pg:leave(Name, C),
    ?assertEqual(A, poolgirl_pg:get(Name)),
    ok = poolgirl_pg:leave(Name, A),
    ?assertEqual({error, {no_process, Name}}, poolgirl_pg:get(Name)),
    ok = poolgirl_pg:delete(Name),
    ?assertEqual({error, {no_such_group, Name}}, poolgirl_pg:get(Name)),
    true = ets:delete(Name),
    stop(Pids).

missing_table_test() ->
    ?assertEqual({error, {no_process, pg_missing}}, poolgirl_pg:get(pg_missing)),
    ?assertEqual([], poolgirl_pg:get_members(pg_missing)).

spread_test() ->
    Name = pg_spread,
    ok = poolgirl_pg:create(Name),
    Pids = spawn_idle(10),
    lists:foreach(fun(P) -> ok = poolgirl_pg:join(Name, P) end, Pids),
    Picks = [poolgirl_pg:get(Name) || _ <- lists:seq(1, 20000)],
    %% back-to-back picks must not stick to the same worker (ideal is 1 in 10)
    ?assert(repeats(Picks, 0) < 3000),
    ?assertEqual(lists:sort(Pids), lists:usort(Picks)),
    true = ets:delete(Name),
    stop(Pids).

spawn_idle(N) ->
    [spawn(fun() -> receive stop -> ok end end) || _ <- lists:seq(1, N)].

stop(Pids) ->
    lists:foreach(fun(P) -> P ! stop end, Pids).

repeats([X, X | T], Count) -> repeats([X | T], Count + 1);
repeats([_ | T], Count) -> repeats(T, Count);
repeats([], Count) -> Count.
