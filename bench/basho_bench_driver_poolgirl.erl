-module(basho_bench_driver_poolgirl).

-export([init/0,
         new/1,
         run/4]).

-record(state, { name = undefined }).

init() ->
    ok.

new(Id) ->
    Opts = basho_bench_config:get(poolgirl, []),
    PoolName = list_to_atom("pool" ++ integer_to_list(Id)),
    poolgirl:start_link([{name, {local, PoolName}},
                        {worker_module, poolgirl_test_worker},
                        {size, proplists:get_value(size, Opts)}]),
    {ok, #state{ name = PoolName }}.

run(checkout, _KeyGen, _ValueGen,
    #state{name = PoolName} = State) ->
    case poolgirl:checkout(PoolName) of
        Pid when is_pid(Pid) -> {ok, State};
        _ -> {error, invalid_pid, State}
    end.
