# poolgirl - A lightweight Erlang worker pool library [![GitHub Actions CI][ci-img]][ci]

[ci]: https://github.com/miniclip/poolgirl
[ci-img]: https://github.com/miniclip/poolgirl/workflows/build/badge.svg

Poolgirl is a small, generic worker pool library for Erlang. It is an
alternative to [poolboy](https://github.com/devinus/poolboy) for workers that
take cast requests, need no reply, and handle each request quickly. Analytics
and stats reporting are typical cases.

The API and the options are close to poolboy's, so switching between the two
takes little work.

## Usage

```erl-sh
1> Worker = poolgirl:checkout(PoolName).
<0.9001.0>
2> gen_server:cast(Worker, Request).
ok
```

## How it works

Poolboy uses one manager process per pool. Each checkout sends a message to
it, so with many workers the manager can become a bottleneck.

Poolgirl takes the manager out of the checkout path. Each pool has a public
ETS table with the same name as the pool, and the table holds the pids of the
pool's workers. `checkout/1` reads this table in the calling process and picks
a worker at random. It sends no message to any process. `checkin/2` does
nothing. It exists only to make a move from poolboy easier.

The manager still starts the workers and monitors them. When a worker dies,
the manager removes it from the table and starts a replacement.

Each pool runs under its own supervision tree:

```
poolgirl_sup (rest_for_one)
├── poolgirl_worker_sup (simple_one_for_one, the workers)
└── poolgirl (gen_server, the pool manager, owns the ETS table)
```

## When not to use it

Workers are picked at random. Poolgirl does not check whether a worker is busy,
so a slow worker's message queue can grow. If your workers handle long
synchronous requests, use poolboy.

The ETS table is local to the node, so `checkout/1` only returns workers from
the node where the pool runs.

## API

- `checkout(Pool)`: returns a random worker pid, or `no_process` if the pool
  has no workers or does not exist.
- `checkin(Pool, Worker)`: does nothing and returns `ok`.
- `transaction(Pool, Fun)`: checks out a worker and calls `Fun(Worker)`.
  Exceptions do not propagate: a throw returns the thrown value, an exit
  returns `{'EXIT', Reason}` and an error returns
  `{'EXIT', {Reason, Stacktrace}}`. If the pool has no workers, `Fun` gets
  `no_process`.
- `get_workers(Pool)`: returns the list of worker pids.
- `status(Pool)`: returns `{ready, NumberOfWorkers}`.
- `spin(up | down, Pool, N)`: adds or removes `N` workers. The change applies
  at once. For a pool started with `workers`, it raises a `no_supervisor`
  error.
- `child_spec(PoolId, PoolArgs)` and `child_spec(PoolId, PoolArgs, WorkerArgs)`:
  return a child spec to start the pool under your own supervisor.
- `start_link(PoolArgs)` and `start_link(PoolArgs, WorkerArgs)`: start the pool
  under the `poolgirl` application's supervisor. The `poolgirl` application
  must be running.
- `stop(Pool)`: stops the pool and all its workers. A pool from `child_spec/2,3`
  is a `permanent` child, so your supervisor restarts it.

## Options

- `name`: the pool name, as `{local, Name}`, `{global, Name}` or
  `{via, Module, Name}`. `Name` must be an atom, and it is also the name of the
  pool's ETS table.
- `worker_module`: the module that implements the workers.
- `size`: the number of workers to start. The default is 5. Use `spin/3` to
  change it later.
- `workers`: a list of pids that are already running, to use instead of
  starting new workers. The pool monitors them, but it does not replace one
  that dies.

## Worker modules

A worker module implements the `poolgirl_worker` behaviour, which has one
callback: `start_link(WorkerArgs)`. Each worker gets the `WorkerArgs` given to
`child_spec/3` or `start_link/2`.

## Example

This example application reports statsd counters over UDP. Each worker owns a
UDP socket and only handles casts, which is the workload Poolgirl is built for.
It uses only `gen_udp` from kernel.

`increment/2` does not check the result of `checkout/1`. If the pool has no
workers or does not exist, `checkout/1` returns `no_process`, and
`gen_server:cast/2` drops the message without an error. For stats, losing a
counter is acceptable.

### example.app

```erlang
{application, example, [
    {description, "An example application"},
    {vsn, "0.1"},
    {applications, [kernel, stdlib, poolgirl]},
    {modules, [example, example_worker]},
    {registered, [example_sup]},
    {mod, {example, []}},
    {env, [
        {pools, [
            {stats, [
                {size, 10}
            ], [
                {host, "127.0.0.1"},
                {port, 8125}
            ]},
            {analytics, [
                {size, 5}
            ], [
                {host, "127.0.0.1"},
                {port, 8126}
            ]}
        ]}
    ]}
]}.
```

### example.erl

```erlang
-module(example).
-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0, increment/2]).
-export([start/2, stop/1]).
-export([init/1]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, example_sup}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    {ok, Pools} = application:get_env(example, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, example_worker}] ++ SizeArgs,
        poolgirl:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

increment(PoolName, Metric) ->
    gen_server:cast(poolgirl:checkout(PoolName), {increment, Metric}).
```

### example_worker.erl

```erlang
-module(example_worker).
-behaviour(gen_server).
-behaviour(poolgirl_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {socket, host, port}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    {ok, Socket} = gen_udp:open(0),
    {ok, #state{socket = Socket, host = Host, port = Port}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({increment, Metric}, #state{socket = Socket, host = Host,
                                         port = Port} = State) ->
    _ = gen_udp:send(Socket, Host, Port, [Metric, ":1|c"]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    ok = gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

Usage:

```erl-sh
1> application:ensure_all_started(example).
{ok,[poolgirl,example]}
2> example:increment(stats, <<"page.views">>).
ok
```

## Original Author

- Luis Rascão (lrascao) <luis.rascao@gmail.com>

## License

Poolgirl is available under the MIT license (see [LICENSE](LICENSE)).
