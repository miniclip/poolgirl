# poolgirl - A sexy Erlang worker pool factory [![GitHub Actions CI][ci-img]][ci]

[ci]: https://github.com/miniclip/poolgirl
[ci-img]: https://github.com/miniclip/poolgirl/workflows/build/badge.svg

Poolgirl is a **lightweight**, **generic** pooling library for Erlang with a
focus on **simplicity**, **performance**, and **rock-solid** disaster recovery.
Poolgirl is Poolboy's little sister, it's directed towards the use-case of simple
workers that process requests with no need for a reply (ie. that only handle cast
requests)

## Usage

```erl-sh
1> Worker = poolgirl:checkout(PoolName).
<0.9001.0>
2> gen_server:call(Worker, Request).
ok
```

Q: Is this in any way related with poolboy?
A: Yes, the interface and configuration is mostly the same, however poolgirl adds
some additional methods.

Q: So what's the difference, why another worker pool manager?
A: Poolboy's manager of workers is a single gen_server process, in pools with a
large number of workers the manager itself can become a bottleneck, a possible
solution is to have multiple managers and load balance these.
In use-case scenarios where the pool workers only handle cast requests and have a
fast execution time per request (eg. analytics reporting, stats reporting) poolgirl
overcomes this issue and lets you have high worker numbers with no additional overhead.

Q: How does it work under the hood?
A: When a client requests a worker from the pool no messages are sent to any other
process in order to accomplish this, poolgirl accesses a pg2 process group and recruits
a random worker.

Q: But won't that overload the workers message queue? You're basically choosing a
worker at random.
A: That is true, hence the fact the poolgirl is best suited to the use-case described
above, if you're making long synchronous requests to workers then you're probably
better off sticking with poolboy.

Q: So if it's the client that recruits the worker what does the checkin operation
do?
A: Nothing, it's there basically to ease the transition between poolboy <-> poolgirl

## API

- `checkout/1`: recruits a worker from the requested pool
- `checkin/1`: does nothing
- `transaction/2`: recruits a worker from the requested pool and executes a funcion
with the worker pid as argument
- `get_workers/1`: returns a list of worker pids
- `status/1`: returns the pool status
- `spin/3`: spins up/down pool workers, these are immediately added/removed from
the worker pool

## Example

This is an example application showcasing database connection pools using
Poolgirl and [epgsql](https://github.com/epgsql/epgsql).

### example.app

```erlang
{application, example, [
    {description, "An example application"},
    {vsn, "0.1"},
    {applications, [kernel, stdlib, sasl, crypto, ssl]},
    {modules, [example, example_worker]},
    {registered, [example]},
    {mod, {example, []}},
    {env, [
        {pools, [
            {pool1, [
                {size, 10}
            ], [
                {hostname, "127.0.0.1"},
                {database, "db1"},
                {username, "db1"},
                {password, "abc123"}
            ]},
            {pool2, [
                {size, 5}
            ], [
                {hostname, "127.0.0.1"},
                {database, "db2"},
                {username, "db2"},
                {password, "abc123"}
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

-export([start/0, stop/0, squery/2, equery/3]).
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

squery(PoolName, Sql) ->
    poolgirl:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
    end).

equery(PoolName, Stmt, Params) ->
    poolgirl:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
    end).
```

### example_worker.erl

```erlang
-module(example_worker).
-behaviour(gen_server).
-behaviour(poolgirl_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    {ok, Conn} = epgsql:connect(Hostname, Username, Password, [
        {database, Database}
    ]),
    {ok, #state{conn=Conn}}.

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
    {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

## Options

- `name`: the pool name
- `worker_module`: the module that represents the workers
- `size`: maximum pool size

## Original Author

- Luis Rascão (lrascao) <luis.rascao@gmail.com>

## License

Poolgirl is available under the MIT license (see [LICENSE](LICENSE)).
