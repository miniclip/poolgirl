%% Poolgirl - A sexy Erlang worker pool factory

-module(poolgirl).
-behaviour(gen_server).

% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% external api
-export([checkout/1, checkin/2, transaction/2, start_link/1, start_link/2, status/1,
         spin/3, child_spec/2, child_spec/3]).

% Copied from gen:start_ret/0
-type start_ret() :: {'ok', pid()} | 'ignore' | {'error', term()}.

-type pool() ::
    Name :: atom().

-record(state, {
	name,
    supervisor :: pid(),
    workers :: [tuple(reference(), pid())],
	worker_module = undefined :: atom(),
    size = 5 :: non_neg_integer()
}).

-spec child_spec(PoolId :: term(), PoolArgs :: proplists:proplist())
    -> supervisor:child_spec().
child_spec(PoolId, PoolArgs) ->
    child_spec(PoolId, PoolArgs, []).

-spec child_spec(PoolId :: term(),
                 PoolArgs :: proplists:proplist(),
                 WorkerArgs :: proplists:proplist())
    -> supervisor:child_spec().
child_spec(PoolId, PoolArgs, WorkerArgs) ->
    {PoolId, {poolgirl, start_link, [PoolArgs, WorkerArgs]},
     permanent, 5000, worker, [poolgirl]}.

-spec checkout(Pool :: pool()) -> pid().
checkout(Pool) ->
    pg2:get_closest_pid(Pool).

-spec checkin(Pool :: pool(), Worker :: pid()) -> ok.
checkin(_Pool, Worker) when is_pid(Worker) -> ok.

-spec transaction(Pool :: pool(), Fun :: fun((Worker :: pid()) -> any()))
    -> any().
transaction(Pool, Fun) ->
    Worker = poolgirl:checkout(Pool),
    try
        Fun(Worker)
    after
        ok = poolgirl:checkin(Pool, Worker)
    end.

-spec status(Pool :: pool()) -> {atom(), integer()}.
status(Pool) ->
    gen_server:call(Pool, status).

-spec spin(up | down, Pool :: pool(), N :: pos_integer()) -> ok.
spin(up, Pool, N) ->
    gen_server:call(Pool, {spin_up, N});
spin(down, Pool, N) ->
    gen_server:call(Pool, {spin_down, N}).

-spec start_link(PoolArgs :: proplists:proplist())
    -> start_ret().
start_link(PoolArgs)  ->
    start_link(PoolArgs, []).

-spec start_link(PoolArgs :: proplists:proplist(),
                 WorkerArgs :: proplists:proplist())
    -> start_ret().
start_link(PoolArgs, WorkerArgs)  ->
    start_pool(start_link, PoolArgs, WorkerArgs).

init({PoolArgs, WorkerArgs}) ->
    init(PoolArgs, WorkerArgs, #state{}).

init([{name, {local, PoolName}} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{name = PoolName});
init([{name, {global, PoolName}} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{name = PoolName});
init([{name, {via, _, PoolName}} | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State#state{name = PoolName});
init([{worker_module, Mod} | Rest], WorkerArgs, State) when is_atom(Mod) ->
    init(Rest, WorkerArgs, State#state{worker_module = Mod});
init([{size, Size} | Rest], WorkerArgs, State) when is_integer(Size) ->
    init(Rest, WorkerArgs, State#state{size = Size});
init([_ | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State);
init([], WorkerArgs, #state{name = PoolName, size = Size, worker_module = Mod} = State) ->
    % create the pg2 group
    ok = pg2:create(PoolName),
	% start up the worker's supervisor and spin the requested number of workers
    {ok, Sup} = poolgirl_sup:start_link(Mod, WorkerArgs),
	Workers = populate(Size, Sup, PoolName),
    {ok, State#state{supervisor = Sup, workers = Workers}}.

handle_call(status, _From, #state{supervisor = Sup} = State) ->
    {reply, {ready, length(supervisor:which_children(Sup)), 0, 0}, State};
handle_call({spin_up, N}, _From, #state{name = PoolName,
                                        size = Size,
                                        supervisor = Sup,
                                        workers = Workers} = State) ->
    NewWorkers = populate(N, Sup, PoolName),
    {reply, ok, State#state{size = Size + N, workers = Workers ++ NewWorkers}};
handle_call({spin_down, N}, _From, #state{supervisor = Sup,
                                          workers = Workers} = State) ->
    % get a sublist of workers to spin down
    Victims = lists:sublist(Workers, N),
    lists:foreach(fun({WorkerRef, WorkerPid}) ->
                    erlang:demonitor(WorkerRef),
                    kill_worker(Sup, WorkerPid)
                  end, Victims),
    {reply, ok, State#state{workers = Workers -- Victims}};
handle_call(get_avail_workers, _From, #state{supervisor = Sup} = State) ->
    Workers = supervisor:which_children(Sup),
    {reply, Workers, State};
handle_call(get_all_workers, _From, #state{supervisor = Sup} = State) ->
    WorkerList = supervisor:which_children(Sup),
    {reply, WorkerList, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Reference, process, Pid, _Reason},
    #state{name = PoolName,
           supervisor = Sup,
           workers = Workers} = State) ->

    % remove the worker from the pg2 group
    ok = pg2:leave(PoolName, Pid),
    % spin up a new worker to replace the one that just died
    NewPid = new_worker(Sup),
    % we want a message if the worker dies
    NewRef = erlang:monitor(process, Pid),
    NewWorker = {NewRef, NewPid},
    % join the worker to the pg2 group
    ok = pg2:join(PoolName, NewPid),
    {noreply, State#state{workers = Workers ++ [NewWorker]}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    true = exit(State#state.supervisor, shutdown),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%%	internal
%%

start_pool(StartFun, PoolArgs, WorkerArgs) ->
    case proplists:get_value(name, PoolArgs) of
        undefined ->
            gen_server:StartFun(?MODULE, {PoolArgs, WorkerArgs}, []);
        PoolName ->
            gen_server:StartFun(PoolName, ?MODULE, {PoolArgs, WorkerArgs}, [])
    end.

new_worker(Sup) ->
    {ok, Pid} = supervisor:start_child(Sup, []),
    Pid.

kill_worker(Sup, Pid) ->
    ok = supervisor:terminate_child(Sup, Pid).

populate(N, Sup, PoolName) ->
    populate(N, Sup, PoolName, []).

populate(0, _Sup, _PoolName, Acc) ->
    Acc;
populate(N, Sup, PoolName, Acc) ->
	Pid = new_worker(Sup),
    % we want a message if the worker dies
    Ref = erlang:monitor(process, Pid),
    % join the worker to the pg2 group
    ok = pg2:join(PoolName, Pid),
    populate(N-1, Sup, PoolName, Acc ++ [{Ref, Pid}]).
