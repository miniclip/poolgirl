%% Poolgirl - A sexy Erlang worker pool factory

-module(poolgirl).
-behaviour(gen_server).

% behaviour callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% external api
-export([checkout/1,
         checkin/2,
         transaction/2,
         get_workers/1,
         start_link/1, start_link/2,
         start_link2/3,
         status/1,
         spin/3,
         child_spec/2, child_spec/3,
         stop/1]).

% Copied from gen:start_ret/0
-type start_ret() :: {'ok', pid()} | 'ignore' | {'error', term()}.

-type pool() ::
    Name :: atom().

-record(state, {
    name,
    parent = undefined :: undefined | pid(),
    supervisor = undefined :: undefined | pid(),
    workers = [] :: [{reference(), pid()}],
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

-spec start_link(PoolArgs :: proplists:proplist())
    -> start_ret().
start_link(PoolArgs)  ->
    start_link(PoolArgs, []).

-spec start_link(PoolArgs :: proplists:proplist(),
                 WorkerArgs :: proplists:proplist())
    -> start_ret().
start_link(PoolArgs, WorkerArgs)  ->
    poolgirl_app_sup:start_child(PoolArgs, WorkerArgs).

-spec start_link2(Parent :: pid(),
                  PoolArgs :: proplists:proplist(),
                  WorkerArgs :: proplists:proplist())
    -> start_ret().
start_link2(Parent, PoolArgs, WorkerArgs) ->
    case proplists:get_value(name, PoolArgs) of
        undefined ->
            gen_server:start_link(?MODULE, {Parent, PoolArgs, WorkerArgs}, []);
        PoolName ->
            gen_server:start_link(PoolName, ?MODULE, {Parent, PoolArgs, WorkerArgs}, [])
    end.

-spec checkout(PoolName :: pool()) -> no_process | pid().
checkout(PoolName) ->
    case poolgirl_pg:get(PoolName) of
        {error, _} -> no_process;
        Pid -> Pid
    end.

-spec checkin(Pool :: pool(), Worker :: pid()) -> ok.
checkin(_Pool, Worker) when is_pid(Worker) -> ok.

-spec transaction(Pool :: pool(), Fun :: fun((Worker :: pid()) -> any()))
    -> any().
transaction(Pool, Fun) ->
    Worker = poolgirl:checkout(Pool),
    catch Fun(Worker).

-spec status(Pool :: pool()) -> {atom(), integer()}.
status(Pool) ->
    gen_server:call(Pool, status).

-spec spin(up | down, Pool :: pool(), N :: pos_integer()) -> ok.
spin(up, Pool, N) ->
    gen_server:call(Pool, {spin_up, N});
spin(down, Pool, N) ->
    gen_server:call(Pool, {spin_down, N}).

-spec stop(Pool :: pool()) -> ok.
stop(Pool) ->
    gen_server:call(Pool, stop).

-spec get_workers(Pool :: pool()) -> list().
get_workers(Pool) ->
    poolgirl_pg:get_members(Pool).

init({Parent, PoolArgs, WorkerArgs}) ->
    init(PoolArgs, WorkerArgs, #state{parent = Parent}).

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
init([{workers, Workers0} | Rest], WorkerArgs, State) when is_list(Workers0) ->
    %% we got a pre started pid list
    Workers = lists:map(fun(Pid) ->
                            Ref = erlang:monitor(process, Pid),
                            {Ref, Pid}
                        end, Workers0),
    init(Rest, WorkerArgs, State#state{size = length(Workers),
                                       workers = Workers});
init([_ | Rest], WorkerArgs, State) ->
    init(Rest, WorkerArgs, State);
init([], _WorkerArgs, State) ->
    gen_server:cast(self(), init),
    {ok, State}.

handle_call(status, _From, #state{supervisor = Sup} = State) ->
    {reply, {ready, length(supervisor:which_children(Sup))}, State};
handle_call({spin_up, N}, _From, #state{name = PoolName,
                                        size = Size,
                                        supervisor = Sup,
                                        workers = Workers} = State) ->
    NewWorkers = populate(N, Sup, PoolName),
    {reply, ok, State#state{size = Size + N, workers = Workers ++ NewWorkers}};
handle_call({spin_down, N}, _From, #state{name = PoolName,
                                          supervisor = Sup,
                                          workers = Workers} = State) ->
    % get a sublist of workers to spin down
    Victims = lists:sublist(Workers, N),
    lists:foreach(fun({WorkerRef, WorkerPid}) ->
                    erlang:demonitor(WorkerRef),
                    kill_worker(Sup, WorkerPid),
                    poolgirl_pg:leave(PoolName, WorkerPid)
                  end, Victims),
    {reply, ok, State#state{workers = Workers -- Victims}};
handle_call(stop, _From, #state{name = PoolName,
                                supervisor = undefined} = State) ->
    poolgirl_pg:delete(PoolName),
    {stop, normal, ok, State};
handle_call(stop, _From, State) ->
    gen_server:cast(self(), stop),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, State}.

handle_cast(init, #state{name = PoolName,
                         parent = Parent,
                         size = Size,
                         workers = Workers0} = State) ->
    % create the pg group
    ok = poolgirl_pg:create(PoolName),
    %% check if we were supplied with pre-started workers
    case Workers0 of
        [] ->
            %% get the chilren of our supervisor
            %% our brother is the worker supervisor
            Children = supervisor:which_children(Parent),
            {poolgirl_worker_sup, Sup, supervisor, [poolgirl_worker_sup]} =
              lists:keyfind(poolgirl_worker_sup, 1, Children),
            Workers = populate(Size, Sup, PoolName),
            {noreply, State#state{supervisor = Sup,
                                  workers = Workers}};
        _ ->
            lists:foreach(fun({_Ref, Pid}) ->
                            % join the worker to the pg group
                            ok = poolgirl_pg:join(PoolName, Pid)
                          end, Workers0),
            {noreply, State#state{supervisor = undefined,
                                  workers = Workers0}}
    end;
handle_cast(stop, #state{name = PoolName,
                         parent = Parent} = State) ->
    poolgirl_pg:delete(PoolName),
    ok = supervisor:terminate_child(poolgirl_app_sup, Parent),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason},
    #state{name = PoolName,
           supervisor = undefined,
           workers = Workers} = State) ->
    % remove the worker from the pg group
    ok = poolgirl_pg:leave(PoolName, Pid),
    {noreply, State#state{workers = Workers -- [{Ref, Pid}]}};
handle_info({'DOWN', Ref, process, Pid, _Reason},
    #state{name = PoolName,
           supervisor = Sup,
           workers = Workers} = State) ->

    % remove the worker from the pg group
    ok = poolgirl_pg:leave(PoolName, Pid),
    % spin up a new worker to replace the one that just died
    NewPid = new_worker(Sup),
    % we want a message if the worker dies
    NewRef = erlang:monitor(process, NewPid),
    NewWorker = {NewRef, NewPid},
    % join the worker to the pg2 group
    ok = poolgirl_pg:join(PoolName, NewPid),
    {noreply, State#state{workers = Workers -- [{Ref, Pid}] ++ [NewWorker]}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{name = PoolName}) ->
    ok = poolgirl_pg:delete(PoolName),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%%  internal
%%

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
    ok = poolgirl_pg:join(PoolName, Pid),
    populate(N-1, Sup, PoolName, Acc ++ [{Ref, Pid}]).
