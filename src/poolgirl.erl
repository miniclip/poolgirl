%% Poolgirl - A lightweight Erlang worker pool library

%% @doc The pool API, and the pool manager process.
%%
%% `checkout/1' picks a random worker from a public ETS table that has the
%% same name as the pool. It sends no message to the pool manager, so it is
%% best for workers that take casts and handle each request quickly.
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

-ignore_xref([checkin/2,
              transaction/2,
              get_workers/1,
              start_link/1, start_link/2,
              start_link2/3,
              status/1,
              spin/3,
              child_spec/2, child_spec/3,
              stop/1]).

% Copied from gen:start_ret/0
-type start_ret() :: {'ok', pid()} | {'error', term()}.

-type pool() ::
    Name :: atom().

-record(state, {
    name,
    internal_directory :: poolgirl_internal_directory:t(),
    supervisor = undefined :: undefined | pid(),
    workers = [] :: [{reference(), pid()}],
    worker_module = undefined :: atom(),
    size = 5 :: non_neg_integer()
}).

-hank([{unnecessary_function_arguments, [checkin/2]}]).

%% @doc The same as `child_spec(PoolId, PoolArgs, [])'.
-spec child_spec(PoolId :: term(), PoolArgs :: proplists:proplist())
    -> supervisor:child_spec().
child_spec(PoolId, PoolArgs) ->
    child_spec(PoolId, PoolArgs, []).

%% @doc Returns a child spec to start the pool under your own supervisor.
%%
%% The child is `permanent'. The `poolgirl' application does not supervise
%% this pool.
%% @param PoolId The child id in your supervisor
%% @param PoolArgs The pool options: `name', `worker_module', `size' and
%% `workers'
%% @param WorkerArgs The argument that each worker gets in
%% `WorkerModule:start_link/1'
-spec child_spec(PoolId :: term(),
                 PoolArgs :: proplists:proplist(),
                 WorkerArgs :: proplists:proplist())
    -> supervisor:child_spec().
child_spec(PoolId, PoolArgs, WorkerArgs) ->
    {PoolId, {poolgirl_sup, start_link, [PoolArgs, WorkerArgs]},
     permanent, 5000, supervisor, [poolgirl]}.

%% @doc The same as `start_link(PoolArgs, [])'.
-spec start_link(PoolArgs :: proplists:proplist())
    -> start_ret().
start_link(PoolArgs)  ->
    start_link(PoolArgs, []).

%% @doc Starts the pool under the `poolgirl' application's supervisor.
%%
%% The `poolgirl' application must be running. On success, the returned pid is
%% the pool's top supervisor, not the pool manager. Use the pool name with the
%% other functions.
%% @param PoolArgs The pool options: `name', `worker_module', `size' and
%% `workers'
%% @param WorkerArgs The argument that each worker gets in
%% `WorkerModule:start_link/1'
-spec start_link(PoolArgs :: proplists:proplist(),
                 WorkerArgs :: proplists:proplist())
    -> start_ret().
start_link(PoolArgs, WorkerArgs)  ->
    poolgirl_app_sup:start_child(PoolArgs, WorkerArgs).

%% @private
-spec start_link2(InternalDirectory :: poolgirl_internal_directory:t(),
                  PoolArgs :: proplists:proplist(),
                  WorkerArgs :: proplists:proplist())
    -> start_ret().
start_link2(InternalDirectory, PoolArgs, WorkerArgs) ->
    case proplists:get_value(name, PoolArgs) of
        undefined ->
            gen_server:start_link(?MODULE, {InternalDirectory, PoolArgs, WorkerArgs}, []);
        PoolName ->
            gen_server:start_link(PoolName, ?MODULE, {InternalDirectory, PoolArgs, WorkerArgs}, [])
    end.

%% @doc Returns a random worker from the pool.
%%
%% Returns `no_process' if the pool has no workers or does not exist.
%% It only finds pools on the local node.
%% @param PoolId The pool name
-spec checkout(PoolId :: pool()) -> no_process | pid().
checkout(PoolId) ->
    case poolgirl_pg:get(PoolId) of
        {error, _} -> no_process;
        Pid -> Pid
    end.

%% @doc Does nothing. It exists only for compatibility with `poolboy'.
-spec checkin(PoolId :: pool(), Worker :: pid()) -> ok.
checkin(_PoolId, Worker) when is_pid(Worker) -> ok.

%% @doc Checks out a worker and calls `Fun(Worker)'.
%%
%% Exceptions from `Fun' do not propagate. A throw returns the thrown value, an
%% exit returns `{'EXIT', Reason}' and an error returns
%% `{'EXIT', {Reason, Stacktrace}}'. If the pool has no workers, `Fun' gets
%% `no_process'.
%% @param PoolId The pool name
%% @param Fun The function to call with the worker pid
-spec transaction(PoolId :: pool(), Fun :: fun((Worker :: pid()) -> any()))
    -> any().
transaction(PoolId, Fun) ->
    Worker = poolgirl:checkout(PoolId),
    try
        Fun(Worker)
    catch
        throw:Value -> Value;
        exit:Reason -> {'EXIT', Reason};
        error:Reason:Stacktrace -> {'EXIT', {Reason, Stacktrace}}
    end.

%% @doc Returns `{ready, N}', where `N' is the number of workers.
%% @param PoolId The pool name
-spec status(PoolId :: pool()) -> {atom(), integer()}.
status(PoolId) ->
    gen_server:call(PoolId, status).

%% @doc Adds or removes workers. The change applies at once.
%%
%% Raises a `no_supervisor' error for a pool started with `workers', because
%% that pool has no supervisor to start or stop workers.
%% @param UpOrDown `up' to add workers, `down' to remove them
%% @param PoolId The pool name
%% @param HowMany The number of workers to add or remove
-spec spin(UpOrDown :: up | down, PoolId :: pool(), HowMany :: pos_integer()) -> ok.
spin(up, PoolId, HowMany) ->
    spin_reply(gen_server:call(PoolId, {spin_up, HowMany}));
spin(down, PoolId, HowMany) ->
    spin_reply(gen_server:call(PoolId, {spin_down, HowMany})).

spin_reply(ok) -> ok;
spin_reply({error, Reason}) -> erlang:error(Reason).

%% @doc Stops the pool and all its workers.
%%
%% A pool from `child_spec/2,3' is a `permanent' child, so your supervisor
%% restarts it.
%% @param PoolId The pool name
-spec stop(PoolId :: pool()) -> ok.
stop(PoolId) ->
    {ok, TopSupPid} = gen_server:call(PoolId, get_pid_of_top_pool_supervisor),
    sys:terminate(TopSupPid, _Reason = shutdown).

%% @doc Returns the pids of the pool's workers.
%% @param PoolId The pool name
-spec get_workers(PoolId :: pool()) -> list().
get_workers(PoolId) ->
    poolgirl_pg:get_members(PoolId).

%% @private
init({InternalDirectory, PoolArgs, WorkerArgs}) ->
    _ = process_flag(trap_exit, true), % Always call `:terminate/2' (unless killed)
    init(PoolArgs, WorkerArgs, #state{internal_directory = InternalDirectory}).

%% @private
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
    % create the pg group
    ok = poolgirl_pg:create(State#state.name),
    %% check if we were supplied with pre-started workers
    case State#state.workers of
        [] ->
            {ok, Sup} = poolgirl_internal_directory:find(State#state.internal_directory,
                                                         poolgirl_worker_sup),
            Workers = populate(State#state.size, Sup, State#state.name),
            {ok, State#state{supervisor = Sup,
                             workers = Workers}};
        Workers ->
            lists:foreach(fun({_Ref, Pid}) ->
                            % join the worker to the pg group
                            ok = poolgirl_pg:join(State#state.name, Pid)
                          end, Workers),
            {ok, State#state{supervisor = undefined}}
    end.

%% @private
handle_call(status, _From, #state{supervisor = undefined, workers = Workers} = State) ->
    {reply, {ready, length(Workers)}, State};
handle_call(status, _From, #state{supervisor = Sup} = State) ->
    {reply, {ready, length(supervisor:which_children(Sup))}, State};
handle_call({Spin, _N}, _From, #state{supervisor = undefined} = State)
  when Spin =:= spin_up; Spin =:= spin_down ->
    {reply, {error, no_supervisor}, State};
handle_call({spin_up, N}, _From, #state{name = PoolName,
                                        size = Size,
                                        supervisor = Sup,
                                        workers = Workers} = State) ->
    NewWorkers = populate(N, Sup, PoolName),
    {reply, ok, State#state{size = Size + N, workers = Workers ++ NewWorkers}};
handle_call({spin_down, N}, _From, #state{name = PoolName,
                                          size = Size,
                                          supervisor = Sup,
                                          workers = Workers} = State) ->
    % get a sublist of workers to spin down
    Victims = lists:sublist(Workers, N),
    lists:foreach(fun({WorkerRef, WorkerPid}) ->
                    % flush: a victim that has just died must not get a replacement
                    erlang:demonitor(WorkerRef, [flush]),
                    kill_worker(Sup, WorkerPid),
                    poolgirl_pg:leave(PoolName, WorkerPid)
                  end, Victims),
    {reply, ok, State#state{size = Size - length(Victims),
                            workers = Workers -- Victims}};
handle_call(get_pid_of_top_pool_supervisor, _From, State) ->
    #state{internal_directory = InternalDirectory} = State,
    Reply = {ok, _Pid} = poolgirl_internal_directory:find(InternalDirectory, poolgirl_sup),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
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
    {noreply, State#state{workers = (Workers -- [{Ref, Pid}]) ++ [NewWorker]}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{name = PoolName}) ->
    ok = poolgirl_pg:delete(PoolName),
    ok.

%% @private
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
    populate(N - 1, Sup, PoolName, Acc ++ [{Ref, Pid}]).
