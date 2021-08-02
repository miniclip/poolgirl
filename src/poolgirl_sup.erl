%% @private
%% Poolbgirl - A sexy Erlang worker pool factory

-module(poolgirl_sup).
-behaviour(supervisor).

-export([start_link/2]).

%% supervisor.
-export([init/1]).

-ignore_xref([start_link/2]).

start_link(PoolArgs, WorkerArgs) ->
    supervisor:start_link(?MODULE, {PoolArgs, WorkerArgs}).

init({PoolArgs, WorkerArgs}) ->
    InternalDirectory = poolgirl_internal_directory:new(),
    true = poolgirl_internal_directory:register(InternalDirectory, ?MODULE),

    WorkerModule = proplists:get_value(worker_module, PoolArgs),
    Flags = {rest_for_one, 0, 5},
    Workers = [{poolgirl_worker_sup,
                {poolgirl_worker_sup, start_link, [InternalDirectory, WorkerModule, WorkerArgs]},
                permanent, infinity, supervisor, [poolgirl_worker_sup]},
               {poolgirl, {poolgirl, start_link2, [InternalDirectory, PoolArgs, WorkerArgs]},
                permanent, 5000, worker, [poolgirl]}],
    {ok, {Flags, Workers}}.
