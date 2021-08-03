%% @private
%% Poolbgirl - A sexy Erlang worker pool factory

-module(poolgirl_worker_sup).
-behaviour(supervisor).

-export([start_link/3]).

%% supervisor.
-export([init/1]).

-ignore_xref([start_link/3]).

start_link(InternalDirectory, WorkerMod, WorkerArgs) ->
    supervisor:start_link(?MODULE, {InternalDirectory, WorkerMod, WorkerArgs}).

init({InternalDirectory, WorkerMod, WorkerArgs}) ->
    true = poolgirl_internal_directory:register(InternalDirectory, ?MODULE),
    {ok, {{simple_one_for_one, 0, 1},
          [{WorkerMod, {WorkerMod, start_link, [WorkerArgs]},
            temporary, 5000, worker, [WorkerMod]}]}}.
