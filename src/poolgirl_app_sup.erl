%% @private
-module(poolgirl_app_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0,
         start_child/2]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(PoolArgs, WorkerArgs) ->
    supervisor:start_child(poolgirl_app_sup, [PoolArgs, WorkerArgs]).

%% supervisor.

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{poolgirl_sup, {poolgirl_sup, start_link, []},
            transient, 5000, supervisor, [poolgirl_sup]}]}}.
