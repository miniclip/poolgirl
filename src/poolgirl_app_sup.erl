%% @private
-module(poolgirl_app_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    %% this supervisor sole function is to create the ets table
    %% and own it
    ok = poolgirl_pg:init(),
    {ok, {{one_for_one, 10, 10}, []}}.
