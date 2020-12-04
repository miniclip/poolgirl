%% @private
%% Poolbgirl - A sexy Erlang worker pool factory

-module(poolgirl_worker_sup).
-behaviour(supervisor).

-export([start_link/2]).

%% supervisor.
-export([init/1]).

-ignore_xref([start_link/2]).

start_link(Mod, Args) ->
    supervisor:start_link(?MODULE, {Mod, Args}).

init({Mod, Args}) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{Mod, {Mod, start_link, [Args]},
            temporary, 5000, worker, [Mod]}]}}.
