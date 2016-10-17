-module(poolgirl_app).

-behaviour(application).

%% Application callbacks
-export([start/0,
         start/2,
         stop/1]).

start() ->
    start(undefined, undefined).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    poolgirl_app_sup:start_link().

stop(_State) ->
    ok.
