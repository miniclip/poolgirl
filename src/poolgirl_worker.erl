%% Poolgirl - A lightweight Erlang worker pool library

%% @doc The behaviour for pool worker modules.
-module(poolgirl_worker).
-ignore_xref([behaviour_info/1]).
-hank([{unused_callbacks, [start_link/1]}]).

%% Starts one worker. `WorkerArgs' is the argument given to
%% `poolgirl:child_spec/3' or `poolgirl:start_link/2'.
-callback start_link(WorkerArgs) -> {ok, Pid} |
                                    {error, {already_started, Pid}} |
                                    {error, Reason} when
    WorkerArgs :: proplists:proplist(),
    Pid        :: pid(),
    Reason     :: term().
