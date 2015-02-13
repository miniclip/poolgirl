%% Poolgirl - A sexy Erlang worker pool factory

-module(poolgirl_worker).

-callback start_link(WorkerArgs) -> {ok, Pid} |
                                    {error, {already_started, Pid}} |
                                    {error, Reason} when
    WorkerArgs :: proplists:proplist(),
    Pid        :: pid(),
    Reason     :: term().
