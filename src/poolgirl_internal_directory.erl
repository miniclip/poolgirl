%% @private
%% Poolgirl - A sexy Erlang worker pool factory

-module(poolgirl_internal_directory).

-include_lib("stdlib/include/ms_transform.hrl").

-export([new/0,
         register/2,
         find/2]).

%%%
%%% Types
%%%

-opaque t() :: ets:tab().
-export_type([t/0]).

%%%
%%% Exported functions
%%%

-spec new() -> t().
new() ->
    ets:new('poolgirl.internal_directory', [public]).

-spec register(t(), atom()) -> boolean().
register(Directory, Name) ->
    case find(Directory, Name) of
        {ok, ExistingPid} ->
            maybe_over_register(Directory, Name, ExistingPid);
        error ->
            ets:insert_new(Directory, {Name, self()})
    end.

-spec find(t(), atom()) -> {ok, pid()} | error.
find(Directory, Name) ->
    case ets:lookup(Directory, Name) of
        [{Name, Pid}] ->
            {ok, Pid};
        [] ->
            error
    end.

%%%
%%% Local functions
%%%

maybe_over_register(Directory, Name, ExistingPid) ->
    (ExistingPid =:= self() orelse not is_process_alive(ExistingPid))
    andalso over_register(Directory, Name, ExistingPid).

over_register(Directory, Name, ExistingPid) ->
    MatchSpec = over_registration_match_spec(Name, ExistingPid),
    case _AmountReplaced = ets:select_replace(Directory, MatchSpec) of
        1 -> true;
        0 -> false
    end.

over_registration_match_spec(Name, ExistingPid) ->
    % Atomic replacement of the entry we've just looked up
    ets:fun2ms(
      fun ({EntryName, EntryPid}) when EntryName =:= Name,
                                       EntryPid =:= ExistingPid ->
              {EntryName, self()}
      end).
