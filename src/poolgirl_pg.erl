%% @private
-module(poolgirl_pg).

-export([get/1,
         create/1,
         delete/1,
         join/2,
         leave/2,
         get_members/1]).

-type name() :: atom().

-export_type([name/0]).

%%%
%%% Exported functions
%%%

-spec create(Name :: name()) -> ok | already_exists.
create(Name) ->
    Name = ets:new(Name,
                    [set, public, named_table,
                     {read_concurrency, true}]),
    Table = pool_name(Name),
    case ets:member(Name, Table) of
      false ->
        true = ets:insert_new(Name, {Table, {}}),
        ok;
      true -> already_exists
    end.

-spec delete(Name :: name()) -> ok.
delete(Name) ->
    true = ets:delete(Name, pool_name(Name)),
    ok.

-spec join(Name, Pid :: pid()) -> ok | {error, {no_such_group, Name}}
      when Name :: name().
join(Name, Pid) when is_pid(Pid) ->
    case get_members(Name) of
        {error, {no_such_group, Name}} -> {error, {no_such_group, Name}};
        Members ->
            true = ets:insert(Name, {pool_name(Name), list_to_tuple(Members ++ [Pid])}),
            ok
    end.

-spec leave(Name, Pid :: pid()) -> ok | {error, {no_such_group, Name}}
      when Name :: name().
leave(Name, Pid) when is_pid(Pid) ->
    case get_members(Name) of
        {error, {no_such_group, Name}} -> {error, {no_such_group, Name}};
        Members ->
            true = ets:insert(Name, {pool_name(Name), list_to_tuple(Members -- [Pid])}),
            ok
    end.

-spec get_members(Name) -> [pid()] | {error, {no_such_group, Name}}
      when Name :: name().
get_members(Name) ->
    case lookup(Name) of
        {error, {no_such_group, Name}} -> {error, {no_such_group, Name}};
        Members -> tuple_to_list(Members)
    end.

-spec get(Name) ->  pid() | {error, Reason} when
      Name :: name(),
      Reason ::  {no_process, Name} | {no_such_group, Name}.
get(Name) ->
    case lookup(Name) of
        {error, {no_such_group, Name}} -> {error, {no_such_group, Name}};
        {} -> {error, {no_process, Name}};
        Members ->
            % stateless pick: leaves the caller's rand state alone
            Index = erlang:phash2(erlang:unique_integer(), tuple_size(Members)) + 1,
            element(Index, Members)
    end.

%%%
%%% Local functions
%%%

pool_name(Name) -> Name.

lookup(Name) ->
    try ets:lookup(Name, Name) of
        [] -> {error, {no_such_group, Name}};
        [{Name, Members}] -> Members
    catch
        _:_ -> {}
    end.
