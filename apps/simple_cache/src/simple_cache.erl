%%%-------------------------------------------------------------------
%% @doc simple_cache public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_cache).

-export([insert/2, lookup/1, delete/1]).

-define(DB_NODE, 'db@localhost').

insert(Key, Value) ->
  case simple_cache_store:lookup(Key) of
    {ok, Pid} ->
      simple_cache_event:replace(Key, Value),
      simple_cache_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = simple_cache_element:create(Value),
      simple_cache_store:insert(Key, Pid),
      simple_cache_event:create(Key, Value)
  end,
  simple_cache_db:put(?DB_NODE, Key, Value).

lookup(Key) ->
  simple_cache_event:lookup(Key),
  try
    case simple_cache_store:lookup(Key) of
      {ok, Pid} ->
        {ok, Value} = simple_cache_element:fetch(Pid),
        {ok, Value};
      {error, _} ->
        {ok, Value} = simple_cache_db:get(?DB_NODE, Key),
        insert(Key, Value),
        {ok, Value}
    end
  catch
    _Class:_Exception ->
      {error, not_found}
  end.

delete(Key) ->
  simple_cache_event:delete(Key),
  case simple_cache_store:lookup(Key) of
    {ok, Pid} ->
      simple_cache_db:delete(?DB_NODE, Key),
      simple_cache_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.