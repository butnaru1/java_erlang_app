%%%-------------------------------------------------------------------
%%% @author olegb
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Mar 2021 10:31 AM
%%%-------------------------------------------------------------------
-module(simple_cache_db).
-author("olegb").

%% API
-export([put/3, get/2, delete/2]).


put(Node, Key, Value) ->
  Ref = make_ref(),
  {db_server, Node} ! {put, self(), Ref, term_to_binary(Key),
    term_to_binary(Value)},
  receive
    {reply, Ref, ok} -> ok
  after 3000 ->
    {error, timeout}
  end.

get(Node, Key) ->
  Ref = make_ref(),
  {db_server, Node} ! {get, self(), Ref, term_to_binary(Key)},
  receive
    {reply, Ref, not_found} ->
      {error, not_found};
    {reply, Ref, Binary} ->
      {ok, binary_to_term(Binary)}
  after 3000 ->
    {error, timeout}
  end.

delete(Node, Key) ->
  Ref = make_ref(),
  {db_server, Node} ! {delete, self(), Ref, term_to_binary(Key)},
  receive
    {reply, Ref, ok} -> ok
  after 3000 ->
    {error, timeout}
  end.