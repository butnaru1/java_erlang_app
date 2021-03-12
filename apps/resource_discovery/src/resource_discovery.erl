%%%-------------------------------------------------------------------
%%% @author olegb
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Mar 2021 18:47
%%%-------------------------------------------------------------------
-module(resource_discovery).
-author("olegb").

%% API
-export([
  add_target_resource_type/1,
  add_local_resource/2,
  fetch_resources/1,
  trade_resources/0
]).

add_target_resource_type(Type) ->
  resource_discovery_server:add_target_resource_type(Type).

add_local_resource(Type, Resource) ->
  resource_discovery_server:add_local_resource(Type, Resource).

fetch_resources(Type) ->
  resource_discovery_server:fetch_resources(Type).

trade_resources() ->
  resource_discovery_server:trade_resources().
