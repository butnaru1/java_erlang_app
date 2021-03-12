%%%-------------------------------------------------------------------
%% @doc simple_cache top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simple_cache_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 4,
    period => 3600},
  ElementSup = #{id => simple_cache_element_sup,
    start => {simple_cache_element_sup, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => supervisor,
    modules => [simple_cache_element]},
  EventSup = #{id => simple_cache_event,
    start => {simple_cache_event, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [simple_cache_event]},
  Children = [ElementSup, EventSup],
  {ok, {SupFlags, Children}}.

%% internal functions
