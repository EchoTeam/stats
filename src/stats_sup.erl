-module(stats_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, reconfigure/0]).

%% Supervisor callbacks
-export([init/1]).

-define(METRICS_CLEANER_PERIOD, 3 * 60 * 60 * 1000).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
                {stats_collector, {stats_collector, start_link, [[]]}, permanent, 10000, worker, [stats_collector]},
                {stats_cleaner, {folsom_timer_server, start_link,
                    [?METRICS_CLEANER_PERIOD, stats_collector, activate_metrics_cleaner, []]}, permanent, 10000, worker, [folsom_timer_server]},
                {stats_submitter, {stats_submitter, start_link, []}, permanent, 10000, worker, [stats_submitter]} ]}}.

reconfigure() ->
    superman:reconfigure_supervisor_init_args(?MODULE, []).

