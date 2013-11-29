%%%
%%% Copyright (c) 2007-2013 JackNyfe. All rights reserved.
%%% THIS SOFTWARE IS PROPRIETARY AND CONFIDENTIAL. DO NOT REDISTRIBUTE.
%%%

-module(stats_collector).

-export([
    collect_metrics/0,
    prepare/2,
    safe_string/1,
    activate_metrics_cleaner/0,
    run_collector/2,
    cleanup_metrics/1
    ]).

-export([
    start_link/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
    ]).

-define(DEFAULT_GAUGE_TTL, 10800). %3 hours
-define(PERIOD, 60000).
-define(EXP, 0.000001).

-type service() :: nonempty_string().
-type metric_value() :: float().
-type time() :: integer().
-type metric() :: {service(), metric_value(), time()} | {service(), metric_value(), time(), list()}.
-type opts() :: [{name, string()} | {group, string()} | {desc, string()}].

-record(state, {opts = [] :: list(),
                collector = undefined :: 'undefined' | {pid(), reference()},
                cleanup_metrics = false :: 'true' | 'false', 
                timer_pid :: reference()}).

-spec start_link(opts()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec prepare([metric()], [{prefix, string()} | {suffix, string()}]) -> [metric()].
prepare(Metrics, Opts) ->
    Prefix = proplists:get_value(prefix, Opts, ""),
    Suffix = proplists:get_value(suffix, Opts, ""),
    lists:map(fun(M) ->
                {Key, Value, Time, Adds} = case M of
                    {K, V, T} -> {K, V, T, [{tags, [sum]}]};
                    _ -> M
                end,
                Name = string:join([P || P <- [Prefix, Key, Suffix], P =/= ""], "."),
                {Name, Value, Time, Adds}
        end, Metrics).

-spec safe_string(string()) -> string().
safe_string(S) ->
    re:replace(type_utils:str(S), "\\W", "_", [global, {return, list}]).

-spec run_collector('true' | 'false', list()) -> ok.
run_collector(IsCleanupActivated, Opts) ->
    Metrics = collect_metrics(),
    PreparedMetrics = prepare(Metrics, [{prefix, namespace()}]),
    stats_submitter:send(PreparedMetrics, Opts),
    cleanup_metrics(IsCleanupActivated),
    ok.

-spec activate_metrics_cleaner() -> ok.
activate_metrics_cleaner() ->
    gen_server:call(?MODULE, activate_metrics_cleaner).

-spec collect_metrics() -> [metric()].
collect_metrics() ->
    FolsomMetrics = get_folsom_metrics(),
    lists:foldl(fun(Metric, Acc) ->
                case safely_construct_metric(Metric) of
                    undefined -> Acc;
                    M -> M ++ Acc
                end
        end, [], FolsomMetrics).

init(Opts) ->
    Pid = erlang:send_after(?PERIOD, self(), tick),
    {ok, #state{cleanup_metrics = false, timer_pid = Pid, opts = Opts}}.

terminate(Reason, #state{timer_pid = TimerPid, collector = Collector}) ->
    erlang:cancel_timer(TimerPid),
    case Collector of
        {MonPid, Ref} ->
            erlang:demonitor(Ref),
            erlang:exit(MonPid, Reason);
        _ -> ok
    end.

handle_info({'DOWN', Ref, process, Pid, Reason}, #state{collector = {Pid, Ref}} = State) ->
    lager:debug("Collector has finished collecting with reason ~p", [Reason]),
    {noreply, State#state{collector = undefined}};

handle_info(tick, #state{collector = C} = State) when C =/= undefined ->
    lager:debug("Collect signal has been received before the previous signal have been processed"),
    Pid = erlang:send_after(?MODULE, self(), tick),
    {noreply, State#state{timer_pid = Pid}};

handle_info(tick, #state{collector = undefined, cleanup_metrics = IsCleanupActivated, opts = Opts} = State) ->
    lager:debug("Start collecting the metrics"),
    TimerPid = erlang:send_after(?PERIOD, self(), tick),
    {MonPid, Ref} = spawn_monitor(?MODULE, run_collector, [IsCleanupActivated, Opts]),
    {noreply, State#state{collector = {MonPid, Ref}, timer_pid = TimerPid, cleanup_metrics = false}}.

handle_call(activate_metrics_cleaner, _From, State) ->
    {reply, ok, State#state{cleanup_metrics = true}};
handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

namespace() ->
    case application:get_env(stats, namespace) of
        undefined -> "";
        {ok, Namespace} -> Namespace
    end.

get_folsom_metrics() ->
    try
        Metrics = folsom_metrics:get_metrics_info(),
        lists:map(fun({Name, Info}) ->
                Type = proplists:get_value(type, Info),
                Tags = sets:to_list(proplists:get_value(tags, Info)),
                {Type, Name, folsom_metrics:get_metric_value(Name), Tags}
            end, Metrics)
    catch
        _:Reason ->
            lager:error("Unable to get folsom's metrics: ~p", [Reason]),
            []
    end.

safely_construct_metric(Metric) ->
    try
        construct_metric(Metric)
    catch
        _:Reason ->
            lager:error("Unable to construct folsom's metrics: ~p~nStacktrace: ~p", [Reason, erlang:get_stacktrace()]),
            undefined
    end.

supported_methods() -> [max, mean, sum].

guarantee_method_in_tags(Tags, Default) ->
    Intersection = sets:intersection(sets:from_list(Tags), sets:from_list(supported_methods())),
    case sets:size(Intersection) of
        0 -> [Default | Tags];
        _ -> Tags
    end.

construct_metric({simple_statistics, Name, Metric, Tags}) when is_list(Name) ->
    Stats = [proplists:lookup(F, Metric) || F <- [max, mean]],
    lists:map(fun({Field,Value}) ->
                CounterName = Name ++ "." ++ atom_to_list(Field),
                Time = time_utils:unixtime(),
                {CounterName, Value, Time, [{tags, [Field | Tags]}]}
        end, [S || S <- Stats, S /= none]);

construct_metric({meter, Name, Metric, Tags}) when is_list(Name) ->
    Value = 60 * proplists:get_value(one, Metric), % folsom store metrics per seconds
    Time = time_utils:unixtime(),
    [{Name, Value, Time, [{tags, guarantee_method_in_tags(Tags, sum)}]}];

construct_metric({spiral, Name, Metric, Tags}) when is_list(Name) ->
    Value = proplists:get_value(one, Metric),
    Time = time_utils:unixtime(),
    [{Name, Value, Time, [{tags, guarantee_method_in_tags(Tags, sum)}]}];

construct_metric({gauge, Name, {Value, TimeTaken}, Tags}) when is_list(Name) ->
    TimeNow = time_utils:unixtime(),
    case is_metric_expired(TimeTaken, TimeNow, Tags) of
        false -> [{Name, Value, TimeNow, [{tags, guarantee_method_in_tags(clean_tags(Tags, []), sum)}]}];
        true  -> []
    end;

% BC
construct_metric({gauge, Name, Value, Tags}) when is_list(Name) ->
    Time = time_utils:unixtime(),
    [{Name, Value, Time, [{tags, guarantee_method_in_tags(Tags, sum)}]}];
% End of BC

construct_metric(_Metric) -> undefined.

is_metric_expired(Taken, Now, Tags) ->
    (Taken + get_ttl(Tags)) < Now.

get_ttl([]) -> ?DEFAULT_GAUGE_TTL;
get_ttl([{ttl, TTL}|_]) -> TTL;
get_ttl([_|T]) -> get_ttl(T).

clean_tags([], Res) -> Res;
clean_tags([{ttl, _}|T], Res) -> clean_tags(T, Res);
clean_tags([E|T], Res) -> clean_tags(T, [E|Res]).

cleanup_metrics(true) ->
    Metrics = get_folsom_metrics(),
    [maybe_cleanup_metric(M) || M <- Metrics];
cleanup_metrics(_) -> skip.

maybe_cleanup_metric({simple_statistics, Name, Metric, _Tags}) ->
    Value = proplists:get_value(max, Metric),
    maybe_cleanup_metric(Name, Value == 0);

maybe_cleanup_metric({meter, Name, Metric, _Tags}) ->
    Value = proplists:get_value(five, Metric),
    maybe_cleanup_metric(Name, Value =< ?EXP);

maybe_cleanup_metric({spiral, Name, Metric, _Tags}) ->
    Value = proplists:get_value(one, Metric),
    maybe_cleanup_metric(Name, Value == 0);

maybe_cleanup_metric({gauge, Name, {Value, Timestamp}, Tags}) ->
    maybe_cleanup_metric(Name, is_metric_expired(Timestamp, time_utils:unixtime(), Tags));

maybe_cleanup_metric(_) -> skip.

maybe_cleanup_metric(Name, true) ->
    try
        folsom_metrics:delete_metric(Name)
    catch _:Reason ->
            lager:error("Unable to delete folsom's metric: ~p ~p~nStacktrace: ~p", [Name, Reason, erlang:get_stacktrace()])
    end;

maybe_cleanup_metric(_, _) -> active_metric.
