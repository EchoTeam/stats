-module(stats_collector_tests).

-include_lib("eunit/include/eunit.hrl").


run_test_() ->
    {foreach, fun setup/0, fun teardown/1,
         [{"Testing collector formats", fun check_formats/0},
          {"Gauge metric ttl", fun check_gauge_metric_ttl/0},
          {"Testing metrics cleaner", fun delete_expired_metrics/0}]}.


setup() ->
    application:start(folsom).

teardown(_) ->
    application:stop(folsom).

check_formats() ->
    [stats:notify("meter", X, meter) || X <- lists:seq(1, 20)],
    folsom_metrics_meter:tick("meter"),

    [stats:notify({<<"cnn.com">>, "cnn_meter", $r}, X, meter) || X <- lists:seq(1, 20)],
    folsom_metrics_meter:tick("cnn_meter.segments.cnn_com.r"),

    [stats:notify("latency", X, simple_statistics) || X <- [10, 20, 30, 40, 50]],
    stats:notify("queue.size", 10, gauge),

    stats:notify("gauge.latency", 10, gauge, [mean]),

    Counters = stats_collector:collect_metrics(),
    Expected = [{"latency.max", 50, [{tags,[max]}]},
                {"latency.mean", 30.0, [{tags,[mean]}]},
                {"queue.size", 10, [{tags,[sum]}]},
                {"gauge.latency", 10, [{tags,[mean]}]},
                {"cnn_meter.segments.cnn_com.r",2520.0, [{tags, [sum]}]},
                {"meter",2520.0, [{tags, [sum]}]}],

    [?assertMatch({Name, Value, _, Adds}, lists:keyfind(Name, 1, Counters)) || {Name, Value, Adds} <- Expected].

check_gauge_metric_ttl() ->
    folsom_metrics:notify("gauge_expired", {123, time_utils:unixtime() - 101}, gauge, [{ttl, 100}]),
    ?assertMatch(false, lists:keyfind("gauge_expired", 1, stats_collector:collect_metrics())),
    folsom_metrics:notify("gauge_alive", {123, time_utils:unixtime()}, gauge, [{ttl, 100}]),
    ?assertMatch({"gauge_alive", _, _, _}, lists:keyfind("gauge_alive", 1, stats_collector:collect_metrics())).

delete_expired_metrics() ->
    lists:foreach(fun(Type) ->
            stats:notify(atom_to_list(Type) ++ "_alive", 10, Type),
            stats:notify(atom_to_list(Type) ++ "_expired", 0, Type)
        end, [meter, spiral, simple_statistics]),

    folsom_metrics:notify("gauge_alive", {10, time_utils:unixtime()}, gauge),
    folsom_metrics:notify("gauge_expired", {10, time_utils:unixtime() - 1000}, gauge, [{ttl, 10}]),

    stats_collector:cleanup_metrics(true),
    ["gauge_alive", "simple_statistics_alive.max", "simple_statistics_alive.mean", "spiral_alive"] = lists:sort([Name || {Name, _, _, _} <- stats_collector:collect_metrics()]).
