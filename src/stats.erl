%%%
%%% Copyright (c) 2007-2012 JackNyfe. All rights reserved.
%%% THIS SOFTWARE IS PROPRIETARY AND CONFIDENTIAL. DO NOT REDISTRIBUTE.
%%%

-module(stats).

-export([
    notify/3,
    notify/4,
    prepare/2,
    safe_string/1,
    host/0
    ]).

-type opts() :: [{name, string()} | {group, string()} | {desc, string()}].

-type service() :: nonempty_string().
-type metric_value() :: float().
-type time() :: integer().
-type metric() :: {service(), metric_value(), time()} | {service(), metric_value(), time(), list()}.

-type provider() :: fun(() -> [metric()]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec notify({binary(), nonempty_string(), char()} | nonempty_string(), float(), atom()) -> ok.
notify({Domain, Counter, Mode}, Value, Type) ->
    Parts = case Domain of
        <<".">> ->
            [Counter, [Mode]];
        _ ->
            [Counter, "segments", safe_string(Domain), [Mode]]
    end,
    Name = string:join([type_utils:str(P) || P <- Parts], "."),
    notify(Name, Value, Type);

notify(Name, Value, gauge = Type) when is_list(Name) ->
    folsom_metrics:safely_notify(Name, {Value, time_utils:unixtime()}, Type);

notify(Name, Value, Type) when is_list(Name) ->
    folsom_metrics:safely_notify(Name, Value, Type).

-spec notify(nonempty_string(), float(), atom(), list()) -> ok.
notify(Name, Value, gauge = Type, Tags) when is_list(Name) ->
    folsom_metrics:safely_notify(Name, {Value, time_utils:unixtime()}, Type, Tags);

notify(Name, Value, Type, Tags) when is_list(Name) ->
    folsom_metrics:safely_notify(Name, Value, Type, Tags).

-spec prepare([metric()], [{prefix, string()} | {suffix, string()}]) -> [metric()].
prepare(Metrics, Opts) ->
    stats_collector:prepare(Metrics, Opts).

-spec safe_string(string()) -> string().
safe_string(S) ->
    stats_collector:safe_string(S).

-spec host() -> nonempty_string().
host() ->
    lists:last(string:tokens(atom_to_list(node()), "@")).
