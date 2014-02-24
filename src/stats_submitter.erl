-module(stats_submitter).
-behaviour(gen_server).

-export([
    pause/0,
    resume/0,
    send/2,
    start_link/0
    ]).

-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
    ]).


-record(state, {
        paused = false
        }).

-type state() :: #state{}.

-type service() :: nonempty_string().
-type metric_value() :: float().
-type time() :: integer().
-type metric() :: {service(), metric_value(), time(), list()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec pause() -> ok.
pause() ->
    gen_server:call(?MODULE, pause).

-spec resume() -> ok.
resume() ->
    gen_server:call(?MODULE, resume).

-spec send([metric()], [{group, string()} | {desc, string()}]) -> ok.
send(Metrics, Opts) ->
    gen_server:call(?MODULE, {send, Metrics, Opts}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(pause, _From, State) ->
    {reply, ok, State#state{paused = true}};

handle_call(resume, _From, State) ->
    {reply, ok, State#state{paused = false}};

handle_call({send, _, _}, _From, #state{paused = true} = State) ->
    {reply, paused, State};

handle_call({send, Metrics, Opts}, _From, State) ->
    Group = proplists:get_value(group, Opts, "main"),
    Desc = proplists:get_value(desc, Opts, Group),
    lager:debug("[(~p) Sending ~p stat records", [Desc, length(Metrics)]),
    Events = lists:map(fun({Key, Value, Time, Adds}) ->
                    Tags = [Group] ++ proplists:get_value(tags, Adds, []),
                    riemann:event([{service, Key}, {metric, Value}, {time, Time}, {tags, Tags}])
            end, Metrics),
    riemann:send_tcp(Events),
    {reply, ok, State};

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

-spec init([]) -> {'ok', state()}.
init(_) ->
    Paused = case application:get_env(stats, submitter_state) of
        {ok, running} -> false;
        _ -> true
    end,
    {ok, #state{paused = Paused}}.

terminate(_Reason, _State) ->
    ok.
