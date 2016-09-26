%% @doc Observing a metric involves periodically recording a snapshot of
%% a scalar metric in the form of a histogram.  These snapshots are recorded
%% over a pre-configured interval. Each histogram will be registered
%% automatically with the `metrics_reader'.
%% Currently, only the folsom backend is supported but it is possible to extend
%% the idea to any metrics backend using the erlang-metrics library
%% interface here:
%% https://github.com/benoitc/erlang-metrics
%% Warning: observing a metric will clear it's current value whenenver a
%% snapshot is recorded.
%% @end

-module(metrics_observer).
-behaviour(gen_server).

-include("metrics_reader.hrl").

%% API
-export([start_link/0,
         observe/2,
         unobserve/1,
         observed/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {running  = false :: boolean(),
                observations = sets:new(),
                slide_interval :: pos_integer(),
                acc_interval :: pos_integer(),
                timer_ref :: reference()}).

-type metric_name() :: binary().

-type histogram_name() :: binary().

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> gen_server_startlink_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec observe(metric_name(), histogram_name()) -> ok.
observe(Name, HistogramName) ->
    gen_server:call(?SERVER, {observe, Name, HistogramName}).

-spec unobserve(metric_name()) -> ok.
unobserve(Name) ->
    gen_server:call(?SERVER, {unobserve, Name}).

-spec observed() -> list().
observed() ->
    gen_server:call(?SERVER, observed).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    %% We want a high priority to ensure reporting accuracy
    process_flag(priority, high),
    SInterval = metrics_reader_helper:opt(histogram_slide_interval_sec, 60),
    AInterval = metrics_reader_helper:opt(histogram_acc_interval_sec, 1),
    {ok, #state{slide_interval = SInterval,
                acc_interval = AInterval * 1000}}.

-spec handle_call(any(), any(), state()) -> {reply, term(), state()}.
handle_call({observe, Name, HistogramName}, _From,
            State = #state{running = false, acc_interval = AccInterval}) ->
    State1 = do_observe({Name, HistogramName}, State),
    TRef = erlang:send_after(AccInterval, self(), tick),
    Reply = ok,
    {reply, Reply, State1#state{running = true, timer_ref = TRef}};

handle_call({observe, Name, HistogramName}, _From,
            State = #state{running = true}) ->
    Reply = ok,
    State1 = do_observe({Name, HistogramName}, State),
    {reply, Reply, State1};

handle_call({unobserve, Name}, _From,
            State = #state{running = true,
                           timer_ref = TRef,
                           observations = OSet}) ->
    Empty = sets:new(),
    [{Name, Histogram}] = [{N, H} || {N, H} <- sets:to_list(OSet), N =:= Name],
    State1 = do_unobserve({Name, Histogram}, State),
    #state{observations = OSet1} = State1,

    case OSet1 of
        Empty ->
            erlang:cancel_timer(TRef),
            {reply, ok, State1#state{running = false, timer_ref = undefined}};
        _ ->
            {reply, ok, State1}
    end;

handle_call(observed, _From, State = #state{observations = OSet}) ->
    Reply = [Name || {Name, _HistogramName} <- sets:to_list(OSet)],
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()} |
                                     {stop, any(), state()}.
handle_info(tick, State = #state{running = true,
                                 observations = OSet,
                                 acc_interval = AccInterval}) ->
    lists:foreach(fun ({Name, HistogramName}) ->
                          Value = folsom_metrics_counter:get_value(Name),
                          folsom_metrics:notify({HistogramName, Value}),
                          folsom_metrics_counter:clear(Name)
                  end, sets:to_list(OSet)),
    TRef = erlang:send_after(AccInterval, self(), tick),
    {noreply, State#state{timer_ref = TRef}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, State = #state{timer_ref = TRef,
                                  observations = OSet}) ->
    Empty = sets:new(),
    #state{observations = Empty} = lists:foldl(
                                     fun do_unobserve/2,
                                     State,
                                     sets:to_list(OSet)),
    erlang:cancel_timer(TRef),
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_observe({Name, HistogramName},
            State = #state{slide_interval = SlideInterval,
                           observations = OSet}) ->

    case folsom_metrics:get_metric_info(Name) of
        [{_, [{type, histogram}]}] ->
            ok;
        _ ->
            folsom_metrics:new_histogram(HistogramName, slide, SlideInterval)
    end,

    metrics_reader:register(HistogramName),
    OSet1 = sets:add_element({Name, HistogramName}, OSet),
    State#state{observations = OSet1}.

do_unobserve({Name, HistogramName},
             State = #state{observations = OSet}) ->
    metrics_reader:deregister(HistogramName),
    folsom_metrics:delete_metric(HistogramName),
    OSet1 = sets:del_element({Name, HistogramName}, OSet),
    State#state{observations = OSet1}.
