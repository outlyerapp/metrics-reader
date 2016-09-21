%% @doc A stateful wrapper around a folsom histogram that serves to accumulate
%% scalar metrics over a pre-configured interval. It is essentially a
%% transformation between a scalar value and a histogram.
%% Currently, only counters are supported, but it is possible to generalize to
%% any scalar type and backend using the erlang-metrics library interface here:
%% https://github.com/benoitc/erlang-metrics
%% @end

-module(counter_histogram).
-behaviour(gen_server).

-include("metrics_reader.hrl").

%% API
-export([start_link/0,
         start/0,
         stop/0,
         new/1,
         inc/1,
         inc/2,
         dec/1,
         dec/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {running  = false :: boolean(),
                registry = sets:new(),
                slide_interval :: pos_integer(),
                acc_interval :: pos_integer(),
                timer_ref :: reference()}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> gen_server_startlink_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start() -> ok.
start() ->
    gen_server:call(?SERVER, start).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

-spec new(term()) -> ok.
new(Name) ->
    gen_server:call(?SERVER, {new, Name}).

-spec inc(term()) -> [integer()] | integer().
inc(Name) ->
    inc(Name, 1).

-spec inc(term(), pos_integer()) -> [integer()] | integer().
inc(Name, N) ->
    Internal = internal_name(Name),
    folsom_metrics_counter:inc(Internal, N).

-spec dec(term()) -> [integer()] | integer().
dec(Name) ->
    dec(Name, 1).

-spec dec(term(), pos_integer()) -> [integer()] | integer().
dec(Name, N) ->
    Internal = internal_name(Name),
    folsom_metrics_counter:dec(Internal, N).

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
handle_call(start, _From,
            State = #state{acc_interval = AccInterval, running = false}) ->
    TRef = erlang:send_after(AccInterval, self(), tick),
    Reply = ok,
    {reply, Reply, State#state{running = true, timer_ref = TRef}};

handle_call(stop, _From, State = #state{running = true, timer_ref = TRef}) ->
    erlang:cancel_timer(TRef),
    Reply = ok,
    {reply, Reply, State#state{running = false}};

handle_call({new, Name}, _From,
            State = #state{running = true, slide_interval = SlideInterval,
                           registry = Registry}) ->
    folsom_metrics:new_histogram(Name, slide, SlideInterval),
    folsom_metrics:new_counter(internal_name(Name)),
    Registry1 = sets:add_element(Name, Registry),
    {reply, ok, State#state{registry = Registry1}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()} |
                                     {stop, any(), state()}.
handle_info(tick, State = #state{running = true, registry = Registry,
                                 acc_interval = AccInterval}) ->
    lists:foreach(fun (Name) ->
                          Internal = internal_name(Name),
                          Value = folsom_metrics_counter:get_value(Internal),
                          folsom_metrics:notify({Name, Value}),
                          folsom_metrics_counter:clear(Internal)
                  end, sets:to_list(Registry)),
    TRef = erlang:send_after(AccInterval, self(), tick),
    {noreply, State#state{timer_ref = TRef}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, #state{registry = Registry}) ->
    lists:foreach(fun (Name) ->
                          Internal = internal_name(Name),
                          folsom_metrics:delete_metric(Internal)
                  end, sets:to_list(Registry)),
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec internal_name(term()) -> {atom(), term()}.
internal_name(Name) ->
    {counter_hist, Name}.
