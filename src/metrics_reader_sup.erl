%%%-------------------------------------------------------------------
%% @doc metrics_reader top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(metrics_reader_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(SUP_TIMEOUT, 5000).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent,
                                        ?SUP_TIMEOUT, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init([term()]) -> {ok, {supervisor:sup_flags(),
                              [supervisor:child_spec()]}}.
init([]) ->
    Children = [?CHILD(counter_histogram, worker, []),
                ?CHILD(metrics_reader, worker, [])],
    {ok, {sup_flags(), Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec sup_flags() -> supervisor:sup_flags().
sup_flags() ->
    #{strategy => one_for_one}.
