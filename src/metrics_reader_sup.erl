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
-define(ACCEPTOR_COUNT, 100).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent,
                                        ?SUP_TIMEOUT, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    WSOpts = web_server_opts(),
    start_web_server(WSOpts),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init([term()]) -> {ok, {supervisor:sup_flags(),
                              [supervisor:child_spec()]}}.
init([]) ->
    Children = [?CHILD(metrics_observer, worker, []),
                ?CHILD(metrics_reader, worker, [])],
    {ok, {sup_flags(), Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec sup_flags() -> supervisor:sup_flags().
sup_flags() ->
    #{strategy => one_for_one}.

web_server_opts() ->
    Defaults = #{enabled => false,
                 port => 9100,
                 endpoint => "/metrics"},
    WSConf = metrics_reader_helper:opt(webserver, []),
    Props = maps:from_list(WSConf),
    maps:merge(Defaults, Props).

start_web_server(#{enabled := false}) ->
    ok;
start_web_server(#{enabled := true, port := Port, endpoint := Endpoint}) ->
    Dispatch = cowboy_router:compile([
        {'_', [{Endpoint, metrics_reader_http, []}]}
    ]),
    Name = metrics_reader_http_listener,
    {ok, _} = cowboy:start_http(Name, ?ACCEPTOR_COUNT,
                                [{port, Port}],
                                [{env, [{dispatch, Dispatch}]},
                                 {max_keepalive, 5},
                                 {timeout, 5000}]),
    ok.
