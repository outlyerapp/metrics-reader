-module(metrics_reader_http).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

-type state() :: map() | no_state.

-spec init(Transport::module(),
           cowboy_req:req(),
           state()) -> {ok, cowboy_req:req(), state()}.
init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.

-dialyzer({no_opaque, handle/2}).
-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
    Metrics = metrics_reader:metrics(),
    {ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], Metrics, Req),
    {ok, Req2, State}.

-spec terminate(Reason::any(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
