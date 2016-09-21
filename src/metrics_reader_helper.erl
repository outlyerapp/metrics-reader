-module(metrics_reader_helper).

-export([opt/2]).

-spec opt(atom(), term()) -> term().
opt(Name, Default) ->
    application:get_env(metrics_reader, Name, Default).

