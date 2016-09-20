-module(exporter_helper).

-export([opt/2]).

-spec opt(atom(), term()) -> term().
opt(Name, Default) ->
    application:get_env(metrics_exporter, Name, Default).

