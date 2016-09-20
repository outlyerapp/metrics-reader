-module(prometheus_format).
-behaviour(exporter_format).

-export([histogram/2]).

-xref_ignore([histogram/2]).

-spec histogram(Name :: binary(), Histogram :: #{}) -> [binary()].
histogram(_Name, _Histogram) ->
    [<<>>].


