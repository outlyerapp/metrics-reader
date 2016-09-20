%% @doc
%% Module that implements this behaviour can be used
%% as `foramt' parameter for exporters.
%% @end
-module(exporter_format).

%%====================================================================
%% Callbacks
%%====================================================================

-callback histogram(Name :: binary(), Histogram :: #{}) -> [binary()].
