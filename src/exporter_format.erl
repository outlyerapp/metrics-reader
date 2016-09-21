%% @doc
%% Module that implements this behaviour can be used
%% as `foramt' parameter for exporters.
%% @end
-module(exporter_format).

%%====================================================================
%% Callbacks
%%====================================================================

-callback histogram(Name :: [binary()], Histogram :: [any()]) -> binary().

-callback combine_lines(L1 :: binary(), L2 :: binary()) -> binary().
