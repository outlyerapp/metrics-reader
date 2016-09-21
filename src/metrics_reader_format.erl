-module(metrics_reader_format).

%%====================================================================
%% Callbacks
%%====================================================================

-callback histogram(Name :: [binary()], Histogram :: [any()]) -> binary().

-callback combine_lines(L1 :: binary(), L2 :: binary()) -> binary().
