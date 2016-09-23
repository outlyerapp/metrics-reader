-module(metrics_reader_format).

-include("metrics_reader.hrl").

%%====================================================================
%% Callbacks
%%====================================================================

-callback histogram(Name :: [binary()], tags(), histogram()) -> binary().

-callback combine_lines(L1 :: binary(), L2 :: binary()) -> binary().
