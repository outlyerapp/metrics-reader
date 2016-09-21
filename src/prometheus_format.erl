%% @doc
%%
%% Formats metric output according to the [Prometheus exposition format]
%% (https://prometheus.io/docs/instrumenting/exposition_formats/).
%%
%% Example output:
%% <pre>
%%   # A histogram, which has a complex representation in the text format:
%%   # HELP http_request_duration_seconds A histogram of the request duration.
%%   # TYPE http_request_duration_seconds histogram
%%   http_request_duration_seconds_bucket{le="0.05"} 24054
%%   http_request_duration_seconds_bucket{le="0.1"} 33444
%%   http_request_duration_seconds_bucket{le="0.2"} 100392
%%   http_request_duration_seconds_bucket{le="0.5"} 129389
%%   http_request_duration_seconds_bucket{le="1"} 133988
%%   http_request_duration_seconds_bucket{le="+Inf"} 144320
%%   http_request_duration_seconds_sum 53423
%%   http_request_duration_seconds_count 144320
%%
%%   # Finally a summary, which has a complex representation, too:
%%   # HELP rpc_duration_seconds A summary of the RPC duration in seconds.
%%   # TYPE rpc_duration_seconds summary
%%   rpc_duration_seconds{quantile="0.01"} 3102
%%   rpc_duration_seconds{quantile="0.05"} 3272
%%   rpc_duration_seconds{quantile="0.5"} 4773
%%   rpc_duration_seconds{quantile="0.9"} 9001
%%   rpc_duration_seconds{quantile="0.99"} 76656
%%   rpc_duration_seconds_sum 1.7560473e+07
%%   rpc_duration_seconds_count 2693
%% </pre>
%%
%% @end
-module(prometheus_format).
-behaviour(metrics_reader_format).

%% -export([histogram/2]).
-compile(export_all).

-xref_ignore([histogram/2]).

-spec histogram(Name :: [binary()], Histogram :: [any()]) -> binary().
histogram(Name, Histogram) when is_list(Name) ->
    MetricName = combine(Name, <<"_">>),
    Prologue = emit_prologue(<<"histogram">>, MetricName),
    Summary = emit_summary(Histogram, MetricName),
    <<Prologue/binary, Summary/binary>>.

-spec combine_lines(L1 :: binary(), L2 :: binary()) -> binary().
combine_lines(L1, L2) when is_binary(L1), is_binary(L2) ->
    combine_two(L1, L2, <<"\\n">>).

%%====================================================================
%% Internal functions
%%====================================================================

emit_prologue(Type, Name) when is_binary(Type) ->
    Type1 = <<"# TYPE ", Type/binary, " ", Name/binary>>,
    Help = <<"# HELP ", Name/binary>>,
    combine_lines(Type1, Help).

emit_series(Name, Value) when is_binary(Name) ->
    emit_series(Name, [], Value).
emit_series(Name, Labels, undefined) when is_binary(Name) ->
  LabelPairs = emit_labels(Labels),
  <<Name/binary, LabelPairs/binary, " Nan">>;
emit_series(Name, Labels, Value) when is_binary(Name) ->
  LabelPairs = emit_labels(Labels),
  ValueBin = v2b(Value),
  <<Name/binary, LabelPairs/binary, " ", ValueBin/binary>>.

emit_labels([]) -> <<"">>;
emit_labels(Labels) when is_list(Labels) ->
    LabelPairs = [label_pair(Label) || Label <- Labels],
    LabelPairs1 = combine(LabelPairs, <<",">>),
    <<${, LabelPairs1/binary, $}>>.

label_pair({Label, Value}) ->
    LabelBin = list_to_binary(Label),
    ValueBin = v2b(Value),
    <<LabelBin/binary, $=, $", ValueBin/binary, $">>.

emit_summary(Histogram, Name) ->
    emit_summary(Histogram, Name, <<>>).

emit_summary([{min, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_min">>, V),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{max, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_max">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{arithmetic_mean, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_arithmetic_mean">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{geometric_mean, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_geometric_mean">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{harmonic_mean, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_harmonic_mean">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{median, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_median">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{variance, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_variance">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{standard_deviation, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_standard_deviation">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{skewness, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_skewness">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{kurtosis, V} | T], Name, Acc) ->
    Series = emit_series(<<Name/binary, "_kurtosis">>, round(V)),
    emit_summary(T, Name, combine_lines(Acc, Series));
emit_summary([{percentile,
                  [{50, P50}, {75, P75}, {95, P95}, {99, P99}, {999, P999}]
              } | T], Name, Acc) ->
    Q1 = emit_series(Name, [{"quantile",  "0.5"}], round(P50)),
    Q2 = emit_series(Name, [{"quantile", "0.75"}], round(P75)),
    Q3 = emit_series(Name, [{"quantile", "0.95"}], round(P95)),
    Q4 = emit_series(Name, [{"quantile", "0.99"}], round(P99)),
    Q5 = emit_series(Name, [{"quantile", "0.999"}], round(P999)),
    Percentiles = combine([Q1, Q2, Q3, Q4, Q5], <<"\\n">>),
    emit_summary(T, Name, combine_lines(Acc, Percentiles));
emit_summary([_ | T], Name, Acc) ->
   emit_summary(T, Name, Acc);
emit_summary([], _Name, Acc) ->
    Acc.

combine(Parts, Sep) ->
    combine(Parts, Sep, <<>>).

combine([], _Sep, Acc) ->
    Acc;
combine([H], _Sep, <<>>) ->
    H;
combine([H], Sep, Acc) ->
    combine_two(Acc, H, Sep);
combine([H | T], Sep, Acc) ->
    combine(T, Sep, combine_two(Acc, H, Sep)).

combine_two(P1, P2, Sep)
  when is_binary(P1),
       is_binary(P2),
       is_binary(Sep) ->
    <<P1/binary, Sep/binary, P2/binary>>.

v2b(V) when is_binary(V) ->
    V;
v2b(V) when is_list(V) ->
    list_to_binary(V);
v2b(V) when is_integer(V) ->
    integer_to_binary(V);
v2b(V) when is_float(V) ->
    float_to_binary(V);
v2b(V) when is_atom(V) ->
    erlang:atom_to_binary(V, utf8).

