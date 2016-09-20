%% @doc
%% Module that implements this behaviour can be used
%% as `foramt' parameter for exporters.
%% Built-in formats:
%% - {@link prometheus_text_format}
%% - {@link prometheus_protobuf_format}
%%
%% Callbacks:
%% - `content_type()` - should return content type of the format;
%% - `format()` - should format `default' regsitry;
%% - `format(Registry)` - should format `Registry'.
%% @end
-module(exporter_format).

%%====================================================================
%% Callbacks
%%====================================================================

-callback histogram(Name :: binary(), Histogram :: #{}) -> [binary()].
