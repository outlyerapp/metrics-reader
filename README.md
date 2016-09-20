metrics_exporter
=====

An application that exposes folsom metrics in common standardised formats.

This is important for several reasons:
 
- Applications provide a common interface for the same purpose
- Exposing application metrics in standardised formats allows existing tools
  to scrape these metrics e.g. Prometheus
- Knowledge is transferable when the same problem is solved in a consistent
  manner.  

The metrics exporter exposes the application metrics via an rpc call to the
nodetool:

```
./bin/metrics_exporter metrics
```

## The exporter server

Metrics should be registered with the exporter server if they are to be
reported via the console:

    exporter_server:register(subscriber_acks).

It is also possible to de-register a registered metric:

    exporter_server:deregister(subscriber_acks).

Finally, in order to export registered metrics:

    exporter_server:metricss().

## The counter histogram

A stateful wrapper around a Folsom histogram is provided for use by clients.
This accumulates scalar values over a configured interval, and creates a sample
resevoir over these accumulated values.  This allows richer statistics to be
derived for this structure.
It is possible to extend this histogram beyond counters, to other scalar types.
Also, it is not necessary to use Folsom as a backend, and future versions may
allow this as a configuration option.

At a conceptual level, the abstraction is leaky, because normally counters
provide monotonic guarantees.  This counter may appear to oscillate between
high and low values as the accumulator clears previous registers.

    counter_histogram:start().
    counter_histogram:new(subscriber_acks_per_second).
    counter_histogram:inc(subscriber_acks_per_second).

## Configuration

    {format, prometheus_format}                         # defaults to prometheus exposition format
    {histogram_acc_interval_sec, 10}                    # defaults to 1 second
    {histogram_slide_interval_sec, 60}                  # defaults to 1 second

## Exportformats

An `export_format` behaviour is defined that consists of the following callbacks:

    callback histogram(Name :: binary(), Histogram :: #{}) -> 
       [binary()].

An default `prometheus_format` formatter is already provided that will convert
the metric values to the prometheus textual exposition format.

## Build

```bash
$ rebar3 compile
```

In order to run tests...{to do}

## Release
```
$ rebar3 release
```
