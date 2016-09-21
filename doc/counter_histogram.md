

# Module counter_histogram #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A stateful wrapper around a folsom histogram that serves to accumulate
scalar metrics over a pre-configured interval.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##
It is essentially a
transformation between a scalar value and a histogram.
Currently, only counters are supported, but it is possible to generalize to
any scalar type and backend using the erlang-metrics library interface here:
https://github.com/benoitc/erlang-metrics
<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = #state{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#dec-1">dec/1</a></td><td></td></tr><tr><td valign="top"><a href="#dec-2">dec/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#inc-1">inc/1</a></td><td></td></tr><tr><td valign="top"><a href="#inc-2">inc/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

<pre><code>
code_change(OldVsn::any(), State::<a href="#type-state">state()</a>, Extra::any()) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="dec-1"></a>

### dec/1 ###

<pre><code>
dec(Name::term()) -&gt; [integer()] | integer()
</code></pre>
<br />

<a name="dec-2"></a>

### dec/2 ###

<pre><code>
dec(Name::term(), N::pos_integer()) -&gt; [integer()] | integer()
</code></pre>
<br />

<a name="handle_call-3"></a>

### handle_call/3 ###

<pre><code>
handle_call(Request::any(), From::any(), State::<a href="#type-state">state()</a>) -&gt; {reply, term(), <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="handle_cast-2"></a>

### handle_cast/2 ###

<pre><code>
handle_cast(Msg::any(), State::<a href="#type-state">state()</a>) -&gt; {noreply, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="handle_info-2"></a>

### handle_info/2 ###

<pre><code>
handle_info(Info::any(), State::<a href="#type-state">state()</a>) -&gt; {noreply, <a href="#type-state">state()</a>} | {stop, any(), <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="inc-1"></a>

### inc/1 ###

<pre><code>
inc(Name::term()) -&gt; [integer()] | integer()
</code></pre>
<br />

<a name="inc-2"></a>

### inc/2 ###

<pre><code>
inc(Name::term(), N::pos_integer()) -&gt; [integer()] | integer()
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::[]) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Name::term()) -&gt; ok
</code></pre>
<br />

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; ok
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; <a href="#type-gen_server_startlink_ret">gen_server_startlink_ret()</a>
</code></pre>
<br />

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

<pre><code>
terminate(Reason::any(), State::any()) -&gt; ok
</code></pre>
<br />

