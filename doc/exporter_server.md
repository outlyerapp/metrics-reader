

# Module exporter_server #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = #state{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#deregister-1">deregister/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#metrics-0">metrics/0</a></td><td></td></tr><tr><td valign="top"><a href="#metrics-1">metrics/1</a></td><td></td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

<pre><code>
code_change(OldVsn::any(), State::<a href="#type-state">state()</a>, Extra::any()) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="deregister-1"></a>

### deregister/1 ###

<pre><code>
deregister(Names::list()) -&gt; ok
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

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::[]) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="metrics-0"></a>

### metrics/0 ###

<pre><code>
metrics() -&gt; any()
</code></pre>
<br />

<a name="metrics-1"></a>

### metrics/1 ###

<pre><code>
metrics(X1::[]) -&gt; any()
</code></pre>
<br />

<a name="register-1"></a>

### register/1 ###

<pre><code>
register(Names::list()) -&gt; ok
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; <a href="#type-gen_server_startlink_ret">gen_server_startlink_ret()</a>
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

<pre><code>
terminate(Reason::any(), State::any()) -&gt; ok
</code></pre>
<br />

