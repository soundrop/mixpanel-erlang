mixpanel-erlang
===============
mixpanel-erlang is an Erlang application for sending data to the Mixpanel analytics service.


Quick Start
-----------

```shell
git clone https://github.com/soundrop/mixpanel-erlang.git
cd mixpanel-erlang
make
ERL_LIBS=deps erl -pa ebin/ -s mixpanel_app -mixpanel token '"TOKEN"'
```

Which should start and erlang shell prompt, where you should be able to type something like this:

```shell
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.3.1  (abort with ^G)
1> mixpanel:track(<<"Login">>, [{distinct_id, <<"0">>}, {name, <<"test user">>}]).
ok
2> q().
ok
```

You should see the event appearing in Mixpanel within **1 minute** because of the buffering done in the mixpanel-erlang application.

Usage
-----

Add mixpanel as a dependency to your application:

```erlang
{application, my_app, [
	{description, "My Application"},
	{vsn, git},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		mixpanel
	]},
	{mod, {my_app_app, []}},
	{env, []}
]}.
```

The mixpanel application itself needs to be configured using the application's environment, this is
generally done in app.config or sys.config:

```erlang
{mixpanel, [
    {token, "TOKEN"},
    {workers, 10}
]}.
 ```

Now you can start tracking events using the ```mixpanel:track/2``` and ```mixpanel:track/3``` functions.
