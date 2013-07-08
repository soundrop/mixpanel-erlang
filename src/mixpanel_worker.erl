-module(mixpanel_worker).
-export([
	start_link/0,
	track/4
]).

-behaviour(gen_server).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-define(BATCH_SIZE, 50).
-define(REQUEST_TIMEOUT, 2 * 60000).
-define(FLUSH_TIMEOUT, 60000).


-type event()      :: {Name :: atom(), mixpanel:properties(), non_neg_integer()}.
-record(state, {
	timeout  :: undefined | reference(),
	pending  :: [event()]
}).


-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec track(pid(), atom(), mixpanel:properties(), erlang:timestamp()) -> ok.
track(Pid, EventName, Properties, {MegaSecs, Secs, _}) ->
	gen_server:cast(Pid, {track, {EventName, Properties, MegaSecs * 1000000 + Secs}}).


%% @hidden
init([]) ->
	{ok, #state{
		timeout = undefined,
		pending = []
	}}.

%% @hidden
handle_call(_Request, _From, State) ->
	{stop, unknown_request, State}.

%% @hidden
handle_cast({track, Event}, #state{pending = Pending} = State) when length(Pending) < (?BATCH_SIZE - 1) ->
	cancel_timeout_i(State#state.timeout),
	{noreply, State#state{
		timeout = schedule_timeout_i(),
		pending = [Event | Pending]
	}};
handle_cast({track, Event}, State) ->
	cancel_timeout_i(State#state.timeout),
	ok = track_i(lists:reverse([Event | State#state.pending])),
	{noreply, State#state{
		timeout = undefined,
		pending = []
	}}.

%% @hidden
handle_info({timeout, Ref, flush}, #state{timeout = Ref} = State) ->
	ok = track_i(lists:reverse(State#state.pending)),
	{noreply, State#state{
		timeout = undefined,
		pending = []
	}};
handle_info(_, State) ->
	{noreply, State}.

%% @hidden
terminate(_, _State) ->
	ok.

%% @hidden
code_change(_Old, State, _Extra) ->
	{ok, State}.


%% @private
schedule_timeout_i() ->
	erlang:start_timer(?FLUSH_TIMEOUT, self(), flush).

%% @private
cancel_timeout_i(undefined) ->
	ok;
cancel_timeout_i(Ref) ->
	_ = erlang:cancel_timer(Ref),
	ok.


%% @private
track_i([]) ->
	ok;
track_i(Events) ->
	case application:get_env(mixpanel, token) of
		{ok, undefined} ->
			ok;
		{ok, Token} ->
			case track_i(list_to_binary(Token), Events) of
				ok ->
					ok;
				{error, Error} ->
					error_logger:error_report([
						{message, "Error while sending events to Mixpanel"},
						{events, Events},
						{exception, {error, Error}},
						{stacktrace, erlang:get_stacktrace()}
					]),
					ok
			end
	end.

%% @private
track_i(Token, Events) ->
	Data = base64:encode(jiffy:encode([begin
		{[
			{event, Event},
			{properties, {[
				{time, Timestamp},
				{token, Token} |
				Properties
			]}}
		]}
	end || {Event, Properties, Timestamp} <- Events], [force_utf8])),

	Headers = [{<<"content-type">>, <<"x-www-form-urlencoded">>}],
	Payload = <<"data=", Data/binary>>,
	case request_i(post, <<"https://api.mixpanel.com/track/">>, Headers, Payload) of
		{ok, <<"1">>} -> ok;
		{ok, _} -> {error, mixpanel_error};
		Error -> Error
	end.

%% @private
request_i(Method, Url, Headers, Payload) ->
	Ref = erlang:make_ref(),
	Parent = self(),
	Pid = spawn(fun() ->
		Options = [{follow_redirect, true}, {recv_timeout, ?REQUEST_TIMEOUT div 2}],
		Result = case hackney:request(Method, Url, Headers, Payload, Options) of
			{ok, _Status, _Headers, Client} ->
				case hackney:body(Client) of
					{ok, Body, _} -> {ok, Body};
					{error, Error} -> {error, Error}
				end;
			{error, Error} ->
				{error, Error}
		end,
		Parent ! {Ref, Result}
	end),
	receive
		{Ref, Result} ->
			Result
	after
		?REQUEST_TIMEOUT ->
			erlang:exit(Pid, timeout),
			{error, timeout}
	end.
