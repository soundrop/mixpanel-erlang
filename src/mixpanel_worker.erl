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
-define(FLUSH_TIMEOUT, 60000).


-type event()      :: {Name :: binary(), mixpanel:properties(), non_neg_integer()}.
-record(state, {
	timeout  :: undefined | reference(),
	pending  :: [event()]
}).


-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec track(pid(), binary(), mixpanel:properties(), erlang:timestamp()) -> ok.
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
handle_cast({track, Event}, #state{pending = Pending} = State) when length(Pending) < ?BATCH_SIZE ->
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
	erlang:cancel_timer(Ref),
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
	Url = <<"https://api.mixpanel.com/track/">>,
	Payload = {form, [
		{data, jiffy:encode([begin
				{[
					{event, Event},
					{properties, {[
						{time, Timestamp},
						{token, Token} |
						Properties
					]}}
				]}
			end || {Event, Properties, Timestamp} <- Events], [force_utf8])}
	]},
	Options = [{follow_redirect, true}, {recv_timeout, 60000}],
	case hackney:request(post, Url, [], Payload, Options) of
		{ok, _Status, _Headers, Client} ->
			case hackney:body(Client) of
				{ok, <<"1">>, _} -> ok;
				_ -> {error, mixpanel_error}
			end;
		{error, Error} ->
			{error, Error}
	end.
