-module(mixpanel_app).
-export([
	start/0
]).

-behaviour(application).
-export([
	start/2,
	stop/1
]).

start() ->
	ensure_started(mixpanel).


start(_StartType, _StartArgs) ->
	mixpanel_sup:start_link().

stop(_State) ->
	ok.

%% @private
ensure_started(App) ->
	case application:start(App) of
		ok ->
			true;
		{error, {already_started, App}} ->
			true;
		{error, {not_started, Other}} ->
			ensure_started(Other),
			ensure_started(App)
	end.
