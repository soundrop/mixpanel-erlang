-module(mixpanel).
-export([
	track/2,
	track/3
]).
-export_type([
	properties/0
]).

-type property()   :: {Key :: binary() | atom(), Value :: binary() | atom() | boolean() | integer() | float()}.
-type properties() :: [property()].


-spec track(atom(), properties()) -> ok.
track(Event, Properties) ->
	track(Event, Properties, os:timestamp()).

-spec track(atom(), properties(), erlang:timestamp()) -> ok.
track(Event, Properties, Timestamp) ->
	mixpanel_worker:track(mixpanel_workers:get(), Event, Properties, Timestamp).
