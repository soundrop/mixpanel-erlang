-module(mixpanel_sup).
-export([
	start_link/0
]).

-behaviour(supervisor).
-export([
	init/1
]).

-define(SUPERVISOR(I),      {I, {supervisor, start_link, [?MODULE, I]}, permanent, infinity, supervisor, [?MODULE]}).
-define(SUPERVISOR(I, N),   {I, {supervisor, start_link, [{local, N}, ?MODULE, I]}, permanent, infinity, supervisor, [?MODULE]}).
-define(WORKER(M, F, A, R), {M,  {M, F, A}, R, 5000, worker, [M]}).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, app).


%% @hidden
init(app) ->
	{ok, WorkersCount} = application:get_env(mixpanel, workers),
	{ok, {
		{one_for_all, 5, 10}, [
			?SUPERVISOR(workers_sup, mixpanel_workers_sup),
			?WORKER(mixpanel_workers, start_link, [mixpanel_workers_sup, WorkersCount], permanent)
		]
	}};
init(workers_sup) ->
	{ok, {
		{simple_one_for_one, 5, 10}, [
			?WORKER(mixpanel_worker, start_link, [], temporary)
		]
	}}.
