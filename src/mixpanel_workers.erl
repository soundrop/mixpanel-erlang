-module(mixpanel_workers).
-export([
	start_link/2,
	get/0
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

-record(state, {
	supervisor  :: pid()
}).


-spec start_link(atom() | pid(), pos_integer()) -> {ok, pid()}.
start_link(Supervisor, Size) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Supervisor, Size], []).

-spec get() -> pid().
get() ->
	Count = ets:lookup_element(?MODULE, workers_count, 2),
	Id = ets:update_counter(?MODULE, workers_index, {2, 1, Count, 1}),
	ets:lookup_element(?MODULE, {worker, Id}, 2).

%% @hidden
init([Supervisor, Size]) ->
	_ = ets:new(?MODULE, [set, public, named_table, {read_concurrency, true}]),

	{Workers, _} = lists:mapfoldl(fun(Worker, Id) ->
		{{{worker, Id}, Worker}, Id + 1}
	end, 1, spawn_workers_i(Supervisor, Size)),
	ets:insert(?MODULE, {workers_index, 0}),
	ets:insert(?MODULE, {workers_count, length(Workers)}),
	ets:insert(?MODULE, Workers),
	{ok, #state{
		supervisor = Supervisor
	}}.

%% @hidden
handle_call(_Request, _From, State) ->
	{stop, unknown_request, State}.

%% @hidden
handle_cast(_Request, State) ->
	{noreply, State}.

%% @hidden
handle_info({'DOWN', _Monitor, process, Pid, _}, State) ->
	[[Id]] = ets:match(?MODULE, {{worker, '$1'}, Pid}),
	[NewPid] = spawn_workers_i(State#state.supervisor, 1),
	ets:insert(?MODULE, {{worker, Id}, NewPid}),
	{noreply, State};

handle_info(_, State) ->
	{noreply, State}.

%% @hidden
terminate(_, _State) ->
	ok.

%% @hidden
code_change(_Old, State, _Extra) ->
	{ok, State}.


%% @private
spawn_workers_i(Supervisor, Count) ->
	spawn_workers_i(Supervisor, Count, []).

spawn_workers_i(_Supervisor, 0, Acc) ->
	Acc;
spawn_workers_i(Supervisor, Count, Acc) ->
	{ok, Pid} = supervisor:start_child(Supervisor, []),
	erlang:monitor(process, Pid),
	spawn_workers_i(Supervisor, Count - 1, [Pid | Acc]).
