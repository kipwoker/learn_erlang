-module(dispatcher).
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, apply/2, start_link/0, test_apply/1, set_workers_count/2]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init(_Args) ->
  {ok, gb_trees:empty()}.

handle_call(Request, _From, Workers) ->
  case Request of
    {set_count, Count} ->
    {func, Func} -> {reply, Func(), Workers}
  end.

handle_cast(_Request, State) ->
  {noreply, State}.

apply(Pid, Func) ->
  gen_server:call(Pid, {func, Func}).

set_workers_count(Pid, Count) ->
  gen_server:call(Pid, {set_count, Count}).

create_workers(Workers, Count) ->
  case Count of
    0 -> Workers;
    _ ->
      %%todo: save it like
      %% key = weight
      %% value = list of workers with same weight
      NewWorkers = gb_trees:enter()
  end.

test_apply(Pid) ->
  apply(Pid, fun () -> 0 + 1 end),
  apply(Pid, fun () -> 1 + 2 end),
  apply(Pid, fun () -> 2 + 3 end),
  apply(Pid, fun () -> 3 + 4 end).