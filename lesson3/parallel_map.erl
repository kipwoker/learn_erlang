-module(parallel_map).

%% API
-export([
  map/4
]).


do(Fun, Item, Caller) ->
  Result = Fun(Item),
  Caller ! {ok, self(), Result}.

collect([], _) -> [];
collect([Pid | T], Timeout) ->
  Result = receive
             {ok, Pid, R} -> R;
             {'EXIT', _, _} -> {error, badarg}
           after Timeout -> {error, timeout}
           end,
  [Result | collect(T, Timeout)].

map(Fun, List, PartialResult, Timeout) ->
  Self = self(),
  Ref = make_ref(),
  Pid = spawn_link(fun() -> Self ! {map_result, Ref, map1(Fun, List, PartialResult, Timeout)} end),
  receive
    {map_result, Ref, Result} -> Result;
    {'EXIT', Pid, _} = Msg -> self() ! Msg, error
  after Timeout -> exit(Pid, kill), {error, timeout}
  end.

map1(Fun, List, PartialResult, Timeout) ->
  Self = self(),
  process_flag(trap_exit, PartialResult),
  Pids = [spawn_link(fun() -> do(Fun, X, Self) end) || X <- List],
  collect(Pids, Timeout).