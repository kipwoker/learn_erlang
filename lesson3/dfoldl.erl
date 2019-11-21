-module(dfoldl).

%% API
-export([
  foldl/5,
  foldl2/3
]).

do(Fun, Item1, Item2, Caller) ->
  Result = Fun(Item1, Item2),
  Caller ! {ok, self(), Result}.

collect([], _) -> [];
collect(List, Timeout) ->
  case List of
    [] -> [];
    [Pid | T] ->
      Result = receive
                 {ok, Pid, R} -> R;
                 {'EXIT', _, _} -> {error, badarg}
               after Timeout -> {error, timeout}
               end,
      [Result | collect(T, Timeout)]
  end.


foldl(Fun, Acc, List, PartialResult, Timeout) ->
  Self = self(),
  Ref = make_ref(),
  Pid = spawn_link(fun() -> Self ! {map_result, Ref, foldl1(Fun, Acc, List, PartialResult, Timeout)} end),
  receive
    {map_result, Ref, Result} -> Result;
    {'EXIT', Pid, _} = Msg -> self() ! Msg, {error, exit}
  after Timeout -> exit(Pid, kill), {error, timeout}
  end.

foldl1(Fun, Acc, List, PartialResult, Timeout) ->
  Self = self(),
  process_flag(trap_exit, PartialResult),
  List = create_list(Fun, Acc, List, Self),
  collect(List, Timeout).


create_list(Fun, Acc, List, Self) ->
  case List of
    [H1, H2 | T] -> [{process, spawn_link(fun() -> do(Fun, H1, H2, Self) end)} | create_list(Fun, Acc, T, Self)];
    [H | T] -> [{item, H} | create_list(Fun, Acc, T, Self)];
    [] -> {item, Acc}
  end.

%% non parallel sample
foldl2(Fun, Acc, List) ->
  case List of
    [H1, H2 | T] -> Fun(Fun(H1, H2), foldl2(Fun, Acc, T));
    [H | T] -> Fun(H, foldl2(Fun, Acc, T));
    [] -> Acc
  end.