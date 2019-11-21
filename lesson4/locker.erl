-module(locker).

%% API
-export([
  enter/2,
  exit/2,
  start/0,
  loop/1
]).

log(Prefix, Object) -> io:format(Prefix ++ ": ~p~n", [Object]).

enter(Pid, LockObj) ->
  Pid ! {lock, {self(), LockObj}},
  receive
    {reply, ok} -> ok;
    Other -> log("Enter Other", Other)
  end.

exit(Pid, LockObj) ->
  Pid ! {unlock, {self(), LockObj}},
  receive
    {reply, ok} -> ok;
    Other -> log("Exit Other", Other)
  end.

loop(LockSet) ->
  receive
    {lock, {From, LockObj}} = Request ->
      AlreadyLocked = sets:is_element(LockObj, LockSet),
      case AlreadyLocked of
        true ->
          log("Already locked", LockObj),
          self() ! Request,
          loop(LockSet);
        _ ->
          log("Locked", LockObj),
          NewSet = sets:add_element(LockObj, LockSet),
          From ! {reply, ok},
          loop(NewSet)
      end;
    {unlock, {From, LockObj}} ->
      log("Unlocked", LockObj),
      NewSet = sets:del_element(LockObj, LockSet),
      From ! {reply, ok},
      loop(NewSet);
    Other -> log("Loop Other", Other)
  end.

start() ->
  LockSet = sets:new(),
  spawn_link(locker, loop, [LockSet]).