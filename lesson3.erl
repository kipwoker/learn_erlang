-module(lesson3).

%% API
-export
([]).
-compile(export_all).

loop_worker() ->
  receive
    _ -> throw(die)
  after 500 ->
    throw(timeout)
  end.

log(Msg) -> io:format("Got message: ~p~n", [Msg]).

loop_supervisor(0) -> too_many;
loop_supervisor(RetryCount) ->
  process_flag(trap_exit, true),
  spawn_link(fun loop_worker/0),
  receive
    {'EXIT', _, _} = Msg -> log(Msg), loop_supervisor(RetryCount - 1);
    Msg -> log(Msg)
  end.

start() ->
  Pid = spawn(fun() -> loop_supervisor(10) end),
  timer:sleep(1000),
  exit(Pid, kill).
