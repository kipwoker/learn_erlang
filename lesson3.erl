%%%-------------------------------------------------------------------
%%% @author user
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. окт. 2019 19:09
%%%-------------------------------------------------------------------
-module(lesson3).
-author("user").

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
