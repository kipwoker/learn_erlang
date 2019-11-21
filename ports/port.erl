-module(port).
-export([start/0, init/1]).

start() ->
  spawn(?MODULE, init, ["python port.py"]).

init(SharedLib) ->
  process_flag(trap_exit, true),
  Port = open_port({spawn, SharedLib}, [stream]),
  loop(Port).

loop(Port) ->
  receive
    {echo, String} ->
      Port ! {self(), {command, String}};
    {Port, {data, Data}} ->
      io:format("~p~n", [Data])
  end,
  loop(Port).