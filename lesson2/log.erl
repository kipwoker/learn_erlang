-module(log).

%% API
-export([
  write/1,
  write/2
]).

write(Msg) ->
  io:format(Msg ++ "~n").

write(Msg, Object) ->
  io:format(Msg ++ ": ~p~n", [Object]).
