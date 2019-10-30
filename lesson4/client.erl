-module(client).


%% API
-export([]).
-compile(export_all).

log(Prefix, Object) -> io:format(Prefix ++ ": ~p~n", [Object]).

start() -> srv:start(client).

add(Proc, A, B, Timeout) ->
  srv:call(Proc, {add, A, B}, Timeout).

sub(Proc, A, B, Timeout) ->
  srv:call(Proc, {sub, A, B}, Timeout).

handle_call({add, A, B}) -> A + B;
handle_call({sub, A, B}) -> A - B;
handle_call(Other) -> log("Unknown op", Other).