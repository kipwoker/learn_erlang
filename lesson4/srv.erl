-module(srv).

%% API
-export([]).
-compile(export_all).

log(Prefix, Object) -> io:format(Prefix ++ ": ~p~n", [Object]).


loop(Module) ->
  receive
    {call, {From, Ref, Request}} ->
      Result = Module:handle_call(Request),
      From ! {reply, Ref, {ok, Result}};
    Other -> log("Got msg", Other)
  end,
  loop(Module).

start(Module) ->
  spawn(srv, loop, [Module]).

call(Proc, Request, Timeout) ->
  Ref = make_ref(),
  Proc ! {call, {self(), Ref, Request}},
  receive
    {reply, Ref, {ok, Result}} -> Result;
    {reply, Ref, Other} -> log("Request error", Other)
  after Timeout ->
    log("Timeout", Request)
  end.