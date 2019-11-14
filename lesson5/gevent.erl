-module(gevent).
-behaviour(gen_event).

%% API
-export([]).
-compile(export_all).

init(_Init) ->
  {ok, []}.

handle_event(Event, State) ->
  io:format("~p~n", [{Event,State}]),
  {ok, [Event | State]}.