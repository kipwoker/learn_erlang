-module(worker).
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2]).


init(Args) ->
  erlang:error(not_implemented).

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).