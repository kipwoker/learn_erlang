-module(lesson2).

%% API
-export([]).
-compile(export_all).

%% concatenate([[1,2,3], [], [4, five]]) => [1,2,3,4,five].

%%concatenate(ListOfList) ->
%%    concatenate(ListOfList, []).

%%concatenate
%%concatenate([[Item| List] | Lists], Acc) ->
%%    concatenate([List| Lists], [Item| Acc]).

%%lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).

%%% Pid ! "MyMessage"

%% self()

%%make_proc() ->
%%    spawn(
%%        fun Loop() ->
%%            receive
%%                {message, From, Msg} -> io:format("Got msg: ~p~n", [Msg]), From ! (Msg ++ Msg)
%%            after 10000 -> %% ms
%%                io:format("Wait 10000 ms ~n")
%%            end
%%        end
%%    ).


ping() ->
  spawn(
    example,
    loop,
    [0, ping, pong, 10]
  ).

pong() ->
  spawn(
    example,
    loop,
    [0, pong, ping, 20]
  ).

loop(State, Input, Output, Freq) ->
  receive
    {Input, Process} -> Process ! {Output, self()};
    {state, From} -> From ! io:format("State: ~p~n", State)
  end,
  if (State rem Freq == 0) -> io:format("Got another ~p~n", Freq) end,
  loop(State + 1, Input, Output, Freq).
