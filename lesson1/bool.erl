-module(bool).

%% API
-export([
  b_not/1,
  b_and/2,
  b_or/2,
  b_xor/2
]).


b_not(Arg) ->
  case Arg of
    true -> false;
    false -> true
  end.

b_and(Arg0, Arg1) ->
  case {Arg0, Arg1} of
    {true, true} -> true;
    _ -> false
  end.

b_or(Arg0, Arg1) ->
  case {Arg0, Arg1} of
    {true, _} -> true;
    {_, true} -> true;
    _ -> false
  end.

b_xor(Arg0, Arg1) ->
  case {Arg0, Arg1} of
    {true, false} -> true;
    {false, true} -> true;
    _ -> false
  end.