-module(lazy).

%% API
-export([]).
-compile(export_all).

%% helpers begin
lazy_list(Begin, End, Step) when Begin =< End, Step > 0 ->
  fun() ->
    [Begin | lazy_list(Begin + Step, End, Step)]
  end;
lazy_list(_, _, _) ->
  fun() ->
    []
  end.

to_list(LazyList) ->
  case LazyList() of
    [] -> [];
    [H | T] ->
      [H | to_list(T)]
  end.

eval_lazy_foldl(Fn) ->
  case Fn of
    Func when is_function(Fn) -> eval_lazy_foldl(Func());
    Number -> Number
  end.

%% helpers end

%% tests begin
assert(Expected, Actual) ->
  case Actual of
    Expected ->
      log:write("ok");
    _ ->
      log:write("error", Actual)
  end.

test() ->
  test_lazy_map(),
  test_lazy_filter(),
  test_lazy_foldl(),
  test_lazy_concat().

test_lazy_map() ->
  log:write("test_lazy_map"),
  LazyList = lazy_list(1, 5, 1),
  Actual = lazy_map(fun(Item) -> Item + 1 end, LazyList),
  Expected = [2, 3, 4, 5, 6],
  assert(Expected, to_list(Actual)).

test_lazy_filter() ->
  log:write("test_lazy_filter"),
  LazyList = lazy_list(1, 5, 1),
  Actual = lazy_filter(fun(Item) -> Item rem 2 == 0 end, LazyList),
  Expected = [2, 4],
  assert(Expected, to_list(Actual)).

test_lazy_foldl() ->
  log:write("test_lazy_foldl"),
  LazyList = lazy_list(1, 3, 1),
  Actual = lazy_foldl(fun(Item, AccIn) -> Item + AccIn end, 10, LazyList),
  Expected = 16,
  assert(Expected, eval_lazy_foldl(Actual)).

test_lazy_concat() ->
  log:write("test_lazy_concat"),
  LazyList1 = lazy_list(1, 3, 1),
  LazyList2 = lazy_list(4, 6, 1),
  Actual = lazy_concat(LazyList1, LazyList2),
  Expected = [1, 2, 3, 4, 5, 6],
  assert(Expected, to_list(Actual)).

%% tests end

lazy_map(Func, LazyList) ->
  fun() ->
    case LazyList() of
      [] -> [];
      [H | T] -> [Func(H) | lazy_map(Func, T)]
    end
  end.

lazy_filter(Filter, LazyList) ->
  fun() ->
    case LazyList() of
      [] -> [];
      [H | T] ->
        case Filter(H) of
          true -> [H | lazy_filter(Filter, T)];
          _ ->
            Fn = lazy_filter(Filter, T),
            Fn()
        end
    end
  end.

lazy_foldl(Func, Acc, LazyList) ->
  fun() ->
    case LazyList() of
      [] -> Acc;
      [H | T] ->
        lazy_foldl(Func, Func(H, Acc), T)
    end
  end.

lazy_concat(LazyList1, LazyList2) ->
  fun() ->
    case LazyList1() of
      [] -> LazyList2();
      [H | T] ->
        [H | lazy_concat(T, LazyList2)]
    end
  end.




lazy_read_line(Device) ->
  fun() -> {file:read_line(Device), Device} end.


%% -2 -1  1 2 3 4  4  0  0 -10 10 20 30  0 -1
%% -2 -3 -2 0 3 7 11 11 11   1 11 31 61 61 60

%%calculate_max_subseq(Device, Acc, MinLength, Candidate) ->
%%  ReadLine = lazy_read_line(Device),
%%  Line = ReadLine(),
%%  Number = string:to_integer(Line),
%%  case Acc of
%%    [] -> calculate_max_subseq(Device, [Number], MinLength);
%%    [PreviousItem | _] ->
%%      NewAcc = [{{origin, Number}, {sum, Number + PreviousItem}}] ++ Acc,
%%      {{sum, Sum}, {list, Seq}} = Candidate,
%%      %%todo condition
%%      NewCandidate = {{sum, Sum + Number}, {list, Seq ++ [Number]}},
%%      calculate_max_subseq(Device, NewAcc, MinLength, NewCandidate)
%%  end.
%%
%%calculate_max_subseq(FileName, MinLength) ->
%%  {ok, Device} = file:open(FileName, [raw]),
%%  calculate_max_subseq(Device, [], MinLength, [], {{sum, 0}, {list, []}}).
