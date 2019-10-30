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
  test_lazy_concat(),
  test_calculate_max_subseq().

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

test_calculate_max_subseq() ->
  assert(
    {{sum, 197}, {list, [100, -3, 100]}},
    calculate_max_subseq("test1.txt", 0)
  ),
  assert(
    {{sum, 43}, {list, [1, 2, 3, 4, 0, -1, 4, 10, 20]}},
    calculate_max_subseq("test2.txt", 0)
  ),
  assert(
    {{sum, 10}, {list, [1, 2, 3, 4]}},
    calculate_max_subseq("test3.txt", 0)
  ),
  assert(
    {{sum, 0}, {list, []}},
    calculate_max_subseq("test4.txt", 0)
  ).

%% tests end

lazy_map(Func, LazyList) ->
  fun() ->
    case LazyList() of
      [] -> [];
      [H | T] ->
        [Func(H) | lazy_map(Func, T)]
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




lazy_line_list(Device) ->
  fun() ->
    case file:read_line(Device) of
      eof -> [];
      {ok, Line} ->
        {Number, _} = string:to_integer(Line),
        [Number | lazy_line_list(Device)];
      Other ->
        log:write("Read line error", Other),
        []
    end
  end.


%% -2 -1  1 2 3 4  4  0  0 -10 10 20 30  0 -1
%% -2 -3 -2 0 3 7 11 11 11   1 11 31 61 61 60

create_acc_item(Current, Previous) ->
  Current + Previous.

create_default_candidate() ->
  {{sum, 0}, {list, []}}.

append_to_candidate(Candidate, Number) ->
  {{sum, CandidateSum}, {list, CandidateSeq}} = Candidate,
  {{sum, Number + CandidateSum}, {list, CandidateSeq ++ [Number]}}.

calculate_max_subseq(LazyLines, Acc, MinLength, Candidate, Max) ->
  LL = LazyLines(),
%%  log:write("LL", LazyLines),
%%  log:write("Acc", Acc),
%%  log:write("Candidate", Candidate),
%%  log:write("Max", Max),
%%  log:write("LL_Eval", LL),
  case LL of
    [] -> Max;
    [Number | Tail] ->
      {SumItem, PreviousSumItem} =
        case Acc of
          [] -> {create_acc_item(Number, 0), 0};
          [Previous | _] -> {create_acc_item(Number, Previous), Previous}
        end,
      NewAcc = [SumItem | Acc],
      {{sum, MaxSum}, _} = Max,
      {NewCandidate, NewMax} =
        case SumItem > PreviousSumItem of
          true ->
            {{sum, NewCandidateSum}, _} = NC = append_to_candidate(Candidate, Number),
            case NewCandidateSum > MaxSum of
              true -> {NC, NC};
              false -> {NC, Max}
            end;
          false ->
            case SumItem > 0 of
              true -> {append_to_candidate(Candidate, Number), Max};
              false -> {create_default_candidate(), Max}
            end
        end,
      calculate_max_subseq(Tail, NewAcc, MinLength, NewCandidate, NewMax)
  end.

calculate_max_subseq(FileName, MinLength) ->
  {ok, Device} = file:open(FileName, [raw]),
  LazyLines = lazy_line_list(Device),
  calculate_max_subseq(LazyLines, [], MinLength, create_default_candidate(), create_default_candidate()).
