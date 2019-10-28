-module(lambda).

%% API
-export([]).
-compile(export_all).

%%Напишите lambda-функцию, которая осуществляет произвольную операцию Operation(A, B) -> C (где A, B, C - числа),
%%%%над двумя числовыми списками попарно, возвращая список результатов операции также в виде списка.
%%%%Проверьте вашу функцию на разных операциях (erlang:'+', erlang:'xor', erlang:'rem', erlang:'/' и собственной фунции,
%%%%которая возвращает среднее гармоническое двух чисел H = 2/(1/A + 1/B)).

log(Object) -> io:format("~p~n", [Object]).
log(Prefix, Object) -> io:format(Prefix ++ ": ~p~n", [Object]).

calculate_harmonic(A, B) ->
  2 / (1 / A + 1 / B).

plus_fun(A, B) -> A + B.
xor_fun(A, B) -> A bxor B.
rem_fun(A, B) -> A rem B.
div_fun(A, B) -> A / B.

test_zip() ->
  Zip =
    fun InnerZip(Operation, A, B) ->
      case {A, B} of
        {[], []} -> [];
        {[HA | TA], [HB | TB]} ->
          %log("HA", HA), log("HB", HB),
          Calc = Operation(HA, HB),
          %log("Calc", Calc),
          [Calc | InnerZip(Operation, TA, TB)];
        Other -> log(Other), error
      end
    end,

  {A, B} = {
    [1, 2, 3],
    [4, 5, 6]
  },

  ExpectedPlus = [5, 7, 9],
  ExpectedXor = [5, 7, 5],
  ExpectedRem = [1, 2, 3],
  ExpectedDiv = [0.25, 0.4, 0.5],
  ExpectedHarm = [1.6, 2.857142857142857, 4.0],

  log("plus", Zip(fun plus_fun/2, A, B) == ExpectedPlus),
  log("xor", Zip(fun xor_fun/2, A, B) == ExpectedXor),
  log("rem", Zip(fun rem_fun/2, A, B) == ExpectedRem),
  log("div", Zip(fun div_fun/2, A, B) == ExpectedDiv),
  log("harm", Zip(fun calculate_harmonic/2, A, B) == ExpectedHarm).

%%Напишите lambda-функцию, которая для каждой точки точки из списка dotsA вычисляет расстояние до всех точек
%%из списка точек dotsB в пространстве размерности N.
%%Напишите функцию, которая читает следующую нотацию:
%%[
%%{dimension, 5},
%%{dotsA, [{1, 2, 3, 4, 5}, {7, 8, 9, 10, 11}]},
%%{dotsB, [{0, 0, 0, 0, 0}, {-1, -2, -3, -4, -5}]}
%%]
%%и возвращает: [ 5.360220495669696, 10.720440991339393, 12.988650063170537, 18.14700750425752 ]

calc(PointA, PointB) ->
  math:sqrt(pre_calc(PointA, PointB)).

pre_calc(PointA, PointB) ->
  case {PointA, PointB} of
    {[], []} -> 0;
    {[HA | TA], [HB | TB]} ->
      Diff = HA - HB,
%%      log("Diff", Diff),
      Result = Diff * Diff + pre_calc(TA, TB),
%%      log("Result", Result),
      Result;
    Other -> log("error_calc", Other)
  end.

make_pairs(Func, ListA, ListB) ->
  %%log("ListA", ListA), log("ListB", ListB),
  case ListA of
    [] -> [];
    [HA | TA] ->
      Iteration =
        case ListB of
          [] -> [];
          [HB | TB] ->
%%            log("HA", HA), log("HB", HB),
            [Func(tuple_to_list(HA), tuple_to_list(HB)) | make_pairs(Func, [HA], TB)]
        end,
      Iteration ++ make_pairs(Func, TA, ListB)
  end.

test_calculate_distance() ->
  CalcDistance =
    fun(In) ->
      [{dimension, Dimension}, {dotsA, DotsA}, {dotsB, DotsB}] = In,
      GetBad = fun(Dots) -> lists:filter(fun(Dot) -> tuple_size(Dot) =/= Dimension end, Dots) end,
      BadA = GetBad(DotsA),
      BadB = GetBad(DotsB),
      case {BadA, BadB} of
        {[], []} -> make_pairs(fun calc/2, DotsA, DotsB);
        {ErrorA, ErrorB} ->
          log("error_A", ErrorA),
          log("error_B", ErrorB)
      end
    end,

  Input = [
    {dimension, 5},
    {dotsA, [{1, 2, 3, 4, 5}, {7, 8, 9, 10, 11}]},
    {dotsB, [{0, 0, 0, 0, 0}, {-1, -2, -3, -4, -5}]}
  ],

  Expected = [7.416198487095663, 14.832396974191326, 20.37154878746336, 27.568097504180443],

  Actual = CalcDistance(Input),
  case Actual of
    Expected -> log("ok");
    Actual -> log("error", Actual)
  end.