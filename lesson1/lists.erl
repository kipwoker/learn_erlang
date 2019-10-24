-module(lists).

%% API
-export([
  create/1,
  create_reverse/1,
  print_seq/1,
  print_odds/1,
  filter/2,
  reverse/1,
  dna_to_rna/1
]).


create(N) ->
  case N of
    1 -> [1];
    _ -> create(N - 1) ++ [N]
  end.

create_reverse(N) ->
  case N of
    1 -> [1];
    _ -> [N] ++ create(N - 1)
  end.

print_seq(N) ->
  [io:fwrite(X) || X <- create(N)].

print_odds(N) ->
  [io:fwrite(X) || X <- create(N), X rem 2 == 1].

filter(List, LessOrEqual) ->
  [X || X <- List, X =< LessOrEqual].

reverse(List) ->
  case List of
    [] -> [];
    [H | T] -> reverse(T) ++ [H]
  end.

%%concatenate(ListOfList) ->
%%  case ListOfList of
%%    [H | T] ->
%%  end.

isAll(Seq, Filter) ->
  case Seq of
    [H] -> Filter(H);
    [H|T] -> Filter(H) or isAll(T, Filter)
  end.

parse(Seq) ->
  Map = #{
    "G" => g,
    "C" => c,
    "T" => t,
    "A" => a
  },
  IsLetters = isAll(Seq, fun (Sym) -> Sym == 'G' or Sym == 'C' or Sym == 'T' or Sym == 'A' end),
  IsAtoms = isAll(Seq, fun (Sym) -> Sym == g or Sym == c or Sym == t or Sym == a end),

  case {IsLetters, IsAtoms} of
    {true, false} -> [maps:get(X, Map) || X <- Seq];
    {false, true} -> Seq;
    _ -> badarg
  end.


dna_to_rna(Seq) ->
  case parse(Seq) of
    badarg -> badarg;
    ParsedSeq ->
      DnaToRnaMap = #{
        g => c,
        c => g,
        t => a,
        a => u
      },

      [maps:get(X, DnaToRnaMap) || X <- ParsedSeq]
  end.

