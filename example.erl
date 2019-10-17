-module(example).
-import(maps, [
put/3,
get/3
]).
-import(io, [
format/2
]).
-import(lists, [
foreach/2
]).

%% API
-export([
  match/2,
  test_match/0
]).

%%TESTS

test_match() ->
  Cases = [
    {[{var, a}], [1], [{bind, a, 1}]},
    {[1], [1], [1]},
    {[1], [0], {values_not_equal, 1, 0}},
    {[{var, a}, {var, b}, 3, 4], [1, 2, 3, 4], [{bind, a, 1}, {bind, b, 2}, 3, 4]},
    {[{var, a}, {var, a}, 3, 4], [1, 1, 3, 4], [{bind, a, 1}, {bind, a, 1}, 3, 4]},
    {[{var, a}, {var, a}, 3, 4], [1, 2, 3, 4], {another_bind, 1, 2}},
    {[0, 2, 3, 4], [1, 2, 3, 4], {values_not_equal, 0, 1}},
    {[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]},
    {[1, {var, a}, 3, [{var, b}, 0, {var, a}], 7], [1, 2, 3, [4, 0, 2], 7], [1, {bind, a, 2}, 3, [{bind, b, 4}, 0, {bind, a, 2}], 7]},
    {[1, {var, a}, 3, [{var, b}, 0, {var, a}], 7], [1, 2, 3, [4, 0, 5], 7], {another_bind, 2, 5}},
    {[1, [{var, a}]], [1, [2]], [1, [{bind, a, 2}]]},
    {[[{var, a}], 1], [[2], 1], [[{bind, a, 2}], 1]}
  ],
  test_match_cases(Cases).

test_match_cases(Cases) ->
  case Cases of
    [Case] -> test_match_case(Case);
    [Case | Tail] -> test_match_case(Case), test_match_cases(Tail)
  end.

test_match_case(Case) ->
  {Template, Input, Expected} = Case,
  io:format("~p --> ", [Template]), io:format("~p -->", [Input]), io:format("~p", [Expected]),
  Actual = match(Template, Input),
  case Actual of
    Expected -> io:format("--> OK~n");
    {error, Expected} -> io:format("--> OK~n");
    {error, _} -> io:format("--> ERROR: ~p~n", [Actual]);
    Unexpected -> io:format("--> UNEXPECTED: ~p~n", [Unexpected])
  end
.


%%

safe_prepend(Item, List) ->
  case {Item, List} of
    {error, Content} -> {error, Content};
    {_, {error, Content}} -> {error, Content};
    {_, [{error, Content}| _]} -> {error, Content};
    {_, L} when is_list(L) -> [Item] ++ List
  end.

match(Template, Input) -> match(Template, Input, #{}).

match(Template, Input, Vars) ->
  case {Template, Input} of
    {[], []} -> [];
    {[{var, Key} | TemplateTail], [Value | InputTail]} ->
      case maps:get(Key, Vars, not_found) of
        not_found ->
          NewVars = maps:put(Key, Value, Vars),
          Bind = {bind, Key, Value},
          Tail = match(TemplateTail, InputTail, NewVars),
          safe_prepend(Bind, Tail);
        Value ->
          Bind = {bind, Key, Value},
          Tail = match(TemplateTail, InputTail, Vars),
          safe_prepend(Bind, Tail);
        AnotherValue -> {error, {another_bind, AnotherValue, Value}}
      end;
    {[TemplateSubList | TemplateTail], [InputSubList | InputTail]}
      when
        is_list(TemplateSubList) and
        is_list(InputSubList) ->
      Head = match(TemplateSubList, InputSubList, Vars),
      Tail = match(TemplateTail, InputTail, Vars),
      safe_prepend(Head, Tail);
    {[InputHead | TemplateTail], [InputHead | InputTail]} ->
      Tail = match(TemplateTail, InputTail, Vars),
      safe_prepend(InputHead, Tail);
    {[TemplateHead | _], [InputHead | _]} -> {error, {values_not_equal, TemplateHead, InputHead}};
    {_, []} -> {error, different_length};
    {[], _} -> {error, different_length};
    {ErrorTemplate, ErrorInput} -> {error, {unknown, ErrorTemplate, ErrorInput}}
  end.