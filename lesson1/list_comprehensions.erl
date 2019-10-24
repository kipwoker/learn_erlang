-module(list_comprehensions).

%% API
-export([]).
-compile(export_all).


cartesian_product(L1, L2) ->
  [{X, Y} || X <- L1, Y <- L2].

flat_list(ListOfList) ->
  [Item || List <- ListOfList, Item <- List].

test_get_dict_values() ->
  get_dict_values([
    #{
      tags => [awesome, erlang]
      },
    #{
      tags => [simple_tag]
    },
    #{
      tags => [just_atom, 'I am ok']
    }
  ]).

get_dict_values(Dicts) ->
  [Item || Dict <- Dicts, Item <- maps:get(tags, Dict, badarg)].

test_get_tuples() ->
  get_tuples(
    [
      john,
      doe,
      {age, 19},
      {height, 182},
      {weight, 72},
      london,
      britain
    ]
  ).

get_tuples(List) ->
  [Item || Item <- List, is_tuple(Item)].

get_atoms(List) ->
  [Item || Item <- List, is_atom(Item)].