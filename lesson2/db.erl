-module(db).

%% API
-export([]).
-compile(export_all).

%%db:new() -> Db.
%%db:destroy(Db) -> ok.
%%db:write(Key, Element, Db) -> NewDb.
%%db:delete(Key, Db) -> NewDb.
%%db:read(Key, Db) -> {ok, Element} | {error, instance}.
%%db:match(Element, Db) -> [Keyl, ..., KeyN].

new() -> [].

read(Key, Db) ->
  case Db of
    [] -> {error, instance};
    [{Key, Element} | _] -> {ok, Element};
    [_ | Tail] -> read(key, Tail)
  end.

write(Key, Element, Db) ->
  case read(Key, Db) of
    {error, instance} -> [{Key, Element}] ++ Db;
    {ok, _} -> Db
  end.

delete(Key, Db) ->
  case Db of
    [] -> [];
    [{Key, _} | Tail] -> Tail;
    [Head | Tail] -> [Head] ++ delete(Key, Tail)
  end.

match(Element, Db) ->
  case Db of
    [] -> [];
    [{Key, Element} | Tail] -> [Key] ++ match(Element, Tail);
    [_ | Tail] -> match(Element, Tail)
  end.

destroy(Db) -> ok.
