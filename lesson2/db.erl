-module(db).

-import(file_manager, [
  read_all_lines/1,
  write_to_end/2,
  create_file/1,
  delete/1
]).

%% API
-export([new/0, read/2, write/3, delete/2, match/2, destroy/1]).

%%db:new() -> Db.
%%db:destroy(Db) -> ok.
%%db:write(Key, Element, Db) -> NewDb.
%%db:delete(Key, Db) -> NewDb.
%%db:read(Key, Db) -> {ok, Element} | {error, instance}.
%%db:match(Element, Db) -> [Keyl, ..., KeyN].

test() ->
  Db = new(),
  {error, instance} = read("RandomKey", Db),
  Db1 = write("Key1", "Element1", Db),
  {ok, "Element1"} = read(key1, Db1),
  Db2 = delete("Key1", Db1),
  {error, instance} = read("Key1", Db2),
  Db3 = write("K1", "E1", Db2),
  Db4 = write("K2", "E1", Db3),
  ["K1", "K2"] = match("E1", Db4),
  ok = destroy(Db4).

int_to_string(Int) ->
  lists:flatten(io_lib:format("~p", [Int])).

generate_name() ->
  Id = erlang:unique_integer(),
  int_to_string(Id).

apply_new_content(Db, NewDbContent) ->
  {{db_name, DbName}, _} = Db,
  file_manager:write_to_end(),
  {{db_name, DbName}, {db_content, NewDbContent}}.

get_content(Db) ->
  {_, {db_content, DbContent}} = Db,
  DbContent.

new(DbName) ->
  {{db_name, DbName}, {db_content, #{}}}.

new() ->
  DbName = generate_name(),
  file_manager:create_file(DbName),
  new(DbName).

read(Key, Db) ->
  {_, {db_content, DbContent}} = Db,
  case maps:get(Key, DbContent, key_not_exist) of
    key_not_exist -> {error, instance};
    Element -> {ok, Element}
  end.

write(Key, Element, Db) ->
  DbContent = get_content(Db),
  NewDbContent = maps:put(Key, Element, DbContent),
  apply_new_content(Db, NewDbContent).

delete(Key, Db) ->
  DbContent = get_content(Db),
  NewDbContent = maps:remove(Key, DbContent),
  apply_new_content(Db, NewDbContent).

match(Element, Db) ->
  DbContent = get_content(Db),
  Predicate = fun(_,V) -> V == Element end,
  FilteredDbContent = maps:filter(Predicate, DbContent),
  maps:keys(FilteredDbContent).

destroy(Db) ->
  file_manager:delete(),
  ok.
