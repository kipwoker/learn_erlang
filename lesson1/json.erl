-module(json).

-import(io_lib, [
char_list/1
]).

%% API
-export([
  new/1,
  read/2,
  write/3
]).

%%Key = string(),
%%KeySpec = string(),
%%BasicValue = string() | boolean() | integer() | float()
%%ValueSpec = BasicValue | [BasicValue] | {Key, ValueSpec} | [{Key, ValueSpec}]
%%
%%json:new([{Key, ValueSpec}]) -> JsonObj.
%%json:read(KeySpec, JsonObj) -> {ok, ValueSpec} | {error, not_found}
%%json:write(KeySpec, ValueSpec, JsonObj) -> JsonObj | {error, not_found}

is_string(Value) -> io_lib:char_list(Value).

validate_key(Key) -> is_string(Key).
validate_keySpec(Key) -> is_string(Key).
validate_basicValue(BasicValue) ->
  is_string(BasicValue) or is_boolean(BasicValue) or is_integer(BasicValue) or is_float(BasicValue).
validate_valueSpec(ValueSpec) ->
  case ValueSpec of
    [Value] -> validate_valueSpec(Value);
    {Key, Value} -> validate_key(Key) and validate_valueSpec(Value);
    Value -> validate_basicValue(Value)
  end.

new([{Key, ValueSpec}]) ->
  IsValid = validate_key(Key) and validate_valueSpec(ValueSpec),
  case IsValid of
    true -> #{Key = ValueSpec};
    false -> {error, badarg}
  end.


read(KeySpec, JsonObj) ->
  IsValid = validate_keySpec(KeySpec),
  case {IsValid, maps:find(KeySpec, JsonObj)} of
    {false, _} -> {error, badarg};
    {_, {ok, Value}} -> {ok, Value};
    {_, error} -> {error, not_found}
  end.

write(KeySpec, ValueSpec, JsonObj) ->
  IsValid = validate_keySpec(KeySpec) and validate_valueSpec(ValueSpec),
  case IsValid of
    true -> maps:put(KeySpec, ValueSpec, JsonObj);
    false -> {error, badarg}
  end.

