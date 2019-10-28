-module(file_manager).

%% API
-export([
  read_all_lines/1,
  write_to_end/2,
  create_file/1,
  delete/1
]).

read_all_lines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
  after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> [];
    Line -> Line ++ get_all_lines(Device)
  end.

create_file(FileName) ->
  file:write_file(FileName, <<"">>, [append]).

write_to_end(FileName, Line) ->
  NewLine = io_lib:nl(),
  FileLine = [Line, NewLine],
  file:write_file(FileName, FileLine, [append]).

delete(FileName) ->
  file:delete(FileName).