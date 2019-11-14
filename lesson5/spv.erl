-module(spv).
-behaviour(supervisor).

%% API
-export([
  init/1
]).


init(_Init) ->
  Strategy =
    one_for_one, %% умер перезапустили
    %% one_for_all умер -- перезапустили всех
    %% rest_for_one перезапуск всех после умершего

  Intensity = 10,
  Period = 60,
  Flags = {Strategy, Intensity, Period},
  Children = [
    #{
      id => locker,
      start => {locker3, start_link, []},
      restart => permanent, %% transient | temporary
      shutdown => brutal_kill, %% integer() > 0
      modules => [locker3],
      type => worker % | supervisor
    }
  ],
  {ok, {Flags, Children}}.