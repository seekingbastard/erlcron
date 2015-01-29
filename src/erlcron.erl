
-module(erlcron).

-export([start/0, stop/0, env/1]).

start() ->
    application:start(erlcron).

stop() ->
    application:stop(erlcron).

env(undefined) -> [];
env({ok,Value}) -> Value;
env(Name) ->
    env(application:get_env(?MODULE,Name)).

