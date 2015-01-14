
-module(erlcron).

-export([start/0, stop/0]).

start() ->
    application:start(erlcron).

stop() ->
    application:stop(erlcron).


