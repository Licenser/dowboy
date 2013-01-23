-module(dowboy).

-export([start/0]).

start() ->
    ok = application:start(erltrace),
    ok = application:start(jsx),
    ok = application:start(cowboy),
    application:start(dowboy).
