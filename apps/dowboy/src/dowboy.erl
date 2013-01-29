-module(dowboy).

-export([start/0, load/1]).

start() ->
    ok = application:start(erltrace),
    ok = application:start(jsx),
    ok = application:start(cowboy),
    application:start(dowboy).


fibo(0) ->
    0;
fibo(1) ->
    1;
fibo(N) when N > 1 ->
    fibo(N-1) + fibo(N-2).

load(N) ->
    {T, R} =timer:tc(fun fibo/1, [N]),
    io:format("fib(~p) took ~.2gs~n", [N, T/1000/1000]),
    R.
