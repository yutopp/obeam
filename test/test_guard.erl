-module(test_guard).

-export([f/1]).

f(N) when is_integer(N) ->
    0;
f(_) ->
    2.
