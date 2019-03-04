-module(test_guard).

-export([f/1]).

%% test case for guards of toplevel
f(N) when is_integer(N) ->
    0;
f(N) when erlang:is_integer(N) ->
    1;
f(N) when erlang:'=:='(N, 2) ->
    2;
f(_) ->
    3.
