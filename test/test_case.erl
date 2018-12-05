-module(test_case).

-export([f/1]).

f(R) ->
    case R of
        ok -> 1;
        error -> 2
    end.
