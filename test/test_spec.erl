-module(test_spec).

-export([f/1, g/1]).

-spec f(A) -> integer() when A :: integer().
f(N) -> N.

-spec g(A) -> B when A :: integer(),
                     B :: integer().
g(N) ->
    N * N.
