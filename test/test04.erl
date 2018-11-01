-module(test04).

-export([f/0, g/1]).

f() ->
    (#{a => 1, b => 2})#{c => 3}.

g(#{a := N}) -> N.
