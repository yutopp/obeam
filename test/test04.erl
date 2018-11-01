-module(test04).

-export([f/0]).

f() ->
    (#{a => 1, b => 2})#{c => 3}.
