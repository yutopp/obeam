-module(test13).
-export([f/0]).

%% test case for list expressions
f() ->
    List = [3, 1, 4, 1, 5],
    List = [3 | [1 | [4 | [1 | [5 | []]]]]],
    [X * 2 || X <- List, X >= 3].
