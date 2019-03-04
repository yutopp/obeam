-module(test_op_pattern).

-export([f/1]).

%% test case for binary operator pattern
f("abc" ++ T) -> T;
f(1 + 2) -> 3.
