-module(test12).

-export([f/0]).

%% test case for tuple expr
f() -> {ok, 42}.
