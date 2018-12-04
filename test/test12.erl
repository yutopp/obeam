-module(test12).

-export([f/0, g/1]).

%% test case for tuple expr
f() -> {ok, 42}.

%% test case for tuple pattern
g({ok, 42}) -> ok.
