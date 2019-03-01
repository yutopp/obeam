-module(test_receive).

-export([f/0]).

%% test case for receive expression
f() ->
    receive
        ok -> ok;
        error -> error
    end.
