-module(test_record).

%% test case for record
-record(r, {a,
            b = 42,
            c :: string(),
            d = 57 :: integer()}).

-export([f/0]).

f() ->
    R = #r{a = 3, c = "hello"}, % Record Creation
    _ = R#r.c,                  % Record Field Access
    Index = #r.b,               % Record Field Index
    R#r{a = 100, c = "hoge"}.   % Record Update
