-module(test_try).

-export([try_catch/0, try_of_catch/0, try_after/0,
         try_of_after/0, try_catch_after/0, try_of_catch_after/0]).

-ifdef('OTP_RELEASE').
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new catch clause syntax or not.
-define(STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE, Class:Err:Stacktrace when Err =:= error -> {Class, Err, Stacktrace}; ).
-define(STACKTRACE_CLAUSE, ; Class:Err:Stacktrace -> {Class, Err, Stacktrace} ).
-else.
-define(STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE, ).
-define(STACKTRACE_CLAUSE, ).
-endif.

%% test case for try-catch expression
try_catch() ->
    try
        R = reason,
        error(R)
    catch
        Err when Err =:= error -> Err;
        error:Err when Err =:= error -> Err;
        ?STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE
        Err -> Err;
        error:Err -> Err
        ?STACKTRACE_CLAUSE
    end.

%% test case for try-of-catch expression
try_of_catch() ->
    try
        R = reason,
        error(R)
    of
        {ok, A} -> A;
        error -> error
    catch
        Err when Err =:= error -> Err;
        error:Err when Err =:= error -> Err;
        ?STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE
        Err -> Err;
        error:Err -> Err
        ?STACKTRACE_CLAUSE
    end.

%% test case for try-after expression
try_after() ->
    try
        R = reason,
        error(R)
    after
        Ok = ok,
        Ok
    end.

%% test case for try-of-after expression
try_of_after() ->
    try
        R = reason,
        error(R)
    of
        {ok, A} -> A;
        error -> error
    after
        Ok = ok,
        Ok
    end.

%% test case for try-catch-after expression
try_catch_after() ->
    try
        R = reason,
        error(R)
    catch
        Err when Err =:= error -> Err;
        error:Err when Err =:= error -> Err;
        ?STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE
        Err -> Err;
        error:Err -> Err
        ?STACKTRACE_CLAUSE
    after
        Ok = ok,
        Ok
    end.

%% test case for try-of-catch-after expression
try_of_catch_after() ->
    try
        R = reason,
        error(R)
    of
        {ok, A} -> A;
        error -> error
    catch
        Err when Err =:= error -> Err;
        error:Err when Err =:= error -> Err;
        ?STACKTRACE_WITH_GUARD_SEQUENCE_CLAUSE
        Err -> Err;
        error:Err -> Err
        ?STACKTRACE_CLAUSE
   after
        Ok = ok,
        Ok
    end.
