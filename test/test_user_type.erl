-module(test_user_type).

-export_type([b/0]).

-type a() :: term().
-type b() :: a(). % to test user_type
