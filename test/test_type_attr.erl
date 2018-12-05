-module(test_type_attr).

-export_type([tuple/2, int/0]).

-type tuple(A, B) :: {A, B}.
-opaque int() :: integer().
