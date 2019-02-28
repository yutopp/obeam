-module(test_bitstring).

-export([f/0]).

%% test case for bitstring constructor expr
f() ->
    <<0,  % value
     1:1, % value and size
     2/little-signed-integer,          % value and type specifier list
     3:3/little-signed-unit:8-integer, % value, size, and type specifier list with size unit
     (2 + 2):(2 + 2) % value and size are expr
     >>.
