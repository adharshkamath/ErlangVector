-module(const).
-export([func/0]).

-define(A, 42).
-define(SQUARED(X), (X)*(X)).

func() ->
    io:format("Value of A squared = ~p~n", [?SQUARED(123)]),
    io:format("Value of A = ~p~n", [?A]).