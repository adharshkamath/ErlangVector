-module(variables).
-export([func/1]).

func(Name) ->
    Greet = fun(FName) -> "Hello, " ++ FName ++ "~n" end,   % Functions can be values
    io:format(Greet(Name)).