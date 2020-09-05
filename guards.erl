-module(guards).
-export([greet/1]).

greet(Day) when Day =:= "Monday" ->
    io:format("Have a great Monday!~n");

greet(_) -> io:format("Have a great week day!~n").
    