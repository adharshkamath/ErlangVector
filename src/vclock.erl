-module(vclock).
-export([tick/2, getTicks/2, merge/2, printVC/1, newProcess/3]).
-import(log, [logThis/3]).

newProcess(PName, Ticks, Vector) ->
    maps:put(PName, Ticks, Vector).

tick(PName, Vector) ->
    maps:update(PName, maps:get(PName, Vector, -1)+1, Vector).

getTicks(PName, Vector) ->
    maps:get(PName, Vector, -1).

printVC(VC) ->
    io:format("~w~n", [VC]).

merge(VC1, VC2) ->
    List1 = maps:to_list(VC1),
    List2 = maps:to_list(VC2),
    Result = [ {Key, erlang:max(Val, Val2)} || {Key, Val} <- List1, {Key2, Val2} <- List2, 
    maps:is_key(Key2, VC1) and maps:is_key(Key, VC2) ],
    maps:from_list(Result).
