-module(util).
-compile(debug_info).
-export([broadcast/1, unicast/2, recordEvent/2, kill/1]).

broadcast(PName) ->
    PName ! broadcast.

unicast(PName, SNode) ->
    PName ! { unicast, SNode }.

recordEvent(PName, EvName) ->
    PName ! { event, EvName }.

kill(PName) ->
    PName ! kill.

