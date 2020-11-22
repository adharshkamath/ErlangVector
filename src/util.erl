-module(util).
-compile(debug_info).
-export([broadcast/0, unicast/1, recordEvent/1, kill/0, connect/1]).

broadcast() ->
    PName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    PName ! broadcast.

unicast(SNode) ->
    PName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    PName ! { unicast, SNode }.

recordEvent(EvName) ->
    PName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    PName ! { event, EvName }.

kill() ->
    PName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    PName ! kill.

connect(NodeName) ->
    net_adm:ping(NodeName).