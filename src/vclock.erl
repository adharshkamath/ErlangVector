-module(vclock).
-compile(debug_info).
-export([init/0]).
-import(log, [logThis/4]).
-record(clock, { localTime=1,
                  events=#{}}).
-record(process, { pName="",
                   pClock=#clock{},
                   tVector=#{}
                }).

init() ->
    ProcessName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    NewP = newProcess(ProcessName),
    log:logThis("Initialization complete", atom_to_list(ProcessName), NewP#process.tVector, [write]),
    AtomName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    register(AtomName, spawn(fun() -> receiveMessage(NewP) end)).

merge(Vector1, Vector2, ThisP = #process{pName=PName}, MsgText) ->
    Result = maps:fold(fun(K, V, Map) -> maps:update_with(K, fun(X) -> erlang:max(X, V) end, V, Map) end, Vector1, Vector2),
    ProcessName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    log:logThis(MsgText ++ " -- Updated local vector clock", atom_to_list(ProcessName), Result, [append]),
    #process{
        pName=PName,
        tVector=Result,
        pClock=ThisP#process.pClock   
    }.

newProcessVector() ->
    Nodes = [ list_to_atom(lists:nth(1, string:tokens(atom_to_list(NName), "@"))) || 
                    NName <- lists:append(nodes(), [node()])],
    maps:from_list([{Node, 1} || Node <- Nodes]).

newProcess(PName) ->
    #process{
        pName=PName,
        tVector=newProcessVector()}.

broadcastVector(P) ->
    ThisNode = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    Nodes = [ list_to_atom(lists:nth(1, string:tokens(atom_to_list(NName), "@"))) || NName <- nodes()],
    NewP = newLocalEvent("Broadcasting message", P),
    [{Proc, Node} ! {ThisNode, NewP#process.tVector} || Proc <- Nodes, Node <- nodes()],
    NewP.

sendMsgToNode(P, Node) ->
    ThisNode = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    NodeReg = list_to_atom(lists:nth(1, string:tokens(atom_to_list(Node), "@"))),
    NewP = newLocalEvent("Sending to " ++ lists:nth(1, string:tokens(atom_to_list(Node), "@")) ++ ", Time : " ++ 
                            integer_to_list(P#process.pClock#clock.localTime + 1), P),
    {NodeReg, Node} ! {ThisNode, NewP#process.tVector},
    NewP.

recvEvent(EvName, #process{pName=PName, tVector=Vector, pClock=Clock}) ->
    ThisName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    NewTime = Clock#clock.localTime + 1,
    NewVector = maps:update(ThisName, NewTime, Vector),
    NewEvents = maps:put(list_to_atom(EvName), NewTime, Clock#clock.events),
    NewClock = #clock{localTime=NewTime, events=NewEvents},
    #process{
            pName=PName,
            pClock=NewClock,
            tVector=NewVector
        }.

newLocalEvent(EvName, #process{pName=PName, tVector=Vector, pClock=Clock}) ->
    ThisName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    NewTime = Clock#clock.localTime + 1,
    NewVector = maps:update(ThisName, NewTime, Vector),
    NewEvents = maps:put(list_to_atom(EvName), NewTime, Clock#clock.events),
    NewClock = #clock{localTime=NewTime, events=NewEvents},
    ProcessName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    log:logThis(EvName, atom_to_list(ProcessName), NewVector, [append]),
    #process{
            pName=PName,
            pClock=NewClock,
            tVector=NewVector
        }.

receiveMessage(P = #process{}) ->
    receive
        broadcast -> 
                    NewP = broadcastVector(P), 
                    io:format("~s~n", ["Valid message sent (Broadcast)"]),
                    receiveMessage(NewP);
        { unicast, Node } -> 
                    NewP = sendMsgToNode(P, Node), 
                    io:format("~s~n", ["Valid message sent (Unicast)"]),
                    receiveMessage(NewP);
        { event, EvName } -> 
                    NewP = newLocalEvent("Event - " ++ EvName, P), 
                    io:format("~s~n", ["Valid Event registered"]),
                    receiveMessage(NewP);
        { Node, Vector } -> 
                    MsgString = "Received Msg From - " ++ atom_to_list(Node),
                    NewP = recvEvent(MsgString, P), 
                    io:format("~s~n", ["Vector message received"]),
                    receiveMessage(merge(Vector, NewP#process.tVector, NewP, MsgString));
        kill -> 
                io:format("~p~nTeminating graciously..~n", [P]);
        _ ->     
                io:format("~p~nInvalid message encountered~nRestarting..", [P]),
                receiveMessage(P)
    after
        6000000 ->                                          % 10 mins
            io:format("~s~n", ["No message received!"]),
            timeout
    end.
