-module(vclock).
-compile(debug_info).
-export([tick/2, getTicks/2, merge/3, printVC/1, newProcess/1, newLocalEvent/2, 
            newProcessVector/0, broadcastVector/1, sendMsgToNode/2, init/0]).
-import(log, [logThis/4]).
-record(clock, { localTime=0,
                  events=#{}}).
-record(process, { pName="",
                   pClock=#clock{},
                   tVector=#{}
                }).

newProcess(PName, Ticks, Vector) ->
    maps:put(PName, Ticks, Vector).

tick(PName, Vector) ->
    maps:update(PName, maps:get(PName, Vector, -1)+1, Vector).

getTicks(PName, Vector) ->
    maps:get(PName, Vector, -1).

printVC(VC) ->
    io:format("~w~n", [VC]).

init() ->
    ProcessName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    NewP = newProcess(ProcessName),
    log:logThis("Initialization complete", atom_to_list(ProcessName), maps:to_list(NewP#process.tVector), [write]),
    AtomName = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    register(AtomName, spawn(fun() -> receiveMessage(NewP) end)).

merge(Vector1, Vector2, ThisP = #process{pName=PName}) ->
    List1 = maps:to_list(Vector1),
    List2 = maps:to_list(Vector2),
    Result = [ {Key, erlang:max(Val, Val2)} || {Key, Val} <- List1, {Key2, Val2} <- List2, 
    maps:is_key(Key2, Vector1) and maps:is_key(Key, Vector2) ],
    #process{
        pName=PName,
        tVector=maps:from_list(Result),
        pClock=ThisP#process.pClock   
    }.

newProcessVector() ->
    Nodes = [ list_to_atom(lists:nth(1, string:tokens(atom_to_list(NName), "@"))) || NName <- lists:append(nodes(), [node()])],
    maps:from_list([{Node, 0} || Node <- Nodes]).

newProcess(PName) ->
    #process{
        pName=PName,
        tVector=newProcessVector()}.

broadcastVector(P) ->
    ThisNode = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    Nodes = [ list_to_atom(lists:nth(1, string:tokens(atom_to_list(NName), "@"))) || NName <- nodes()],
    NewP = newLocalEvent("Broadcasting message", P),
    [{Proc, Node} ! {ThisNode, NewP#process.tVector} || Proc <- Nodes, Node <- nodes()].

sendMsgToNode(P, Node) ->
    ThisNode = list_to_atom(lists:nth(1, string:tokens(atom_to_list(node()), "@"))),
    NodeReg = list_to_atom(lists:nth(1, string:tokens(atom_to_list(Node), "@"))),
    NewP = newLocalEvent("Sending Msg : " ++ integer_to_list(P#process.pClock#clock.localTime + 1), P),
    {NodeReg, Node} ! {ThisNode, NewP#process.tVector}.

newLocalEvent(EvName, #process{pName=PName, tVector=Vector, pClock=Clock}) ->
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

receiveMessage(P = #process{}) ->
    receive
        {Node, Vector} -> 
            newLocalEvent("Received Msg From - " ++ atom_to_list(Node) ++ " : ", P), 
              io:format("~s~n", ["Valid message received"]);
        broadcast -> broadcastVector(P), 
                    io:format("~s~n", ["Valid message received"]);
        _ -> io:format("~p~n", [P])
    after
        20000 ->
            io:format("~s~n", ["No message received, But works!"])
    end.
