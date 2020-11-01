-module(vector).
-export([sendMsg/2, monitorRecv/0, init/2]).
-import(log, [logThis/3]).
-import(vclock, [tick/2, getTicks/2, merge/2, printVC/1, newProcess/3]).

init(Pid, LogName) ->
    CurVC = vclock:tick(Pid, vclock:newProcess(Pid, 0, #{})),
    log:logThis("Initialization complete", LogName, maps:to_list(CurVC), [write]),
    log:logLocalEvent("Event " ++ integer_to_list(vclock:getTicks(Pid, CurVC)), LogName, CurVC, Pid).

sendMsg(Pid, Message) ->
    Pid ! Message.

monitorRecv() ->
    receive
        _ -> ok
    end.