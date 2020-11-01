-module(log).
-export([logThis/4, logLocalEvent/4, newProcess/2]).
-import(file, [open/2, write/2]).


logThis(Message, FileName, VClock, Modes) ->
    {ok, Device} = file:open(FileName ++ "-log.txt", Modes),
    try 
        writeLogs(Device, Message, VClock)
        after file:close(Device)
    end,
    ok.

writeLogs(S, Message, VClock) ->
    io:format(S, "~s~n~w~n", [Message, VClock]).

logLocalEvent(Message, FileName, Vc, Pid) ->
    NewVC = vclock:tick(Pid, Vc),
    log:logThis(Message, FileName, maps:to_list(NewVC), [append]),
    ok.

newProcess(NameOrID, Time) ->
    #{NameOrID=>Time}.