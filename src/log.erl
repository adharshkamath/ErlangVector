-module(log).
-export([logThis/4, logLocalEvent/4, newProcess/2]).
-import(file, [open/2, write/2]).


logThis(Message, FileName, Vcs, Modes) ->
    {ok, Device} = file:open(FileName ++ "-log.txt", Modes),
    try 
        writeLogs(Device, Message, Vcs)
        after file:close(Device)
    end,
    ok.

writeLogs(S, Message, Vcs) ->
    io:format(S, "~s~n", [Message]),
    io:format(S, "~w~n", [Vcs]).

logLocalEvent(Message, FileName, Vc, Pid) ->
    NewVC = vclock:tick(Pid, Vc),
    log:logThis(Message, FileName, maps:to_list(NewVC), [append]),
    ok.

newProcess(NameOrID, Time) ->
    #{NameOrID=>Time}.