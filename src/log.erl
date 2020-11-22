-module(log).
-compile(debug_info).
-export([logThis/4]).
-import(file, [open/2, write/2]).


logThis(Message, FileName, VClock, Modes) ->
    {ok, Device} = file:open(FileName ++ "_log.txt", Modes),
    try 
        writeLogs(Device, Message, VClock, FileName)
        after file:close(Device)
    end,
    ok.

writeLogs(S, Message, VClock, Node) ->
    io:format(S, "~s~n~s ~w~n", [Message, Node, VClock]).
