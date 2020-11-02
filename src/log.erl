-module(log).
-compile(debug_info).
-export([logThis/4]).
-import(file, [open/2, write/2]).


logThis(Message, FileName, VClock, Modes) ->
    {ok, Device} = file:open(FileName ++ "_log.txt", Modes),
    try 
        writeLogs(Device, Message, VClock)
        after file:close(Device)
    end,
    ok.

writeLogs(S, Message, VClock) ->
    io:format(S, "~s~n~n~w~n~n", [Message, VClock]).
