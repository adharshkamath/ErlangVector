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

getVCFormat(VClock) ->
    TempVC = maps:fold(fun(K,V,AccIn) when is_atom(K) -> 
                                            AccIn ++ "\"" ++ atom_to_list(K) ++ "\"" ++ ":" ++ integer_to_list(V) ++ ","
                                        end, 
                        "{", VClock),
    string:substr(TempVC, 1, length(TempVC)-1) ++ "}".
    

writeLogs(S, Message, VClock, Node) ->
    io:format(S, "~s ~s~n~s~n", [Node, getVCFormat(VClock), Message]).
