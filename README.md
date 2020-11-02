# Erlang Vector

A logging module in Erlang for distributed systems (A part of IT303 Software Engneering course project)

To build:

```
erl -make
```

To run:

```
erl -pa ebin -sname <node_name>
```

To clean 

```
rm ebin/*.beam
```

Functions:

```
vclock:init() - starts the clock and waits to receive messages from connected nodes

util:broadcast(PName) - Sends a broadcast signal to the local process with registered
                                name PName, which then broadcasts its current vector

util:unicast(PName, Dest) - Sends a unicast signal to the local process with registered name 
                      PName, which then sends its current vector to a remote node named Dest

util:recordEvent(PName, EvName) - Registers a new event with name EvName, 
                                        in a local process with registered name PName

util:kill(PName) - Sends a kill signal to the local process with registered name PName 

```

The logs for each node will be written to a file named *\<NodeName\>*_log.txt
