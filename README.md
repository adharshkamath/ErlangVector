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

util:broadcast() - Sends a broadcast signal to the local process,
                             which then broadcasts its current vector

util:unicast(Dest) - Sends a unicast signal to the local process, which then 
                        sends its current vector to a remote node named Dest

util:recordEvent(EvName) - Registers a new event with name EvName, 
                                        in the local process

util:kill() - Sends a kill signal to the local process

```

The logs for each node will be written to a file named *\<NodeName\>*_log.txt
