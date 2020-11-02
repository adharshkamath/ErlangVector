# Erlang Vector

A logging module in Erlang for distributed systems (A part of IT303 Software Engneering course project)

To build:

```
erl -make
```

To run:

```
erl -pa ebin
```

To clean 

```
rm ebin/*.beam
```

Functions:

```
vclock:init() - starts the clock and waits to receive messages from connected nodes

util:broadcast(ProcessName) - Sends a broadcast signal to the local process with registered name 
                                ProcessName, which then broadcasts its current vector

util:unicast(ProcessName, Recepient) - Sends a unicast signal to the local process with with registered name 
                                ProcessName, which then sends its current vector to a remote node named Recepient

util:recordEvent(ProcessName, EventName) - Registers a new event with name EventName, 
                                        in a local process with registered name ProcessName

util:kill(ProcessName) - Sends a kill signal to the local process with registered name ProcessName 

```