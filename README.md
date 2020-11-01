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

Currently supports:
```
vector:init(Pid, LogFileName) - Initializes the log file for a given process

vector:sendMsg(Pid, Message) - Sends *Message* to process with Pid

vclock:newProcess(PName, Ticks, Vector) - Adds a new process to the existing vector clock and initializes its ticks to one

vclock:tick(PName, Vector) - Increments the ticks of the given PName by one

vclock:merge(VectorClock1, VectorClock2) - Merges two vector clocks, taking the highest value of time for common processes
