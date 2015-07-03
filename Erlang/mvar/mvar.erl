-module(mvar).
-export([
     new/0,
     take/1,
     put/2,
     read/1,
     isEmpty/1,
     clear/1,
     swap/2,
     tryTake/1,
     tryPut/2,
     overWrite/2
]).

empty() ->
    receive 
        {put, Value, Pid} -> full(Value), 
                             Pid!ok;
        {check, Pid} -> Pid!empty,
                        empty();
        {tryTake, Pid} -> Pid!empty,
                          empty();
        {tryPut, Value, Pid} -> Pid!ok,
                                full(Value);
        {overWrite, Value, Pid} -> Pid!overwritten,
                                   full(Value)
    end.

full(Value) ->
    receive
        {take, Pid} -> Pid!{taken, Value},
                       empty();
        {read, Pid} -> Pid!{taken, Value},
                       full(Value);
        {check, Pid} -> Pid!full,
                        full(Value);
        {clear, Pid} -> Pid!cleared,
                        empty();
        {swap, NewValue, Pid} -> Pid!{swapped, Value},
                                full(NewValue);
        {tryTake, Pid} -> Pid!{taken, Value},
                          empty();
        {tryPut, NewValue, Pid} -> Pid!full,
                                   full(Value);
        {overWrite, NewValue, Pid} -> Pid!overwritten,
                                      full(NewValue)

    end.

take(MVar) ->
    MVar!{take, self()},
    receive
        {taken, Value} -> Value
    end.

put(MVar, Value) ->
    MVar!{put, Value, self()},
    receive
        ok -> wooho
    end.

read(MVar) ->
    MVar!{read, self()},
    receive
        {taken, Value} -> Value
    end.

isEmpty(MVar) ->
    MVar!{check, self()},
    receive
        empty -> true;
        full -> false
    end.

clear(MVar) ->
    MVar!{clear, self()},
    receive
        cleared -> true
    end.

swap(MVar, Value) ->
    MVar!{swap, Value, self()},
    receive
        {swapped, OldValue} -> OldValue
    end.

tryTake(MVar) ->
    MVar!{tryTake, self()},
    receive
        {taken, Value} -> Value;
        empty -> empty
    end.

tryPut(MVar, Value) ->
    MVar!{tryPut, Value, self()},
    receive
        ok -> true;
        full -> false
    end.

overWrite(MVar, Value) ->
    MVar!{overWrite, Value, self()},
    receive
        overwritten -> true
    end.

takeTimeout(MVar, TimeoutMillis) ->
    Pid = self(),
    spawn(fun() -> timeout(MVar, Pid, TimeoutMillis) end),
    receive 
        {taken, Value} -> Value;
        timeout -> empty
    end.

% Falsch!!!!
timeout(MVar, Pid, TimeoutMillis) ->
    MVar!{take, self()},
    receive
        {taken, Value} -> Pid!{taken, Value}
    after TimeoutMillis ->
        Pid!timeout
    end.

new() -> spawn(fun() -> empty() end).

