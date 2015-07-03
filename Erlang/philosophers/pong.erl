-module(pong).
-export([pingTest/0]).

ping() ->
    receive
    {ping,Pid} -> Pid!pong,
                  ping()
    end.

pingTest() -> spawn(fun() -> ping() end).

