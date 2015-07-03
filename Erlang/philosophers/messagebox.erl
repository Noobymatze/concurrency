-module(messagebox).
-export([start/0]).

test() ->
    receive
        message1 -> base:putStrLn("Erster Fall: "),
                          test();
        _ -> base:putStrLn("Zweiter Fall"),
                   test()
    end.

start() -> 
    Pid = spawn(fun() -> test() end),
    Pid!test,
    Pid!message1,
    Pid!message1,
    Pid!message1,
    Pid!halo,
    Pid!message1.
