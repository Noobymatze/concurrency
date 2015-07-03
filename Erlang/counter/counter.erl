-module(counter).
-export([start/1]).


running(N, Timeout, Pid) ->
    receive
        stop -> stopped(N, Timeout, Pid);
        close -> whohooo
    after Timeout -> Pid!{setValue, N + 1},
                     running(N + 1, Timeout, Pid)
    end.

stopped(N, Timeout, Gui) ->
    receive
        start -> running(N, Timeout, Gui);
        stop -> stopped(N, Timeout, Gui);
        close -> whohooo
    end.

eventHandler(N, Timeout, Gui, Counter) ->
    receive
        {setValue, V} -> 
            Gui!{setValue, V},
            eventHandler(V, Timeout, Gui, Counter);
        start -> Counter!start,
                 eventHandler(N, Timeout, Gui, Counter);
        {close, GuiPid} -> Counter!close;
        stop -> Counter!stop,
                eventHandler(N, Timeout, Gui, Counter);
        copy -> spawn(fun() -> create(0, Timeout) end),
                eventHandler(N, Timeout, Gui, Counter);
        closeAll -> ok
    end.

create(N, Timeout) ->
    Me = self(),
    Gui = counterGui:start(N, self()),
    Counter = spawn(fun() -> stopped(N, Timeout, Me) end),
    eventHandler(N, Timeout, Gui, Counter).

start(Timeout) -> create(0, Timeout).


