-module(factorial).
-export([fac/1, fac2/1, fac3/1, app/2, start/0, other/0]).

fac(N) when N >= 0 -> 
          case N of
              0 -> 1;
              _ -> N * fac(N-1)
          end.

fac2(N) when N == 0 -> 1;
fac2(N) when N > 0 -> N * fac2(N-1).

fac3(0) -> 1;
fac3(N) when N > 0 -> N * fac3(N-1).

app([], Ys) -> Ys;
app([X|Xs], Ys) -> [X|app(Xs,Ys)].

start() -> Pid = spawn(fun() -> other() end),
           Pid!hallo,
           Pid!holla,
           Pid!{exit,42}.

other() -> receive
               hallo -> base:putStrLn("hallo empfangen"),
                        other();
               {exit,Msg} -> base:putStrLn("Exited with Msg: " ++ base:show(Msg));
               Msg -> base:putStrLn("Other Msg: " ++ base:show(Msg)),
                      other()
           end.
