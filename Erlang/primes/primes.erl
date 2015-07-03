-module(primes).
-export([start/0, nextPrime/1]).

allNumbers(N) ->
    receive
        {next, Pid} -> Pid!{number, N},
                       allNumbers(N + 1)
    end.

next(Parent, Prime, Client) ->
    Parent!{next, self()},
    receive
        {number, N} -> 
            case N rem Prime == 0 of
                true -> next(Parent, Prime, Client);
                false -> Client!{number, N},
                         withPrime(Parent, Prime)
            end
    end.

withPrime(Parent, Prime) ->
    receive
        {next, Pid} -> next(Parent, Prime, Pid)
    end.
    

noPrime(Parent) ->
    receive
        {next, Pid} -> Parent!{next, self()},
                       receive 
                           {number, Prime} -> Pid!{number, Prime, sieve(self())},
                                              withPrime(Parent, Prime)
                       end
    end.

sieve(PrimeGenerator) -> 
    spawn(fun() -> noPrime(PrimeGenerator) end).

nextPrime(Collector) ->
    Collector!{next, self()},
    receive 
        {prime, Prime} -> Prime
    end.

start() -> 
    Pid = spawn(fun() -> allNumbers(2) end),
    spawn(fun() -> collect(sieve(Pid)) end).

collect(PrimeGenerator) ->
    receive
        {next, P} -> 
            PrimeGenerator!{next, self()},
            receive
                {number, Prime, NextGenerator} -> 
                    P!{prime, Prime},
                    collect(NextGenerator)
            end
    end.

