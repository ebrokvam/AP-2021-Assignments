-module(async).

-export([new/2, wait/1, poll/1]).

new(Fun, Arg) -> start(Fun, Arg).
wait(Aid) -> request_reply(Aid, wait).
poll(Aid) -> request_reply(Aid, poll).

start(Fun, Arg) -> spawn(fun() -> loop({Fun, Arg, nothing}) end).

request_reply(Pid, Request) -> 
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response 
  end.

loop({Fun, Arg, Result}) -> 
  receive
    {From, poll} ->
      case Result of
        nothing -> From ! {self(), nothing};
        Res -> From ! {self(), Res}
      end,
      loop({Fun, Arg, Result});
    {From, wait} ->
      case Result of
        nothing ->
          try 
            Result = Fun(Arg),
            From ! {self(), {ok, Result}},
            loop({Fun, Arg, {ok, Result}})
          catch
            _ : Ex -> 
              From ! {self(), {exception, Ex}},
              loop({Fun, Arg, {exception, Ex}})
          end
      end
  end.