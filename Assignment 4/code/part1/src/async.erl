-module(async).

-export([new/2, wait/1, poll/1]).

new(Fun, Arg) ->
  Aid = spawn(fun() -> async(incomplete) end),
  Aid ! {start, {Fun, Arg}},
  Aid.

wait(Aid) ->
  Aid ! {self(), {wait, Aid}},
  receive
    {Aid, {ok, Res}} -> Res;
    {Aid, {exception, Ex}} -> throw(Ex)
  end.

poll(Aid) ->
  Aid ! {self(), poll},
  receive
    {Aid, Res} -> Res
  end.

async(State) ->
  receive
    {start, {Fun, Arg}} ->
      spawn(fun() -> async_fun() end) ! {self(), Fun, Arg},
      async(State);

    {result, Res} ->
      async(Res);

    {From, {wait, Aid}} ->
      case State of
        incomplete ->
          self() ! {From, {wait, Aid}};
        _ ->
          From ! {self(), State}
      end,
      async(State);
      
    {From, poll} ->
      case State of
        incomplete ->
          From ! {self(), nothing};
        _ ->
          From ! {self(), State}
      end,
      async(State)
end.

async_fun() ->
  receive
    {From, Fun, Arg} ->
      try 
        Res = Fun(Arg),
        From ! {result, {ok, Res}}
      catch
        _ : Ex ->
          From ! {result, {exception, Ex}}
      end
  end.