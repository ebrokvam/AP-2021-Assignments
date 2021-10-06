-module(async).

-export([new/2, wait/1, poll/1]).

new(Fun, Arg) ->
  Aid = spawn(fun() -> loop(incomplete) end),
  Aid ! {start, {Fun, Arg}},
  Aid.

wait(Aid) ->
  Aid ! {self(), {wait, Aid}},
  receive
    {Aid, {ok, Result}} -> Result;
    {Aid, {exception, Ex}} -> throw(Ex)
  end.

poll(Aid) ->
  Aid ! {self(), poll},
  receive
    {Aid, Result} -> Result
  end.

loop(State) ->
  receive
    {start, {Fun, Arg}} ->
      spawn(fun() -> do_fun() end) ! {self(), Fun, Arg},
      loop(State);

    {fun_result, Result} ->
      loop(Result);

    {From, {wait, Aid}} ->
      case State of
        incomplete ->
          self() ! {From, {wait, Aid}};
        _ ->
          From ! {self(), State}
      end,
      loop(State);
      
    {From, poll} ->
      case State of
        incomplete ->
          From ! {self(), nothing};
        _ ->
          From ! {self(), State}
      end,
      loop(State)
end.

do_fun() ->
  receive
    {From, Fun, Arg} ->
      try 
        Result = Fun(Arg),
        From ! {fun_result, {ok, Result}}
      catch
        _ : Ex ->
          From ! {fun_result, {exception, Ex}}
      end
  end.