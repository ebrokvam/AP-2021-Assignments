-module(async).

-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1, never_returns/1]).

% State functions
-export([incomplete/3, complete/3]).

% Callback functions
-export([init/1, callback_mode/0]).

-behaviour(gen_statem).

never_returns(A) -> never_returns(A).

new(Fun, Arg) ->
  {ok, Aid} = gen_statem:start(?MODULE, nothing, []),
  gen_statem:cast(Aid, {start, {Fun, Arg}}),
  Aid.

wait(Aid) ->
  case call_until_response(Aid) of
    {ok, Result} -> Result;
    {exception, Ex} -> throw(Ex)
  end.

poll(Aid) ->
  case gen_statem:call(Aid, poll) of
    Result -> Result
  end.

wait_catch(Aid) -> 
  case call_until_response(Aid) of
    Result -> Result
  end.

wait_any(Aids) ->
  Ref = make_ref(),
  lists:foreach(fun(Aid) -> spawn(fun() -> async_wait() end) ! {Ref, self(), Aid} end, Aids),
  receive
    {Ref, Aid, {ok, Result}} -> {Aid, Result};
    {Ref, _, {exception, Ex}} -> throw(Ex)
  end.


% Callback functions

callback_mode() ->
	state_functions.

init(Init) -> 
	{ok, incomplete, Init}.

% State functions

incomplete(EventType, EventContent, Data) ->
  case EventType of
    cast ->
      case EventContent of
        {start, {Fun, Arg}} ->
          Ref = make_ref(),
          spawn(fun() -> do_fun() end) ! {Ref, self(), Fun, Arg},
          {keep_state, Ref};
        {Ref, fun_result, Res} ->
          if Data == Ref ->
            {next_state, complete, Res}
          end
      end;
    {call, From} ->
      case EventContent of
        wait ->
          {keep_state_and_data, {reply, From, nothing}};
        poll ->
          {keep_state_and_data, {reply, From, nothing}}
      end
  end.

complete({call, From}, EventContent, Data) ->
  case EventContent of
    wait ->
      {keep_state_and_data, {reply, From, Data}};
    poll ->
      {keep_state_and_data, {reply, From, Data}}
  end.

% Asynchronous function
do_fun() ->
  receive
    {Ref, From, Fun, Arg} ->
      try 
        Result = Fun(Arg),
        gen_statem:cast(From, {Ref, fun_result, {ok, Result}})
      catch
        _ : Ex ->
          gen_statem:cast(From, {Ref, fun_result, {exception, Ex}})
      end
  end.

async_wait() ->
  receive
    {Ref, From, Aid} ->
      case call_until_response(Aid) of
        Result -> 
          From ! {Ref, Aid, Result}
      end
  end.


% Aux functions

call_until_response(Aid) ->
  case gen_statem:call(Aid, wait) of
    nothing ->
      call_until_response(Aid);
    Result -> Result
  end.

  % c(async), async:wait_any([async:new(fun (X) -> X end, 42)]).
  % c(async), async:wait_any([async:new(fun (X) -> X end, 42), async:new(fun (X) -> timer:sleep(3000) end, 42)]).
  % c(async), async:wait_any([async:new(fun (X) -> throw("no") end, 40)]).
  % c(async), async:wait_any([async:new(fun (X) -> throw("no") end, 40), async:new(fun (X) -> X end, 42)]).
  % c(async), async:wait_any([async:new(fun async:never_returns/1, 40), async:new(fun (X) -> X end, 42)]).
  % c(async), async:wait_any([async:new(fun (X) -> X end, 42), async:new(fun async:never_returns/1, 40)]).