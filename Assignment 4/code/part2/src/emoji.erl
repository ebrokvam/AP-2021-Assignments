-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

start(Initial) ->
  case length(Initial) == sets:size(sets:from_list(proplists:get_keys(Initial))) of
    true -> {ok, spawn(fun() -> loop(maps:from_list(init_state(Initial))) end)};
    false -> {error, "shortcodes must be unique"}
  end.

new_shortcode(E, Short, Emo) -> request_reply(E, {new, Short, Emo}).

alias(E, Short1, Short2) -> request_reply(E, {alias, Short1, Short2}).

delete(E, Short) -> non_blocking(E, {delete, Short}).

lookup(E, Short) -> request_reply(E, {lookup, Short}).

analytics(E, Short, Fun, Label, Init) -> request_reply(E, {analytics, Short, Fun, Label, Init}).

get_analytics(E, Short) -> request_reply(E, {get_analytics, Short}).

remove_analytics(E, Short, Label) -> non_blocking(E, {remove_analytics, Short, Label}).

stop(E) -> request_reply(E, stop).

request_reply(Pid, Request) -> 
  Ref = make_ref(),
  Pid ! {self(), Ref, Request},
  receive
    {Ref, Response} -> Response
  end.

non_blocking(Pid, Msg) -> 
  Pid ! {non_blocking, Msg}.

% {Shortcode => {Emo, [Aliases], [Label => {Fun, State}]}}
loop(State) ->
  receive

    % Handle worker normal exits
    {'EXIT', _, normal} ->
      loop(State);

    {non_blocking, Request} -> 
      NewState = handle_nonblock(Request, State),
      loop(NewState);
    {From, Ref, stop} ->
      From ! {Ref, ok};
    {From, Ref, Request} -> 
      {NewState, Res} = handle_call(Request, State),
      From ! {Ref, Res}, 
      loop(NewState)
  end.

handle_call(Request, State) ->
  case Request of
    {new, Short, Emo} ->
      case get_emoji(Short, State) of
        {ok, _} -> 
          {State, {error, "shortcode already exists"}};
        _ ->
          NewState = maps:put(Short, {Emo, [], maps:new()}, State),
          {NewState, ok}
      end;

    {alias, Short1, Short2} ->
      case get_emoji(Short2, State) of
        {ok, _} ->
          {State, {error, "alias already exists"}};
        no_emoji ->
          case get_emoji(Short1, State) of
            {ok, {OrigShort, {Emo, Aliases, Analytics}}} ->
              NewState = maps:put(OrigShort, {Emo, Aliases ++ [Short2], Analytics}, State),
              {NewState, ok};
            no_emoji ->
              {State, {error, "shortcode does not exist"}}
          end
      end;

    {lookup, Short} ->
      case get_emoji(Short, State) of
        {ok, Emoji} ->
          handle_analytics(State, Emoji);
        no_emoji ->
          {State, no_emoji}
      end;

    {analytics, Short, Fun, Label, Init} ->
      case get_emoji(Short, State) of
        {ok, {OrigShort, {Emo, Aliases, Analytics}}} ->
          case maps:find(Label, Analytics) of
            {ok, _} ->
              {State, {error, "label already exists for shortcode"}};
            _ ->
              NewAnalytics = maps:put(Label, {Fun, Init}, Analytics),
              NewState = maps:put(OrigShort, {Emo, Aliases, NewAnalytics}, State),
              {NewState, ok}
          end;
        no_emoji ->
          {State, {error, "shortcode does not exist"}}
      end;

    {get_analytics, Short} ->
      case get_emoji(Short, State) of
        {ok, {_, {_, _, Analytics}}} ->
          ReturnList = read_analytics(maps:to_list(Analytics)),
          {State, {ok, ReturnList}};
        no_emoji ->
          {State, {error, "shortcode does not exist"}}
      end
  end.

handle_nonblock(Request, State) ->
  case Request of
    {delete, Short} ->
      case lookup_alias(Short, maps:to_list(State)) of
        {ok, {OrigShort, _}} ->
          maps:remove(OrigShort, State);
        _ ->
          maps:remove(Short, State)
      end;

    {remove_analytics, Short, Label} ->
      case get_emoji(Short, State) of
        {ok, {OrigShort, {Emo, Aliases, Analytics}}} ->
          NewAnalytics = maps:remove(Label, Analytics),
          maps:put(OrigShort, {Emo, Aliases, NewAnalytics}, State);
        no_emoji -> 
          State
      end
  end.

% Updates analytics as a worker process
handle_analytics(State, {Short, {Emo, Aliases, Analytics}}) ->
  Me = self(),
  process_flag(trap_exit, true),
  Worker = spawn_link(fun() ->
              NewAnalytics = maps:from_list(update_analytics(Short, maps:to_list(Analytics))),
              Me ! {Me, NewAnalytics}
            end),
  receive
    {Me, NewAnalytics} ->
      Updated = {Emo, Aliases, NewAnalytics},
      NewState = maps:put(Short, Updated, State),
      {NewState, {ok, Emo}};
    {'EXIT', Worker, Reason} ->
      {State, {error, Reason}}
  end.

% Converts initial list to initial state of server
init_state([]) -> [];
init_state([{Short, Emo} | Shortcodes]) ->
  [{Short, {Emo, [], maps:new()}}] ++ init_state(Shortcodes).

% Gets the emoji for a given short/alias
get_emoji(Short, State) ->
  case maps:find(Short, State) of
    {ok, {Emo, Aliases, Analytics}} ->
      {ok, {Short, {Emo, Aliases, Analytics}}};
    _ ->
      lookup_alias(Short, maps:to_list(State))
  end.

% Finds short in aliases
lookup_alias(_, []) -> no_emoji;
lookup_alias(Alias, [{Short, {Emo, Aliases, Analytics}} | Shortcodes]) ->
  case lists:member(Alias, Aliases) of
    true -> {ok, {Short, {Emo, Aliases, Analytics}}};
    false -> lookup_alias(Alias, Shortcodes)
  end.

% Converts analytics to the expected return list
read_analytics([]) -> [];
read_analytics([{Label, {_, State}} | Analytics]) ->
  [{Label, State}] ++ read_analytics(Analytics).

% Run all the analytics for a shortcode, called as worker process
update_analytics(_, []) -> [];
update_analytics(Short, [{Label, {Fun, State}} | Analytics]) ->
  NewState = Fun(Short, State),
  [{Label, {Fun, NewState}}] ++ update_analytics(Short, Analytics).