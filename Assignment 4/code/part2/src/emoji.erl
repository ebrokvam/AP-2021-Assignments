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

delete(E, Short) -> E ! {delete, Short}. % non-blocking

lookup(E, Short) -> request_reply(E, {lookup, Short}).

analytics(E, Short, Fun, Label, Init) -> request_reply(E, {analytics, Short, Fun, Label, Init}).

get_analytics(E, Short) -> request_reply(E, {get_analytics, Short}).

remove_analytics(E, Short, Label) -> E ! {remove_analytics, Short, Label}. % non-blocking

stop(E) -> request_reply(E, stop).

request_reply(Pid, Request) -> 
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

% {Shortcode => {Emo, [Aliases], [Label => {Fun, State}]}}
loop(State) ->
  receive

    {From, {new, Short, Emo}} ->
      case maps:find(Short, State) of
        {ok, _} -> 
          From ! {self(), {error, "shortcode already exists"}},
          loop(State);
        _ ->
          NewState = maps:put(Short, {Emo, [], []}, State),
          From ! {self(), ok},
          loop(NewState)
      end;

    {From, {alias, Short1, Short2}} ->

      case maps:find(Short2, State) of
        % Short2 already a shortcode
        {ok, _} -> 
          From ! {self(), {error, "alias already exists as a shortcode"}},
          loop(State);
        % Short2 not a shortcode
        _ ->
          case lookup_alias(Short2, maps:to_list(State)) of
            % Short2 already an alias
            {ok, _} ->
              From ! {self(), {error, "alias already exists for another shortcode"}},
              loop(State);
            % Short2 not an alias
            no_emoji ->
              case maps:find(Short1, State) of
                % Short1 a shortcode, create alias for shortcode
                {ok, {Emo, Aliases, Analytics}} ->
                  NewState = maps:put(Short1, {Emo, Aliases ++ [Short2], Analytics}, State),
                  From ! {self(), ok},
                  loop(NewState);
                % Short1 not a shortcode
                _ ->
                  case lookup_alias(Short1, maps:to_list(State)) of
                    % Short1 an alias, create alias for alias
                    {ok, {OrigShort, {Emo, Aliases, Analytics}}} ->
                      NewState = maps:put(OrigShort, {Emo, Aliases ++ [Short2], Analytics}, State),
                      From ! {self(), ok},
                      loop(NewState);
                    % Short1 does not exist
                    no_emoji ->
                      From ! {self(), {error, "shortcode does not exist"}},
                      loop(State)
                  end
              end
          end
      end;

    {delete, Short} ->
      case lookup_alias(Short, maps:to_list(State)) of
        {ok, {OrigShort, _}} ->
          NewState = maps:remove(OrigShort, State);
        _ ->
          NewState = maps:remove(Short, State)
      end,
      loop(NewState);

    {From, {lookup, Short}} ->
      case maps:find(Short, State) of
        {ok, {Emo, Aliases, Analytics}} -> 
          NewAnalytics = maps:from_list(update_analytics(Short, maps:to_list(Analytics))),
          NewState = maps:put(Short, {Emo, Aliases, NewAnalytics}, State),
          From ! {self(), {ok, Emo}},
          loop(NewState);
        _ ->
          case lookup_alias(Short, maps:to_list(State)) of
            {ok, {OrigShort, {Emo, Aliases, Analytics}}} ->
              NewAnalytics = maps:from_list(update_analytics(OrigShort, maps:to_list(Analytics))),
              NewState = maps:put(OrigShort, {Emo, Aliases, NewAnalytics}, State),
              From ! {self(), {ok, Emo}},
              loop(NewState);
            no_emoji ->
              From ! {self(), no_emoji},
              loop(State)
          end
      end;

    {From, {analytics, Short, Fun, Label, Init}} ->
       case maps:find(Short, State) of
        {ok, {Emo, Aliases, Analytics}} -> 
          case maps:find(Label, Analytics) of
            {ok, _} ->
              From ! {self(), {error, "label already exists for shortcode"}},
              loop(State);
            _ ->
              NewAnalytics = maps:put(Label, {Fun, Init}, Analytics),
              NewState = maps:put(Short, {Emo, Aliases, NewAnalytics}, State),
              From ! {self(), ok},
              loop(NewState)
          end;
        _ ->
          case lookup_alias(Short, maps:to_list(State)) of
            {ok, {OrigShort, {Emo, Aliases, Analytics}}} ->
              case maps:find(Label, Analytics) of
                {ok, _} ->
                  From ! {self(), {error, "label already exists for shortcode"}},
                  loop(State);
                _ ->
                  NewAnalytics = maps:put(Label, {Fun, Init}, Analytics),
                  NewState = maps:put(OrigShort, {Emo, Aliases, NewAnalytics}, State),
                  From ! {self(), ok},
                  loop(NewState)
              end;
            no_emoji ->
              From ! {self(), {error, "shortcode does not exist"}},
              loop(State)
          end
      end;

    {From, {get_analytics, Short}} ->
      case maps:find(Short, State) of
        {ok, {_, _, Analytics}} -> 
          From ! {self(), {ok, read_analytics(maps:to_list(Analytics))}},
          loop(State);
        _ ->
          case lookup_alias(Short, maps:to_list(State)) of
            {ok, {_, {_, _, Analytics}}} ->
              From ! {self(), {ok, read_analytics(maps:to_list(Analytics))}},
              loop(State);
            no_emoji ->
              From ! {self(), {error, "shortcode does not exist"}},
              loop(State)
          end
      end;

    {remove_analytics, Short, Label} ->
      case maps:find(Short, State) of
        {ok, {Emo, Aliases, Analytics}} ->
          NewAnalytics = maps:remove(Label, Analytics),
          NewState = maps:put(Short, {Emo, Aliases, NewAnalytics}, State),
          loop(NewState);
        _ ->
          case lookup_alias(Short, maps:to_list(State)) of
            {ok, {OrigShort, {Emo, Aliases, Analytics}}} ->
              NewAnalytics = maps:remove(Label, Analytics),
              NewState = maps:put(OrigShort, {Emo, Aliases, NewAnalytics}, State),
              loop(NewState);
            no_emoji -> 
              loop(State)
          end
      end;

    {From, stop} ->
      From ! {self(), ok} % or error reason??

  end.


init_state([]) -> [];
init_state([{Short, Emo} | Shortcodes]) ->
  [{Short, {Emo, [], maps:new()}}] ++ init_state(Shortcodes).

lookup_alias(_, []) -> no_emoji;
lookup_alias(Alias, [{Short, {Emo, Aliases, Analytics}} | Shortcodes]) ->
  case lists:member(Alias, Aliases) of
    true -> {ok, {Short, {Emo, Aliases, Analytics}}};
    false -> lookup_alias(Alias, Shortcodes)
  end.

read_analytics([]) -> [];
read_analytics([{Label, {_, State}} | Analytics]) ->
  [{Label, State}] ++ read_analytics(Analytics).

update_analytics(_, []) -> [];
update_analytics(Short, [{Label, {Fun, State}} | Analytics]) ->
  NewState = Fun(Short, State),
  [{Label, {Fun, NewState}}] ++ update_analytics(Short, Analytics).