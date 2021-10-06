-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
       [ test_start_server(),
         test_start_server_one_shortcode(),
         test_start_server_not_unique_shortcode(),
         test_start_server_not_unique_binary(),
         test_start_server_small(),
         test_start_server_medium(),
        
         test_stop_server(),
         test_stop_server_multiple(),

         test_new_shortcode_unique(),
         test_new_shortcode_non_unique(),
         test_new_shortcode_already_alias(),

         test_alias(),
         test_alias_non_existing_shortcode(),
         test_alias_existing_alias(),
                  
         test_lookup_existing(),
         test_lookup_non_existing(),
         test_lookup_alias(),
         test_lookup_from_list_of_aliases(),
         test_lookup_alias_of_alias(),

         test_delete_shortcode(),
         test_delete_alias(),
         test_delete_non_existing(),

         test_analytics(),
         test_analytics_alias(),
         test_analytics_multiple(),
         test_analytics_non_unique_label(),
         test_analytics_non_existing_short(),

         test_get_analytics_empty(),
         test_get_analytics_init(),
         test_get_analytics_multiple_funs_init(),
         test_get_analytics_new_alias(),
         test_get_analytics_after_lookups(),
         test_get_analytics_multiple_funs_after_lookups(),
         test_get_analytics_after_lookups_alias(),
         test_get_analytics_non_existing_short(),

         test_remove_analytics_shortcode(),
         test_remove_analytics_alias(),
         test_remove_analytics_one_out_of_many(),
         test_remove_analytics_non_existing_label(),
         test_remove_analytics_non_existing_shortcode(),

        % load (efficiency) tests
         test_medium_new(),
         test_medium_lookup(),
         test_medium_delete(),
         test_medium_alias(),
         test_medium_get_analytics(),
         test_medium_remove_analytics(),

        % robustness tests
         test_analytics_broken_fun(),
         test_analytics_forever_fun()
       ]
      }
    ].

% analytics functions from example
hit(_, N) -> N+1.
accessed(SC, TS) ->
  Now = calendar:local_time(),
  [{SC,Now} | TS].
broken(_, _) -> throw("I don't like you").
forever(SC, State) -> State ++ forever(SC, State).


test_start_server() ->
  {"We can call start/1 and it does not crash",
    fun () ->
      ?assertMatch({ok, _}, emoji:start([]))
    end }.

test_start_server_one_shortcode() ->
  {"We can call start/1 with one shortcode and it does not crash",
    fun () ->
      ?assertMatch({ok, _}, emoji:start([{"smiley", <<240,159,152,131>>}]))
    end }.

test_start_server_not_unique_shortcode() ->
  {"We can call start/1 with two non-unique shortcodes and it produces an error",
    fun () ->
      ?assertMatch({error, _}, emoji:start([{"smiley", <<240,159,152,131>>}, 
                                          {"smiley", <<240,159,164,166>>}]))
    end }.

test_start_server_not_unique_binary() ->
  {"We can call start/1 with two non-unique binaries and it works",
    fun () ->
      ?assertMatch({ok, _}, emoji:start([{"smiley", <<240,159,152,131>>}, 
                                          {"facepalm", <<240,159,152,131>>}]))
    end }.

test_start_server_small() ->
  {"We can call start/1 with small list and it does not crash",
    fun () ->
      ?assertMatch({ok, _}, emoji:start(someemoji:small()))
    end }.

test_start_server_medium() ->
  {"We can call start/1 with medium list and it does not crash",
    fun () ->
      ?assertMatch({ok, _}, emoji:start(someemoji:medium()))
    end }.

test_stop_server() ->
  {"We stop a server, check it returns ok",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertEqual(ok, emoji:stop(S))
    end }.

test_stop_server_multiple() ->
  {"We start then stop multiple servers, check it returns ok",
    fun () ->
      {ok, S} = emoji:start([]),
      {ok, S1} = emoji:start(someemoji:small()),
      {ok, S2} = emoji:start(someemoji:medium()),
      ?assertEqual(ok, emoji:stop(S)),
      ?assertEqual(ok, emoji:stop(S1)),
      ?assertEqual(ok, emoji:stop(S2))
    end }.

test_new_shortcode_unique() ->
  {"Register new unique shortcode",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
    end }.

test_new_shortcode_non_unique() ->
  {"Register new non-unique shortcode, error",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ?assertMatch({error, _}, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
    end }.

test_new_shortcode_already_alias() ->
  {"Register new shortcode that is already an alias, error",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ?assertMatch({error, _}, emoji:new_shortcode(S, "happy",
                                            <<240,159,152,131>>))
    end }.

test_alias() ->
  {"Register alias, no error",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ?assertEqual(ok, emoji:alias(S, "smiley", "happy"))
    end }.

test_alias_non_existing_shortcode() ->
  {"Register alias for non-existing shortcode, error",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertMatch({error, _}, emoji:alias(S, "smiley", "happy"))
    end }.

test_alias_existing_alias() ->
  {"Register alias that already exists, error",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>},
                              {"facepalm", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ?assertMatch({error, _}, emoji:alias(S, "facepalm", "happy"))
    end }.

test_lookup_existing() ->
  {"Lookup an existing shortcode",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "smiley"))
    end }.

test_lookup_non_existing() ->
  {"Lookup a non-existing shortcode, error",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end }.

test_lookup_alias() ->
  {"Register alias and then look it up",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "happy"))
    end }.

test_lookup_from_list_of_aliases() ->
  {"Register alias for a shortcode that has multiple aliases and then look it up",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ok = emoji:alias(S, "smiley", "content"),
      ok = emoji:alias(S, "smiley", "glad"),
      ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "content"))
    end }.

test_lookup_alias_of_alias() ->
  {"Register alias that already exists, error",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ok = emoji:alias(S, "happy", "glad"),
      ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "glad"))
    end }.

test_delete_shortcode() ->
  {"Delete an existing shortcode and then lookup, no_emoji",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end }.

test_delete_alias() ->
  {"Delete an alias and then look it up, no_emoji",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      emoji:delete(S, "happy"),
      ?assertEqual(no_emoji, emoji:lookup(S, "happy"))
    end }.

test_delete_non_existing() ->
  {"Delete a non-existing shortcode/alias, no error, then lookup, no_emoji",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end }.

test_analytics() ->
  {"Create an analytics",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ?assertEqual(ok, emoji:analytics(S, "smiley", fun hit/2, "Counter", 0))
    end }.

test_analytics_alias() ->
  {"Create an analytics for an alias",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ?assertEqual(ok, emoji:analytics(S, "happy", fun hit/2, "Counter", 0))
    end }.

test_analytics_multiple() ->
  {"Create multiple analytics for a short",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ?assertEqual(ok, emoji:analytics(S, "happy", fun hit/2, "Counter", 0)),
      ?assertEqual(ok, emoji:analytics(S, "smiley", fun accessed/2, "Accessed", []))
    end }.

test_analytics_non_unique_label() ->
  {"Create analytics with a non-unique label, error",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ok = emoji:analytics(S, "happy", fun hit/2, "Counter", 0),
      ?assertMatch({error, _}, emoji:analytics(S, "smiley", fun accessed/2, "Counter", []))
    end }.

test_analytics_non_existing_short() ->
  {"Create analytics for a short that does not exist, error",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertMatch({error, _}, emoji:analytics(S, "smiley", fun accessed/2, "Accessed", []))
    end }.

test_get_analytics_empty() ->
  {"Get analyitics for shortcode with no analytics",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ?assertEqual({ok, []}, emoji:get_analytics(S, "smiley"))
    end }.

test_get_analytics_init() ->
  {"Get initial state for analyitics function",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      ?assertEqual({ok, [{"Counter", 0}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_get_analytics_multiple_funs_init() ->
  {"Get initial state for multiple analyitics function",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:analytics(S, "smiley", fun accessed/2, "Accessed", []),
      {ok, Analytics} = emoji:get_analytics(S, "smiley"),
      ?assertEqual(true, lists:member({"Counter", 0}, Analytics)),
      ?assertEqual(true, lists:member({"Accessed", []}, Analytics))
    end }.

test_get_analytics_new_alias() ->
  {"Create analytics, set alias, then get analytics of alias",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:alias(S, "smiley", "happy"),
      ?assertEqual({ok, [{"Counter", 0}]}, emoji:get_analytics(S, "happy"))
    end }.

test_get_analytics_after_lookups() ->
  {"Get analyitics after three lookups",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      {ok, _} = emoji:lookup(S, "smiley"),
      {ok, _} = emoji:lookup(S, "smiley"),
      {ok, _} = emoji:lookup(S, "smiley"),
      ?assertEqual({ok, [{"Counter", 3}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_get_analytics_multiple_funs_after_lookups() ->
  {"Get multiple analytics after three lookups",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:analytics(S, "smiley", fun accessed/2, "Accessed", []),
      {ok, _} = emoji:lookup(S, "smiley"),
      {ok, _} = emoji:lookup(S, "smiley"),
      {ok, _} = emoji:lookup(S, "smiley"),
      {ok, Analytics} = emoji:get_analytics(S, "smiley"),
      ?assertEqual(true, lists:member({"Counter", 3}, Analytics)),
      % have to match this way due to time constantly changing
      ?assertMatch(_, maps:get("Accessed", maps:from_list(Analytics)))
    end }.

test_get_analytics_after_lookups_alias() ->
  {"Create analytics, lookup alias, then get analytics of shortcode",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:alias(S, "smiley", "happy"),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      {ok, _} = emoji:lookup(S, "happy"),
      {ok, _} = emoji:lookup(S, "happy"),
      ?assertEqual({ok, [{"Counter", 2}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_get_analytics_non_existing_short() ->
  {"Get analytics for a non-existing short",
    fun () ->
      {ok, S} = emoji:start([]),
      ?assertMatch({error, _}, emoji:get_analytics(S, "smiley"))
    end }.

test_remove_analytics_shortcode() ->
  {"Remove analytics from a shortcode and then get, empty list",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      emoji:remove_analytics(S, "smiley", "Counter"),
      ?assertEqual({ok, []}, emoji:get_analytics(S, "smiley"))
    end }.

test_remove_analytics_alias() ->
  {"Add analytics for shortcode, add alias, remove analytics from alias and then get, empty list",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:alias(S, "smiley", "happy"),
      emoji:remove_analytics(S, "happy", "Counter"),
      ?assertEqual({ok, []}, emoji:get_analytics(S, "happy"))
    end }.

test_remove_analytics_one_out_of_many() ->
  {"Add two analytics for shortcode, remove one, and then get, shows state of other analytics",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun hit/2, "Counter", 0),
      ok = emoji:analytics(S, "smiley", fun accessed/2, "Accessed", []),
      emoji:remove_analytics(S, "smiley", "Accessed"),
      ?assertEqual({ok, [{"Counter", 0}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_remove_analytics_non_existing_label() ->
  {"Remove analytics from a non-existing label, no error, then get, empty list",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      emoji:remove_analytics(S, "smiley", "Counter"),
      ?assertEqual({ok, []}, emoji:get_analytics(S, "smiley"))
    end }.

test_remove_analytics_non_existing_shortcode() ->
  {"Remove analytics from a non-existing shortcode/alias, no error, then get, no_emoji",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:remove_analytics(S, "smiley", "Counter"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end }.

test_medium_new() ->
  {"Create new shortcode with larger emoji server",
    fun () ->
      {ok, S} = emoji:start(someemoji:medium()),
      ?assertEqual(ok, emoji:new_shortcode(S, "thisisanewemoji",
                                            <<240,159,152,131>>))
    end }.

test_medium_lookup() ->
  {"Lookup shortcode with larger emoji server",
    fun () ->
      {ok, S} = emoji:start(someemoji:medium()),
      ?assertMatch({ok, _}, emoji:lookup(S, "pensive face"))
    end }.

test_medium_delete() ->
  {"Delete shortcode with larger emoji server",
    fun () ->
      {ok, S} = emoji:start(someemoji:medium()),
      emoji:delete(S, "pensive face"),
      ?assertEqual(no_emoji, emoji:lookup(S, "pensive face"))
    end }.

test_medium_alias() ->
  {"Delete shortcode with larger emoji server",
    fun () ->
      {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:alias(S, "pensive face", "newalias"),
      ?assertMatch({ok, _}, emoji:lookup(S, "pensive face"))
    end }.

test_medium_get_analytics() ->
  {"Add analytics, lookup, then get analytics with larger emoji server",
    fun () ->
      {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:analytics(S, "pensive face", fun hit/2, "Counter", 0),
      {ok, _} = emoji:lookup(S, "pensive face"),
      ?assertEqual({ok, [{"Counter", 1}]}, emoji:get_analytics(S, "pensive face"))
    end }.

test_medium_remove_analytics() ->
  {"Add analytics then remove with larger emoji server",
    fun () ->
      {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:analytics(S, "pensive face", fun hit/2, "Counter", 0),
      {ok, _} = emoji:lookup(S, "pensive face"),
      emoji:remove_analytics(S, "pensive face", "Counter"),
      ?assertMatch({ok, []}, emoji:get_analytics(S, "pensive face"))
    end }.

test_analytics_broken_fun() ->
  {"Create analytics with a function that throws an error, then lookup",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun broken/2, "Broken", 0),
      ?assertMatch({error, _}, emoji:lookup(S, "smiley"))
    end }.

test_analytics_forever_fun() ->
  {"Create analytics with a function that loops forever, then lookup",
    fun () ->
      {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
      ok = emoji:analytics(S, "smiley", fun forever/2, "Forever", []),
      ?assertMatch({error, _}, emoji:lookup(S, "smiley"))
    end }.