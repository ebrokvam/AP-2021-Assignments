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
         test_start_server_wrong_input_shortcode(),
         test_start_server_wrong_input_binary(),
         test_start_server_small(),
         test_start_server_medium(),

         test_new_shortcode_unique(),
         test_new_shortcode_non_unique(),

         test_alias(),
         test_alias_non_existing_shortcode(),
         test_alias_existing_alias(),
                  
         test_lookup_existing(),
         test_lookup_non_existing(),
         test_lookup_alias(),
         test_lookup_from_list_of_alias(),
         test_lookup_alias_of_alias(),

         test_delete_existing(),
         test_delete_non_existing(),
         test_delete_alias(),

         test_stop_server()
       ]
      }
    ].

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

test_start_server_wrong_input_shortcode() ->
    {"We can call start/1 with an integer as the shortcode and it produces an error",
     fun () ->
       ?assertMatch({error, _}, emoji:start([{1, <<240,159,152,131>>}]))
     end }.

test_start_server_wrong_input_binary() ->
    {"We can call start/1 with an atom as the binary and it produces an error",
     fun () ->
       ?assertMatch({error, _}, emoji:start([{"facepalm", facepalm}]))
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
    {"Register alias and then look it up",
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

test_delete_existing() ->
    {"Delete an existing shortcode and then lookup, no_emoji",
     fun () ->
       {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
       emoji:delete(S, "smiley"),
       ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
     end }.

test_delete_non_existing() ->
    {"Delete a non-existing shortcode, no error, then lookup, no_emoji",
     fun () ->
       {ok, S} = emoji:start([]),
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

test_stop_server() ->
    {"We stop a server, check it returns ok",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:stop([S]))
     end }.

%% THERE IS A WAY TO TEST THIS, IS SWEAR
% test_stop_server_then_lookup() -> 
%     {"We stop a server, then try to lookup",
%      fun () ->
%        {ok, S} = emoji:start([{"smiley", <<240,159,152,131>>}]),
%        ok = emoji:stop(S),
%        ?assert(ok, emoji:lookup(S, "smiley"))
%      end }.

%% alias of alias ??
%% do some tests with multiple servers
%% test robustness somehow