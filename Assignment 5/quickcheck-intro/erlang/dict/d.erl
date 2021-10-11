%%%
%%% QuickCheck example, checking properties of the dict module
%%%
%%% Created by Ken Friis Larsen <kflarsen@diku.dk>

-module(d).
-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during demonstration
-compile(nowarn_export_all).
-compile(export_all).

no_duplicates(Lst) ->
    length(Lst) =:= length(lists:usort(Lst)).

prop_unique_keys() ->
    ?FORALL(D,dict,
	    no_duplicates(dict:fetch_keys(eval(D)))).

dict() ->
    dict_3().


dict_fl() ->
  ?LET(X, list({key(), value()}),
       dict:from_list(X)).

dict_0() ->
    ?LAZY(
       oneof([dict:new(),
	      ?LET({K,V,D},{key(), value(), dict_0()},
               dict:store(K,V,D))])
      ).

key() ->
    oneof([atom(), int(), real()]).

value() ->
    oneof([int(), atom()]).

atom() ->
    elements([a,b,c,d]).

dict_1() ->
    ?LAZY(
       oneof([{call,dict,new,[]},
	      ?LET(D, dict_1(),
               {call,dict,store,[key(),value(),D]})])
      ).

% Remember to evaluate your dict
% and measure it
prop_measure() ->
    ?FORALL(D,dict(),
	    collect(length(dict:fetch_keys(eval(D))),true)).


prop_aggregate() ->
    ?FORALL(D,dict(),
            aggregate(call_names(D), true)).

dict_2() ->
    ?LAZY(
       frequency([{1,{call,dict,new,[]}},
                  {4,?LET(D, dict_2(),
                          {call,dict,store,[key(),value(),D]})}])
      ).

dict_3() ->
    ?LAZY(
       frequency([{1,{call,dict,new,[]}},
                  {4,?LETSHRINK([D],[dict_3()],
                                {call,dict,store,[key(),value(),D]})}])
      ).
