%%%
%%% QuickCheck example, checking properties of the dict module
%%%
%%% Created by Ken Friis Larsen <kflarsen@diku.dk>

-module(dict_eqc).
-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during demonstration
-compile(nowarn_export_all).
-compile(export_all).

% not a good function for checking dict:fetch_keys, because list:usort
% consider the values 0 and 0.0 to be equal, make your own.
no_duplicates(Lst) ->
    length(Lst) =:= length(lists:usort(Lst)).

prop_unique_keys() ->
    ?FORALL(D,dict(),
	    no_duplicates(dict:fetch_keys(eval(D)))).

dict() ->
    dict_6().

dict_0() ->
    ?LAZY(
       oneof([dict:new(),
	      ?LET({K,V,D},{key(), value(), dict_0()},
               dict:store(K,V,D))])
      ).

dict_1() ->
    ?LAZY(
       oneof([{call,dict,new,[]},
	      ?LET(D,dict_1(),
                     {call,dict,store,[key(),value(),D]})])
      ).

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

dict_4() ->
    ?LAZY(
       frequency([{1,{call,dict,new,[]}},
                  {4,?LETSHRINK([D],[dict_4()],
                                {call,dict,store,[key(),value(),D]})},
                  {4, ?LETSHRINK([D],[dict_4()],
                                {call,dict,erase,[key(),D]})}])
      ).


dict_5() ->
    ?LAZY(
       frequency([{1,{call,dict,new,[]}},
                  {4,?LETSHRINK([D],[dict_5()],
                                {call,dict,store,[key(),value(),D]})},
                  {4,?LETSHRINK([D],[dict_5()],
                                ?LET(K, key_of(D),
                                     {call,dict,erase,[K,D]}))}])
      ).

key_of(D) ->
    elements(dict:fetch_keys(eval(D)) ++ [snowflake]).

dict_6() ->
    ?SIZED(Size, dict_sized(Size)).

% Sized generator, that also demonstrate how to use ?SUCHTHAT
dict_sized(0) ->
    oneof([{call, dict, new, []},
           {call, dict, from_list, [[{key(), value()}]]}]);
dict_sized(N) ->
    ?LAZY(
       frequency([{1,{call,dict,new,[]}},
                  {N,?LETSHRINK([D],[dict_sized(N-1)],
                                {call,dict,store,[key(),value(),D]})},
                  {N, ?LETSHRINK([D],
                                 [?SUCHTHAT(D1, dict_sized(N-1), dict:size(eval(D1)) > 0)],
                                 {call,dict,erase,[key_from(D),D]})}]
                )
      ).

key_from(D) ->
    elements(dict:fetch_keys(eval(D))).


key() ->
    oneof([atom(), int()]).

value() ->
    oneof([atom(), int(), real()]).

atom() ->
    elements([a,b,c,d]).


prop_measure(DGen) ->
    ?FORALL(D, DGen(),
	    collect(length(dict:fetch_keys(eval(D))),true)).


prop_aggregate(DGen) ->
    ?FORALL(D, DGen(),
            aggregate(call_names(D), true)).


wrong_model(Dict) ->
    dict:to_list(Dict).

wrong_model_store(K, V, M) ->
    [ {K,V} | M ].



model(Dict) ->
    lists:sort(dict:to_list(Dict)).

model_store(K, V, M) ->
    M1 = proplists:delete(K, M),
    lists:sort([ {K,V} | M1 ]).

model_erase(K, M) ->
    proplists:delete(K, M).


prop_store() ->
    ?FORALL({K,V,D}, {key(),value(),dict()},
            begin
                Dict = eval(D),
                equals(model(dict:store(K,V,Dict)),
                       model_store(K,V,model(Dict)))
            end).

prop_erase() ->
    ?FORALL(D, dict(),
    ?FORALL(K, key_of(D),
     begin
       Dict = eval(D),
       equals(model(dict:erase(K, Dict)),
              model_erase(K,model(Dict)))
     end)).


% Showing how to use ?WHENFAIL and eqc:format to make nice
% explanations when properties fail.
% Try to use wrong_model instead of model to see an example.
prop_pretty_erase() ->
    ?FORALL(D, dict(),
    ?FORALL(K, key_from(D),
     begin
       Dict = eval(D),
       ?WHENFAIL(eqc:format("Trying to erase the key '~p' from '~s'~n",
                            [K, eqc_symbolic:pretty_print(D)]),
                 equals(model(dict:erase(K,Dict)),
                        model_erase(K,model(Dict))))
     end)).
