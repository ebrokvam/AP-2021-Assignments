-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A non-symbolic generator for bst, parameterised by key and value generators
bst(Key, Value) ->
  ?LET(KVS, eqc_gen:list({Key, Value}),
    lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
                empty(),
                KVS)).


bst_sym(Key, Value) ->
  ?LAZY(
    eqc_gen:oneof([{call, eqc_gen, list, [{Key, Value}]},
      ?LET(KVS, bst_sym(Key, Value),
        lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
        empty(),
        KVS))])).
        % {call, lists, foldl, 
        %   [fun({K,V}, T) -> insert(K, V, T) end, 
        %   empty(), 
        %   KVS]})])).


% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).
%atom_key() -> eqc_gen:int().

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
prop_arbitrary_valid() ->
  ?FORALL(T, bst(atom_key(), int_value()),
    valid(T)).

% prop_arbitrary_valid() ->
%   ?FORALL(T, bst_sym(atom_key(), int_value()),
%     begin
%       Tree = eval(T),
%       valid(T)
%     end).

% if we insert into a valid tree it stays valid
%insert (K, V, leaf) -> {branch, leaf, K, V, leaf};
prop_insert_valid() ->
  ?FORALL(
    {K, V, T}, 
    {atom_key(), int_value(), bst(atom_key(), int_value())},
    valid (insert(K, V, T))).

%empty() -> leaf.
prop_empty_valid() -> valid (empty()).

%delete (_K, leaf) -> leaf;
prop_delete_valid() -> 
  ?FORALL(
    {K, T}, 
    {atom_key(), bst(atom_key(), int_value())},
    valid (delete(K, T))).

% for union we only need to tjek with trees since a leaf
% is also a tree, a little tree, sort of tree
prop_union_valid() -> 
  ?FORALL(
    {T1, T2},
    {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
    valid (union(T1, T2))).


%%% -- postcondition properties
% tjeks whether the inserted key K1 already eksist as key K2 
% that is, did it insrt it the first time around?
prop_insert_post() ->
  ?FORALL(
    {K1, K2, V, T},
    {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
    eqc:equals(
      find(K2, insert(K1, V, T)),
      case K1 =:= K2 of
          true ->  {found, V};
          false -> find(K2, T)
      end)).

% devotes all test effort to the case of finding a newly inserted key
prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
  ?FORALL(
    {K, V, T}, 
    {atom_key(), int_value(), bst(atom_key(), int_value())},
    eqc:equals(
      find(K, insert(K, V, T)), 
      {found, V})).

% tries to find all keys and trees, that are give, and delete them 
prop_find_post_absent() -> 
   % ∀ k t. find k (delete k t) === nothing
  ?FORALL(
    {K, T}, 
    {atom_key(), bst(atom_key(), int_value())},
    eqc:equals(
      find(K, delete(K, T)), 
      nothing)).

prop_delete_post() -> 
  ?FORALL(
    {K1, K2, T},
    {atom_key(), atom_key(), bst(atom_key(), int_value())},
    eqc:equals(find(K2, delete(K1, T)),
      case K1 =:= K2 of 
        true -> nothing;
        false -> find(K2, T)
      end)).


% {{branch,leaf,a,1,{branch,leaf,d,-1,leaf}},
%  {branch,leaf,a,2,{branch,leaf,g,-3,leaf}},
%  g}
% {found,-3} /= nothing
% Shrinking .xx.xxxxxx.xxxxxxxxxxxx(3 times)
% {leaf,{branch,leaf,g,0,leaf},g}
% {found,0} /= nothing
% false
% eqc:quickcheck(test_bst:prop_union_post()).
prop_union_post() ->
  % ∀ t t' k. find k (union t t') === (find(k, t) orelse find(k, t'))
  ?FORALL(
    {T1, T2, K},
    {bst(atom_key(), int_value()), bst(atom_key(), int_value()), atom_key()},
    eqc:equals(
      find(K, union(T1, T2)), 
      (find(K, T1) orelse (find(K, T2) orelse nothing)) )).


% -spec union_find(Key1, [{Key1, Value1}], Key2, [{Key2, Value2}]) -> [{Key, Value}].
% union_find(Key1, KVS1, Key2, KVS2) -> 
%   find(K1, KVS1) orelse find(K2, KVS2).
  % case find(Key1, KVS1) of 
  %   {found, Value1} -> [{Key1, Value1}];
  %   nothing -> 
  %     case find(Key2, KVS2) of 
  %       {found, Value2} -> [{Key2, Value2}];
  %       nothing -> nothing
  %     end
  % end. 
%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
  % ∀ k v t. size (insert k v t) >= size t
  ?FORALL(
    {K, V, T}, 
    {atom_key(), int_value(), bst(atom_key(), int_value())},
    bst:size(insert(K, V, T)) >= bst:size(T)).


% tjek if two trees are equal using sorting
obs_equals(T1, T2) ->
  eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).

% checks whether an inserted key K1 instered in a another
% insterted three. it then tjeks if K1 == K2, then we know
% that insert(k1, v1, t) == insert(K1, V1, insert(K2, V2, T) 
% otherwise insert(K2, V2, insert(K1, V1, T))
prop_insert_insert() ->
  ?FORALL(
    {K1, K2, V1, V2, T},
    {atom_key(), atom_key(), int_value(), int_value(), bst(atom_key(), int_value())},
    obs_equals(insert(K1, V1, insert(K2, V2, T)),
      case K1 =:= K2 of
        true ->  insert(K1, V1, T);
        false -> insert(K2, V2, insert(K1, V1, T))
      end)).

% not sure of the following three
prop_insert() -> 
  ?FORALL(
  {K, V, T},
  {atom_key(), int_value(), bst(atom_key(), int_value())},
  obs_equals(insert(K, V, T), insert(K, V, T))).

prop_insert_delete() -> 
  ?FORALL(
  {K1, K2, V, T},
  {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
  obs_equals(insert(K1, V, delete(K2, T)),
    case K1 =:= K2 of
      true -> insert(K1, V, T);
      false -> delete(K2, insert(K1, V, T))
    end)).

prop_insert_union() -> 
  ?FORALL(
  {K, V, T1, T2},
  {atom_key(), int_value(), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
  obs_equals(insert(K, V, union(T1, T2)), union(insert(K, V, T1), T2))).


%%% -- Model based properties
model(T) -> to_sorted_list(T).


prop_insert_model() ->
  % ∀ k v t. model(insert(k v t)) === sorted_insert(K, V, delete_key(K, model(T)))
  ?FORALL(
    {K, V, T}, 
    {atom_key(), int_value(), bst(atom_key(), int_value())},
    eqc:equals(
      model(insert(K, V, T)),
      sorted_insert(K, V, delete_key(K, model(T))))).

prop_find_model() ->
  % ∀ k t. find k t === lookup_list(k (model(T)))
  ?FORALL(
    {K, T},
    {atom_key(), bst(atom_key(), int_value())},
    eqc:equals(
      find(K, T),
      lookup_list(K, model(T)))).


prop_empty_model() -> 
  % ∀ k t. model(empty()) === []
  eqc:equals(model(empty()), []).


prop_delete_model() -> 
  % ∀ k t. model(delete k t) === delete_key( k model(t))
  ?FORALL(
    {K, T},
    {atom_key(), bst(atom_key(), int_value())},
    eqc:equals(
      model(delete(K, T)),
      delete_key(K, model(T)))).

prop_union_model() -> 
  % ∀ t t'. model(union t t') === model(L.unionBy(model(t),model(t')))
  ?FORALL(
    {T1, T2},
    {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
    eqc:equals(
      model(union(T1, T2)),
      model(union_by(model(T1),model(T2))))).



-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].

-spec lookup_list(Key, [{Key, Value}]) -> [{Key, Value}].
lookup_list(Key, KVS) -> 
  case proplists:lookup_all(Key, KVS) of
    [] -> nothing;
    [{Key, Value}] -> {found, Value}
  end.

-spec union_by([{Key1, Value1}], [{Key2, Value2}]) -> [[{Key1, Value1}] | [{Key2, Value2}]].
union_by(KVS1, KVS2) -> [ [T1 | T2] || T1 <- KVS1, T2 <- KVS2, lists:nth(0, KVS1) =:= lists:nth(0, KVS2) ].
 % List<Entry<Integer, Integer>> bst2Model = bst2.toList();
 %    for (Entry<Integer, Integer> entry : bst1.toList()) {
 %        bst2Model = removeKey(bst2Model, entry.getKey());
 %    }
    
%% -- Test all properties in the module: eqc:module(test_bst)
%%  c('src/test_bst.erl'), c('src/bst.erl'), eqc:module(test_bst).
