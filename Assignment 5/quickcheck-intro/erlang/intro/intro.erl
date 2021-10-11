-module(intro).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during demonstration
-compile(nowarn_export_all).
-compile(export_all).

prop_plusmax() ->
  ?FORALL({K, X, Y}, {int(), int(), int()},
          eqc:equals(K + max(X, Y),
                     max(K + X, K + Y))).

prop_minusmax() ->
  ?FORALL({K, X, Y}, {int(), int(), int()},
          eqc:equals(K - max(X, Y),
                     max(K - X, K - Y))).

prop_reverse() ->
  ?FORALL(XS, list(int()),
         eqc:equals(lists:reverse(lists:reverse(XS)),
                    XS)).

prop_int_plus_associative() ->
  ?FORALL({X, Y, Z}, {int(), int(), int()},
          eqc:equals(X + (Y + Z),
                     (X + Y) + Z)).

prop_real_plus_associative() ->
  ?FORALL({X, Y, Z}, {real(), real(), real()},
          eqc:equals(X + (Y + Z),
                     (X + Y) + Z)).
