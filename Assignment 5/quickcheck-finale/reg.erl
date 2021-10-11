%%%
%%% QuckCheck example, testing the process registry
%%% Based on code from the paper "QuickCheck Testing for Fun and Profit"
%%%
%%% This version by Ken Friis Larsen <kflarsen@diku.dk>

-module(reg).

-include_lib("eqc/include/eqc.hrl").
%-include_lib("eqc/include/eqc_statem.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

-export([prop_registration/0]).
-export([spawn/0, register/2, unregister/1]).


%%% Correctness property

prop_registration() ->
  ?FORALL(Cmds, commands(?MODULE),
     begin
         {_,S,_} = Result = run_commands(?MODULE, Cmds),
         cleanup(S),
         check_commands(Cmds, Result)
     end).

check_commands(Cmds, {_,_,Res} = HSRes) ->
    pretty_commands(?MODULE, Cmds, HSRes,
                    aggregate(command_names(Cmds),
                              equals(Res, ok))).


% Cleanup, unregister all names (potentially) registered during test,
% and kill all processes stated.
cleanup(S)->
    [catch erlang:unregister(N) || N<-names()],
    [exit(P,kill) || P <- maps:get(pids, S)].



%%% Model of the register internal state

-type proplist() :: [{atom(), term()}].

-type model_state() :: #{ pids := [pid()]     % list of spawned pids
                        , regs := proplist()  % list of registered names and pids
                        }.

-spec initial_state() -> model_state().
initial_state() ->
    #{ pids => []
     , regs => []}.


%%% Command generator

%% Better implementation of command
command(#{pids := Pids} = S) ->
    oneof(
      [{call,erlang,register, [name(),pid(S)]}
       || Pids /= []] % small trick to make sure that pid(S) is only called when there are pids to choose
      ++
      [{call,erlang,unregister,[name()]}, % When neg testing instead use {call,?MODULE,unregister,[name()]},
       {call,?MODULE,spawn,[]},
       {call,erlang,whereis,[name()]}]).


pid(#{pids := Pids}) ->
    elements(Pids).

names() ->
    [a,b,c,d].

name() ->
    elements(names()).

spawn() ->
    spawn(fun() -> receive after 30000 -> ok end end).



%%% State transitions

next_state(#{pids := Pids} = S,V,{call, ?MODULE, spawn,[]}) ->
    S#{pids := Pids ++ [V]};

next_state(#{regs := Regs} = S,_V,{call, _, register, [Name,Pid]}) ->
    S#{regs := [{Name,Pid} | Regs]};

next_state(#{regs := Regs} = S,_V,{call, _, unregister, [Name]}) ->
    S#{regs := proplists:delete(Name, Regs)};

next_state(S,_V,_) ->
    S.


unregister_ok(#{regs := Regs}, Name) ->
    proplists:is_defined(Name, Regs).

register_ok(#{regs := Regs}, Name,P) ->
    not (proplists:is_defined(Name, Regs))
    and not (lists:member(P, [ Val || {_K, Val } <- Regs ])).

precondition(S,{call,_,unregister,[Name]}) ->
   unregister_ok(S,Name);
precondition(S,{call,_,register,[Name, P]}) ->
   register_ok(S,Name,P);
precondition(_S,{call,_,_,_}) ->
    true.


%% For negative testing, uncomment the following clauses.
%% REMEMBER to use the register/unregister functions from this module
%% in the command generator

%% postcondition(S,{call,_,register,[Name, P]},Res) ->
%%      case Res of
%%          {'EXIT',_} -> not register_ok(S,Name, P);
%%          true       ->     register_ok(S,Name, P)
%%      end;
%% postcondition(S,{call,_,unregister,[Name]},Res) ->
%%      case Res of
%%          {'EXIT',_} -> not unregister_ok(S,Name);
%%          true       ->     unregister_ok(S,Name)
%%      end;
postcondition(_S,{call,_,_,_},_R) ->
    true.




% Functions used for negative testing
register(Name,Pid) ->
    catch erlang:register(Name,Pid).

unregister(Name) ->
    catch erlang:unregister(Name).
