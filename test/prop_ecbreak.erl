%%%-------------------------------------------------------------------
%%% File    : prop_ecbreak.erl
%%% Author  : Carlos Varela <carlos.varela.paz@gmail.com>
%%% Description :
%%%
%%% Created : 19 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(prop_ecbreak).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all]).

%%-------------------------------------------------------------------
%% Generators
%%-------------------------------------------------------------------

bad_call() ->
    {{kk, kk, []}, {throw, bad_call}}.

fail_call() ->
    {{m, fail, []}, {throw, failure}}.

incorrect_call() ->
    eqc_gen:oneof([bad_call(), fail_call()]).

ok_call() ->
    {{m, ok, []}, ok}.

call() ->
    eqc_gen:oneof([bad_call(), fail_call(), ok_call()]).

list_call() ->
    eqc_gen:list([bad_call(), fail_call(), ok_call()]).


%% generates and dep list of calls that finished qith the circuit
%% opened. The trick is to generate incorrect calls or one good call
%% followed by two bad calls
list_call_to_open_circuit(Threshold) ->
    lists:map(
      fun(_) ->
              eqc_gen:oneof(
                [incorrect_call(),
                 [ok_call(), incorrect_call(), incorrect_call()]])
      end, lists:seq(0,Threshold)).

threshold() ->
    ?SUCHTHAT(X, eqc_gen:nat() , X>0).

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------
generate_module() ->
    file:write_file("m.erl",
                    "-module(m).\n"
                    "-export([ok/0, fail/0]).\n"
                    "ok() -> ok.\n"
                    "fail() -> throw(failure).\n"),
    compile:file("m.erl").

delete_module() ->
    file:delete("m.erl"),
    file:delete("m.beam").

check_result(Result, Expected)
  when Result =:= Expected ->
    true;
check_result(_,_) ->
    false.

check_throw(Result, {throw, Expected})
  when Result =:= Expected ->
    true;
check_throw(_,_) ->
    false.

%%--------------------------------------------------------------------
%% Properties
%%--------------------------------------------------------------------

%% Check that calls returns what it is expected
prop_return_calls() ->
    ?FORALL({{M,F,A}, ExpectedReturn}, call(),
            begin
                application:start(ecbreak),
                generate_module(),
                Res =
                    try
                        check_result(
                          ecbreak:call(M,F,A),
                          ExpectedReturn)
                    catch
                        Exception ->
                            check_throw(
                              Exception,
                              ExpectedReturn)
                    end,
                application:stop(ecbreak),
                delete_module(),
                Res
            end).

%% Check that a circuit is opened after threshold is reached
prop_open_circuit_calls() ->
    ?FORALL(
       Threshold, threshold(),
       ?FORALL(
          List, list_call_to_open_circuit(Threshold),
          begin
              application:start(ecbreak),
              ecbreak:set_failure_threshold(Threshold),
              ecbreak:reset(),
              generate_module(),
              {Opened, ProcesedElements} =
                  lists:foldl(
                    fun(_, {true, Acc}) ->
                         {true, Acc};
                       ({{M,F,A}, _ExpectedReturn},
                        {false, Acc}) ->
                            try
                                ecbreak:call(M,F,A),
                                case Acc of
                                    0 ->
                                        {false, Acc};
                                    _ ->
                                        {false, Acc-1}
                                end
                            catch
                                open_circuit ->
                                    {true, Acc+1};
                                _Else ->
                                    {false, Acc+1}
                            end
                    end, {false, 0}, lists:flatten(List)),
              application:stop(ecbreak),
              delete_module(),
              ?WHENFAIL(
                 io:format("Expected ~p, got ~p~n",
                           [{true, Threshold+1},
                            {Opened, ProcesedElements}]),
                 {Opened, ProcesedElements} =:= {true, Threshold+1}
                )
          end)).
