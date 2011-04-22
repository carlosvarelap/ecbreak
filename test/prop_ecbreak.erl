%%%-------------------------------------------------------------------
%%% @author Carlos Varela <carlos.varela.paz@gmail.com>
%%% @copyright (C) 2011, Carlos Varela
%%% @doc
%%%
%%% @end
%%% Created : 19 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(prop_ecbreak).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

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

call_list() ->
    eqc_gen:list(eqc_gen:oneof([bad_call(), fail_call(), ok_call()])).


%% generates and dep list of calls that finished qith the circuit
%% opened. The trick is to generate incorrect calls or one good call
%% followed by two bad calls
call_list_to_open_circuit(Threshold) ->
    ql_gen:list_of_size(
      Threshold*2,
      call()).

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

prop_fsm() ->
    ?FORALL(Cmds,commands(ecbreak_fsm),
	    begin
		application:start(ecbreak),
		ecbreak:set_failure_threshold(2),
		{H,S,Res} = run_commands(ecbreak_fsm,Cmds),
		application:stop(ecbreak),
		?WHENFAIL(
		   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
		   aggregate(zip(state_names(H),
				 command_names(Cmds)),
			     Res == ok))
	    end).

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
          List, call_list_to_open_circuit(Threshold),
          begin
              application:start(ecbreak),
              ecbreak:set_failure_threshold(Threshold),
              ecbreak:reset(),
              generate_module(),
              {Fail, ProcesedElements} =
                  lists:foldl(
                    fun(_, {true, Acc}) ->
                         {true, Acc};
                       ({{M,F,A}, _ExpectedReturn},
                        {false, Acc}) ->
                            try
                                ecbreak:call(M,F,A),
                                {false, Acc+1}
                            catch
                                open_circuit ->
                                    {true, Acc+1};
                                _Else ->
                                    {false, Acc+1}
                            end
                    end, {false, 0}, List),
              application:stop(ecbreak),
              delete_module(),
              ?WHENFAIL(
                 io:format("Expected ~p, got ~p~n",
                           [should_fail(List, Threshold),
                            {Fail, ProcesedElements}]),
                 collect(with_title("Circuit opened"),
                         element(1,should_fail(List, Threshold)),
                   {Fail, ProcesedElements} =:= should_fail(List, Threshold)
                  )
                )
          end)).

%% @returns false | {true, NumberOfPProcesedElements}
should_fail(List, Threshold) ->
    should_fail(List,  0, Threshold, Threshold).

should_fail(
  [{_,_} | _L], ProcesedElements,
  _Threshold, Remainder) when Remainder =:= 0->
    {true, ProcesedElements+1};
should_fail(
  [{_,{throw, _}} | L], ProcesedElements,
  Threshold, Remainder) ->
    should_fail(L, ProcesedElements+1, Threshold, Remainder-1);
should_fail(
  [{_,_}|L], ProcesedElements, Threshold, Remainder)
  when Threshold =:= Remainder ->
    should_fail(L, ProcesedElements+1, Threshold, Remainder);
should_fail(
  [{_,_}|L], ProcesedElements, Threshold, Remainder) ->
    should_fail(L, ProcesedElements+1, Threshold, Remainder+1);
should_fail(
  [], ProcesedElements, _Threshold, _Remainder) ->
    {false, ProcesedElements}.

prop_after_reset_ok_call_always_works() ->
    ?FORALL(
       Calls, call_list(),
       ?FORALL(
	  Threshold, threshold(),
	  ?LET({{OKM,OKF,OKA}, _}, ok_call(),
	       begin
		   application:start(ecbreak),
		   ecbreak:set_failure_threshold(Threshold),
		   ecbreak:reset(),

		   lists:foreach(
		     fun({{M,F,A}, _ExpectedReturn}) ->
			     try
				 ecbreak:call(M,F,A)
			     catch _ -> ok
			     end
		     end, Calls),
		   ecbreak:reset(),
		   Res =
		       try
			   ecbreak:call(OKM,OKF,OKA),
			   true
		       catch _ -> false
		       end,
		   application:stop(ecbreak),
		   Res
	       end))).

