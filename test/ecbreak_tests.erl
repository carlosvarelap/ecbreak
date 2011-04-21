%%%-------------------------------------------------------------------
%%% @author Carlos Varela <carlos.varela.paz@gmail.com>
%%% @copyright (C) 2011, Carlos Varela
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(ecbreak_tests).

-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

launch_test() ->
    ok = application:start(ecbreak),
    ?assertNot(undefined == whereis(ecbreak_sup)).

create_test_module() ->
    file:write_file("m.erl",
		    "-module(m).\n"
		    "-export([ok/0, fail/0]).\n"
		    "ok() -> ok.\n"
		    "fail() -> throw(failure).\n"),
    compile:file("m.erl").

cleanup_test_module() ->
    file:delete("m.erl"),
    file:delete("m.beam").

calls_test_() ->
    {setup,
     fun() ->
	     application:start(ecbreak),
	     create_test_module()
     end,
     fun(_) ->
	     cleanup_test_module(),
	     application:stop(ecbreak)
     end,
     [
      ?_assertMatch(ok, ecbreak:call(m, ok, [])),
      {inorder,
       [?_assertMatch(ok, ecbreak:call(m, ok, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertMatch(ok, ecbreak:set_failure_threshold(5))]
      }
     ]
    }.

threshold_test_() ->
    {setup,
     fun() ->
	     application:start(ecbreak),
	     create_test_module(),
	     ecbreak:set_failure_threshold(5)
     end,
     fun(_) ->
	     application:stop(ecbreak),
	     cleanup_test_module()
     end,
     [
      {inorder,
       [?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(open_circuit, ecbreak:call(m, fail, []))]},
      {inorder,
       [?_assertMatch(ok, ecbreak:reset()),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertMatch(ok, ecbreak:call(m, ok, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(bad_call, ecbreak:call(kk, kk, [])),
	?_assertThrow(open_circuit, ecbreak:call(m, fail, []))]}
     ]
    }.

attempt_timeout_test_() ->
    {setup,
     fun() ->
	     application:start(ecbreak),
	     create_test_module(),
	     ecbreak:set_failure_threshold(5),
	     ecbreak:set_attempt_timeout(5000)
     end,
     fun(_) ->
	     application:stop(ecbreak),
	     cleanup_test_module()
     end,
     {inorder,
      [%% After the attempt timeout When the circuit is open, whe can make a
       %% call. If success we can do more calls,if not the circuit is opened
       %% again.
       ?_test(do_n_fails(6)),
       {timeout, 10, ?_test(timer:sleep(5000))},
       ?_assertMatch(ok, ecbreak:call(m, ok, [])),
       ?_assertMatch(ok, ecbreak:call(m, ok, [])),
       ?_test(do_n_fails(6)),
       {timeout, 10, ?_test(timer:sleep(5000))},
       ?_assertThrow(failure, ecbreak:call(m, fail, [])),
       ?_assertThrow(open_circuit, ecbreak:call(m, ok, []))
      ]}}.

attempt_timeout2_test_() ->
    {setup,
     fun() ->
	     application:start(ecbreak),
	     create_test_module(),
	     ecbreak:set_failure_threshold(5),
	     ecbreak:set_attempt_timeout(5000)
     end,
     fun(_) ->
	     application:stop(ecbreak),
	     cleanup_test_module()
     end,
     {inorder,
      [%% Making calls during half-open state does not affect the timeout
       ?_test(do_n_fails(6)),
       {timeout, 5, ?_test(do_call_for_n_seconds(4))},
       ?_test(timer:sleep(2000)),
       ?_assertMatch(ok, ecbreak:call(m, ok, []))
      ]}}.

-spec do_n_fails(integer()) -> ok.
do_n_fails(N) ->
    lists:foreach(
      fun(_) ->
	      try
		  ecbreak:call(m, fail, [])
	      catch _ -> ok
	      end
      end, lists:seq(1,N)).

-spec do_call_for_n_seconds(integer()) -> ok.
do_call_for_n_seconds(N) ->
    T=erlang:now(),
    do_call_for_n_microseconds(N*1000000, T, T).

-spec do_call_for_n_microseconds(integer(), integer(), integer()) -> ok.
do_call_for_n_microseconds(N, StartTime, CurrentTime) ->
    case timer:now_diff(CurrentTime,StartTime) < N of
	true ->
	    try
		ecbreak:call(m, ok, [])
	    catch _ -> ok
	    end,
	    do_call_for_n_microseconds(N, StartTime, erlang:now());
	false ->
	    ok
    end.

-endif.
