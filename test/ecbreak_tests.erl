%%%-------------------------------------------------------------------
%%% File    : ecbreak_tests.erl
%%% Author  : Carlos Varela <carlos.varela.paz@gmail.com>
%%% Description : 
%%%
%%% Created : 18 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(ecbreak_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

launch_test() ->
    ok = application:start(ecbreak),
    ?assertNot(undefined == whereis(ecbreak_sup)).

calls_test_() ->
    {setup,
     fun() ->
	     application:start(ecbreak),
	     file:write_file("m.erl",
			    "-module(m).\n"
			    "-export([ok/0, fail/0]).\n"
			     "ok() -> ok.\n"
			     "fail() -> throw(failure).\n"),
	     compile:file("m.erl")	     
     end,
     fun(_) ->
	     application:stop(ecbreak),
	     file:delete("m.erl"),
	     file:delete("m.beam")
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
	     file:write_file("m.erl",
			    "-module(m).\n"
			    "-export([ok/0, fail/0]).\n"
			     "ok() -> ok.\n"
			     "fail() -> throw(failure).\n"),
	     compile:file("m.erl"),
	     ecbreak:set_failure_threshold(5)
     end,
     fun(_) ->
	     application:stop(ecbreak),
	     file:delete("m.erl"),
	     file:delete("m.beam")
     end,
     [
      {inorder,
       [?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(failure, ecbreak:call(m, fail, [])),
	?_assertThrow(open_circuit, ecbreak:call(m, fail, []))
       ]
      }
     ]
    }.

-endif.
