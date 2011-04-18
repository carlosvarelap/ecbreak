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
			    "-export([ok/0]).\n"
			     "ok() -> ok."),
	     compile:file("m.erl")	     
     end,
     fun(_) ->
	     application:stop(ecbreak),
	     file:delete("m.erl"),
	     file:delete("m.beam")
     end,
     fun(_) ->
	     ?_assertMatch(ok, ecbreak:call(m, ok, []))
     end
    }.


-endif.
