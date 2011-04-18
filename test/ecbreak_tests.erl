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
-endif.
