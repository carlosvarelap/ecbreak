%%%-------------------------------------------------------------------
%%% File    : prop_ecbreak.erl
%%% Author  : Carlos Varela <carlos.varela.paz@gmail.com>
%%% Description :
%%%
%%% Created : 19 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(prop_ecbreak).

-include_lib("eqc/include/eqc.hrl").

%% properties
-export([prop_return_calls/0]).

%% generators
-export([call/0]).

%%-------------------------------------------------------------------
%% Generators
%%-------------------------------------------------------------------

bad_call() ->
    {{kk, kk, []}, {throw, bad_call}}.

fail_call() ->
    {{m, fail, []}, {throw, failure}}.

ok_call() ->
    {{m, ok, []}, ok}.

call() ->
    eqc_gen:oneof([bad_call(), fail_call(), ok_call()]).

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
                try
                    application:start(ecbreak),
                    generate_module(),
                    check_result(
                      ecbreak:call(M,F,A),
                      ExpectedReturn)
                catch
                    Exception ->
                        check_throw(
                          Exception,
                          ExpectedReturn)
                after
                    application:stop(ecbreak),
                    delete_module()
                end
            end).
