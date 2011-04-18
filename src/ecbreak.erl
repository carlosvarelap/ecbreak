%%%-------------------------------------------------------------------
%%% File    : ecbreak.erl
%%% Author  : Carlos Varela <carlos.varela.paz@gmail.com>
%%% Description : Main module
%%%
%%% Created : 18 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(ecbreak).

-export([call/3, set_failure_threshold/1]).

%% @doc calls Module:Function(Args) with the same semantic as erlang:apply/3
%% @spec call(Module::atom(), Function::atom(), Args::[term()]) -> term()
%% @end
-spec call(Module::atom(), Function::atom(), Args::[term()]) -> term().
call(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

%% @doc Sets the failure threshold under wich the circuit will be closed
%% @spec set_failure_threshold(integer()) -> ok
%% @end
-spec set_failure_threshold(integer()) -> ok.
set_failure_threshold(Threshold) when is_integer(Threshold) ->
    ok.
    
