%%%-------------------------------------------------------------------
%%% File    : ecbreak.erl
%%% Author  : Carlos Varela <carlos.varela.paz@gmail.com>
%%% Description : Main module
%%%
%%% Created : 18 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(ecbreak).

-export([call/3]).

%% @doc calls Module:Function(Args) with the same semantic as erlang:apply/3
%% @spec call(Module::atom(), Function::atom(), Args::[term()]) -> term()
%% @end
%% -spec call(Module::atom(), Function::atom(), Args::[term()]) -> term()
call(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).
