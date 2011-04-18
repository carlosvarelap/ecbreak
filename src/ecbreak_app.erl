%%%-------------------------------------------------------------------
%%% File    : ecbreak_app.erl
%%% Author  : Carlos Varela <carlos.varela.paz@gmail.com>
%%% Description : 
%%%
%%% Created : 18 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(ecbreak_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ecbreak_sup:start_link().

stop(_State) ->
    ok.
