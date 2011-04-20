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
-spec start(atom(), [term()]) -> {ok, pid()} | ignore | {error, {already_started, pid()} | shutdown | term()}.
start(_StartType, _StartArgs) ->
    ecbreak_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
