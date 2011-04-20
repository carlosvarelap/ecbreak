%%%-------------------------------------------------------------------
%%% File    : ecbreak_sup.erl
%%% Author  : Carlos Varela <carlos.varela.paz@gmail.com>
%%% Description :
%%%
%%% Created : 18 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(ecbreak_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | shutdown | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init([]) -> {ok, { {one_for_one, 5, 10}, [?CHILD(ecbreak, worker)]} }.
init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(ecbreak, worker)]} }.


