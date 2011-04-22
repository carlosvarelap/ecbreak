%%% @author Carlos Varela <carlos.varela.paz@gmail.com>
%%% @copyright (C) 2011, Carlos Varela
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>

-module(ecbreak_fsm).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

-record(state,{threshold :: integer(), error_count :: integer()}).

wrap_ecbreak_call({{M, F, A}, _Result}) ->
    try
	ecbreak:call(M, F, A)
    catch
	Error -> {throw, Error}
    end.

%% Definition of the states. Each state is represented by a function,
%% listing the transitions from that state, together with generators
%% for the calls to make each transition.
closed(_S) ->
    [ %% {target_state,{call,?MODULE,target_function,[]}}
      {closed, {call, ecbreak, reset, []}},
      {closed, {call, ?MODULE, wrap_ecbreak_call, [prop_ecbreak:incorrect_call()]}},
      {open_state, {call, ?MODULE, wrap_ecbreak_call, [prop_ecbreak:incorrect_call()]}}
    ].

open_state(_S) ->
    [{open_state, {call, ?MODULE, wrap_ecbreak_call, [prop_ecbreak:call()]}},
     {closed, {call, ecbreak, reset, []}}
    ].

%% Identify the initial state
initial_state() ->
    closed.

%% Initialize the state data
initial_state_data() ->
    #state{threshold=2, error_count=0}.

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_From, closed,S,_V, {call, ecbreak, reset, []}) ->
    S#state{error_count=0};
next_state_data(open_state, open_state,S,_V,{call,_,_,_}) ->
    S;
next_state_data(_From,_To,S,_V,
		{call ,?MODULE, wrap_ecbreak_call, [{_MFA, {throw, _}}]}) ->
    S#state{error_count=S#state.error_count+1};
next_state_data(_From,_To,S,_V,
		{call ,?MODULE, wrap_ecbreak_call, [{_MFA, _Result}]}) ->
    case S#state.error_count of
	0 -> S;
	_ -> S#state{error_count=S#state.error_count-1}
    end.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(open_state, open_state, _S,
	     {call ,?MODULE, wrap_ecbreak_call, [{_MFA, _ExpectedResult}]}) ->
    true;
precondition(_From, open_state, S,
	     {call ,?MODULE, wrap_ecbreak_call, [{_MFA, {throw, _Result}}]}) ->
    case S#state.threshold - S#state.error_count of
	1 -> true;
	_ -> false
    end;
precondition(_From, closed, S,
	     {call ,?MODULE, wrap_ecbreak_call, [{_MFA, {throw, _Result}}]}) ->
    case S#state.threshold - S#state.error_count of
	1 -> false;
	_ -> true
    end;
precondition(_From, open_state, _S,
	     {call ,?MODULE, wrap_ecbreak_call, [{_MFA, _ExpectedResult}]}) ->
    false;
precondition(_From,closed,_S,
	     {call ,?MODULE, wrap_ecbreak_call, [{_MFA, _}]}) ->
    true;
precondition(_From,closed,_S,
	     {call ,ecbreak, reset, []}) ->
    true.


%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>)
postcondition(open_state, open_state,_S,{call,_,_,_},Res) ->
    Res =:= {throw, open_circuit};
postcondition(_From,_To,_S,{call,_,_,_},_Res) ->
    true.

%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(closed,closed,{call,ecbreak,reset,[]}) -> 8;
weight(closed,closed,{call,ecbreak_fsm,wrap_ecbreak_call,[_]}) -> 8;
weight(closed,open_state,{call,ecbreak_fsm,wrap_ecbreak_call,[_]}) -> 9;
weight(open_state,closed,{call,ecbreak,reset,[]}) -> 1;
weight(open_state,open_state,{call,ecbreak_fsm,wrap_ecbreak_call,[_]}) -> 1.
