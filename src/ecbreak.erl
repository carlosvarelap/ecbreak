%%%-------------------------------------------------------------------
%%% File    : ecbreak.erl
%%% Author  : Carlos Varela <carlos.varela.paz@gmail.com>
%%% Description : 
%%%
%%% Created : 18 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(ecbreak).

-behaviour(gen_fsm).

%% API
-export([start_link/0, call/3, set_failure_threshold/1]).

%% gen_fsm callbacks
-export([init/1, closed/2, closed/3, handle_event/3,
	 open/2, open/3, reset/0,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {threshold = 10, 
		remainder_fails = 10
	       }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc calls Module:Function(Args) with the same semantic as erlang:apply/3
%% @spec call(Module::atom(), Function::atom(), Args::[term()]) -> term()
%% @throws open_circuit | term()
%% @end
%%--------------------------------------------------------------------
-spec call(Module::atom(), Function::atom(), Args::[term()]) -> term().
call(Module, Function, Args) ->
    case gen_fsm:sync_send_event(
	   ?SERVER, 
	   {call, Module, Function, Args}) of
	{throw, Exception} ->
	    throw(Exception);
	Result ->
	    Result
    end.
       
%%--------------------------------------------------------------------
%% @doc Sets the failure threshold under which the circuit will be opened
%% @spec set_failure_threshold(integer()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec set_failure_threshold(integer()) -> ok.
set_failure_threshold(Threshold) when is_integer(Threshold) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {set_failure_threshold, Threshold}).

%%--------------------------------------------------------------------
%% @doc Reset counter and puts in closed state
%% @spec reset() -> ok
%% @end
%%--------------------------------------------------------------------
-spec reset() -> ok.
reset()  ->
    gen_fsm:sync_send_all_state_event(?SERVER, reset).
    
%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%% @private
%%--------------------------------------------------------------------
init([]) ->
    {ok, closed, #state{}}.

%%--------------------------------------------------------------------
%% Function: 
%% closed(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%% @private
%%--------------------------------------------------------------------
closed(_Event, State) ->
    {next_state, closed, State}.

%% @private
open(_Event, State) ->
    {next_state, open, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%% @private
%%--------------------------------------------------------------------
closed({call, Module, Function, Args}, _From, State) ->
    {Reply, ReturnState} = 
	try
	    NewState = 
		case State#state.remainder_fails =:= State#state.threshold of
		    true ->
			State;
		    false ->
			State#state{remainder_fails=
				    State#state.remainder_fails+1}
		end,
	    {private_call(Module, Function, Args),	     
	     NewState}
	catch
	    Exception ->
		{{throw, Exception},
		 State#state{remainder_fails=
			     State#state.remainder_fails-1}}
	end,
    NextState = 
	case ReturnState#state.remainder_fails of
	    0 ->
		open;
	    _ ->
		closed
	end,
    {reply, Reply, NextState, ReturnState}.

%% @private
open({call, _Module, _Function, _Args}, _From, State) ->
    {reply, {throw, open_circuit}, open, State}.
    
%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%% @private
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%% @private
%%--------------------------------------------------------------------
handle_sync_event({set_failure_threshold, Threshold}, 
		  _From, StateName, State) ->
    Reply = ok,
    NewState = State#state{threshold=Threshold,
			   remainder_fails=
			   case (State#state.remainder_fails>Threshold) of 
			       true ->
				   Threshold;
			       false ->
				   State#state.remainder_fails
			   end},
    {reply, Reply, StateName, NewState};
handle_sync_event(reset, _From, _StateName, State) ->
    Reply = ok,
    NewState = State#state{remainder_fails=
			   State#state.threshold},
    {reply, Reply, closed, NewState};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%% @private
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%% @private
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc calls Module:Function(Args) with the same semantic as erlang:apply/3
%% @spec private_call(Module::atom(), Function::atom(), Args::[term()]) -> term()
%% @private
%% @end
-spec private_call(Module::atom(), Function::atom(), Args::[term()]) -> term().
private_call(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

