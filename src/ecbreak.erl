%%%-------------------------------------------------------------------
%%% @author Carlos Varela <carlos.varela.paz@gmail.com>
%%% @copyright (C) 2011, Carlos Varela
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2011 by Carlos Varela <carlos.varela.paz@gmail.com>
%%%-------------------------------------------------------------------
-module(ecbreak).

-behaviour(gen_fsm).

%% API
-export([start_link/0, call/3, set_failure_threshold/1, set_attempt_timeout/1]).

%% gen_fsm callbacks
-export([init/1, closed/2, closed/3, handle_event/3,
	 open/2, open/3, half_open/2, half_open/3, reset/0,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {threshold = 10 :: integer(),
		remainder_fails = 10 :: integer(),
		attempt_timeout = 10000 :: integer(),
		timeout_timer = undefined :: reference() | undefined
	       }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | shutdown | term()}.
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc calls Module:Function(Args) with the same semantic as erlang:apply/3
%% @spec call(Module::atom(), Function::atom(), Args::[term()]) -> term()
%% @throws open_circuit | bad_call | term()
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
%% @doc Sets the failure threshold under which the circuit will be
%% opened. Counter is not reset. If current counter is greater than
%% threshold, it is set to Threshold value.
%% @spec set_failure_threshold(integer()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec set_failure_threshold(integer()) -> ok.
set_failure_threshold(Threshold) when is_integer(Threshold) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {set_failure_threshold, Threshold}).

%%--------------------------------------------------------------------
%% @doc Sets the attempt timeout under which the circuit will pass
%% from open to half-open. If the current state is open, the new value
%% will be taken into account in the next transition
%% @spec set_attempt_timeout(integer()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec set_attempt_timeout(integer()) -> ok.
set_attempt_timeout(Milliseconds) ->
    gen_fsm:sync_send_all_state_event(?SERVER, {set_attempt_timeout, Milliseconds}).

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
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, closed, #state{}}.
init([]) ->
    {ok, closed, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec closed(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
-spec closed(term(), #state{}) -> term().
closed(_Event, State) ->
    {next_state, closed, State}.

%% @private
-spec open(term(), #state{}) -> term().
open({timeout, _Ref, try_half_open}, State) ->
    {next_state, half_open, cancel_timer(State)}.

%% @private
-spec half_open(term(), #state{}) -> term().
half_open(_Event, State) ->
    {next_state, half_open, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec closed(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
-spec closed({call, atom(), atom(), [term()]}, pid(), #state{}) -> term().
closed({call, Module, Function, Args}, _From, State) ->
    {Reply, NewState, _Fail} = do_call(Module, Function, Args, State),
    {NextState, ReturnState} =
	case NewState#state.remainder_fails of
	    0 ->
		{open, start_timer(NewState)};
	    _ ->
		{closed, NewState}
	end,
    {reply, Reply, NextState, ReturnState}.

%% @private
-spec open({call, atom(), atom(), [term()]}, pid(), #state{}) -> term().
open({call, _Module, _Function, _Args}, _From, State) ->
    {reply, {throw, open_circuit}, open, State}.

%% @private
-spec half_open({call, atom(), atom(), [term()]}, pid(), #state{}) -> term().
half_open({call, Module, Function, Args}, _From, State) ->
    {Reply, ReturnState, Fail} = do_call(Module, Function, Args, State),
    case Fail of
	false ->
	    {reply, Reply, closed, start_timer(reset_state(ReturnState))};
	true ->
	    {reply, Reply, open, start_timer(ReturnState)}
    end.

-spec do_call(atom(), atom(), [term()], #state{}) -> {term(), #state{}, boolean()}.
do_call(Module, Function, Args, State) ->
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
	 NewState, false}
    catch
	Exception ->
	    {{throw, Exception},
	     case State#state.remainder_fails of
		 0 -> State;
		 _ ->
		     State#state{remainder_fails=
				     State#state.remainder_fails-1}
	     end, true}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(term(), atom(), #state{}) -> term().
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_sync_event(
        {set_failure_threshold, integer()} | reset,
        pid(), atom(), #state{}) -> term().
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
    {reply, ok, closed, cancel_timer(reset_state(State))};
handle_sync_event({set_attempt_timeout, Milliseconds}, _From,
		  StateName, State) ->
    Reply = ok,
    NewState = State#state{attempt_timeout=Milliseconds},
    {reply, Reply, StateName, NewState};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), atom(), #state{}) -> term().
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), atom(), #state{}) -> term().
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(integer, atom(), #state{}, term()) -> term().
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
    try
        erlang:apply(Module, Function, Args)
    catch
        error:undef ->
            throw(bad_call)
    end.

-spec reset_state(#state{}) -> #state{}.
reset_state(State) ->
    State#state{remainder_fails=
		    State#state.threshold}.

-spec cancel_timer(#state{}) -> #state{}.
cancel_timer(State) when State#state.timeout_timer =:= undefined ->
    State;
cancel_timer(State) ->
    _ = gen_fsm:cancel_timer(State#state.timeout_timer),
    State#state{timeout_timer=undefined}.

-spec start_timer(#state{}) -> #state{}.
start_timer(State) ->
    State#state{
      timeout_timer=
	  gen_fsm:start_timer(State#state.attempt_timeout, try_half_open)}.
