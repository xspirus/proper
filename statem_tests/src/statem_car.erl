-module(statem_car).

-behaviour(gen_server).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------

%% API
-export([start_link/0, stop/0]).
-export([start_car/0, stop_car/0, accelerate/1, brake/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

%% state
-record(state, {speed :: non_neg_integer(),
                onoff :: 'on' | 'off' }).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

-spec start_car() -> 'on'.
start_car() ->
    gen_server:call(?MODULE, {start_car}).

-spec stop_car() -> 'off'.
stop_car() ->
    gen_server:call(?MODULE, {stop_car}).

-spec accelerate(pos_integer()) -> integer().
accelerate(Value) ->
    gen_server:call(?MODULE, {accelerate, Value}).

-spec brake(pos_integer()) -> integer().
brake(Value) ->
    gen_server:call(?MODULE, {brake, Value}).

%%-----------------------------------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------------------------------

init([]) ->
   {ok, #state{speed = 0, onoff = off}}.

handle_call({start_car}, _From, S) ->
    {reply, on, S#state{onoff = on}};
handle_call({stop_car}, _From, S) ->
    {reply, off, S#state{onoff = off}};
handle_call({accelerate, Value}, _From, S) ->
    #state{speed = Speed} = S,
    {reply, Speed + Value, S#state{speed = Speed + Value}};
handle_call({brake, Value}, _From, S) ->
    #state{speed = Speed} = S,
    case Speed - Value >= 0 of
        true -> {reply, Speed - Value, S#state{speed = Speed - Value}};
        false -> {reply, 0, S#state{speed = 0}}
    end.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
