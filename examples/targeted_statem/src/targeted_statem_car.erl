-module(targeted_statem_car).

-behaviour(gen_server).

%% ---------------------------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------------------------
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ---------------------------------------------------------------------------
%% API exports
%% ---------------------------------------------------------------------------

-export([accelerate/1, brake/1, travel/1, refuel/1]).

-include("targeted_statem_car.hrl").

%% ---------------------------------------------------------------------------
%% Records
%% ---------------------------------------------------------------------------

-record(state, {
    fuel :: float(),
    speed :: non_neg_integer()
}).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

accelerate(Value) ->
    gen_server:call(?MODULE, {accelerate, Value}).

brake(Value) ->
    gen_server:call(?MODULE, {brake, Value}).

travel(Distance) ->
    gen_server:call(?MODULE, {travel, Distance}).

refuel(Amount) ->
    gen_server:call(?MODULE, {refuel, Amount}).

%% ---------------------------------------------------------------------------
%% gen_server callbacks implementation
%% ---------------------------------------------------------------------------

init([]) ->
    {ok, #state{fuel = ?MAX_FUEL, speed = 0}}.

handle_call({accelerate, Value}, _From, S) ->
    #state{fuel = Fuel, speed = Speed} = S,
    {Distance, Acceleration, Consumption, Burnt} = acceleration_calculations({Speed, Value}, Fuel),
    {reply, {Distance, Consumption}, S#state{fuel = Fuel - Burnt, speed = Speed + Acceleration}};

handle_call({brake, Value}, _From, S) ->
    #state{fuel = Fuel, speed = Speed} = S,
    {Distance, Deceleration, Consumption, Burnt} = acceleration_calculations({Speed, -Value}, Fuel),
    {reply, {Distance, Consumption}, S#state{fuel = Fuel - Burnt, speed = Speed + Deceleration}};

handle_call({travel, Distance}, _From, S) ->
    #state{fuel = Fuel, speed = Speed} = S,
    {RealDistance, Consumption, Burnt} = travel_calculations(Distance, Speed, Fuel),
    {reply, {RealDistance, Consumption}, S#state{fuel = Fuel - Burnt}};

handle_call({refuel, Amount}, _From, S) ->
    #state{fuel = Fuel, speed = Speed} = S,
    {Distance, _Deceleration, Consumption, Burnt} = acceleration_calculations({Speed, -Speed}, Fuel),
    {reply, {Distance, Consumption}, S#state{fuel = Fuel - Burnt + Amount, speed = 0}}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    {ok}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.
