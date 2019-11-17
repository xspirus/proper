%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright (c) 2019, Spiros Dontas <spirosdontas@gmail.com>
%%%                and  Kostis Sagonas <kostis@it.uu.se>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2019 Spiros Dontas and Kostis Sagonas
%%% @version {@version}
%%% @author Spiros Dontas

-module(car_statem).
-behaviour(gen_server).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").


%% -----------------------------------------------------------------------------
%% Definitions
%% -----------------------------------------------------------------------------


-define(HOUR, 3600).
-define(ACCELERATION, 5).
-define(DECELERATION, 20).
-define(MAX_FUEL, 70).
-define(MAX_SPEED, 200).
-define(AVG(X), lists:sum(X) / length(X)).


%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------


%% api
-export([start_link/0, stop/0, accelerate/1, brake/1, travel/1, refuel/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
%% proper_statem
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).
-export([list_commands/1]).
%% properties
-export([prop_normal_distance/0, prop_weighted_distance/0]).
%% proper
-export([test/1, test/2]).


%% -----------------------------------------------------------------------------
%% Records
%% -----------------------------------------------------------------------------


-record(gen_state,
        {fuel :: float(),
         speed :: non_neg_integer()}).

-record(state,
        {fuel :: float(),
         speed :: non_neg_integer(),
         distance :: float(),
         burnt :: float()}).


%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------


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


%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------


init([]) ->
  {ok, #gen_state{fuel = ?MAX_FUEL, speed = 0}}.

handle_call({accelerate, Value}, _From, S) ->
  #gen_state{fuel = Fuel, speed = Speed} = S,
  {Distance, Acceleration, Burnt} =
    acceleration_calculations({Speed, Value}, Fuel),
  {reply, {Distance, Burnt}, S#gen_state{fuel = Fuel - Burnt,
                                         speed = Speed + Acceleration}};

handle_call({brake, Value}, _From, S) ->
  #gen_state{fuel = Fuel, speed = Speed} = S,
  {Distance, Deceleration, Burnt} =
    acceleration_calculations({Speed, -Value}, Fuel),
  {reply, {Distance, Burnt}, S#gen_state{fuel = Fuel - Burnt,
                                         speed = Speed + Deceleration}};

handle_call({travel, Distance}, _From, S) ->
  #gen_state{fuel = Fuel, speed = Speed} = S,
  {RealDistance, Burnt} = travel_calculations(Distance, Speed, Fuel),
  {reply, {RealDistance, Burnt}, S#gen_state{fuel = Fuel - Burnt}};

handle_call({refuel, Amount}, _From, S) ->
  #gen_state{fuel = Fuel, speed = Speed} = S,
  {Distance, _Deceleration, Burnt} =
    acceleration_calculations({Speed, -Speed}, Fuel),
  {reply, {Distance, Burnt}, S#gen_state{fuel = Fuel - Burnt + Amount, speed = 0}}.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info(_Msg, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  {ok}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.


%% -----------------------------------------------------------------------------
%% Generators
%% -----------------------------------------------------------------------------


accelerator(Speed) ->
  case Speed > ?MAX_SPEED - 1 of
    true -> exactly(0);
    false -> integer(1, ?MAX_SPEED - Speed)
  end.

braker(Speed) ->
  case Speed - 1 < 1 of
    true -> exactly(0);
    false -> integer(1, Speed)
  end.

traveler() ->
  integer(1, 100).

refueler(Fuel) ->
  case Fuel > ?MAX_FUEL - 1 of
    true -> exactly(0);
    false -> integer(0, round(?MAX_FUEL - Fuel))
  end.


%% -----------------------------------------------------------------------------
%% proper_statem callbacks
%% -----------------------------------------------------------------------------


initial_state() ->
  #state{fuel     = ?MAX_FUEL,
         speed    = 0,
         burnt    = 0,
         distance = 0}.

command(S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  oneof([{call, ?MODULE, accelerate, [accelerator(Speed)]},
         {call, ?MODULE, brake, [braker(Speed)]},
         {call, ?MODULE, travel, [traveler()]},
         {call, ?MODULE, refuel, [refueler(Fuel)]}]).

list_commands(S) ->
  #state{fuel = Fuel, speed = Speed} = S,
  [{call, ?MODULE, accelerate, [accelerator(Speed)]},
   {call, ?MODULE, brake, [braker(Speed)]},
   {call, ?MODULE, travel, [traveler()]},
   {call, ?MODULE, refuel, [refueler(Fuel)]}].

precondition(#state{fuel = Fuel, speed = Speed}, {call, _, accelerate, _}) ->
  Fuel > ?MAX_FUEL * 0.1 andalso Speed < 200;
precondition(#state{speed = Speed}, {call, _, brake, _}) ->
  Speed > 0;
precondition(#state{speed = Speed}, {call, _, travel, _}) ->
  Speed > 20;
precondition(#state{fuel = Fuel}, {call, _, refuel, _}) ->
  Fuel < ?MAX_FUEL * 0.8;
precondition(_, _) ->
  true.

postcondition(_S, _, {Distance, Burnt}) ->
  Distance >= 0.0 andalso Burnt >= 0.0.

next_state(S, _V, {call, _, accelerate, [Value]}) ->
  #state{fuel = Fuel,
         speed = Speed,
         distance = Distance,
         burnt = B} = S,
  {Travelled, Acceleration, Burnt} =
    acceleration_calculations({Speed, Value}, Fuel),
  S#state{fuel = Fuel - Burnt,
          speed = Speed + Acceleration,
          distance = Distance + Travelled,
          burnt = B + Burnt};
next_state(S, _V, {call, _, brake, [Value]}) ->
  #state{fuel = Fuel,
         speed = Speed,
         distance = Distance,
         burnt = B} = S,
  {Travelled, Acceleration, Burnt} =
    acceleration_calculations({Speed, -Value}, Fuel),
  S#state{fuel = Fuel - Burnt,
          speed = Speed + Acceleration,
          distance = Distance + Travelled,
          burnt = B + Burnt};
next_state(S, _V, {call, _, travel, [Value]}) ->
  #state{fuel = Fuel,
         speed = Speed,
         distance = Distance,
         burnt = B} = S,
  {Travelled, Burnt} = travel_calculations(Value, Speed, Fuel),
  S#state{fuel = Fuel - Burnt,
          distance = Distance + Travelled,
          burnt = B + Burnt};
next_state(S, _V, {call, _, refuel, [Amount]}) ->
  #state{fuel = Fuel,
         speed = Speed,
         distance = Distance,
         burnt = B} = S,
  {Travelled, Acceleration, Burnt} =
    acceleration_calculations({Speed, -Speed}, Fuel),
  S#state{fuel = Fuel - Burnt + Amount,
          speed = Speed + Acceleration,
          distance = Distance + Travelled,
          burnt = B + Burnt}.


%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------


%% This should rarely produce a counter example.
prop_normal_distance() ->
  ?FORALL(Cmds, commands(?MODULE),
          ?TRAPEXIT(
             begin
               start_link(),
               {_H, S, R} = run_commands(?MODULE, Cmds),
               stop(),
               #state{distance = Distance, burnt = Burnt} = S,
               Consumption = case Distance > 0 of
                               true -> 100 * Burnt / Distance;
                               false -> 0
                             end,
               ?WHENFAIL(
                  io:format("Distance: ~p~nConsumption: ~p~n",
                            [Distance, Consumption]),
                  aggregate(command_names(Cmds),
                            R =:= ok andalso (Distance < 1000 orelse
                                              Consumption > 10)))
             end)).


%% This should produce a counter example in 1000 runs.
prop_weighted_distance() ->
  Weights = #{accelerate => 1, brake => 2, travel => 5, refuel => 1},
  ?FORALL(Cmds, proper_statem:weighted_commands(?MODULE, Weights),
          ?TRAPEXIT(
             begin
               start_link(),
               {_H, S, R} = run_commands(?MODULE, Cmds),
               stop(),
               #state{distance = Distance, burnt = Burnt} = S,
               Consumption = case Distance > 0 of
                               true -> 100 * Burnt / Distance;
                               false -> 0
                             end,
               ?WHENFAIL(
                  io:format("Distance: ~p~nConsumption: ~p~n",
                            [Distance, Consumption]),
                  aggregate(command_names(Cmds),
                            R =:= ok andalso (Distance < 1000 orelse
                                              Consumption > 10)))
             end)).

%% -----------------------------------------------------------------------------
%% Automatic Testing
%% -----------------------------------------------------------------------------


test(Prop) ->
  test(Prop, 100).

test(Prop, N) ->
  proper:quickcheck(Prop, N).


%% -----------------------------------------------------------------------------
%% Calculation Functions
%% -----------------------------------------------------------------------------


travel_calculations(Distance, Speed, Fuel) when Speed > 0 ->
  Consumption = fuel_consumption(Speed),
  Burn = Consumption * Distance / 100,
  case Burn > Fuel of
    true -> {Fuel * 100 / Consumption, Fuel};
    false -> {Distance, Burn}
  end;
travel_calculations(_D, _S, _F) ->
  {0, 0.0}.

acceleration_calculations({Speed, Acceleration}, Fuel) when Acceleration > 0 ->
  Consumption = fuel_consumption(Speed, Acceleration),
  Distance = calculate_distance(Speed, Acceleration),
  Burn = Consumption * Distance / 100,
  case Burn > Fuel of
    true -> acceleration_calculations({Speed, Acceleration - ?ACCELERATION},
                                      Fuel);
    false -> {Distance, Acceleration, Burn}
  end;
acceleration_calculations({Speed, Acceleration}, Fuel) ->
  Consumption = fuel_consumption(Speed, Acceleration),
  Distance = calculate_distance(Speed, Acceleration),
  Burn = Consumption * Distance / 100,
  case Burn > Fuel of
    true -> {Distance, Acceleration, Fuel};
    false -> {Distance, Acceleration, Burn}
  end.


%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------


%% Calculate distance driven when accelerating - decelerating.
calculate_distance(Speed, Acceleration) when Acceleration > 0 ->
  T = Acceleration / ?ACCELERATION,
  Speed / ?HOUR * T + 1 / 2 * ?ACCELERATION / ?HOUR * T * T;
calculate_distance(Speed, Acceleration)->
  T = -Acceleration / ?DECELERATION,
  Speed / ?HOUR * T - 1 / 2 * ?DECELERATION / ?HOUR * T * T.

%% Low speeds give rewards to consumption.
%% High speed give penalty to consumption.
fuel_speed_penalty(Speed) when Speed =< 50 -> 0.7;
fuel_speed_penalty(Speed) when Speed =< 100 -> 0.9;
fuel_speed_penalty(Speed) when Speed =< 150 -> 1.1;
fuel_speed_penalty(_) -> 1.5.

%% Acceleration penalty.
%% Deceleration reward.
fuel_acceleration_penalty(Acceleration) when Acceleration > 0 -> 2.0;
fuel_acceleration_penalty(_) -> 0.1.

%% Fuel Consumption (stable speed).
fuel_consumption(Speed) ->
  Speed * fuel_speed_penalty(Speed) / 10.

%% Fuel Consumption (acc - dec).
fuel_consumption(Speed, Acceleration) ->
  Consumptions = [fuel_consumption(S) *
                    fuel_acceleration_penalty(Acceleration)
                  || S <- intermediate_speeds(Speed, Acceleration)],
  ?AVG(Consumptions).

%% Intermediate speeds from accelerating - decelerating.
intermediate_speeds(Speed, Acceleration) when Acceleration > 0 ->
  T = Acceleration / ?ACCELERATION,
  [Speed + X / 10 * ?ACCELERATION || X <- lists:seq(0, round(T * 10))];
intermediate_speeds(Speed, Acceleration) ->
  T = -Acceleration / ?DECELERATION,
  [Speed - X / 10 * ?DECELERATION || X <- lists:seq(0, round(T * 10))].
