-module(prop_car_statem).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).

-type fuel() :: float().
-type speed() :: non_neg_integer().
-type distance() :: float().

-record(state, {fuel :: fuel(),
                speed :: speed(),
                distance :: distance(),
                consumptions :: [float()]}).

-define(SERVER, statem_car_server).

-include("statem_car_model.hrl").

%% ---------------------------------------------------------------------------
%% Generators
%% ---------------------------------------------------------------------------

% accelerate(Speed) when Speed > 20 ->
%     integer(10, round(Speed * 0.1));
accelerate(Speed) ->
    integer(0, 200 - Speed).

% brake(Speed) when Speed > 20 ->
%     integer(10, round(Speed * 0.1));
brake(Speed) ->
    integer(0, Speed).

refuel(Fuel) ->
    integer(0, round(?MAX_FUEL - Fuel)).

%% ----------------------------------------------------------------------------
%% statem callbacks
%% ----------------------------------------------------------------------------

initial_state() ->
    #state{
        fuel = ?MAX_FUEL,
        speed = 0,
        consumptions = [],
        distance = 0
    }.

command(S) ->
    #state{fuel = Fuel, speed = Speed} = S,
    oneof([
        {call, ?SERVER, accelerate, [accelerate(Speed)]},
        {call, ?SERVER, brake, [brake(Speed)]},
        {call, ?SERVER, travel, [pos_integer()]},
        {call, ?SERVER, refuel, [refuel(Fuel)]}
    ]).

precondition(#state{fuel = Fuel, speed = Speed}, {call, _, accelerate, _}) ->
    Fuel > ?MAX_FUEL * 0.1 andalso Speed < 200;
precondition(#state{speed = Speed}, {call, _, brake, _}) ->
    Speed > 0;
precondition(#state{speed = Speed}, {call, _, travel, _}) ->
    Speed > 40;
precondition(#state{fuel = Fuel}, {call, _, refuel, _}) ->
    Fuel < ?MAX_FUEL * 0.8;
precondition(_, _) ->
    true.

postcondition(_S, _, {Distance, _Consumptions}) ->
    Distance >= 0.

next_state(S, _V, {call, _, accelerate, [Value]}) ->
    #state{
        fuel = Fuel,
        speed = Speed,
        distance = Distance,
        consumptions = Consumptions
    } = S,
    {Travelled, Acceleration, Consumption, Burnt} = acceleration_calculations({Speed, Value}, Fuel),
    S#state{
        fuel = Fuel - Burnt,
        speed = Speed + Acceleration,
        distance = Distance + Travelled,
        consumptions = Consumptions ++ Consumption
    };
next_state(S, _V, {call, _, brake, [Value]}) ->
    #state{
        fuel = Fuel,
        speed = Speed,
        distance = Distance,
        consumptions = Consumptions
    } = S,
    {Travelled, Deceleration, Consumption, Burnt} = acceleration_calculations({Speed, -Value}, Fuel),
    S#state{
        fuel = Fuel - Burnt,
        speed = Speed + Deceleration,
        distance = Distance + Travelled,
        consumptions = Consumptions ++ Consumption
    };
next_state(S, _V, {call, _, travel, [Value]}) ->
    #state{
        fuel = Fuel,
        speed = Speed,
        distance = Distance,
        consumptions = Consumptions
    } = S,
    {Travelled, Consumption, Burnt} = travel_calculations(Value, Speed, Fuel),
    S#state{
        fuel = Fuel - Burnt,
        distance = Distance + Travelled,
        consumptions = Consumptions ++ Consumption
    };
next_state(S, _V, {call, _, refuel, [Amount]}) ->
    #state{
        fuel = Fuel,
        speed = Speed,
        distance = Distance,
        consumptions = Consumptions
    } = S,
    {Travelled, _Deceleration, Consumption, Burnt} = acceleration_calculations({Speed, -Speed}, Fuel),
    S#state{
        fuel = Fuel - Burnt + Amount,
        speed = 0,
        distance = Distance + Travelled,
        consumptions = Consumptions ++ Consumption
    }.

update_state(S, {Distance, Consumptions, {Fuel, Speed}}) ->
    S#state{
        fuel = Fuel,
        speed = Speed,
        consumptions = S#state.consumptions ++ Consumptions,
        distance = S#state.distance + Distance
    };
update_state(S, _) -> S.

%% ----------------------------------------------------------------------------
%% Properties
%% ----------------------------------------------------------------------------

prop_server_works() ->
    ?FORALL(Cmds, commands(?MODULE),
        ?TRAPEXIT(
            begin
                ?SERVER:start_link(),
                {_History, State, Result} = run_commands(?MODULE, Cmds),
                ?SERVER:stop(),
                #state{distance = Distance, consumptions = Consumptions} = State,
                ?WHENFAIL(io:format("Distance: ~p~nConsumption: ~p~nResult: ~p~n",
                                    [Distance, ?AVG(Consumptions), Result]),
                          aggregate(command_names(Cmds), Result =:= ok andalso (Distance < 200 orelse ?AVG(Consumptions) > 7)))
            end)).
