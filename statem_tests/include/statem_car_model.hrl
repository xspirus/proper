%% ---------------------------------------------------------------------------
%% Definitions
%% ---------------------------------------------------------------------------

-define(HOUR, 3600).
-define(ACCELERATION, 5).
-define(DECELERATION, 20).
-define(MAX_FUEL, 70).

%% AVG function for lists
-define(AVG(X), lists:sum(X) / length(X)).

%% ---------------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------------

travel_calculations(Distance, Speed, Fuel) ->
    Consumption = fuel_consumption(Speed),
    Burn = Consumption * Distance / 100,
    if
        Burn > Fuel ->
            Burnt = Fuel,
            Travelled = Fuel * 100 / Consumption;
        Burn =< Fuel ->
            Burnt = Burn,
            Travelled = Distance
    end,
    T = round(Travelled / Speed * 10),
    Consumptions = lists:duplicate(T, Consumption),
    {Distance, Consumptions, Burnt}.

acceleration_calculations({Speed, Acceleration}, Fuel) when Acceleration > 0 ->
    Consumption = fuel_consumption(Speed, Acceleration),
    Distance = calculate_distance(Speed, Acceleration),
    Burn = Consumption * Distance / 100,
    if
        Burn > Fuel ->
            acceleration_calculations({Speed, Acceleration - ?ACCELERATION}, Fuel);
        Burn =< Fuel ->
            T = round(Acceleration / ?ACCELERATION * 10),
            {Distance, Acceleration, lists:duplicate(T, Consumption), Burn}
    end;
acceleration_calculations({Speed, Acceleration}, Fuel) ->
    Consumption = fuel_consumption(Speed, Acceleration),
    Distance = calculate_distance(Speed, Acceleration),
    Burn = Consumption * Distance / 100,
    T = round(-Acceleration / ?DECELERATION * 10),
    Consumptions = lists:duplicate(T, Consumption),
    if
        Burn > Fuel ->
            {Distance, Acceleration, Consumptions, Fuel};
        Burn =< Fuel ->
            {Distance, Acceleration, Consumptions, Burn}
    end.

%% ---------------------------------------------------------------------------
%% Private Functions (Helpers)
%% ---------------------------------------------------------------------------

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
    Speeds = intermediate_speeds(Speed, Acceleration),
    Consumptions = lists:map(fun(S) ->
        fuel_consumption(S) * fuel_acceleration_penalty(Acceleration)
    end, Speeds),
    ?AVG(Consumptions).

%% Intermediate speeds from accelerating - decelerating.
intermediate_speeds(Speed, Acceleration) when Acceleration > 0 ->
    T = Acceleration / ?ACCELERATION,
    [Speed + X / 10 * ?ACCELERATION || X <- lists:seq(0, round(T * 10))];
intermediate_speeds(Speed, Acceleration) ->
    T = -Acceleration / ?DECELERATION,
    [Speed - X / 10 * ?DECELERATION || X <- lists:seq(0, round(T * 10))].
