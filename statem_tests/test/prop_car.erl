-module(prop_car).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([test/1]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).

-type speed() :: non_neg_integer().

-record(state, {speed :: speed(),
                onoff :: 'on' | 'off'}).

-define(SERVER, statem_car).

%% ----------------------------------------------------------------------------
%% statem callbacks
%% ----------------------------------------------------------------------------

initial_state() ->
    #state{speed = 0, onoff = off}.

command(S) ->
    On = (S#state.onoff =:= on),
    Off = (S#state.onoff =:= off),
    oneof([{call, ?SERVER, start_car, []} || Off] ++
          [{call, ?SERVER, stop_car, []} || On] ++
          [{call, ?SERVER, accelerate, [pos_integer()]} || On] ++
          [{call, ?SERVER, brake, [pos_integer()]} || On]).

next_state(S, _V, {call, _, start_car, []}) ->
    S#state{onoff = on};
next_state(S, _V, {call, _, stop_car, []}) ->
    S#state{speed = 0, onoff = off};
next_state(S, _V, {call, _, accelerate, [Value]}) ->
    S#state{speed = S#state.speed + Value};
next_state(S, _V, {call, _, brake, [Value]}) ->
    S#state{speed = S#state.speed - Value}.

precondition(_, _) -> true.

postcondition(_S, {call, _, start_car, []}, Result) ->
    Result =:= on;
postcondition(_S, {call, _, stop_car, []}, Result) ->
    Result =:= off;
postcondition(_S, {call, _, accelerate, [_Value]}, Result) ->
    Result < 1000;
postcondition(_S, {call, _, brake, [_Value]}, Result) ->
    Result >= 0.

%% ----------------------------------------------------------------------------
%% Properties
%% ----------------------------------------------------------------------------

prop_server_works() ->
    ?FORALL(Cmds, commands(?MODULE),
        ?TRAPEXIT(
            begin
                ?SERVER:start_link(),
                {History, State, Result} = run_commands(?MODULE, Cmds),
                ?SERVER:stop(),
                ?WHENFAIL(io:format("History: ~p~nState: ~p~nResult: ~p~n",
                                    [History, State, Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end)).

test(Number) ->
    proper:quickcheck(prop_server_works(), Number).
