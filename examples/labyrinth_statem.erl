%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2020-     Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
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

%%% @copyright 2020 Spiros Dontas and Kostis Sagonas
%%% @version {@version}
%%% @author Spiros Dontas

-module(labyrinth_statem).
-behaviour(gen_server).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------

%% API
-export([start_link/1, stop/0, up/1, right/1, down/1, left/1]).
%% Helpers
-export([maze/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
%% proper_statem callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).
%% Properties
-export([prop_random/1, prop_targeted/1]).

%% -----------------------------------------------------------------------------
%% Defines
%% -----------------------------------------------------------------------------

-define(NAME, labyrinth).

%% -----------------------------------------------------------------------------
%% Types
%% -----------------------------------------------------------------------------

-type position() :: {integer(), integer()}.
-type brick()    :: exit | entrance | none | wall.
-type maze()     :: list(string()).
-type walls()    :: sets:set(position()).
-type step()     :: up | right | down | left.

%% -----------------------------------------------------------------------------
%% Records
%% -----------------------------------------------------------------------------

-record(state,
        {exit     :: position(),
         position :: position(),
         walls    :: walls()}).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

-spec start_link(maze()) -> pid().
start_link(Maze) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Maze], []),
  put('$server', Pid).

-spec stop() -> ok.
stop() ->
  gen_server:stop(get('$server')).

-spec up(integer()) -> brick().
up(Num) ->
  gen_server:call(get('$server'), {up, Num}).

-spec right(integer()) -> brick().
right(Num) ->
  gen_server:call(get('$server'), {right, Num}).

-spec down(integer()) -> brick().
down(Num) ->
  gen_server:call(get('$server'), {down, Num}).

-spec left(integer()) -> brick().
left(Num) ->
  gen_server:call(get('$server'), {left, Num}).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

init([Maze]) ->
  {Entrance, Exit, Walls} = draw_map(Maze),
  {ok, #state{exit = Exit, position = Entrance, walls = Walls}}.

handle_call({Step, Num}, _From, S) ->
  #state{exit = Exit, position = Position, walls = Walls} = S,
  NewPos = follow_steps(lists:duplicate(Num, Step), Position, Exit, Walls),
  Brick = case NewPos of
            Exit -> exit;
            _    -> none
          end,
  {reply, Brick, S#state{position = NewPos}}.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info(_Msg, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%% -----------------------------------------------------------------------------
%% Generators
%% -----------------------------------------------------------------------------

numsteps() ->
  integer(1, 5).

%% -----------------------------------------------------------------------------
%% proper_statem callbacks
%% -----------------------------------------------------------------------------

initial_state() ->
  {Entrance, Exit, Walls} = draw_map(maze(0)),
  #state{exit = Exit, position = Entrance, walls = Walls}.

command(_S) ->
  oneof([{call, ?MODULE, up, [numsteps()]},
         {call, ?MODULE, right, [numsteps()]},
         {call, ?MODULE, down, [numsteps()]},
         {call, ?MODULE, left, [numsteps()]}]).

precondition(State, {call, _, Step, [Num]}) ->
  #state{position = Position, walls = Walls} = State,
  not steps_hit_wall(lists:duplicate(Num, Step), Position, Walls);
precondition(_, _) -> true.

postcondition(_State, {call, _, _Step, [_Num]}, Res) ->
  Res =/= wall andalso Res =/= exit;
postcondition(_, _, _) -> true.

next_state(State, _V, {call, _, Step, [Num]}) ->
  #state{exit = Exit, position = Position, walls = Walls} = State,
  NextPos = follow_steps(lists:duplicate(Num, Step), Position, Exit, Walls),
  State#state{position = NextPos}.

%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------

prop_random(Maze) ->
  {Entrance, Exit, Walls} = draw_map(Maze),
  State = #state{exit = Exit, position = Entrance, walls = Walls},
  ?FORALL(Cmds, commands(?MODULE, State),
          begin
            start_link(Maze),
            {_H, _S, R} = run_commands(?MODULE, Cmds),
            stop(),
            ?WHENFAIL(io:format("~w~n", [path(Cmds)]),
                      aggregate(command_names(Cmds), R =:= ok))
          end).

prop_targeted(Maze) ->
  {Entrance, Exit, Walls} = draw_map(Maze),
  State = #state{exit = Exit, position = Entrance, walls = Walls},
  ?FORALL_TARGETED(
     Cmds, commands(?MODULE, State),
     begin
       start_link(Maze),
       {_H, S, R} = run_commands(?MODULE, Cmds),
       stop(),
       #state{exit = E, position = P} = S,
       ?MINIMIZE(distance(P, E)),
       ?WHENFAIL(io:format("~w~n", [path(Cmds)]),
                 aggregate(command_names(Cmds), R =:= ok))
     end).

%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------

-spec maze(0..3) -> maze().
maze(0) ->
  ["#########",
   "#X     E#",
   "#########"];
maze(1) ->
  ["######################################################################",
   "#                                                                    #",
   "#   E                                                                #",
   "#                                  #####                             #",
   "#                                  #####                             #",
   "#        #####                     #####        #####                #",
   "#        #####                                  #####                #",
   "#        #####                                  #####                #",
   "#                          #####                                     #",
   "#                          #####                                     #",
   "#                          #####                                     #",
   "#                                         #####          ##########  #",
   "#                                         #####          ##########  #",
   "#             #####                       #####          ##########  #",
   "#             #####                                                  #",
   "#             #####                                                  #",
   "#                                #####                               #",
   "#                                #####                               #",
   "#                                #####         #####                 #",
   "#                                              #####                 #",
   "#                                              #####                 #",
   "#                                                              X     #",
   "#                                                                    #",
   "######################################################################"];
maze(2) ->
  ["######################################################################",
   "#                                                                    #",
   "#    X                                                               #",
   "#                                                                    #",
   "#                        ########   #####     ####   ########        #",
   "#                           ##      ##   #    ##  #     ##           #",
   "##########                  ##      #####     ####      ##           #",
   "#                           ##      ##        ##  #     ##           #",
   "#                           ##      ##        ####      ##           #",
   "#                                                                    #",
   "#                                                                    #",
   "#                   #                                                #",
   "#                   #                                                #",
   "#                   #                #################################",
   "#                   #                                                #",
   "#                   #                                                #",
   "#                   #                                                #",
   "#                   ####################################             #",
   "#                                                                    #",
   "#                                                                    #",
   "################################                                     #",
   "#                                     E                              #",
   "#                                                                    #",
   "######################################################################"];
maze(3) ->
  ["######################################################################",
   "#                                                                    #",
   "#   X                                                                #",
   "#                                  #####                             #",
   "#                                  #####                             #",
   "#        #####                     #####        #####                #",
   "#        #####                                  #####                #",
   "#        #####                                  #####                #",
   "#                          #####                                     #",
   "#                          #####                                     #",
   "#                          #####                                     #",
   "#                                         #####          ##########  #",
   "#                                         #####          ##########  #",
   "#             #####                       #####          ##########  #",
   "#             #####                                                  #",
   "#             #####                                                  #",
   "#                                #####                               #",
   "#                                #####                               #",
   "#                                #####         #####                 #",
   "#                                              #####                 #",
   "#                                              #####                 #",
   "#                                                              E     #",
   "#                                                                    #",
   "######################################################################"].

-spec draw_map(maze()) -> {position(), position(), walls()}.
draw_map(Maze) ->
  draw_map(Maze, {undefined, undefined, sets:new()}, 0).

draw_map([], Acc, _) -> Acc;
draw_map([Line | T], Acc, X) ->
  NewAcc = draw_line(Line, Acc, X, 0),
  draw_map(T, NewAcc, X + 1).

draw_line([], Acc, _, _) -> Acc;
draw_line([$ | T], Acc, X, Y) ->
  draw_line(T, Acc, X, Y + 1);
draw_line([$# | T], {Entrance, Exit, Walls}, X, Y) ->
  draw_line(T, {Entrance, Exit, sets:add_element({X, Y}, Walls)}, X, Y + 1);
draw_line([$X | T], {Entrance, _, Walls}, X, Y) ->
  draw_line(T, {Entrance, {X, Y}, Walls}, X, Y + 1);
draw_line([$E | T], {_, Exit, Walls}, X, Y) ->
  draw_line(T, {{X, Y}, Exit, Walls}, X, Y + 1).

-spec distance(position(), position()) -> integer().
distance({X1, Y1}, {X2, Y2}) ->
  abs(X1 - X2) + abs(Y1 - Y2).

-spec follow_steps([step()], position(), position(), walls()) -> position().
follow_steps(_Steps, Exit, Exit, _Walls) -> Exit;
follow_steps([], Position, _Exit, _Walls) -> Position;
follow_steps([Step | Steps], Position, Exit, Walls) ->
  NextPos = make_step(Step, Position),
  NewPos = case sets:is_element(NextPos, Walls) of
             true  -> Position;
             false -> NextPos
           end,
  follow_steps(Steps, NewPos, Exit, Walls).

-spec steps_hit_wall([step()], position(), walls()) -> boolean().
steps_hit_wall([], _Position, _Walls) -> false;
steps_hit_wall([Step | Steps], Position, Walls) ->
  NextPos = make_step(Step, Position),
  case sets:is_element(NextPos, Walls) of
    true  -> true;
    false -> steps_hit_wall(Steps, NextPos, Walls)
  end.

-spec make_step(step(), position()) -> position().
make_step(Step, {X, Y}) ->
  case Step of
    up    -> {X - 1, Y};
    right -> {X, Y + 1};
    down  -> {X + 1, Y};
    left  -> {X, Y - 1}
  end.

path(Cmds) ->
  path(Cmds, []).

path([], Path) ->
  lists:reverse(Path);
path([{set, _V, {call, _, Step, [Num]}} | Cmds], Path) ->
  path(Cmds, lists:duplicate(Num, Step) ++ Path);
path([_ | Cmds], Path) ->
  path(Cmds, Path).
