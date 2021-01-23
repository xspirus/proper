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
-export([start_link/1, stop/0, move/1]).
%% Helpers
-export([maze/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
%% proper_statem callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).
%% Properties
-export([prop_labyrinth/1, prop_labyrinth_targeted/1]).

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
  gen_server:start_link({local, ?NAME}, ?MODULE, [Maze], []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?NAME).

-spec move(step()) -> brick().
move(Step) ->
  gen_server:call(?NAME, Step).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

init([Maze]) ->
  {Entrance, Exit, Walls} = draw_map(Maze),
  {ok, #state{exit = Exit, position = Entrance, walls = Walls}}.

handle_call(Step, _From, S) ->
  #state{exit = Exit, position = Position, walls = Walls} = S,
  {X, Y} = Position,
  NextPos = case Step of
              up    -> {X - 1, Y};
              right -> {X, Y + 1};
              down  -> {X + 1, Y};
              left  -> {X, Y - 1}
            end,
  NewPos = case sets:is_element(NextPos, Walls) of
             true  -> Position;
             false -> NextPos
           end,
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
%% proper_statem callbacks
%% -----------------------------------------------------------------------------

initial_state() ->
  {Entrance, Exit, Walls} = draw_map(maze(0)),
  #state{exit = Exit, position = Entrance, walls = Walls}.

command(_S) ->
  oneof([{call, ?MODULE, move, [up]},
         {call, ?MODULE, move, [right]},
         {call, ?MODULE, move, [down]},
         {call, ?MODULE, move, [left]}]).

precondition(State, {call, _, move, [Step]}) ->
  #state{position = Position, walls = Walls} = State,
  {X, Y} = Position,
  NextPos = case Step of
              up    -> {X - 1, Y};
              right -> {X, Y + 1};
              down  -> {X + 1, Y};
              left  -> {X, Y - 1}
            end,
  not sets:is_element(NextPos, Walls);
precondition(_, _) -> true.

postcondition(_State, {call, _, move, [_Step]}, Res) ->
  Res =/= wall andalso Res =/= exit;
postcondition(_, _, _) -> true.

next_state(State, _V, {call, _, move, [Step]}) ->
  #state{position = Position, walls = Walls} = State,
  {X, Y} = Position,
  NextPos = case Step of
              up    -> {X - 1, Y};
              right -> {X, Y + 1};
              down  -> {X + 1, Y};
              left  -> {X, Y - 1}
            end,
  case sets:is_element(NextPos, Walls) of
    true  -> State;
    false -> State#state{position = NextPos}
  end.

%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------

prop_labyrinth(Maze) ->
  {Entrance, Exit, Walls} = draw_map(Maze),
  State = #state{exit = Exit, position = Entrance, walls = Walls},
  ?FORALL(Cmds, commands(?MODULE, State),
          begin
            start_link(Maze),
            {_H, _S, R} = run_commands(?MODULE, Cmds),
            stop(),
            ?WHENFAIL(io:format("~w~n", [path(Cmds)]),
                      aggregate(command_names(normalize(Cmds)), R =:= ok))
          end).

prop_labyrinth_targeted(Maze) ->
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
                 aggregate(command_names(normalize(Cmds)), R =:= ok))
     end).

%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------

-spec maze(0..2) -> maze().
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

path(Cmds) ->
  path(Cmds, []).

path([], Path) ->
  lists:reverse(Path);
path([{set, _V, {call, _, move, [Step]}} | Cmds], Path) ->
  path(Cmds, [Step | Path]);
path([_ | Cmds], Path) ->
  path(Cmds, Path).

normalize(Cmds) ->
  normalize(Cmds, []).

normalize([], Normalized) ->
  lists:reverse(Normalized);
normalize([{set, V, {call, Mod, move, [Step]}} | Cmds], Normalized) ->
  normalize(Cmds, [{set, V, {call, Mod, Step, []}} | Normalized]);
normalize([Cmd | Cmds], Normalized) ->
  normalize(Cmds, [Cmd | Normalized]).
