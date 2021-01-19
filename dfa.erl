-module(dfa).

-include_lib("proper/include/proper.hrl").

%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------

%% api
-export([start_link/0, stop/0, same/0, increase/0, decrease/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
%% proper_statem callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).
%% properties
-export([prop_dfa/1, prop_dfa_targeted/1]).

-behaviour(gen_server).
-behaviour(proper_statem).

%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------

dfa(N, S) -> dfa(N, 0, S).

dfa(N, St, []) -> abs(St) =< N;
dfa(N, St, [o|S]) -> dfa(N, St, S);
dfa(N, St, [u|S]) -> (St <  N) andalso dfa(N, St+1, S);
dfa(N, St, [d|S]) -> (St > -N) andalso dfa(N, St-1, S).

measure(S) -> measure(S, 0).

measure([], M) -> M;
measure([o|S], M) -> measure(S, M);
measure([u|S], M) -> measure(S, M + 1);
measure([d|S], M) -> measure(S, M - 1).

oracle(N, S) when N >= length(S) -> true; % all such strings are accepted
oracle(N, S) ->
  lists:all(fun(SS) -> abs(measure(SS)) =< N end, substrings(S, N + 1)).

%% returns all substrings of length at least K
substrings(S, K) ->
  N = length(S),
  [lists:sublist(S, I, M)
   || I <- lists:seq(1, N + 1 - K), M <- lists:seq(K, N), I + M =< N + 1].

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

same() ->
  gen_server:call(?MODULE, same).

increase() ->
  gen_server:call(?MODULE, increase).

decrease() ->
  gen_server:call(?MODULE, decrease).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

init([]) ->
  {ok, []}.

handle_call(same, _From, S) ->
  NewS = S ++ [o],
  {reply, NewS, NewS};
handle_call(increase, _From, S) ->
  NewS = S ++ [u],
  {reply, NewS, NewS};
handle_call(decrease, _From, S) ->
  NewS = S ++ [d],
  {reply, NewS, NewS}.

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
  {10, []}.

command(_S) ->
  oneof([{call, ?MODULE, same, []},
         {call, ?MODULE, increase, []},
         {call, ?MODULE, decrease, []}]).

precondition(_, _) -> true.

postcondition({N, _}, _Call, S) -> dfa(N, S) =:= oracle(N, S).

next_state({N, S}, _V, {call, ?MODULE, same, []}) -> {N, S ++ [o]};
next_state({N, S}, _V, {call, ?MODULE, increase, []}) -> {N, S ++ [u]};
next_state({N, S}, _V, {call, ?MODULE, decrease, []}) -> {N, S ++ [d]}.

%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------

prop_dfa(N) ->
  ?FORALL(Cmds, commands(?MODULE, {N, []}),
          begin
            start_link(),
            {_H, _S, R} = run_commands(?MODULE, Cmds),
            stop(),
            ?WHENFAIL(
               io:format("~w~n", [Cmds]),
               aggregate(command_names(Cmds), R =:= ok))
          end).

prop_dfa_targeted(N) ->
  ?FORALL_TARGETED(
     Cmds, targeted_commands(?MODULE, {N, []}),
     begin
       start_link(),
       {_H, {N, S}, R} = run_commands(?MODULE, Cmds),
       stop(),
       UV = abs(measure(S)),
       L = length(S),
       Lower = case L < N of
                 true  -> L div 2;
                 false -> N div 2
               end,
      %  io:format("~w ~w~n", [UV, S]),
       ?BOUND(UV, {Lower, N}),
      %  io:format("~w ~w~n", [uv(N, S), S]),
       ?WHENFAIL(
          io:format("~w~n", [S]),
          aggregate(command_names(Cmds), R =:= ok))
     end).

uv(N, S) -> abs(measure(S)) - N.
