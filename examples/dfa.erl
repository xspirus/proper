-module(dfa).

-include_lib("proper/include/proper.hrl").

%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------

%% api
-export([start_link/1, stop/0, o/0, i/0, d/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
%% proper_statem callbacks
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).
%% properties
-export([prop_random/1, prop_targeted/1]).

-behaviour(gen_server).
-behaviour(proper_statem).

%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------

dfa(N, S) ->
  dfa(N, 0, S).

dfa(N, St, []) ->
  abs(St) =< N;
dfa(N, St, [o | S]) ->
  dfa(N, St, S);
dfa(N, St, [i | S]) ->
  St < N andalso dfa(N, St + 1, S);
dfa(N, St, [d | S]) ->
  St > -N andalso dfa(N, St - 1, S).

measure(S) ->
  measure(S, 0).

measure([], M) ->
  M;
measure([o | S], M) ->
  measure(S, M);
measure([i | S], M) ->
  measure(S, M + 1);
measure([d | S], M) ->
  measure(S, M - 1).

oracle(N, S) when N >= length(S) ->
  true; % all such strings are accepted
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

start_link(N) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [N], []),
  put('$server', Pid).

stop() ->
  gen_server:stop(get('$server')).

o() ->
  gen_server:call(get('$server'), o).

i() ->
  gen_server:call(get('$server'), i).

d() ->
  gen_server:call(get('$server'), d).

is_valid() ->
  gen_server:call(get('$server'), valid).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

init([N]) ->
  {ok, {N, 0}}.

handle_call(_, _From, false) ->
  {reply, false, false};
handle_call(o, _From, S) ->
  {reply, S, S};
handle_call(i, _From, {N, S}) ->
  NewS = S + 1,
  case abs(NewS) > N of
    true ->
      {reply, false, false};
    false ->
      {reply, NewS, {N, NewS}}
  end;
handle_call(d, _From, {N, S}) ->
  NewS = S - 1,
  case abs(NewS) > N of
    true ->
      {reply, false, false};
    false ->
      {reply, NewS, {N, NewS}}
  end;
handle_call(valid, _From, S) ->
  {reply, true, S}.

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
  undefined.

command(_S) ->
  oneof([{call, ?MODULE, o, []},
         {call, ?MODULE, i, []},
         {call, ?MODULE, d, []}]).

precondition(_, _) ->
  true.

postcondition(_, _, _) ->
  true.

next_state(_, _, _) ->
  undefined.

%% -----------------------------------------------------------------------------
%% Properties
%% -----------------------------------------------------------------------------

prop_random(N) ->
  ?FORALL(Cmds,
          commands(?MODULE),
          begin
            start_link(N),
            {_H, _S, R} = run_commands(?MODULE, Cmds),
            Res = is_valid(),
            stop(),
            S = reconstruct(Cmds),
            ?WHENFAIL(io:format("~w~n", [S]),
                      aggregate(command_names(Cmds),
                                R =:= ok andalso Res =:= oracle(N, S)))
          end).

prop_targeted(N) ->
  ?FORALL_TARGETED(Cmds,
                   commands(?MODULE),
                   begin
                     start_link(N),
                     {_H, _S, R} = run_commands(?MODULE, Cmds),
                     Res = is_valid(),
                     stop(),
                     S = reconstruct(Cmds),
                     case Res of
                       false -> proper_target:reset();
                       true  -> ?MINIMIZE(uv(N, S))
                     end,
                     ?WHENFAIL(io:format("~w~n", [S]),
                               aggregate(command_names(Cmds),
                                         R =:= ok andalso Res =:= oracle(N, S)))
                   end).

reconstruct(Cmds) ->
  reconstruct(Cmds, []).

reconstruct([], S) ->
  lists:reverse(S);
reconstruct([{set, {var, _N}, {call, _Mod, A, []}} | Cmds], S) ->
  reconstruct(Cmds, [A | S]);
reconstruct([_Cmd | Cmds], S) ->
  reconstruct(Cmds, S).

uv(N, S) when length(S) < N ->
  10000;
uv(N, S) ->
  abs(abs(measure(S)) - N).
