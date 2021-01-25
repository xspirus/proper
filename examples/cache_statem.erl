-module(cache_statem).
-behaviour(gen_server).

-include_lib("proper/include/proper.hrl").

-export([start_link/1, stop/0, cache/2, find/1, flush/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).
-export([prop_random/0, prop_random/1, prop_targeted/0, prop_targeted/1,
         prop_parallel/0]).

-define(CACHE_SIZE, 10).

-record(state, {max = ?CACHE_SIZE, count = 0, entries = []}).

start_link(N) ->
  {ok, Pid} = gen_server:start_link(?MODULE, N, []),
  put('$server', Pid).

stop() ->
  Server = get('$server'),
  gen_server:stop(Server).

find(Key) ->
  Server = get('$server'),
  gen_server:call(Server, {find, Key}).

cache(Key, Val) ->
  Server = get('$server'),
  gen_server:call(Server, {cache, Key, Val}).

flush() ->
  Server = get('$server'),
  gen_server:call(Server, {flush}).

init(N) ->
  Table = ets:new(cache, [public]),
  % ets:insert(Table, {count, 0, N-1}),  %% erroneous initialization
  ets:insert(Table, {count, 0, N}),    %% correct initialization
  {ok, Table}.

handle_call({find, Key}, _From, Table) ->
  case ets:match(Table, {'_', {Key, '$1'}}) of
    [[Val]] -> {reply, {ok, Val}, Table};
    [] -> {reply, {error, not_found}, Table}
  end;
handle_call({cache, Key, Val}, _From, Table) ->
  case ets:match(Table, {'$1', {Key, '_'}}) of % find dupes
    [[N]] ->
      {reply, ets:insert(Table, {N,{Key,Val}}), Table};    % overwrite dupe
    [] ->
      %% erlang:yield(),  %% to expose bugs during parallel testing
      case ets:lookup(Table, count) of     % insert new
        [{count,Max,Max}] ->
          {reply, ets:insert(Table, [{1,{Key,Val}}, {count,1,Max}]), Table};
        [{count,Current,Max}] ->
          {reply, ets:insert(Table, [{Current+1,{Key,Val}},
                                     {count,Current+1,Max}]), Table}
      end
  end;
handle_call({flush}, _From, Table) ->
  [{count,_,Max}] = ets:lookup(Table, count),
  ets:delete_all_objects(Table),
  %% erlang:yield(),          %% to expose bugs during parallel testing
  {reply, ets:insert(Table, {count, 0, Max}), Table};
handle_call(_Call, _From, State) -> {noreply, State}.

handle_cast(_Cast, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

prop_random() ->  % called prop_test in the book
  ?FORALL(Cmds, commands(?MODULE),
          begin
            start_link(?CACHE_SIZE),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            stop(),
            ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                [History,State,Result]),
                      aggregate(command_names(Cmds), Result =:= ok))
          end).

prop_random(Size) ->  % called prop_test in the book
  ?FORALL(Cmds, commands(?MODULE, #state{max = Size}),
          begin
            start_link(Size),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            stop(),
            ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                [History,State,Result]),
                      aggregate(command_names(Cmds), Result =:= ok))
          end).

prop_targeted() ->
  ?FORALL_TARGETED(Cmds, commands(?MODULE),
                   begin
                     start_link(?CACHE_SIZE),
                     {History, State, Result} = run_commands(?MODULE, Cmds),
                     stop(),
                     #state{count = Count} = State,
                     ?MAXIMIZE(Count),
                     ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                         [History,State,Result]),
                               aggregate(command_names(Cmds), Result =:= ok))
                   end).

prop_targeted(Size) ->
  ?FORALL_TARGETED(Cmds, commands(?MODULE, #state{max = Size}),
                   begin
                     start_link(Size),
                     {History, State, Result} = run_commands(?MODULE, Cmds),
                     stop(),
                     #state{count = Count} = State,
                     ?MAXIMIZE(Count),
                     ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                         [History,State,Result]),
                               aggregate(command_names(Cmds), Result =:= ok))
                   end).

prop_parallel() ->
  ?FORALL(Cmds, parallel_commands(?MODULE),
          begin
            start_link(?CACHE_SIZE),
            {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
            stop(),
            ?WHENFAIL(io:format("=======~n"
                                "Failing command sequence:~n~w~n"
                                "At state: ~w~n"
                                "=======~n"
                                "Result: ~w~n"
                                "History: ~w~n",
                                [Cmds,State,Result,History]),
                      aggregate(command_names(Cmds), Result =:= ok))
          end).

%% Initial model value at system start. Should be deterministic.
initial_state() ->
  #state{}.

command(State) ->
  oneof([{call, cache_statem, find, [key(State)]},
         {call, cache_statem, cache, [key(State), val()]},
         {call, cache_statem, flush, []}]).

key(#state{max = Max}) ->
  oneof([range(1, Max), % reusable keys, raising chance of dupes
         integer()]).   % random keys

val() ->
  integer().

%% Picks whether a command should be valid under the current state.
precondition(#state{count = 0}, {call, cache_statem, flush, []}) ->
  false; % don't flush an empty cache for no reason
precondition(#state{}, {call, _Mod, _Fun, _Args}) ->
  true.

%% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}',
%% determine whether the result `Res' (coming from the actual system)
%% makes sense.
postcondition(#state{entries = L}, {call, cache_statem, find, [Key]}, Res) ->
  case lists:keyfind(Key, 1, L) of
    false      -> Res =:= {error, not_found};
    {Key, Val} -> Res =:= {ok, Val}
  end;
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
  true.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _, {call, cache_statem, flush, _}) ->
  State#state{count = 0, entries = []};
next_state(S=#state{entries = L, count = N, max = M}, _Res,
           {call, cache_statem, cache, [K, V]}) ->
  case lists:keyfind(K, 1, L) of
    false when N =:= M -> S#state{entries = tl(L) ++ [{K,V}]};
    false when N < M   -> S#state{entries = L ++ [{K,V}], count = N+1};
    {K,_}              -> S#state{entries = lists:keyreplace(K,1,L,{K,V})}
  end;
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
  State.
