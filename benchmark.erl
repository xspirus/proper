-module(benchmark).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

-record(opts,
        {output_fun = fun io:format/2,
         long_result = false,
         numtests = 100,
         search_steps = 1000,
         search_strategy = proper_sa,
         start_size = 1,
         seed = os:timestamp(),
         max_size = 42,
         max_shrinks = 500,
         noshrink = false,
         constraint_tries = 50,
         expect_fail = false,
         any_type,
         spec_timeout = infinity,
         skip_mfas = [],
         false_positive_mfas,
         setup_funs = [],
         nocolors = false}).
-record(pass, {reason, samples, printers, performed, actions}).
-record(fail, {reason, samples, bound, actions, performed}).

%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------

args_to_string(Args) ->
  args_to_string(Args, "").

args_to_string([], Acc) ->
  Acc;
args_to_string([A | Args], "") ->
  args_to_string(Args, io_lib:format("~w", [A]));
args_to_string([A | Args], Acc) ->
  args_to_string(Args, io_lib:format("~s, ~w", [Acc, A])).

mfa_to_string(M, F, A) ->
  io_lib:format("~s:~s(~s)", [M, F, args_to_string(A)]).

mfa_to_filename(M, F, _A) ->
  io_lib:format("~s_~s", [M, F]).

list_to_csv(L) ->
  list_to_csv(L, "").

list_to_csv([], Acc) ->
  Acc;
list_to_csv([H | T], "") when is_atom(H) ->
  list_to_csv(T, io_lib:format("~s", [H]));
list_to_csv([H | T], "") ->
  case io_lib:char_list(H) of
    true ->
      list_to_csv(T, io_lib:format("~s", [H]));
    false ->
      list_to_csv(T, io_lib:format("~w", [H]))
  end;
list_to_csv([H | T], Acc) when is_atom(H) ->
  list_to_csv(T, io_lib:format("~s,~s", [Acc, H]));
list_to_csv([H | T], Acc) ->
  case io_lib:char_list(H) of
    true ->
      list_to_csv(T, io_lib:format("~s,~s", [Acc, H]));
    false ->
      list_to_csv(T, io_lib:format("~s,~w", [Acc, H]))
  end.

int_to_string(I) ->
  io_lib:write(I).

transpose([[X | Xs] | Xss]) ->
  [[X | [H || [H | _] <- Xss]] | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) ->
  transpose(Xss);
transpose([]) ->
  [].

pexecute(Parent, Fn, Arg) ->
  Parent ! {self(), catch Fn(Arg)}.

pgather(Pids) ->
  pgather(Pids, []).

pgather([], Acc) ->
  lists:reverse(Acc);
pgather([Pid | Pids], Acc) ->
  receive
    {Pid, Res} ->
      pgather(Pids, [Res | Acc])
  end.

pmap(Fn, List) ->
  Parent = self(),
  Schedulers = erlang:system_info(schedulers) div 2,
  Processes = min(length(List), Schedulers),
  Parts = split(List, Processes),
  Pids =
    [spawn(fun() -> pexecute(Parent, fun(L) -> lists:map(Fn, L) end, SubList)
           end)
     || SubList <- Parts],
  lists:flatten(pgather(Pids)).

split(List, Max) ->
  L = length(List),
  case L rem Max of
    0 ->
      split(List, L div Max, []);
    _ ->
      split(List, L div Max + 1, [])
  end.

split([], _N, Acc) ->
  lists:reverse(Acc);
split(List, N, Acc) when length(List) =< N ->
  split([], N, [List | Acc]);
split(List, N, Acc) ->
  {Part, NewList} = lists:split(N, List),
  split(NewList, N, [Part | Acc]).

%% -----------------------------------------------------------------------------
%% PropEr overrides
%% -----------------------------------------------------------------------------

samplescheck(OuterTest) ->
  samplescheck(OuterTest, []).

samplescheck(OuterTest, UserOpts) ->
  try proper:parse_opts(UserOpts) of
    ImmOpts ->
      {Test, Opts} = proper:peel_test(OuterTest, ImmOpts),
      samplestest({test, Test}, Opts)
  catch
    {_Err, _Opt} = Reason ->
      {error, Reason}
  end.

samplestest(RawTest, Opts) ->
  proper:global_state_init(Opts),
  Finalizers = proper:setup_test(Opts),
  Result = inner_test(RawTest, Opts),
  ok = proper:finalize_test(Finalizers),
  proper:global_state_erase(),
  case Result of
    #pass{samples = [Sample]} ->
      {true, Sample};
    #fail{samples = [Sample]} ->
      {false, Sample}
  end.

passfailcheck(OuterTest) ->
  passfailcheck(OuterTest, []).

passfailcheck(OuterTest, UserOpts) ->
  try proper:parse_opts(UserOpts) of
    ImmOpts ->
      {Test, Opts} = proper:peel_test(OuterTest, ImmOpts),
      passfailtest({test, Test}, Opts)
  catch
    {_Err, _Opt} = Reason ->
      {error, Reason}
  end.

passfailtest(RawTest, Opts) ->
  proper:global_state_init(Opts),
  Finalizers = proper:setup_test(Opts),
  Result = inner_test(RawTest, Opts),
  ok = proper:finalize_test(Finalizers),
  proper:global_state_erase(),
  case Result of
    #pass{performed = Performed} ->
      {true, Performed};
    #fail{performed = Performed} ->
      {false, Performed}
  end.

inner_test(RawTest, Opts) ->
  #opts{numtests = Numtests} = Opts,
  Test = proper:cook_test(RawTest, Opts),
  proper:perform(Numtests, Test, Opts).

allcheck(OuterTest) ->
  allcheck(OuterTest, []).

allcheck(OuterTest, UserOpts) ->
  try proper:parse_opts(UserOpts) of
    ImmOpts ->
      {Test, Opts} = proper:peel_test(OuterTest, ImmOpts),
      alltest({test, Test}, Opts)
  catch
    {_Err, _Opt} = Reason ->
      {error, Reason}
  end.

alltest(RawTest, Opts) ->
  proper:global_state_init(Opts),
  Finalizers = proper:setup_test(Opts),
  Result = inner_test(RawTest, Opts),
  ok = proper:finalize_test(Finalizers),
  proper:global_state_erase(),
  case Result of
    #pass{performed = Performed, samples = [Sample]} ->
      {true, Performed, Sample};
    #fail{performed = Performed, samples = [Sample]} ->
      {false, Performed, Sample}
  end.

%% -----------------------------------------------------------------------------
%% Stats gatherers
%% -----------------------------------------------------------------------------

perform_samples_tests(Num, Test, Opts) ->
  Results =
    lists:map(fun(_) ->
                 {true, Sample} = samplescheck(Test, Opts),
                 io:format("."),
                 Sample
              end,
              lists:seq(1, Num)),
  io:format("~n"),
  Results.

count_and_remove_commands(_X, Count, [], Acc) ->
  {Count, lists:reverse(Acc)};
count_and_remove_commands(X, Count, [X | Commands], Acc) ->
  count_and_remove_commands(X, Count + 1, Commands, Acc);
count_and_remove_commands(X, Count, [Y | Commands], Acc) ->
  count_and_remove_commands(X, Count, Commands, [Y | Acc]).

count_commands(Commands) ->
  count_commands(Commands, []).

count_commands([], Counts) ->
  Counts;
count_commands([Command | Commands], Counts) ->
  {Count, Rest} = count_and_remove_commands(Command, 1, Commands, []),
  count_commands(Rest, [{Count, element(2, Command)} | Counts]).

get_commands(Counts) ->
  lists:map(fun({_Count, Cmd}) -> Cmd end, Counts).

flatten_stats(Stats) ->
  flatten_stats(Stats, []).

flatten_stats([], Acc) ->
  lists:reverse(Acc);
flatten_stats([{Num, Stats} | Rest], Acc) ->
  NumStats = [{Num, Stat} || Stat <- Stats],
  flatten_stats(Rest, lists:flatten([NumStats | Acc])).

flatten_generic(Stats) ->
  flatten_generic(Stats, []).

flatten_generic([], Acc) ->
  lists:reverse(Acc);
flatten_generic([{Lst, Stats} | Rest], Acc) ->
  NumStats =
    [list_to_tuple(lists:append(Lst, tuple_to_list(Stat))) || Stat <- Stats],
  flatten_generic(Rest, lists:flatten([NumStats | Acc])).

perform_passfail_tests(Num, Test, Opts) ->
  Results =
    pmap(fun(_) ->
            Res = timer:tc(fun() -> passfailcheck(Test, Opts) end),
            {Time, {Passed, Performed}} = Res,
            case Passed of
              true -> io:format(".");
              false -> io:format("!")
            end,
            {Time, Passed, Performed}
         end,
         lists:seq(1, Num)),
  io:format("~n"),
  Results.

perform_allinone_tests(Num, Test, Opts) ->
  #opts{numtests = N} = proper:parse_opts(Opts),
  Results =
    pmap(fun(_) ->
            Res = timer:tc(fun() -> allcheck(Test, Opts) end),
            {Time, {Passed, Performed, Sample}} = Res,
            case Passed of
              true -> io:format(".");
              false -> io:format("!")
            end,
            {N, Time, Passed, Performed, Sample}
         end,
         lists:seq(1, Num)),
  io:format("~n"),
  Results.

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

commands_test({M, F, A}) ->
  commands_test({M, F, A}, [100]).

commands_test({M, F, A}, Nums) ->
  commands_test({M, F, A}, Nums, 100).

commands_test({M, F, A}, Nums, Tests) ->
  commands_test({M, F, A}, Nums, Tests, [quiet, noshrink]).

commands_test({M, F, A}, Nums, Tests, Opts) ->
  Test = erlang:apply(M, F, A),
  PerformTest =
    fun(Numtests) ->
       NewOpts = [{numtests, Numtests} | Opts],
       io:format("Performing ~s with ~w numtests ~w times.~n",
                 [mfa_to_string(M, F, A), Numtests, Tests]),
       perform_samples_tests(Tests, Test, NewOpts)
    end,
  Samples = lists:map(PerformTest, Nums),
  CommandsCount = [lists:map(fun count_commands/1, S) || S <- Samples],
  Stats = flatten_stats(lists:zip(Nums, CommandsCount)),
  Commands = get_commands(element(2, lists:nth(1, Stats))),
  {ok, File} =
    file:open(
      io_lib:format("~s_~s.csv", [mfa_to_filename(M, F, A), "commands"]),
      [write]),
  io:format(File, "tests,~s~n", [list_to_csv(Commands)]),
  WriteStats =
    fun({Numtests, Counts}) ->
       Cnts =
         lists:map(fun(Cmd) ->
                      {Count, Cmd} = lists:keyfind(Cmd, 2, Counts),
                      Count
                   end,
                   Commands),
       CntsCSV = lists:map(fun int_to_string/1, [Numtests | Cnts]),
       io:format(File, "~s~n", [list_to_csv(CntsCSV)])
    end,
  lists:foreach(WriteStats, Stats),
  file:close(File).

passfail_test(Args, {M, F}) ->
  passfail_test(Args, {M, F}, [100]).

passfail_test(Args, {M, F}, Nums) ->
  passfail_test(Args, {M, F}, Nums, 100).

passfail_test(Args, {M, F}, Nums, Tests) ->
  passfail_test(Args, {M, F}, Nums, Tests, [quiet, noshrink, {max_size, 50}]).

passfail_test(Args, {M, F}, Nums, Tests, Opts) ->
  ArgNames = [element(1, Arg) || Arg <- Args],
  NewArgs = transpose([element(2, Arg) || Arg <- Args]),
  Results =
    lists:flatten([passfail_test_helper({M, F, A}, Nums, Tests, Opts)
                   || A <- NewArgs]),
  {ok, File} =
    file:open(
      io_lib:format("~s_~s.csv", [mfa_to_filename(M, F, Args), "performance"]),
      [write]),
  Header = lists:flatten([tests, ArgNames, time, passed, performed]),
  io:format(File, "~s~n", [list_to_csv(Header)]),
  WriteStats =
    fun(L) -> io:format(File, "~s~n", [list_to_csv(tuple_to_list(L))]) end,
  lists:foreach(WriteStats, Results),
  file:close(File).

passfail_test_helper({M, F, A}, Nums, Tests, Opts) ->
  Test = erlang:apply(M, F, A),
  PerformTest =
    fun(Numtests) ->
       NewOpts = [{numtests, Numtests} | Opts],
       io:format("Performing ~s with ~w numtests ~w times.~n",
                 [mfa_to_string(M, F, A), Numtests, Tests]),
       {Time, Res} =
         timer:tc(fun() -> perform_passfail_tests(Tests, Test, NewOpts) end),
       io:format("Finished in ~.3f seconds.~n", [Time / 1000000]),
       Res
    end,
  PassFail = lists:map(PerformTest, Nums),
  Stats = flatten_generic(lists:zip([[N | A] || N <- Nums], PassFail)),
  Stats.

all_test({M, F, A}) ->
  all_test({M, F, A}, [100]).

all_test({M, F, A}, Nums) ->
  all_test({M, F, A}, Nums, 100).

all_test({M, F, A}, Nums, Tests) ->
  all_test({M, F, A}, Nums, Tests, [quiet, noshrink]).

all_test({M, F, A}, Nums, Tests, Opts) ->
  Test = erlang:apply(M, F, A),
  PerformTest =
    fun(Numtests) ->
       NewOpts = [{numtests, Numtests} | Opts],
       io:format("Performing ~s with ~w numtests ~w times.~n",
                 [mfa_to_filename(M, F, A), Numtests, Tests]),
       perform_allinone_tests(Tests, Test, NewOpts)
    end,
  Results =
    lists:flatten(
      lists:map(PerformTest, Nums)),
  Samples = lists:map(fun({_, _, _, _, S}) -> S end, Results),
  CommandsCount = [count_commands(S) || S <- Samples],
  Stats =
    lists:map(fun({{Num, Time, Passed, Performed, _}, Counts}) ->
                 {Num, Time, Passed, Performed, Counts}
              end,
              lists:zip(Results, CommandsCount)),
  Commands = get_commands(element(5, lists:nth(1, Stats))),
  {ok, CommandsFile} =
    file:open(
      io_lib:format("~s_~s.csv", [mfa_to_filename(M, F, A), "commands"]),
      [write]),
  io:format(CommandsFile, "tests,~s~n", [list_to_csv(Commands)]),
  WriteCommands =
    fun({N, _, _, _, Counts}) ->
       Cnts =
         lists:map(fun(Cmd) ->
                      CountCmd = lists:keyfind(Cmd, 2, Counts),
                      case CountCmd of
                        {Count, Cmd} -> Count;
                        false -> 0
                      end
                   end,
                   Commands),
       CntsCSV = lists:map(fun int_to_string/1, [N | Cnts]),
       io:format(CommandsFile, "~s~n", [list_to_csv(CntsCSV)])
    end,
  lists:foreach(WriteCommands, Stats),
  file:close(CommandsFile),
  {ok, PerformanceFile} =
    file:open(
      io_lib:format("~s_~s.csv", [mfa_to_filename(M, F, A), "performance"]),
      [write]),
  Header = [tests, time, passed, performed],
  io:format(PerformanceFile, "~s~n", [list_to_csv(Header)]),
  WriteStats =
    fun({N, Time, Passed, Performed, _}) ->
       io:format(PerformanceFile,
                 "~s~n",
                 [list_to_csv([N, Time, Passed, Performed])])
    end,
  lists:foreach(WriteStats, Stats),
  file:close(PerformanceFile),
  ok.

all_test_args(Args, {M, F}) ->
  all_test_args(Args, {M, F}, [100]).

all_test_args(Args, {M, F}, Nums) ->
  all_test_args(Args, {M, F}, Nums, 100).

all_test_args(Args, {M, F}, Nums, Tests) ->
  all_test_args(Args, {M, F}, Nums, Tests, [quiet, noshrink]).

all_test_args(Args, {M, F}, Nums, Tests, Opts) ->
  ArgNames = [element(1, Arg) || Arg <- Args],
  NewArgs = transpose([element(2, Arg) || Arg <- Args]),
  Stats =
    lists:flatten([all_test_args_helper({M, F, A}, Nums, Tests, Opts)
                   || A <- NewArgs]),
  Commands = get_commands(element(6, lists:nth(1, lists:reverse(Stats)))),
  {ok, CommandsFile} =
    file:open(
      io_lib:format("~s_~s.csv", [mfa_to_filename(M, F, []), "commands"]),
      [write]),
  io:format(CommandsFile, "tests,~s~n", [list_to_csv(Commands)]),
  WriteCommands =
    fun({N, _, _, _, _, Counts}) ->
       Cnts =
         lists:map(fun(Cmd) ->
                      CountCmd = lists:keyfind(Cmd, 2, Counts),
                      case CountCmd of
                        {Count, Cmd} -> Count;
                        false -> 0
                      end
                   end,
                   Commands),
       CntsCSV = lists:map(fun int_to_string/1, [N | Cnts]),
       io:format(CommandsFile, "~s~n", [list_to_csv(CntsCSV)])
    end,
  lists:foreach(WriteCommands, Stats),
  file:close(CommandsFile),
  {ok, PerformanceFile} =
    file:open(
      io_lib:format("~s_~s.csv", [mfa_to_filename(M, F, []), "performance"]),
      [write]),
  Header = lists:flatten([tests, ArgNames, time, passed, performed]),
  io:format(PerformanceFile, "~s~n", [list_to_csv(Header)]),
  WriteStats =
    fun({N, A, Time, Passed, Performed, _}) ->
       io:format(PerformanceFile,
                 "~s~n",
                 [list_to_csv(lists:flatten([N, A, Time, Passed, Performed]))])
    end,
  lists:foreach(WriteStats, Stats),
  file:close(PerformanceFile),
  ok.

all_test_args_helper({M, F, A}, Nums, Tests, Opts) ->
  Test = erlang:apply(M, F, A),
  [Size | _] = A,
  MaxSize = max(42, 2 * Size + 10),
  PerformTest =
    fun(Numtests) ->
       NewOpts = [{numtests, Numtests}, {max_size, MaxSize} | Opts],
       io:format("~w~n", [NewOpts]),
       io:format("Performing ~s with ~w numtests ~w times.~n",
                 [mfa_to_string(M, F, A), Numtests, Tests]),
       lists:map(fun({N, Time, Passed, Performed, Samples}) ->
                    {N, A, Time, Passed, Performed, Samples}
                 end,
                 perform_allinone_tests(Tests, Test, NewOpts))
    end,
  Results =
    lists:flatten(
      lists:map(PerformTest, Nums)),
  Samples = lists:map(fun({_, _, _, _, _, S}) -> S end, Results),
  CommandsCount = [count_commands(S) || S <- Samples],
  lists:map(fun({{Num, Args, Time, Passed, Performed, _}, Counts}) ->
               {Num, Args, Time, Passed, Performed, Counts}
            end,
            lists:zip(Results, CommandsCount)).

%% -----------------------------------------------------------------------------
%% Benchmarks
%% -----------------------------------------------------------------------------

benchmark_cache_statem_random() ->
  passfail_test([{size, lists:seq(5, 40, 5)}],
                {cache_statem, prop_random},
                [100, 200, 500, 1000, 2000, 5000, 10000],
                1000).

benchmark_cache_statem_targeted() ->
  passfail_test([{size, lists:seq(5, 40, 5)}],
                {cache_statem, prop_targeted},
                [100, 200, 500, 1000, 2000, 5000, 10000],
                1000).

benchmark_labyrinth_4commands_1() ->
  all_test({labyrinth_statem, prop_targeted, [labyrinth_statem:maze(1)]},
           [20, 100, 200, 400, 600, 800, 1000, 1500, 2000],
           1000).

benchmark_labyrinth_4commands_2() ->
  all_test({labyrinth_statem, prop_targeted, [labyrinth_statem:maze(3)]},
           [20, 100, 200, 400, 600, 800, 1000, 1500, 2000],
           1000).

benchmark_labyrinth_8commands_1() ->
  all_test({labyrinth_statem_more,
            prop_targeted,
            [labyrinth_statem_more:maze(1)]},
           [20, 100, 200, 400, 600, 800, 1000, 1500, 2000],
           1000).

benchmark_labyrinth_8commands_2() ->
  all_test({labyrinth_statem_more,
            prop_targeted,
            [labyrinth_statem_more:maze(3)]},
           [20, 100, 200, 400, 600, 800, 1000, 1500, 2000],
           1000).

benchmark_dfa_random() ->
  all_test_args([{size, lists:seq(5, 60, 5)}],
                {dfa, prop_random},
                [100, 500, 1000, 2000],
                1000).

benchmark_dfa_targeted() ->
  all_test_args([{size, lists:seq(5, 60, 5)}],
                {dfa, prop_targeted},
                [100, 500, 1000, 2000],
                1000).
