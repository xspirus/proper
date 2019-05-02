%%%-------------------------------------------------------------------
%% @doc statem_tests public API
%% @end
%%%-------------------------------------------------------------------

-module(statem_tests_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    statem_tests_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
