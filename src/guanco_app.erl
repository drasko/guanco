%%%-------------------------------------------------------------------
%% @doc guanco public API
%% @end
%%%-------------------------------------------------------------------

-module(guanco_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Ensure Hackney and its dependencies are started
    {ok, _} = application:ensure_all_started(hackney),

    %% Start the supervisor which will start the workers
    guanco_sup:start_link().

stop(_State) ->
    %% Cleanup if necessary
    ok.

%% internal functions
