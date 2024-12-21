%%%-------------------------------------------------------------------
%% @doc guanco public API
%% @end
%%%-------------------------------------------------------------------

-module(guanco_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Read the environment variable for the base URL and store it in ETS
    case os:getenv("OLLAMA_API_URL") of
        undefined -> 
            %% Default URL if the ENV var is not set
            ApiUrl = "http://localhost:11434";
        Url -> 
            ApiUrl = Url
    end,

    %% Initialize ETS table to store the base URL
    case ets:info(ollama_api_url) of
        undefined -> 
            ets:new(ollama_api_url, [named_table, public, set]),
            ets:insert(ollama_api_url, {url, ApiUrl});
        _ -> ok
    end,

    %% Start the supervisor which will start the workers
    guanco_sup:start_link().

stop(_State) ->
    %% Cleanup if necessary
    ok.

%% internal functions
