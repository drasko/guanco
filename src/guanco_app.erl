%%%-------------------------------------------------------------------
%% @doc guanco public API
%% @end
%%%-------------------------------------------------------------------

-module(guanco_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start Hackney if not already started
    case application:ensure_all_started(hackney) of
        {ok, _} -> ok;
        {error, _} -> 
            {ok, _} = application:start(hackney)
    end,

    %% Read the host and port from the Erlang application environment using proplists
    case application:get_env(guanco, ollama) of
        {ok, OllamaConfig} ->
            Host = proplists:get_value(host, OllamaConfig, "localhost"),
            Port = proplists:get_value(port, OllamaConfig, 11434),
            ApiUrl = lists:concat([Host, ":", integer_to_list(Port)]);
        _ ->
            %% Default URL if the configuration is not found
            ApiUrl = "http://localhost:11434"
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
