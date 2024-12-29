%%%-------------------------------------------------------------------
%% @doc guanco public API
%% @end
%%%-------------------------------------------------------------------

-module(guanco_app).

-behaviour(application).

-export([start/2, stop/1, generate_completion/3, generate_chat_completion/3, show_model_info/1, generate_embeddings/2]).

start(_StartType, _StartArgs) ->
    %% Ensure Hackney and its dependencies are started
    {ok, _} = application:ensure_all_started(hackney),

    %% Start the supervisor which will start the workers
    guanco_sup:start_link().

stop(_State) ->
    %% Cleanup if necessary
    ok.

%% Generate a completion
generate_completion(ModelName, Prompt, OptParams) ->
    poolboy:transaction(guanco_worker_pool, fun(Worker) ->
        guanco_worker:generate_completion(Worker, ModelName, Prompt, OptParams)
    end).

%% Generate chat completion
generate_chat_completion(ModelName, Messages, OptParams) ->
    poolboy:transaction(guanco_worker_pool, fun(Worker) ->
        guanco_worker:generate_chat_completion(Worker, ModelName, Messages, OptParams)
    end).

%% Show model information
show_model_info(ModelName) ->
    poolboy:transaction(guanco_worker_pool, fun(Worker) ->
        guanco_worker:show_model_info(Worker, ModelName)
    end).

%% Generate embeddings
generate_embeddings(ModelName, InputText) ->
    poolboy:transaction(guanco_worker_pool, fun(Worker) ->
        guanco_worker:generate_embeddings(Worker, ModelName, InputText)
    end).
