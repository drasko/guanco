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
    wpool:call(guanco_worker_pool, {generate_completion, ModelName, Prompt, OptParams}).

%% Generate chat completion
generate_chat_completion(ModelName, Messages, OptParams) ->
    wpool:call(guanco_worker_pool, {generate_chat_completion, ModelName, Messages, OptParams}).

%% Show model information
show_model_info(ModelName) ->
    wpool:call(guanco_worker_pool, {show_model_info, ModelName}).

%% Generate embeddings
generate_embeddings(ModelName, InputText) ->
    wpool:call(guanco_worker_pool, {generate_embeddings, ModelName, InputText}).
