-module(guanco_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, generate_chat_completion/3, show_model_info/1, generate_embeddings/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Start the gen_server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initialize state (read from ETS)
init([]) ->
    %% Retrieve the base URL from ETS
    case ets:lookup(ollama_api_url, url) of
        [{url, BaseUrl}] -> 
            {ok, #{url => BaseUrl}};
        [] -> 
            {stop, {error, missing_url_in_ets}}
    end.

%% Generate Chat Completion function
generate_chat_completion(ModelName, Messages, Options) ->
    %% Endpoint and method for chat completion
    Endpoint = "/api/chat/completion",
    Method = post,

    %% Prepare the request body
    Body = prepare_chat_completion_body(ModelName, Messages, Options),

    %% Call the Ollama API
    call_ollama_api(Endpoint, Method, [], Body).

%% Prepare the body for the Chat Completion request
prepare_chat_completion_body(ModelName, Messages, Options) ->
    Body = #{ 
        "model" => ModelName,
        "messages" => Messages,
        "options" => Options
    },
    %% Encode the body to JSON
    jiffy:encode(Body).

%% Show Model Information
show_model_info(ModelName) ->
    %% Endpoint for model info
    Endpoint = "/api/model/" ++ ModelName,
    Method = get,

    %% Call the Ollama API
    call_ollama_api(Endpoint, Method, [], <<>>). %% No body for GET request

%% Generate Embeddings
generate_embeddings(ModelName, InputText) ->
    %% Endpoint and method for generating embeddings
    Endpoint = "/api/embeddings",
    Method = post,

    %% Prepare the request body
    Body = prepare_embeddings_body(ModelName, InputText),

    %% Call the Ollama API
    call_ollama_api(Endpoint, Method, [], Body).

%% Prepare the body for the Embedding request
prepare_embeddings_body(ModelName, InputText) ->
    Body = #{
        "model" => ModelName,
        "input" => InputText
    },
    %% Encode the body to JSON
    jiffy:encode(Body).

%% Common function to call Ollama API
call_ollama_api(Endpoint, Method, Headers, Body) ->
    %% Get the base URL from the worker state
    {ok, State} = gen_server:call(?MODULE, {get_state}),
    BaseUrl = maps:get(url, State),

    %% Build full URL
    URL = BaseUrl ++ Endpoint,

    %% Make the HTTP request using hackney
    case hackney:request(Method, URL, Headers, Body, []) of
        {ok, Status, _RespHeaders, ClientRef} when Status >= 200, Status < 300 ->
            {ok, Response} = hackney:body(ClientRef),
            parse_api_response(Response);
        {ok, Status, _RespHeaders, _ClientRef} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Generic response parser (can be reused for all APIs)
parse_api_response(Response) ->
    try
        ParsedResponse = jiffy:decode(Response, [return_maps]),
        case maps:get("status", ParsedResponse, undefined) of
            undefined -> 
                {error, {invalid_response, Response}};
            "success" ->
                {ok, maps:get("result", ParsedResponse)};
            _ ->
                {error, {api_error, ParsedResponse}}
        end
    catch
        _:_ -> 
            {error, {invalid_json, Response}}
    end.

%% Parse the response for Chat Completion
parse_chat_completion_response(Response) ->
    try
        ParsedResponse = jiffy:decode(Response, [return_maps]),
        case maps:get("status", ParsedResponse, undefined) of
            undefined -> 
                {error, {invalid_response, Response}};
            "success" ->
                {ok, maps:get("result", ParsedResponse)};
            _ ->
                {error, {api_error, ParsedResponse}}
        end
    catch
        _:_ -> 
            {error, {invalid_json, Response}}
    end.

%% Parse the response for Model Information
parse_model_info_response(Response) ->
    try
        ParsedResponse = jiffy:decode(Response, [return_maps]),
        case maps:get("status", ParsedResponse, undefined) of
            undefined -> 
                {error, {invalid_response, Response}};
            "success" ->
                {ok, maps:get("model_info", ParsedResponse)};
            _ ->
                {error, {api_error, ParsedResponse}}
        end
    catch
        _:_ -> 
            {error, {invalid_json, Response}}
    end.

%% Parse the response for Embeddings
parse_embeddings_response(Response) ->
    try
        ParsedResponse = jiffy:decode(Response, [return_maps]),
        case maps:get("status", ParsedResponse, undefined) of
            undefined -> 
                {error, {invalid_response, Response}};
            "success" ->
                {ok, maps:get("embeddings", ParsedResponse)};
            _ ->
                {error, {api_error, ParsedResponse}}
        end
    catch
        _:_ -> 
            {error, {invalid_json, Response}}
    end.

%% Get the worker state (including the base URL)
handle_call({get_state}, _From, State) ->
    {reply, State, State};
handle_call(_, _From, State) ->
    {noreply, State}.

%% Handle other gen_server callbacks
handle_cast(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
