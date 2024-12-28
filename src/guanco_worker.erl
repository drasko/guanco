-module(guanco_worker).
-behaviour(gen_server).

%% API
-export([start_link/0, generate_completion/3, generate_chat_completion/3, show_model_info/1, generate_embeddings/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Start the gen_server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initialize state
init([]) ->
    case ets:lookup(ollama_api_url, url) of
        [{url, BaseUrl}] ->   %% Use "BaseUrl" as before
            {ok, #{base_url => BaseUrl}};  %% Store the BaseUrl in the state
        [] ->
            {stop, {error, missing_url_in_ets}}
    end.

%% Generate a completion
generate_completion(ModelName, Prompt, OptParams) ->
    gen_server:call(?MODULE, {generate_completion, ModelName, Prompt, OptParams}, 30000).

%% Generate chat completion
generate_chat_completion(ModelName, Messages, OptParams) ->
    gen_server:call(?MODULE, {generate_chat_completion, ModelName, Messages, OptParams}, 30000).

%% Show model information
show_model_info(ModelName) ->
    gen_server:call(?MODULE, {show_model_info, ModelName}).

%% Generate embeddings
generate_embeddings(ModelName, InputText) ->
    gen_server:call(?MODULE, {generate_embeddings, ModelName, InputText}).

%% Handle gen_server calls
handle_call({generate_completion, ModelName, Prompt, OptParams}, _From, State) ->
    BaseBody = #{model => ModelName, prompt => Prompt},
    OptionalParams = [suffix, images, format, options, system, template, stream, raw, keep_alive, context],
    Stream = maps:get(stream, OptParams, false),
    BodyWithOptionalParams = lists:foldl(
        fun(Param, Acc) ->
            case maps:find(Param, OptParams) of
                {ok, Value} -> maps:put(Param, Value, Acc);
                error -> Acc
            end
        end,
        BaseBody,
        OptionalParams
    ),
    BodyEncoded = jiffy:encode(BodyWithOptionalParams),
    logger:info("Generated completion request: ~p", [BodyWithOptionalParams]),
    BaseUrl = maps:get(base_url, State),
    Url = BaseUrl ++ "/api/generate", %% Calculate the full URL
    Result = call_ollama_api(Url, post, [], BodyEncoded, Stream),
    {reply, Result, State}; %% Return the full state

handle_call({generate_chat_completion, ModelName, Messages, OptParams}, _From, State) ->
    BaseBody = #{model => ModelName, messages => Messages},
    OptionalParams = [tools, format, options, stream, keep_alive],
    Stream = maps:get(stream, OptParams, false),
    BodyWithOptionalParams = lists:foldl(
        fun(Param, Acc) ->
            case maps:find(Param, OptParams) of
                {ok, Value} -> maps:put(Param, Value, Acc);
                error -> Acc
            end
        end,
        BaseBody,
        OptionalParams
    ),
    BodyEncoded = jiffy:encode(BodyWithOptionalParams),
    logger:info("Generated chat completion request: ~p", [BodyWithOptionalParams]),
    BaseUrl = maps:get(base_url, State),
    Url = BaseUrl ++ "/api/chat", %% Calculate the full URL
    Result = call_ollama_api(Url, post, [], BodyEncoded, Stream),
    {reply, Result, State}; %% Return the full state

handle_call({show_model_info, ModelName}, _From, State) ->
    %% Extract BaseUrl from the state
    BaseUrl = maps:get(base_url, State),

    Body = jiffy:encode(#{model => ModelName}),
    logger:info("Fetching model info for: ~p", [ModelName]),
    Url = BaseUrl ++ "/api/show", %% Calculate the full URL
    Result = call_ollama_api(Url, post, [], Body, false),
    {reply, Result, State}; %% Return the full state

handle_call({generate_embeddings, ModelName, InputText}, _From, State) ->
    %% Extract BaseUrl from the state
    BaseUrl = maps:get(base_url, State),

    Body = #{model => ModelName, input => InputText},
    BodyEncoded = jiffy:encode(Body),
    logger:info("Generating embeddings for: ~p", [InputText]),
    Url = BaseUrl ++ "/api/embed", %% Calculate the full URL
    Result = call_ollama_api(Url, post, [], BodyEncoded, false),
    {reply, Result, State}; %% Return the full state

handle_call({get_state}, _From, State) ->
    {reply, State, State}; %% Return the full state

handle_call(_, _From, State) ->
    {reply, {error, unknown_request}, State}. %% Return the full state

%% Handle gen_server casts
handle_cast(_, State) ->
    {noreply, State}.

%% Call Ollama API
call_ollama_api(Url, Method, Headers, Body, Stream) ->
    %% Log the method, URL, and request body
    io:format("Making HTTP request to URL: ~p with method: ~p~n", [Url, Method]),
    io:format("Request headers: ~p~n", [Headers]),
    io:format("Request body: ~p~n", [Body]),
    
    Options = [
        {connect_timeout, 5000}, %% Connection timeout in milliseconds
        {recv_timeout, 30000}    %% Receive timeout in milliseconds
    ],
    %% Make the HTTP request
    case hackney:request(Method, Url, Headers, Body, Options) of
        {ok, Status, RespHeaders, ClientRef} when Status >= 200, Status < 300 ->
            io:format("HTTP request succeeded with status: ~p~n", [Status]),
            io:format("Response headers: ~p~n", [RespHeaders]),
            if
                Stream ->
                    io:format("Streaming response enabled. Handling stream...~n"),
                    handle_streaming_response(ClientRef);
                true ->
                    io:format("Non-streaming response. Reading body...~n"),
                    {ok, Response} = hackney:body(ClientRef),
                    io:format("Response body received: ~p~n", [Response]),
                    try
                        jiffy:decode(Response, [return_maps])
                    of
                        ParsedResponse ->
                            io:format("Decoded JSON response: ~p~n", [ParsedResponse]),
                            {ok, ParsedResponse}
                    catch
                        _:_ ->
                            io:format("Failed to decode JSON response.~n"),
                            {error, {invalid_json, Response}}
                    end
            end;
        {ok, Status, _RespHeaders, _ClientRef} ->
            io:format("HTTP request failed with status: ~p~n", [Status]),
            {error, {http_error, Status}};
        {error, Reason} ->
            io:format("HTTP request failed with error: ~p~n", [Reason]),
            {error, Reason}
    end.


%%% Handle streaming response
handle_streaming_response(ClientRef) ->
    io:format("Initiating streaming response handling...~n"),
    case hackney:stream_body(ClientRef) of
        {ok, Chunk} ->
            io:format("Streaming chunk received: ~p~n", [Chunk]),
            try
                Decoded = jiffy:decode(Chunk, [return_maps]),
                io:format("Decoded streaming chunk: ~p~n", [Decoded]),
                case maps:get(<<"done">>, Decoded, false) of
                    true ->
                        io:format("Streaming completed.~n"),
                        {ok, finished};
                    false ->
                        io:format("Streaming chunk processed. Awaiting next chunk...~n"),
                        handle_streaming_response(ClientRef)
                end
            catch
                _:_ ->
                    io:format("Failed to decode streaming chunk: ~p~n", [Chunk]),
                    {error, {invalid_chunk, Chunk}}
            end;
        {error, Reason} ->
            io:format("Streaming failed with error: ~p~n", [Reason]),
            {error, Reason}
    end.


%% Handle termination
terminate(_, _) ->
    ok.

%% Handle code changes
code_change(_, State, _) ->
    {ok, State}.
