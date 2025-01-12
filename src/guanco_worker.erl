-module(guanco_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, generate_completion/4, generate_chat_completion/4, show_model_info/2, generate_embeddings/3, call_ollama_api/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Macros
-define(GEN_SERVER_TIMEOUT, 30000).

%% Start the gen_server with arguments (required by poolboy)
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% Initialize state
init(Args) ->
    io:format("Initializing guanco_worker with args: ~p...~n", [Args]),
    case application:get_env(guanco, ollama_api_url) of
        {ok, BaseUrl} ->   %% Use "BaseUrl" from sys.config
            io:format("Base URL found: ~p~n", [BaseUrl]),
            {ok, #{base_url => BaseUrl}};
        undefined ->
            io:format("Base URL not found in config~n"),
            {stop, {error, missing_url_in_config}}
    end.

%% Generate a completion
generate_completion(Pid, ModelName, Prompt, OptParams) ->
    gen_server:call(Pid, {generate_completion, ModelName, Prompt, OptParams}, ?GEN_SERVER_TIMEOUT).

%% Generate chat completion
generate_chat_completion(Pid, ModelName, Messages, OptParams) ->
    gen_server:call(Pid, {generate_chat_completion, ModelName, Messages, OptParams}, ?GEN_SERVER_TIMEOUT).

%% Show model information
show_model_info(Pid, ModelName) ->
    gen_server:call(Pid, {show_model_info, ModelName}, ?GEN_SERVER_TIMEOUT).

%% Generate embeddings
generate_embeddings(Pid, ModelName, InputText) ->
    gen_server:call(Pid, {generate_embeddings, ModelName, InputText}, ?GEN_SERVER_TIMEOUT).

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
    BodyEncoded = json:encode(BodyWithOptionalParams),
    logger:info("Generated completion request: ~p", [BodyWithOptionalParams]),
    BaseUrl = maps:get(base_url, State),
    Url = BaseUrl ++ "/api/generate", %% Calculate the full URL
    Result = ?MODULE:call_ollama_api(Url, post, [], BodyEncoded, Stream),
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
    BodyEncoded = json:encode(BodyWithOptionalParams),
    logger:info("Generated chat completion request: ~p", [BodyWithOptionalParams]),
    BaseUrl = maps:get(base_url, State),
    Url = BaseUrl ++ "/api/chat", %% Calculate the full URL
    Result = ?MODULE:call_ollama_api(Url, post, [], BodyEncoded, Stream),
    {reply, Result, State}; %% Return the full state

handle_call({show_model_info, ModelName}, _From, State) ->
    %% Extract BaseUrl from the state
    BaseUrl = maps:get(base_url, State),

    Body = json:encode(#{model => ModelName}),
    logger:info("Fetching model info for: ~p", [ModelName]),
    Url = BaseUrl ++ "/api/show", %% Calculate the full URL
    Result = ?MODULE:call_ollama_api(Url, post, [], Body, false),
    {reply, Result, State}; %% Return the full state

handle_call({generate_embeddings, ModelName, InputText}, _From, State) ->
    %% Extract BaseUrl from the state
    BaseUrl = maps:get(base_url, State),

    Body = #{model => ModelName, input => InputText},
    BodyEncoded = json:encode(Body),
    logger:info("Generating embeddings for: ~p", [InputText]),
    Url = BaseUrl ++ "/api/embed", %% Calculate the full URL
    Result = ?MODULE:call_ollama_api(Url, post, [], BodyEncoded, false),
    {reply, Result, State}; %% Return the full state

handle_call({get_state}, _From, State) ->
    {reply, {ok, State}, State}; %% Return the full state

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
                is_pid(Stream) ->
                    io:format("Streaming response enabled. Sending chunks to PID: ~p~n", [Stream]),
                    handle_streaming_response(ClientRef, Stream);
                Stream == true ->
                    io:format("Lazy streaming response enabled.~n"),
                    {ok, fun() -> lazy_stream(ClientRef) end};
                true ->
                    io:format("Non-streaming response. Reading body...~n"),
                    handle_non_streaming_response(ClientRef)
            end;
        {ok, Status, _RespHeaders, _ClientRef} ->
            io:format("HTTP request failed with status: ~p~n", [Status]),
            {error, {http_error, Status}};
        {error, Reason} ->
            io:format("HTTP request failed with error: ~p~n", [Reason]),
            {error, Reason}
    end.

%%% Handle streaming response
handle_streaming_response(ClientRef, StreamPid) ->
    io:format("Initiating streaming response handling...~n"),
    case hackney:stream_body(ClientRef) of
        {ok, Chunk} ->
            io:format("Streaming chunk received: ~p~n", [Chunk]),
            try
                Decoded = json:decode(Chunk),
                io:format("Decoded streaming chunk: ~p~n", [Decoded]),
                case maps:get(<<"done">>, Decoded, false) of
                    true ->
                        io:format("Streaming completed.~n"),
                        StreamPid ! {stream_finished, Decoded},
                        {ok, finished};
                    false ->
                        io:format("Streaming chunk processed. Sending to PID: ~p~n", [StreamPid]),
                        StreamPid ! {stream_chunk, Decoded},
                        handle_streaming_response(ClientRef, StreamPid)
                end
            catch
                _:_ ->
                    io:format("Failed to decode streaming chunk: ~p~n", [Chunk]),
                    StreamPid ! {stream_error, {invalid_chunk, Chunk}},
                    {error, {invalid_chunk, Chunk}}
            end;
        {error, Reason} ->
            io:format("Streaming failed with error: ~p~n", [Reason]),
            StreamPid ! {stream_error, Reason},
            {error, Reason}
    end.

%%% Handle non-streaming response
handle_non_streaming_response(ClientRef) ->
    {ok, Response} = hackney:body(ClientRef),
    io:format("Response body received: ~p~n", [Response]),
    try
        json:decode(Response)
    of
        ParsedResponse ->
            io:format("Decoded JSON response: ~p~n", [ParsedResponse]),
            {ok, ParsedResponse}
    catch
        _:_ ->
            io:format("Failed to decode JSON response.~n"),
            {error, {invalid_json, Response}}
    end.

%%% Lazy stream generator
lazy_stream(ClientRef) ->
    case hackney:stream_body(ClientRef) of
        {ok, Chunk} ->
            try
                Decoded = json:decode(Chunk),
                case maps:get(<<"done">>, Decoded, false) of
                    true ->
                        {done, Decoded};
                    false ->
                        {cont, Decoded, fun() -> lazy_stream(ClientRef) end}
                end
            catch
                _:_ ->
                    {error, {invalid_chunk, Chunk}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Handle termination
terminate(_, _) ->
    ok.

%% Handle code changes
code_change(_, State, _) ->
    {ok, State}.
