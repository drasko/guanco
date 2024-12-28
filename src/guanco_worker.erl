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
    %% Retrieve the base URL from ETS
    case ets:lookup(ollama_api_url, url) of
        [{url, BaseUrl}] -> 
            {ok, #{url => BaseUrl}};
        [] -> 
            {stop, {error, missing_url_in_ets}}
    end.

%% Generate a completion
-spec generate_completion(atom(), binary(), map()) -> any().
generate_completion(ModelName, Prompt, OptParams) ->
    %% Initialize the base request body with required parameters
    BaseBody = #{
        model => ModelName,
        prompt => Prompt
    },

    %% List of optional parameters to check and add if present in OptParams
    OptionalParams = [suffix, images, format, options, system, template, stream, raw, keep_alive, context],

    %% Check if the stream flag is present in OptParams
    Stream = case maps:find(stream, OptParams) of
                {ok, StreamValue} -> StreamValue;  %% Use a different name for this case
                error -> false
            end,

    %% Add optional parameters to the base body only if they exist in OptParams
    BodyWithOptionalParams = lists:foldl(fun(Param, Acc) ->
        case maps:find(Param, OptParams) of
            {ok, ParamValue} -> maps:put(Param, ParamValue, Acc);  %% Use a different name for this case
            error -> Acc  %% Keep body unchanged if param is not found
        end
    end, BaseBody, OptionalParams),


    %% Log the request body after adding optional parameters
    io:format("Request Body with Optional Parameters: ~p~n", [BodyWithOptionalParams]),

    %% Encode the body to JSON format
    BodyEncoded = jiffy:encode(BodyWithOptionalParams),

    %% Log Encoded Body
    io:format("Encoded Body: ~p~n", [BodyEncoded]),

    %% Call the Ollama API with the stream flag as part of the request
    call_ollama_api("/api/generate", post, [], BodyEncoded, Stream).

%% Generate chat completion
-spec generate_chat_completion(atom(), list(), map()) -> any().
generate_chat_completion(ModelName, Messages, OptParams) ->
    %% Initialize the base request body with required parameters
    BaseBody = #{
        model => ModelName,
        messages => Messages
    },

    %% List of optional parameters to check and add if present in OptParams
    OptionalParams = [tools, format, options, stream, keep_alive],

    %% Check if the stream flag is present in OptParams
    Stream = case maps:find(stream, OptParams) of
                {ok, StreamValue} -> StreamValue;  %% Use a different name for this case
                error -> false
            end,

    %% Add optional parameters to the base body only if they exist in OptParams
    BodyWithOptionalParams = lists:foldl(fun(Param, Acc) ->
        case maps:find(Param, OptParams) of
            {ok, ParamValue} -> maps:put(Param, ParamValue, Acc);  %% Use a different name for this case
            error -> Acc  %% Keep body unchanged if param is not found
        end
    end, BaseBody, OptionalParams),


    %% Log the request body after adding optional parameters
    io:format("Request Body with Optional Parameters: ~p~n", [BodyWithOptionalParams]),

    %% Encode the body to JSON format
    BodyEncoded = jiffy:encode(BodyWithOptionalParams),

    %% Log Encoded Body
    io:format("Encoded Body: ~p~n", [BodyEncoded]),

    %% Call the Ollama API with the stream flag as part of the request
    call_ollama_api("/api/chat", post, [], BodyEncoded, Stream).

%% Show model information
-spec show_model_info(atom()) -> any().
show_model_info(ModelName) ->
    Body = jiffy:encode(#{model => ModelName}),
    call_ollama_api("/api/show", post, [], Body, false).

%% Generate embeddings
-spec generate_embeddings(atom(), binary()) -> any().
generate_embeddings(ModelName, InputText) ->
    Body = #{
        model => ModelName,
        input => InputText
    },
    BodyEncoded = jiffy:encode(Body),
    call_ollama_api("/api/embed", post, [], BodyEncoded, false).

%% Call Ollama API
-spec call_ollama_api(binary(), atom(), list(), binary(), boolean()) -> any().
call_ollama_api(Endpoint, Method, Headers, Body, Stream) ->
    %% Retrieve the state and base URL
    State = gen_server:call(?MODULE, {get_state}),
    BaseUrl = maps:get(url, State),
    URL = BaseUrl ++ Endpoint,

    io:format("Making HTTP request to URL: ~p with method: ~p~n", [URL, Method]),

    %% Set timeout options
    Options = [
        {connect_timeout, 5000}, %% Timeout for connecting to the server (5 seconds)
        {recv_timeout, 30000}    %% Timeout for receiving a response (30 seconds)
    ],

    %% Make the request
    case hackney:request(Method, URL, Headers, Body, Options) of
        {ok, Status, _RespHeaders, ClientRef} when Status >= 200, Status < 300 ->
            io:format("Request successful with Status: ~p~n", [Status]),
            if
                Stream ->
                    io:format("Stream flag is true, handling streaming response...~n"),
                    handle_streaming_response(ClientRef);
                true ->
                    io:format("Stream flag is false, processing non-streaming response...~n"),
                    {ok, Response} = hackney:body(ClientRef),
                    io:format("Received non-streaming response body: ~p~n", [Response]),
                    parse_api_response(Response)
            end;
        {ok, Status, _RespHeaders, _ClientRef} ->
            io:format("Request failed with Status: ~p~n", [Status]),
            {error, {http_error, Status}};
        {error, Reason} ->
            io:format("Request failed with error: ~p~n", [Reason]),
            {error, Reason}
    end.


%% Parse API response
-spec parse_api_response(binary()) -> any().
parse_api_response(Response) ->
    try
        ParsedResponse = jiffy:decode(Response, [return_maps]),
        {ok, ParsedResponse}
    catch
        _:_ -> 
            {error, {invalid_json, Response}}
    end.


%% Handle streaming response
-spec handle_streaming_response(reference()) -> any().
handle_streaming_response(ClientRef) ->
    %% Stream the response body chunk by chunk
    io:format("Start streaming from client: ~p~n", [ClientRef]),
    case hackney:stream_body(ClientRef) of
        {ok, Chunk} ->
            %% Log received chunk
            io:format("Received chunk: ~p~n", [Chunk]),

            %% Decode the chunk into a map
            case jiffy:decode(Chunk, [return_maps]) of
                {ok, Map} ->
                    %% Log successful decoding of JSON chunk
                    io:format("Successfully decoded chunk to Map: ~p~n", [Map]),

                    %% Extract the "done" field, defaulting to false
                    Done = maps:get(<<"done">>, Map, false),
                    io:format("Done: ~p~n", [Done]),

                    %% Process the "done" field and handle the response
                    case Done of
                        true ->
                            %% If "done" is true, the response is finished
                            io:format("Streaming completed (done = true)~n"),
                            ok;  %% Handle completion here (e.g., final processing)
                        false ->
                            %% If "done" is false, continue streaming
                            io:format("Received chunk (done = false): ~p~n", [Map]),
                            io:format("Continuing to stream...~n"),
                            handle_streaming_response(ClientRef)  %% Recursive call to continue streaming
                    end;

                {error, Reason} ->
                    %% Failed to decode chunk as JSON
                    io:format("Failed to parse chunk: ~p~n", [Chunk]),
                    io:format("Error decoding chunk: ~p~n", [Reason]),
                    {error, invalid_json}
            end;
        {error, Reason} ->
            %% Handle any errors that occur during streaming
            io:format("Streaming failed with error: ~p~n", [Reason]),
            {error, Reason}
    end.


%% Handle gen_server calls
handle_call({get_state}, _From, State) ->
    {reply, State, State};
handle_call(_, _From, State) ->
    {noreply, State}.

%% Handle gen_server casts
handle_cast(_, State) ->
    {noreply, State}.

%% Handle termination
terminate(_, _) ->
    ok.

%% Handle code changes
code_change(_, State, _) ->
    {ok, State}.
