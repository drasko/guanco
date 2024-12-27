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
    %% Define defaults for optional parameters
    DefaultOptions = #{
        suffix => <<"">>,
        images => [],
        format => <<"json">>,
        options => #{},
        system => <<"default_system">>,
        template => <<"default_template">>,
        stream => false,
        raw => false,
        keep_alive => false,
        context => <<"">>
    },
    %% Merge provided options with defaults
    Options = maps:merge(DefaultOptions, OptParams),
    Body = #{ 
        model => ModelName,
        prompt => Prompt,
        suffix => maps:get(suffix, Options),
        images => maps:get(images, Options),
        format => maps:get(format, Options),
        options => maps:get(options, Options),
        system => maps:get(system, Options),
        template => maps:get(template, Options),
        stream => maps:get(stream, Options),
        raw => maps:get(raw, Options),
        keep_alive => maps:get(keep_alive, Options),
        context => maps:get(context, Options)
    },
    BodyEncoded = jiffy:encode(Body),
    call_ollama_api("/api/generate", post, [], BodyEncoded).

%% Generate chat completion
-spec generate_chat_completion(atom(), list(), map()) -> any().
generate_chat_completion(ModelName, Messages, OptParams) ->
    %% Define defaults for optional parameters
    DefaultOptions = #{
        options => #{},
        system => <<"default_system">>,
        template => <<"default_template">>,
        stream => false,
        context => <<"">>
    },

    %% Merge provided options with defaults
    Options = maps:merge(DefaultOptions, OptParams),

    %% Extract the `options` map and other parameters from merged options
    OptionsMap = maps:get(options, Options),
    SystemMessage = maps:get(system, Options),
    Template = maps:get(template, Options),
    Stream = maps:get(stream, Options),
    Context = maps:get(context, Options),

    %% Build the request body
    Body = #{
        model => ModelName,
        messages => Messages,
        options => OptionsMap,  %% Use options directly from the merged map
        system => SystemMessage,
        template => Template,
        stream => Stream,
        context => Context
    },

    %% Encode the body to JSON format
    BodyEncoded = jiffy:encode(Body),

    %% Call the Ollama API
    call_ollama_api("/api/chat", post, [], BodyEncoded).

%% Show model information
-spec show_model_info(atom()) -> any().
show_model_info(ModelName) ->
    Body = jiffy:encode(#{model => ModelName}),
    call_ollama_api("/api/show", post, [], Body).

%% Generate embeddings
-spec generate_embeddings(atom(), binary()) -> any().
generate_embeddings(ModelName, InputText) ->
    Body = #{
        model => ModelName,
        input => InputText
    },
    BodyEncoded = jiffy:encode(Body),
    call_ollama_api("/api/embed", post, [], BodyEncoded).

%% Call Ollama API
-spec call_ollama_api(binary(), atom(), list(), binary()) -> any().
call_ollama_api(Endpoint, Method, Headers, Body) ->
    State = gen_server:call(?MODULE, {get_state}),
    BaseUrl = maps:get(url, State),
    URL = BaseUrl ++ Endpoint,

    io:format("Making HTTP request to URL: ~p with method: ~p~n", [URL, Method]),
    case hackney:request(Method, URL, Headers, Body, []) of
        {ok, Status, _RespHeaders, ClientRef} when Status >= 200, Status < 300 ->
            io:format("Request successful with Status: ~p~n", [Status]),
            {ok, Response} = hackney:body(ClientRef),
            parse_api_response(Response);
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
