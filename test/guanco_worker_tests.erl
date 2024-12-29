-module(guanco_worker_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test initialization with environment variable
init_with_env_test() ->
    application:set_env(guanco, ollama_api_url, "http://env-url"),
    io:format("Set environment variable: ~p~n", [application:get_env(guanco, ollama_api_url)]),
    {ok, _} = application:ensure_all_started(guanco),
    Result = poolboy:transaction(guanco_worker_pool, fun(Pid) ->
        gen_server:call(Pid, {get_state})
    end),
    {ok, State} = Result,
    io:format("State received: ~p~n", [State]),
    BaseUrl = maps:get(base_url, State),
    io:format("Base URL in state: ~p~n", [BaseUrl]),
    ?assertEqual("http://env-url", BaseUrl),
    application:unset_env(guanco, ollama_api_url).

%% Test initialization with default value from app.src
init_with_default_test() ->
    application:stop(guanco),
    application:start(guanco),
    Result = poolboy:transaction(guanco_worker_pool, fun(Pid) ->
        gen_server:call(Pid, {get_state})
    end),
    {ok, State} = Result,
    io:format("State received: ~p~n", [State]),
    BaseUrl = maps:get(base_url, State),
    io:format("Base URL in state: ~p~n", [BaseUrl]),
    ?assertEqual("http://localhost:11434", BaseUrl).

%% Test generate_completion
generate_completion_test() ->
    meck:new(guanco_worker, [passthrough]),
    {ok, _} = application:ensure_all_started(guanco),
    MockResponse = #{result => <<"completion">>},
    meck:expect(guanco_worker, call_ollama_api, fun(_, _, _, _, _) -> {ok, MockResponse} end),
    Result = guanco_app:generate_completion(mistral, <<"This is the test">>, #{}),
    ?assertEqual({ok, MockResponse}, Result),
    meck:unload(guanco_worker).

%% Test generate_chat_completion
generate_chat_completion_test() ->
    meck:new(guanco_worker, [passthrough]),
    {ok, _} = application:ensure_all_started(guanco),
    MockResponse = #{result => <<"chat_completion">>},
    meck:expect(guanco_worker, call_ollama_api, fun(_, _, _, _, _) -> {ok, MockResponse} end),
    Messages = [#{role => <<"user">>, content => <<"Some text">>}],
    Result = guanco_app:generate_chat_completion(mistral, Messages, #{}),
    ?assertEqual({ok, MockResponse}, Result),
    meck:unload(guanco_worker).

%% Test show_model_info
show_model_info_test() ->
    meck:new(guanco_worker, [passthrough]),
    {ok, _} = application:ensure_all_started(guanco),
    MockResponse = #{result => <<"model_info">>},
    meck:expect(guanco_worker, call_ollama_api, fun(_, _, _, _, _) -> {ok, MockResponse} end),
    Result = guanco_app:show_model_info(mistral),
    ?assertEqual({ok, MockResponse}, Result),
    meck:unload(guanco_worker).

%% Test generate_embeddings
generate_embeddings_test() ->
    meck:new(guanco_worker, [passthrough]),
    {ok, _} = application:ensure_all_started(guanco),
    MockResponse = #{result => <<"embeddings">>},
    meck:expect(guanco_worker, call_ollama_api, fun(_, _, _, _, _) -> {ok, MockResponse} end),
    Result = guanco_app:generate_embeddings(mistral, <<"Some text">>),
    ?assertEqual({ok, MockResponse}, Result),
    meck:unload(guanco_worker).