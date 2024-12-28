-module(guanco_worker_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper function to ensure the application is stopped
ensure_application_stopped() ->
    case lists:keyfind(guanco, 1, application:which_applications()) of
        false -> ok;
        _ -> 
            application:stop(guanco),
            timer:sleep(1000)  %% Add a delay to ensure the application has stopped
    end.

%% Test initialization with environment variable
init_with_env_test() ->
    ensure_application_stopped(),
    timer:sleep(1000),  %% Add a delay to ensure the application has stopped
    application:set_env(guanco, ollama_api_url, "http://env-url"),
    io:format("Set environment variable: ~p~n", [application:get_env(guanco, ollama_api_url)]),
    application:start(guanco),
    io:format("Application started.~n"),
    {ok, State} = gen_server:call(guanco_worker, {get_state}),
    io:format("State received: ~p~n", [State]),
    BaseUrl = maps:get(base_url, State),
    io:format("Base URL in state: ~p~n", [BaseUrl]),
    ?assertEqual("http://env-url", BaseUrl),
    application:unset_env(guanco, ollama_api_url),
    application:stop(guanco).

%% Test initialization with default value from app.src
init_with_default_test() ->
    ensure_application_stopped(),
    timer:sleep(1000),  %% Add a delay to ensure the application has stopped
    application:start(guanco),
    io:format("Application started.~n"),
    {ok, State} = gen_server:call(guanco_worker, {get_state}),
    io:format("State received: ~p~n", [State]),
    BaseUrl = maps:get(base_url, State),
    io:format("Base URL in state: ~p~n", [BaseUrl]),
    ?assertEqual("http://localhost:11434", BaseUrl),
    application:stop(guanco).