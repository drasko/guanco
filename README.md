# Guanco 🦙

![Erlang CI](https://github.com/drasko/guanco/actions/workflows/ci.yml/badge.svg)

Guanco is an Erlang client for Ollama that provides various functionalities such as generating completions, chat completions, showing model information, and generating embeddings.

## Features 🚀

- **Chat Completion**: Generate AI-powered chat completions using various models.
- **Model Info**: Fetch information about a specific model.
- **Embeddings**: Generate embeddings for input text.
- **Configurable**: Ollama API URL, host, and port can be customized via `sys.config`.
- **Worker Pooling**: Efficient resource management using `worker_pool`.

## Installation 📥

To install Guanco, clone the repository and build the project using `rebar3`:

### 1. Clone the Repository

```sh
git clone https://github.com/yourusername/guanco.git
cd guanco
```

### 2. Compile the Project

Use `rebar3` to compile the project and fetch dependencies:

```sh
rebar3 compile
```

## Configuration ⚙️

The application uses environment variables for configuration, including `ollama_api_url` for the base URL and `worker_pool` settings for worker management.

Example `sys.config`:

```erlang
[
 {guanco, [
   {ollama_api_url, "http://localhost:11434"},
   {pool_size, 5},
   {max_overflow, 10}
 ]}
].
```

Ensure the `sys.config` file is in the same directory where you run the Erlang VM or specify its location when starting the VM.

## Usage 📋

### 1. Start the Application

```erlang
application:start(guanco).
```

### 2. Generate Completion ✍️

#### Normal Completion

```erlang
ModelName = mistral,
Prompt = <<"Tell me a story about space exploration.">>.
OptParams = #{
    system => <<"Make it very short - 2-3 phrases.">>,
    stream => false
}.

{ok, Response} = guanco_app:generate_completion(ModelName, Prompt, OptParams).
io:format("Response: ~p~n", [Response]).
```

#### Streaming Completion ✨

```erlang
ModelName = mistral,
Prompt = <<"Tell me a story about space exploration.">>.
OptParams = #{
    system => <<"Make it very short - 2-3 phrases.">>,
    stream => self()
}.

ok = guanco_app:generate_completion(ModelName, Prompt, OptParams),

receive
    {stream_chunk, Chunk} ->
        io:format("Received chunk: ~p~n", [Chunk]);
    {stream_finished, FinalResponse} ->
        io:format("Streaming finished: ~p~n", [FinalResponse]);
    {stream_error, Reason} ->
        io:format("Streaming error: ~p~n", [Reason])
end.
```

#### Lazy Streaming Completion 🌀

```erlang
ModelName = mistral,
Prompt = <<"This is the test">>.
OptParams = #{stream => true}.

Result = guanco_app:generate_completion(ModelName, Prompt, OptParams),
case Result of
    {ok, StreamFun} ->
        handle_lazy_stream(StreamFun);
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason])
end.

handle_lazy_stream(StreamFun) ->
    case StreamFun() of
        {done, FinalChunk} ->
            io:format("Lazy streaming finished: ~p~n", [FinalChunk]);
        {cont, Chunk, NextFun} ->
            io:format("Received lazy chunk: ~p~n", [Chunk]),
            handle_lazy_stream(NextFun);
        {error, Reason} ->
            io:format("Lazy streaming error: ~p~n", [Reason])
    end.
```

### 3. Generate Chat Completion 💬

#### Normal Chat Completion

```erlang
ModelName = mistral,
Messages = [
    #{role => <<"system">>, content => <<"You're an AI assistant. Be concise.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].
OptParams = #{stream => false}.

{ok, Response} = guanco_app:generate_chat_completion(ModelName, Messages, OptParams).
io:format("Chat Response: ~p~n", [Response]).
```

#### Streaming Chat Completion 💡

```erlang
ModelName = mistral,
Messages = [
    #{role => <<"system">>, content => <<"You're an AI assistant. Be concise.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].
OptParams = #{stream => self()}.

ok = guanco_app:generate_chat_completion(ModelName, Messages, OptParams),

receive
    {stream_chunk, Chunk} ->
        io:format("Received chunk: ~p~n", [Chunk]);
    {stream_finished, FinalResponse} ->
        io:format("Streaming finished: ~p~n", [FinalResponse]);
    {stream_error, Reason} ->
        io:format("Streaming error: ~p~n", [Reason])
end.
```

#### Lazy Streaming Chat Completion 🌀

```erlang
ModelName = mistral,
Messages = [
    #{role => <<"system">>, content => <<"You're an AI assistant. Be concise.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].
OptParams = #{stream => true}.

Result = guanco_app:generate_chat_completion(ModelName, Messages, OptParams),
case Result of
    {ok, StreamFun} ->
        handle_lazy_stream(StreamFun);
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason])
end.

handle_lazy_stream(StreamFun) ->
    case StreamFun() of
        {done, FinalChunk} ->
            io:format("Lazy streaming finished: ~p~n", [FinalChunk]);
        {cont, Chunk, NextFun} ->
            io:format("Received lazy chunk: ~p~n", [Chunk]),
            handle_lazy_stream(NextFun);
        {error, Reason} ->
            io:format("Lazy streaming error: ~p~n", [Reason])
    end.
```

### 4. Retrieve Model Information ℹ️

```erlang
ModelName = mistral.

{ok, Info} = guanco_app:show_model_info(ModelName).
io:format("Model Info: ~p~n", [Info]).
```

### 5. Generate Embeddings 🧠

```erlang
ModelName = mistral,
InputText = <<"Some text">>.

{ok, Embeddings} = guanco_app:generate_embeddings(ModelName, InputText).
io:format("Embeddings: ~p~n", [Embeddings]).
```

## Testing 🧪

To run the tests, use `rebar3`:

```sh
rebar3 eunit
```

## License 📜

Guanco is released under the Apache-2.0 License. See the [LICENSE](LICENSE) file for details.
