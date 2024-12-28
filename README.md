# Guanco 🦙

Guanco is an Erlang-based client for interacting with the Ollama API. It simplifies the use of Ollama's API by wrapping calls into Erlang worker functions, allowing you to perform tasks such as generating chat completions, retrieving model information, and generating embeddings with minimal setup.

## Features 🚀

- **Chat Completion**: Generate AI-powered chat completions using various models.
- **Model Info**: Fetch information about a specific model.
- **Embeddings**: Generate embeddings for input text.
- **Configurable**: Ollama API URL, host, and port can be customized via application environment configuration.
- **Tool Support**: Enhanced support for tools in chat interactions (e.g., multimodal models or external APIs).

## Installation 📥

Follow these steps to install and set up Guanco:

### 1. Clone the Repository

```bash
git clone https://github.com/yourusername/guanco.git
cd guanco
```

### 2. Install Dependencies

Ensure you have Erlang installed. Then, use `rebar3` to install the dependencies:

```bash
rebar3 compile
```

This will fetch the required dependencies, such as `hackney` for HTTP requests and `jiffy` for JSON handling.

### 3. Configure the Ollama API URL

The Ollama API URL can be configured in the `app.src` file under the `env` section. This configuration allows you to set the host and port for the Ollama API.

Update the `guanco.app.src` file in the `src/` directory:

```erlang
{env, [{ollama, [{host, "localhost"}, {port, 11434}]}]},
```

### 4. Start the Application

To start the Guanco application, run the following command:

```bash
erl -sname guanco_app -setcookie yourcookie -s guanco_app
```

## Usage 📋

Guanco provides a simple interface to interact with Ollama's API via the `guanco_worker` module.

### Generate Completion ✍️

Generate a completion from a specific model:

```erlang
guanco_worker:generate_completion("model_name", Prompt, OptParams).
```

Parameters:
- **model_name**: The name of the model to use (e.g., `mistral`).
- **Prompt**: The input prompt for the model (binary format).
- **OptParams**: A map of optional parameters (e.g., suffix, context, streaming options).

#### Example:

```erlang
Prompt = <<"Tell me a story about space exploration.">>.
OptParams = #{
    system => <<"Make it very short - 2-3 phrases.">>,
    stream => false
}.

{ok, Response} = guanco_worker:generate_completion(mistral, Prompt, OptParams).
io:format("Response: ~p~n", [Response]).
```

#### Streaming Example with PID:

```erlang
Prompt = <<"Tell me a story about space exploration.">>.
OptParams = #{
    system => <<"Make it very short - 2-3 phrases.">>,
    stream => self()
}.

guanco_worker:generate_completion(mistral, Prompt, OptParams),

receive
    {stream_chunk, Chunk} ->
        io:format("Received chunk: ~p~n", [Chunk]),
        %% Handle the chunk
        ok;
    {stream_finished, FinalResponse} ->
        io:format("Streaming finished: ~p~n", [FinalResponse]),
        %% Handle the final response
        ok;
    {stream_error, Reason} ->
        io:format("Streaming error: ~p~n", [Reason]),
        %% Handle the error
        ok
end.
```

#### Streaming Example with Generator:

```erlang
Prompt = <<"Tell me a story about space exploration.">>.
OptParams = #{
    system => <<"Make it very short - 2-3 phrases.">>,
    stream => true
}.

{ok, StreamFun} = guanco_worker:generate_completion(mistral, Prompt, OptParams),

process_stream(StreamFun).

process_stream(StreamFun) ->
    case StreamFun() of
        {done, FinalResponse} ->
            io:format("Streaming finished: ~p~n", [FinalResponse]),
            %% Handle the final response
            ok;
        {cont, Chunk, NextFun} ->
            io:format("Received chunk: ~p~n", [Chunk]),
            %% Handle the chunk
            process_stream(NextFun);
        {error, Reason} ->
            io:format("Streaming error: ~p~n", [Reason]),
            %% Handle the error
            ok
    end.
```

### Generate Chat Completion 💬

Interact with the API for chat completions:

```erlang
guanco_worker:generate_chat_completion("model_name", Messages, OptParams).
```

Parameters:
- **Messages**: A list of maps representing the chat history (roles, content, and optional tools).
- **OptParams**: A map of optional parameters, including `tools`, `stream`, and more.

#### Example:

```erlang
Messages = [
    #{role => <<"system">>, content => <<"You're an AI assistant. Give very short answers, not more than 2-3 phrases.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].

OptParams = #{stream => false}.

{ok, Response} = guanco_worker:generate_chat_completion(mistral, Messages, OptParams).
io:format("Chat: ~p~n", [Response]).
```

#### Streaming Example with PID:

```erlang
Messages = [
    #{role => <<"system">>, content => <<"You're an AI assistant. Give very short answers, not more than 2-3 phrases.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].

OptParams = #{stream => self()}.

guanco_worker:generate_chat_completion(mistral, Messages, OptParams),

receive
    {stream_chunk, Chunk} ->
        io:format("Received chunk: ~p~n", [Chunk]),
        %% Handle the chunk
        ok;
    {stream_finished, FinalResponse} ->
        io:format("Streaming finished: ~p~n", [FinalResponse]),
        %% Handle the final response
        ok;
    {stream_error, Reason} ->
        io:format("Streaming error: ~p~n", [Reason]),
        %% Handle the error
        ok
end.
```

#### Streaming Example with Generator:

```erlang
Messages = [
    #{role => <<"system">>, content => <<"You're an AI assistant. Give very short answers, not more than 2-3 phrases.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].

OptParams = #{stream => true}.

{ok, StreamFun} = guanco_worker:generate_chat_completion(mistral, Messages, OptParams),

process_stream(StreamFun).

process_stream(StreamFun) ->
    case StreamFun() of
        {done, FinalResponse} ->
            io:format("Streaming finished: ~p~n", [FinalResponse]),
            %% Handle the final response
            ok;
        {cont, Chunk, NextFun} ->
            io:format("Received chunk: ~p~n", [Chunk]),
            %% Handle the chunk
            process_stream(NextFun);
        {error, Reason} ->
            io:format("Streaming error: ~p~n", [Reason]),
            %% Handle the error
            ok
    end.
```

### Retrieve Model Information ℹ️

Fetch details about a model:

```erlang
guanco_worker:show_model_info("model_name").
```

#### Example:

```erlang
{ok, Info} = guanco_worker:show_model_info(mistral).
io:format("Model Info: ~p~n", [Info]).
```

### Generate Embeddings 🧠

Create embeddings for text:

```erlang
guanco_worker:generate_embeddings("model_name", InputText).
```

#### Example:

```erlang
{ok, Embeddings} = guanco_worker:generate_embeddings(mistral, <<"Sample text">>).
io:format("Embeddings: ~p~n", [Embeddings]).
```

## License 📜

Guanco is released under the Apache-2.0 License. See the [LICENSE](LICENSE) file for details.
