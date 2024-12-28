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
OptParams = #{stream => true}.

{ok, Response} = guanco_worker:generate_completion(mistral, Prompt, OptParams).
io:format("Response: ~p~n", [Response]).
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
    #{role => <<"system">>, content => <<"You're an AI assistant.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].

OptParams = #{stream => true}.

{ok, Response} = guanco_worker:generate_chat_completion(mistral, Messages, OptParams).
io:format("Chat: ~p~n", [Response]).
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
