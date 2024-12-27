# Guanco 🦙

Guanco is an Erlang-based client for interacting with the Ollama API. It simplifies the use of Ollama's API by wrapping calls into Erlang worker functions, allowing you to perform tasks such as generating chat completions, retrieving model information, and generating embeddings with minimal setup.

## Features 🚀

- **Chat Completion**: Generate AI-powered chat completions using various models.
- **Model Info**: Fetch information about a specific model.
- **Embeddings**: Generate embeddings for input text.
- **Configurable**: Ollama API URL, host, and port can be customized via application environment configuration.

## Installation 📥

Follow these steps to install and set up Guanco:

### 1. Clone the Repository

```bash
git clone https://github.com/yourusername/guanco.git
cd guanco
```

### 2. Install Dependencies

Make sure you have Erlang installed. Then, use `rebar3` to install the dependencies:

```bash
rebar3 compile
```

This will fetch the required dependencies, such as `hackney` for HTTP requests and `jiffy` for JSON handling.

### 3. Configure the Ollama API URL

The Ollama API URL can be configured in the `app.src` file under the `env` section. This configuration allows you to set the host and port for the Ollama API.

In the `src/` directory, ensure the following is set in your `guanco.app.src` file:

```erlang
{env, [{ollama, [{host, "localhost"}, {port, 11434}]}]},
```

### 4. Start the Application

To start the Guanco application, run the following command:

```bash
erl -sname guanco_app -setcookie yourcookie -s guanco_app
```

This will start the Guanco application and initialize the workers.

## Usage 📋

Guanco provides a simple interface to interact with Ollama's API via the `guanco_worker` module.

### Generate Completion ✍️

Generate a completion from a specific model with:

```erlang
guanco_worker:generate_completion("model_name", Prompt, OptParams).
```

- **model_name**: The name of the model you want to use (e.g., `mistral`).
- **Prompt**: The input prompt for the model to generate a response from (in binary format).
- **OptParams**: A map of additional options (optional params). OptParams include:
  - **suffix**: Text to append to the completion (default is `""`).
  - **images**: A list of images to include (default is `[]`).
  - **format**: The response format (`json` by default).
  - **options**: Additional options for the request.
  - **system**: The system message (default is `"default_system"`).
  - **template**: The template to use (default is `"default_template"`).
  - **stream**: Whether to stream the response (default is `false`).
  - **raw**: Whether to receive raw output (default is `false`).
  - **keep_alive**: Whether to keep the connection alive (default is `false`).
  - **context**: Additional context for the request (default is `""`).

#### Example:

```erlang
Prompt = <<"Write a short story about a curious cat exploring a garden.">>.
OptParams = #{
    suffix => <<" The adventure begins.">>,
    system => <<"You're an imaginative storyteller.">>,
    stream => true
}.

{ok, Response} = guanco_worker:generate_completion(mistral, Prompt, OptParams).
io:format("Generated Response: ~p~n", [Response]).
```

### Generate Chat Completion 💬

Generate a chat completion from a specific model using the following function:

```erlang
guanco_worker:generate_chat_completion("model_name", Messages, OptParams).
```

#### Parameters:
- **model**: The name of the model you want to use (e.g., `mistral`).
- **messages**: A list of messages exchanged in the chat, where each message is a map containing:
  - **role**: The role of the message (either `system`, `user`, `assistant`, or `tool`).
  - **content**: The content of the message.
  - **images** (optional): A list of images to include in the message (for multimodal models such as LLAVA).
  - **tool_calls** (optional): A list of tools the model wants to use.
- **OptParams**: A map of optional parameters for customization. Here are the possible fields:
  - **format**: The format to return the response in (default is `json`).
  - **options**: Additional model parameters like temperature, max tokens, etc. (default is an empty map).
  - **stream**: If `false`, the response will be returned as a single response object rather than a stream (default is `false`).
  - **keep_alive**: Controls how long the model will stay loaded into memory following the request (default is `5m`).
  - **tools**: Tools for the model to use, if supported (requires `stream` to be `false`).

#### Example Usage:

```erlang
Messages = [
    #{role => <<"system">>, content => <<"You are a helpful assistant.">>},
    #{role => <<"user">>, content => <<"What's the weather like today?">>}
].

OptParams = #{
    stream => false,
    options => #{"temperature" => 0.7, "seed" => 42}
}.

{ok, Response} = guanco_worker:generate_chat_completion(mistral, Messages, OptParams).
io:format("Chat Response: ~p~n", [Response]).
```

### Show Model Information ℹ️

Retrieve detailed information about a specific model:

```erlang
guanco_worker:show_model_info("model_name").
```

- **model_name**: The name of the model you want to get information about.

#### Example:

```erlang
{ok, ModelInfo} = guanco_worker:show_model_info(mistral).
io:format("Model Information: ~p~n", [ModelInfo]).
```

### Generate Embeddings 🧠

Generate embeddings for the given input text:

```erlang
guanco_worker:generate_embeddings("model_name", InputText).
```

- **model_name**: The name of the model to generate embeddings.
- **InputText**: The text for which to generate embeddings.

#### Example:

```erlang
Text = <<"This is a test sentence.">>.
{ok, Embeddings} = guanco_worker:generate_embeddings(mistral, Text).
io:format("Embeddings: ~p~n", [Embeddings]).
```

## License 📜

Guanco is released under the Apache-2.0 License. See the [LICENSE](LICENSE) file for more details.
