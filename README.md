Sure! Here's the rewritten README with the necessary updates:

---

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

### Generate Chat Completion 💬

Generate a chat completion from a specific model with:

```erlang
guanco_worker:generate_chat_completion("model_name", Messages, Options).
```

- **model_name**: The name of the model you want to use (e.g., `"gpt-3"`).
- **Messages**: A list of messages exchanged in the chat (format: `[{role, content}]`).
- **Options**: A map of additional options for the completion (optional).

#### Example:

```erlang
Messages = [{"system", "You are a helpful assistant."},
            {"user", "What's the weather like today?"}].

Options = #{ "temperature" => 0.7, "max_tokens" => 100 }.

{ok, Response} = guanco_worker:generate_chat_completion("gpt-3", Messages, Options).
io:format("Response: ~p~n", [Response]).
```

### Show Model Information ℹ️

Retrieve detailed information about a specific model:

```erlang
guanco_worker:show_model_info("model_name").
```

- **model_name**: The name of the model you want to get information about.

#### Example:

```erlang
{ok, ModelInfo} = guanco_worker:show_model_info("gpt-3").
io:format("Model Information: ~p~n", [ModelInfo]).
```

### Generate Embeddings 🧠

Generate embeddings for the given input text:

```erlang
guanco_worker:generate_embeddings(InputText, Options).
```

- **InputText**: The text for which to generate embeddings.
- **Options**: Additional options for the embedding generation (optional).

#### Example:

```erlang
Text = "This is a test sentence.".
Options = #{}.

{ok, Embeddings} = guanco_worker:generate_embeddings(Text, Options).
io:format("Embeddings: ~p~n", [Embeddings]).
```

## License 📜

Guanco is released under the Apache-2.0 License. See the [LICENSE](LICENSE) file for more details.

---

This updated README includes clear instructions on how to install and configure the application, as well as usage examples for generating chat completions, retrieving model information, and generating embeddings.