# Guanco 🦙

Guanco is an Erlang-based client for interacting with the Ollama API, offering easy integration with various endpoints such as **generate chat completions**, **show model information**, and **generate embeddings**. The project simplifies the use of Ollama's API by wrapping the calls into Erlang worker functions, allowing you to perform these tasks with minimal setup.

## Features 🚀

- **Chat Completion**: Generate AI-powered chat completions with various models.
- **Model Info**: Fetch information about a specific model.
- **Embeddings**: Generate embeddings for input text using Ollama's API.
- **Configurable**: URL and other parameters can be customized via environment variables.

## Installation 📥

To get started with Guanco, follow these installation steps:

### 1. Clone the Repository

```bash
git clone https://github.com/yourusername/guanco.git
cd guanco
```

### 2. Install Dependencies

Ensure you have Erlang installed. You can install dependencies using `rebar3`:

```bash
rebar3 compile
```

This command will fetch the required dependencies like `hackney` (for HTTP requests) and `jiffy` (for JSON handling).

### 3. Set Up Environment Variables

You can configure the Ollama API base URL through the `OLLAMA_API_URL` environment variable. If it's not set, the default URL will be used.

```bash
export OLLAMA_API_URL="https://api.ollama.com"
```

### 4. Start the Application

To start the Guanco worker, run the following command:

```bash
erl -sname guanco_app -setcookie yourcookie -s guanco_app
```

This will start the application and initialize the worker.

## Usage 📋

Guanco provides a simple interface to interact with Ollama's API via the `guanco_worker` module.

### Generate Chat Completion 💬

Use this function to generate a chat completion from a specific model:

```erlang
guanco_worker:generate_chat_completion("model_name", Messages, Options).
```

- **model_name**: The name of the model you'd like to use (e.g., `"gpt-3"`).
- **Messages**: A list of messages exchanged in the chat (format: `[{role, content}]`).
- **Options**: A map of additional options for the completion (optional).

### Example:

```erlang
Messages = [{"system", "You are a helpful assistant."},
            {"user", "What's the weather like today?"}].

Options = #{ "temperature" => 0.7, "max_tokens" => 100 }.

{ok, Response} = guanco_worker:generate_chat_completion("gpt-3", Messages, Options).
io:format("Response: ~p~n", [Response]).
```

### Show Model Information ℹ️

You can fetch detailed information about a specific model:

```erlang
guanco_worker:show_model_info("model_name").
```

- **model_name**: The name of the model you want information about.

### Example:

```erlang
{ok, ModelInfo} = guanco_worker:show_model_info("gpt-3").
io:format("Model Information: ~p~n", [ModelInfo]).
```

### Generate Embeddings 🧠

Generate embeddings for a given input text:

```erlang
guanco_worker:generate_embeddings(InputText, Options).
```

- **InputText**: The text for which to generate embeddings.
- **Options**: Additional options for the embedding generation (optional).

### Example:

```erlang
Text = "This is a test sentence.".
Options = #{}.

{ok, Embeddings} = guanco_worker:generate_embeddings(Text, Options).
io:format("Embeddings: ~p~n", [Embeddings]).
```

## License 📜

Guanco is released under the Apache-2.0 License. See the [LICENSE](LICENSE) file for details.