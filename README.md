# Guanco 🦙

Guanco is an Erlang-based client for interacting with the Ollama API. It simplifies the use of Ollama's API by wrapping calls into Erlang worker functions, allowing you to perform tasks such as generating chat completions, retrieving model information, and generating embeddings with minimal setup.

## Features 🚀

- **Chat Completion**: Generate AI-powered chat completions using various models.
- **Model Info**: Fetch information about a specific model.
- **Embeddings**: Generate embeddings for input text.
- **Configurable**: Ollama API URL, host, and port can be customized via `sys.config`.
- **Streaming Support**: Stream responses directly to a PID or use lazy streaming with generators.

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

The Ollama API URL is configured in `sys.config`. Add the following to your `sys.config` file:

```erlang
[
 {guanco, [
    {ollama_api_url, "http://localhost:11434"}
 ]}
].
```

Ensure the `sys.config` file is in the same directory where you run the Erlang VM or specify its location when starting the VM.

### 4. Start the Worker

To start the `guanco_worker`, include it in your application supervision tree or manually start it:

```erlang
{ok, Pid} = guanco_worker:start_link().
```

## Usage 📋

Guanco provides a simple interface to interact with Ollama's API via the `guanco_worker` module. Functions are invoked using the `Pid` of the started worker.

---

### **Generate Completion ✍️**

Generate a text completion from a specific model.

```erlang
{ok, Pid} = guanco_worker:start_link().

Prompt = <<"Tell me a story about space exploration.">>.
OptParams = #{
    system => <<"Make it very short - 2-3 phrases.">>,
    stream => false
}.

{ok, Response} = guanco_worker:generate_completion(Pid, mistral, Prompt, OptParams).
io:format("Response: ~p~n", [Response]).
```

---

### **Streaming Completion ✨**

Stream responses directly to a PID.

```erlang
{ok, Pid} = guanco_worker:start_link().

Prompt = <<"Tell me a story about space exploration.">>.
OptParams = #{
    system => <<"Make it very short - 2-3 phrases.">>,
    stream => self()
}.

ok = guanco_worker:generate_completion(Pid, mistral, Prompt, OptParams),

receive
    {stream_chunk, Chunk} ->
        io:format("Received chunk: ~p~n", [Chunk]);
    {stream_finished, FinalResponse} ->
        io:format("Streaming finished: ~p~n", [FinalResponse]);
    {stream_error, Reason} ->
        io:format("Streaming error: ~p~n", [Reason])
end.
```

---

### **Generate Chat Completion 💬**

Interact with the API for chat completions.

```erlang
{ok, Pid} = guanco_worker:start_link().

Messages = [
    #{role => <<"system">>, content => <<"You're an AI assistant. Be concise.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].

OptParams = #{stream => false}.

{ok, Response} = guanco_worker:generate_chat_completion(Pid, mistral, Messages, OptParams).
io:format("Chat Response: ~p~n", [Response]).
```

---

### **Streaming Chat Completion 💡**

Stream chat completion responses directly to a PID.

```erlang
{ok, Pid} = guanco_worker:start_link().

Messages = [
    #{role => <<"system">>, content => <<"You're an AI assistant. Be concise.">>},
    #{role => <<"user">>, content => <<"What is quantum computing?">>}
].

OptParams = #{stream => self()}.

ok = guanco_worker:generate_chat_completion(Pid, mistral, Messages, OptParams),

receive
    {stream_chunk, Chunk} ->
        io:format("Received chunk: ~p~n", [Chunk]);
    {stream_finished, FinalResponse} ->
        io:format("Streaming finished: ~p~n", [FinalResponse]);
    {stream_error, Reason} ->
        io:format("Streaming error: ~p~n", [Reason])
end.
```

---

### **Retrieve Model Information ℹ️**

Fetch details about a specific model.

```erlang
{ok, Pid} = guanco_worker:start_link().

{ok, Info} = guanco_worker:show_model_info(Pid, mistral).
io:format("Model Info: ~p~n", [Info]).
```

---

### **Generate Embeddings 🧠**

Create embeddings for input text.

```erlang
{ok, Pid} = guanco_worker:start_link().

{ok, Embeddings} = guanco_worker:generate_embeddings(Pid, mistral, <<"Sample text">>).
io:format("Embeddings: ~p~n", [Embeddings]).
```

---

## License 📜

Guanco is released under the Apache-2.0 License. See the [LICENSE](LICENSE) file for details.
