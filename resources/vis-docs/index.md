Vis is a coding agent for working with a codebase. It can read and edit files,
run commands and tests, and use tools you add through extensions. Use it in a
terminal or connect through the Companion app.

[Install](#install) · [First session](#first-session) · [Configuration](configuration.md)

## Install

On macOS or Linux, run:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

You can [read the installer](https://github.com/Blockether/vis/releases/download/installer/install-vis-agent)
before running it. It installs `vis-agent` in `~/.local/bin` and downloads its runtime.
Git is required; Java and the Clojure CLI are installed automatically when needed.

For runtime options and manual setup, see [Runtime distributions](distributions.md).

## First session

Open a terminal in your project and start Vis:

```bash
cd /path/to/project
vis-agent
```

1. Add a provider in the provider picker and follow its sign-in instructions.
2. Select a model.
3. Enter a task, such as “Explain how this project is organized.”

Use an API key or a supported provider account. You can also connect a local model
through Ollama or LM Studio. See [Providers and models](configuration.md#providers-and-models)
for setup details.

Vis can modify files and run commands in your workspace. Review its changes
before committing them. [Process jail and network policy](jail.md) explains filesystem and
network permissions.

## Work with a project

- Put shared project instructions in `AGENTS.md`. See [Project instructions](context-and-prompts.md).
- Send follow-up instructions while a task runs. See [Controlling a session](queue-and-cancel.md).
- Add reusable workflows with [Skills](skills.md), or custom tools with [Extending Vis](extending.md).
- Use your phone or another machine through [Remote access and the Companion app](gateway.md).

## Update

```bash
vis-agent update
```

To check which runtime is installed, run `vis-agent runtime`.

## Learn more

### Guides

- [Configuration](configuration.md) — providers, models and project settings.
- [Project instructions](context-and-prompts.md) — tell the agent how your codebase works.
- [Skills](skills.md) — reusable task instructions.
- [Controlling a session](queue-and-cancel.md) — send follow-ups, cancel a task and exit.
- [Exporting sessions](exporting-sessions.md) — save or share a session.
- [Remote access and the Companion app](gateway.md) — use Vis from your phone or another machine.
- [Reporting a bug](reporting-bugs.md) — report a problem without exposing private data.

### Extensions

- [Extending Vis](extending.md) — write a Python extension: tools, commands, guards and state.
- [Asking the human](human-input.md) — show a typed form and read the answer.
- [Live views](live-views.md) — show progress a person can watch and stop.
- [Provider extensions](provider-extensions.md) — register an LLM provider from an extension.
- [Clojure extensions](clojure-extensions.md) — engine integrations that ship inside the binary.
- [Python SDK](https://pypi.org/project/vis-agent/) — develop and test extensions outside Vis.

### Concepts

- [How Vis manages context](token-optimization.md) — why the agent writes programs and folds history.
- [Python sandbox](python-sandbox.md) — what the model's Python can reach.

### Reference

- [Process jail and network policy](jail.md) — rules for child processes.
- [Content-block protocol](content-blocks.md) — message and streaming formats.
- [Runtime distributions](distributions.md) — installation methods and updates.
- [Building the native binary](jvm-native-image.md) — GraalVM build, metadata and TLS proxies.
