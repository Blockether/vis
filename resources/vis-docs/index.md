Vis is a coding agent that combines tools into Python programs.
It can chain operations, run them in parallel, and inspect results before deciding
what belongs in the conversation.

[Install](#install) · [First session](#first-session) · [Configuration](configuration.md)

## Install

On macOS or Linux, run:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

<div class="store-links" aria-label="Install the Companion app">
<a class="store-apple" href="https://testflight.apple.com/join/4anYT4Wk"><span><small>iOS / iPadOS</small><strong>TestFlight</strong></span></a>
<a class="store-android" href="https://play.google.com/apps/testing/com.blockether.viscompanion"><span><small>Android</small><strong>Google Play beta</strong></span></a>
</div>

Public beta apps. Connect them to your Vis gateway: [pairing instructions](gateway.md).

You can [read the installer](https://github.com/Blockether/vis/releases/download/installer/install-vis-agent)
before running it. It installs the stable native engine, bundled Python and terminal
client in `~/.local/bin`. Installation requires `curl` and `tar`, not Java or Git.

Tracks choose which version of Vis you install:

- `release` (default) installs the latest stable version and does not need Java.
- `beta` installs the latest published preview that passed automated checks and does not need Java.
- `dev` runs the latest code from `main` on the JVM and needs Git and JDK 25+.

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

`vis-agent update` always selects `release`; use `vis-agent update --track beta`
or `vis-agent update --track dev` for the other tracks. See
[Runtime distributions](distributions.md).

## Learn more

### Guides

- [Configuration](configuration.md) — providers, models and project settings.
- [Project instructions](context-and-prompts.md) — tell the agent how your codebase works.
- [Skills](skills.md) — reusable task instructions.
- [Controlling a session](queue-and-cancel.md) — send follow-ups, cancel a task and exit.
- [Drafts](drafts.md) — let the agent work in an isolated copy and land its changes on a branch.
- [Exporting sessions](exporting-sessions.md) — save or share a session.
- [Remote access and the Companion app](gateway.md) — use Vis from your phone or another machine.
- [Council](council.md) — exchange project messages and explicit pings between active sessions.
- [Reporting a bug](reporting-bugs.md) — report a problem without exposing private data.

### Extensions

- [Extending Vis](extending.md) — choose a capability and build your first tool.
- [Extension design](extension-design.md) — design and test typed tools.
- [Installing and sharing extensions](extension-packages.md) — layouts, installation, reload and distribution.
- [Using an existing Python project](extension-development.md) — prepare editable uv packages for Vis.
- [Extension API](extension-api.md) — declarations, tool contracts and host operations.
- [Extension troubleshooting](extension-troubleshooting.md) — diagnose loading and call errors.
- [Asking the human](human-input.md) — show a typed form and read the answer.
- [Live views](live-views.md) — show progress a person can watch and stop.
- [Provider extensions](provider-extensions.md) — register an LLM provider from an extension.
- [Python SDK](https://pypi.org/project/vis-agent/) — develop and test extensions outside Vis.

### Concepts

- [How Vis manages context](token-optimization.md) — why the agent writes programs and folds history.
- [Python sandbox](python-sandbox.md) — Python execution, packages and permissions.

### Reference

- [Process jail and network policy](jail.md) — rules for child processes.
- [Content-block protocol](content-blocks.md) — message and streaming formats.
- [Runtime distributions](distributions.md) — installation methods and updates.
- [Building the native binary](jvm-native-image.md) — GraalVM build, metadata and TLS proxies.
