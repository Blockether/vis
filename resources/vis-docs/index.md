Vis is a coding agent that combines tools into Python programs.
It can search your project, make changes and run tests, checking the results
as it goes.

<nav class="quick-links" aria-label="Getting started">
  <a href="#why-vis">Why Vis</a>
  <a href="#install">Install</a>
  <a href="#first-session">First session</a>
  <a href="configuration.md">Configuration</a>
</nav>

## Why Vis

You know how your project should be built, tested and checked. Vis lets you
put that knowledge into functions the agent can use, so repeatable work
doesn't depend only on written instructions.

### Put your expertise into code

For example, you can give the agent a function that selects the right test
suite, checks its inputs and reports failures. It can use that function
instead of working out a shell command each time.

Start with the built-in tools, then add [Python extensions](extending.md)
for work you repeat. Refine them as you use the agent. Keep `AGENTS.md` and
skills for guidance; enforce an operation's rules in code. This makes
individual operations more predictable, not the model's decisions.

Once your functions cover a workflow, you can [disable shell access](jail.md)
with `toggles.shell: false`. Extensions run as trusted host code with full
CPython access, while the model's Python environment is
[sandboxed](python-sandbox.md). Only install extensions you trust.

### Combine steps in Python

Models already use Python to get things done. Vis makes that the way the
agent uses tools: through one tool called `python_execution`, with the
available operations exposed as Python functions. The agent finds them with
`apropos()` and reads how to use them with `doc()`.

It can search several files, check the matches and summarize what matters
in one program. It can run independent work in parallel and inspect results
in Python rather than send every result back to the conversation. You can
also run and inspect sessions from your own code with the
[Python SDK](https://pypi.org/project/vis-agent/).

### Keep useful work when you return

The agent can define Python helpers for repeated work and reuse them later
in the same session. Their definitions survive restarting Vis and reloading
extensions. This preserves their source, not every live Python object.

The `session` dictionary shows the agent its workspace, permissions and how
much context it is using. As the conversation grows, it can summarize
completed work to make room for what's next. The full history stays stored,
even when it is no longer sent to the model. See
[How Vis manages context](token-optimization.md).

## Install

On macOS or Linux, run:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

Download the latest stable desktop app from GitHub Releases. Choose the universal
macOS `.dmg` or the Linux `.AppImage` for your architecture.

<div class="store-links" aria-label="Download the desktop app">
<a class="store-macos" href="https://github.com/Blockether/vis/releases/latest"><img src="assets/install-macos.png" alt="Latest desktop release for macOS" width="224" height="56"></a>
<span aria-hidden="true">&nbsp;&nbsp;</span>
<a class="store-linux" href="https://github.com/Blockether/vis/releases/latest"><img src="assets/install-linux.png" alt="Latest desktop release for Linux" width="224" height="56"></a>
</div>

For automatic download and launch, run `vis-agent desktop --track release`.
See [Desktop setup](distributions.md#open-the-desktop-app) for details.

<div class="store-links" aria-label="Install the Companion app">
<a class="store-apple" href="https://testflight.apple.com/join/4anYT4Wk"><img src="assets/install-testflight.png" alt="TestFlight for iOS and iPadOS" width="224" height="56"></a>
<span aria-hidden="true">&nbsp;&nbsp;</span>
<a class="store-android" href="https://play.google.com/apps/testing/com.blockether.viscompanion"><img src="assets/install-google-play.png" alt="Google Play beta for Android" width="224" height="56"></a>
</div>

The mobile apps are public betas. Connect desktop or mobile apps to your Vis gateway: [pairing instructions](gateway.md).

You can [read the installer](https://github.com/Blockether/vis/releases/download/installer/install-vis-agent)
before running it. It installs the stable native engine, bundled Python and terminal
client in `~/.local/bin`. Installation requires `curl` and `tar`, not Java or Git.

Tracks choose which version of Vis you install:

- `release` (default) installs the latest stable version and does not need Java.
- `beta` installs the latest published preview that passed automated checks and does not need Java.
- `dev` runs the latest code from `main` on the JVM and needs Git and JDK 25+.

For runtime options and manual setup, see [Runtime distributions](distributions.md).

## Native vs JVM

**Choose native for everyday use.** Choose JVM if you're developing Vis itself
or want to run the latest code from `main`.

| What matters | Native | JVM |
| --- | --- | --- |
| Best for | Daily work and multiple sessions | Developing Vis and trying the latest changes |
| Version track | `release` (default) or `beta` | `dev` |
| Java / Git | Not needed | Git and JDK 25+ |
| Gateway startup | ~0.7 s | ~8.6 s |
| Gateway RAM | ~122 MiB | ~566 MiB |
| RAM per session (light use) | ~60 MiB | ~153 MiB |

*Performance figures: Vis v0.2.0 on Apple M4 Max.*

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
- [Drafts](drafts.md) — try a change in an isolated working copy and review it before approval.
- [Exporting sessions](exporting-sessions.md) — save or share a session.
- [Remote access and the Companion app](gateway.md) — use Vis from your phone or another machine.
- [Council](council.md) — ask another session for help or a second review.
- [Reporting a bug](reporting-bugs.md) — report a problem without exposing private data.

### Extensions

- [Extending Vis](extending.md) — choose a capability and build your first tool.
- [Extension design](extension-design.md) — design and test typed tools.
- [Installing and sharing extensions](extension-packages.md) — layouts, installation, reload and distribution.
- [Using an existing Python project](extension-development.md) — prepare editable uv packages for Vis.
- [Extension API](extension-api.md) — declarations, tool contracts and host operations.
- [Extension troubleshooting](extension-troubleshooting.md) — diagnose loading and call errors.
- [Forms and user input](human-input.md) — ask for choices, credentials or confirmation.
- [Live views](live-views.md) — show progress a person can watch and stop.
- [Provider extensions](provider-extensions.md) — register an LLM provider from an extension.
- [Python SDK](https://pypi.org/project/vis-agent/) — develop and test extensions outside Vis.

### Concepts

- [How Vis manages context](token-optimization.md) — how filtering and summaries keep conversations manageable.
- [Python sandbox](python-sandbox.md) — Python execution, packages and permissions.

### Reference

- [Process jail and network policy](jail.md) — rules for child processes.
- [Runtime distributions](distributions.md) — installation methods and updates.
- [Building the native binary](jvm-native-image.md) — GraalVM build, metadata and TLS proxies.
