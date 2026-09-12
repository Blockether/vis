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

## Native vs JVM

The native engine is the default for the `release` and `beta` tracks; the JVM
runs source on the `dev` track. This comparison uses the **same v0.2.0 revision**
for both, not a newer `main` against an older native release.

Measured on **2026-09-12**, on an **Apple M4 Max with 36 GiB RAM**, running
**macOS 26.6.2 (arm64)**. Values are medians of three fresh process runs per
runtime, alternating native and JVM:

| Metric | Native | JVM |
| --- | --- | --- |
| Gateway startup to `/healthz` ready | 0.71 s | 8.59 s |
| Gateway RAM, no sessions | 122 MiB | 566 MiB |
| Additional RAM per initialized session, averaged over 10 | 60 MiB | 153 MiB |
| Gateway + 10 initialized sessions | 724 MiB | 2150 MiB |
| First Python-backed turn after gateway readiness | 6.54 s | 2.32 s |

### Measurement method

- **Runtimes:** the published macOS arm64 [v0.2.0 native release](https://github.com/Blockether/vis/releases/tag/v0.2.0)
  and JVM source at commit `e477440ff511e46763744fef0e73ccf3255bcbbe`, on
  GraalVM CE JDK 25.0.3. The JVM used that revision's `:vis` JVM flags, including
  `-Xmx5g`, and a pre-resolved source classpath. This is not an AOT JVM JAR benchmark.
- **Startup:** launch `gateway start --host 127.0.0.1` on a fresh port, with a
  fresh home, workspace and database, then wait for the first successful
  `/healthz` response through the gateway client. Dependencies were cached;
  installation, downloads and Clojure CLI dependency resolution are excluded.
  The OS filesystem cache was not cleared.
- **Sessions:** create 10 sessions sequentially. Each completes one turn with
  a `python_execution` call running `print(42)`, followed by a final reply from
  the native test suite's local stub provider. All 60 turns completed with
  successful Python output; no paid model calls were made.
- **RAM:** sample five seconds after gateway readiness, and again five seconds
  after the first and tenth sessions complete their turns. Sum `ps` RSS for the
  gateway and all its descendants. The final sample contains the gateway and
  10 live session workers. The UI, benchmark driver and stub provider are
  excluded. One MiB is 1024² bytes.
- **Per-session RAM:** calculate `(RSS with 10 sessions − RSS with no sessions) / 10`
  for each run, then take the median. This includes shared first-use costs,
  rather than claiming every additional session allocates the same amount.
  Each table row is calculated independently.

Native used less RAM and made the gateway ready sooner in these runs, but its
first Python-backed turn was slower: **6.35–13.93 s native**, versus
**1.94–2.55 s JVM**, excluding session creation. Gateway startup ranged from
**0.69–0.97 s native** and **7.35–10.03 s JVM**. Empty-gateway RSS ranged from
**121–123 MiB native** and **560–875 MiB JVM**.

These are small-workload measurements, not a memory limit or a model-response
latency benchmark. Long histories, imports, extensions, browsers, builds and
REPLs change memory use. Garbage collection and idle-session eviction also
change later samples. Summed RSS can count shared pages more than once; it is
not unique physical memory. The JVM's `-Xmx5g` is a heap ceiling, not its measured
RAM consumption.

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
- [Council](council.md) — share knowledge, delegate authorized work and review results through asynchronous messages.
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
