# Configuration

Change most settings in the terminal or in the Companion app. Edit YAML to share settings across
projects or to commit them to a repository. This page describes each configuration file and key.

## When to use

- **You want a model from OpenAI, Ollama, another provider, or your own endpoint, or you must set
  its API key.** Use [Quick setup](#quick-setup) in the Companion app, or YAML under [Providers and
  models](#providers-and-models).
- **Your team must share the same settings.** Commit a project `vis.yml`. Read [Configuration
  files](#configuration-files).
- **One session must not use your global or project setup, or some extensions.** Read [Leave out
  global or project configuration](#leave-out-global-or-project-configuration).
- **One session needs only some tools of an extension.** Read [Choose single
  tools](#choose-single-tools).
- **Some tools must stay off in every session of a project or group.** Read [Turn tools off for a
  project, group or session](#turn-tools-off-for-a-project-group-or-session).
- **One session or group needs different behavior.** Open its [scoped
  settings](#project-group-and-session-settings).
- **A provider hits rate limits or fails, or a task must stay within a budget.** Set a [fallback
  model](#default-and-fallback) and the limits under [Router](#router).
- **New sessions think too much or too little on one provider.** Set its [default thinking
  level](#default-thinking-level).
- **Commands that Vis runs need the environment variables of your project.** Read
  [Environment](#environment).
- **Vis must use tools from an MCP server.** Read [MCP servers](#mcp-servers).
- **Search skips a directory that `.gitignore` excludes.** Read [Grep](#grep).
- **Python HTTPS requests fail a strict certificate check, or packages must come from a private
  index.** Read [Python TLS validation](#python-tls-validation) and [Python package
  index](#python-package-index).
- **Your project installs its Python dependencies only into shared packages.** Read [Python
  extension environment](#python-extension-environment).
- **A gateway on a server shows in your apps as a long host name.** Give it a [machine
  name](#machine-name).
- **You serve Vis under your own name, and nobody must rename the agent.** Give the agent [one
  fixed name](#one-fixed-name).
- **Your own program must read or change settings.** Read [Configuration
  API](configuration-api.md).

For rules about work in your codebase, read [Project instructions](project-instructions.md). To
limit what commands can access, read [Process jail and network policy](jail.md).

## Quick setup

In the terminal, open the provider picker and select **Add Provider**. In the Companion app, open
**Settings**, then **Providers**, and select **Add provider**. Sign in, then pick a model.

For API keys, local models, and custom endpoints, read [Providers and
models](#providers-and-models).

## Coding-agent name

The default name is `Vis`. To name the agent for one project, set `agent_name` in its `vis.yml`:

```yaml
agent_name: Ada
```

To name the agent for the whole gateway, open **Settings**, **General**, then **Agent name** in the
terminal. In the Companion app, open **Settings**, your gateway, then **General**. The gateway
saves this name in its own `~/.vis/state.yml`, and the name overrides project names. To use project
names again, remove the key from `state.yml`.

A name has at most 80 characters. It cannot be blank or contain control characters. Vis removes
spaces at the start and at the end.

A change in Settings updates open sessions immediately, also on remote clients. After you edit the
YAML file, open the session again to see the new name. The name changes the next default system
prompt. It does not change product branding, session titles, or a full custom prompt.

Programs read and save the name with the [Configuration
API](configuration-api.md#change-one-setting).

### One fixed name

When you serve Vis under your own name, fix the agent name, so that no project and no client can
change it.

**The environment variable.** Set `VIS_AGENT_NAME` before the gateway starts:

```bash
VIS_AGENT_NAME=Acme vis-agent gateway start --require-token
```

For a service, put `Environment=VIS_AGENT_NAME=Acme` in the service unit. The variable wins over
each saved name and each project name, and it applies to each session. While it is set, Vis refuses
each change of the name. Vis ignores a value that breaks the rules for a name.

**The lock.** Put these keys in your global `~/.vis/config.yml`:

```yaml
agent_name: Acme
agent_name_locked: true
```

A locked name without `agent_name` is `Vis`. A project file cannot lock the name. A session that
leaves out the global configuration does not read the lock.

A program can save the same lock with the [Configuration
API](configuration-api.md#keep-one-agent-name). The gateway then writes the two keys to its
`~/.vis/state.yml`, and that file wins over `config.yml`. To rename the agent again, remove the
lock with that API, or set `agent_name_locked: false`.

With a fixed name, Settings shows the name with **(locked)** and the reason, and you cannot edit
it.

## Machine name

A client shows each gateway as one machine, by default under the host of the gateway address. To
give the gateway a name, put this key in your global `~/.vis/config.yml`:

```yaml
machine_name: Studio Mac
```

A project file does not change the name, and Settings does not show it. Three sources set the name,
and a later source in this list wins:

1. The `machine_name` key in `~/.vis/config.yml`.
2. A name that a program saves, as [Name the machine](configuration-api.md#name-the-machine) shows.
   The gateway writes it to its `~/.vis/state.yml`.
3. The environment variable `VIS_MACHINE_NAME`, for a gateway that a service or a container starts.

A name has at most 64 characters. The other rules are those of the agent name. Vis ignores a
`VIS_MACHINE_NAME` value that breaks them.

A saved name shows in connected Companion apps immediately. In the Companion app, a name that you
give with **Rename** wins over the gateway name in that client only. To show the gateway name again,
select **Rename** and clear the field.

## Configuration files

Vis reads the files in this order. A later file overrides an earlier file. Nested maps merge. A
scalar or a list replaces the earlier value.

| File | Purpose |
| --- | --- |
| `~/.vis/config.yml` | Your global settings |
| `~/.vis/state.yml` | Settings and credentials that Vis writes. Change them in the UI. |
| `<project>/vis.yml` | Settings shared with the project |
| `<project>/.vis/config.yml` | Local project overrides, which you keep out of Git |

Two keys do not follow that order:

- The gateway-wide `agent_name` in `state.yml` overrides the project files.
- `extensions` merges by package name. Each complete declaration replaces the earlier one. For the
  global and project installation scopes and `vis-agent extension sync --trust`, read [Declare
  packages in configuration](extension-packages.md#declare-packages-in-configuration).

The project is the directory where you start `vis-agent`. `~/.vis` accepts `config.yml`,
`config.yaml`, `vis.yml`, or `vis.yaml`. The project root accepts `vis.yml` or `vis.yaml`. The
`<project>/.vis` directory accepts `config.yml` or `config.yaml`.

Keys are `snake_case` strings. Most boolean keys start with `is_`, but some do not, for example
`enabled` and `tls_strict`. Vis refuses unknown keys. For an invalid file, Vis prints each wrong
path and exits with status 2:

```text
Invalid Vis configuration in /project/.vis/config.yml:

  - grep.include-gitignored-paths: unknown key (config is closed) — did you mean "grep.include_gitignored_paths"?
  - mcp.servers.docs.transport: value rejected by the transport contract
```

Vis does not validate model names. A wrong name fails at the provider.

This example is a small configuration:

```yaml
# vis.yml
system_prompt: Prefer restructuredText docstrings.
router:
  budget:
    max_cost: 5.0
environment:
  ANTHROPIC_API_KEY: {env: ANTHROPIC_API_KEY}
```

### Leave out global or project configuration

When a session must not use part of your setup, start the terminal or a one-shot run with a flag:

```bash
vis-agent tui --no-global                 # only the project configuration
vis-agent tui --no-project                # only your global configuration
vis-agent tui --vanilla                   # neither: plain Vis with your providers
vis-agent tui --extensions gh,clj         # only these extensions
vis-agent tui --extensions -spel,-uplink  # every extension except these
vis-agent tui --extensions none           # no optional extension
```

| Flag | What the session leaves out |
| --- | --- |
| `--no-global` | `~/.vis/config.yml`, the settings in `~/.vis/state.yml`, `~/.vis/extensions/`, `~/.vis/AGENTS.md` and global skills |
| `--no-project` | `vis.yml`, `.vis/config.yml`, `.vis/extensions/`, `AGENTS.md` files and `.vis/skills` |
| `--vanilla` | Both of the rows above |

Providers, sign-in data, and your default and fallback models always stay.

The same flags work for a one-shot run, which starts with `-p` or `--print`, also with `--json-schema`:

```bash
vis-agent -p --vanilla --json-schema @city.json "Name the capital of Poland"
vis-agent -p --no-global --extensions -spel --persist "Fix the failing test"
```

In the terminal, the flags apply to each new session that the terminal opens. A saved session keeps
the choice that it started with. Do not combine the flags with `--session-id`, `--resume`, or
`--continue`.

An extension name is the name in its manifest. Do not mix names and `-names` in one list. Vis
refuses an unknown name before the session starts. A part of Vis without an **Auto**, **On**, and
**Off** setting always stays on.

For scripts, set `VIS_SOURCES` to `global`, `project`, or `none`, and set `VIS_EXTENSIONS` to an
extension list. A flag wins over its variable.

A session cannot change the files of a tier that it leaves out, and Vis refuses the change with a
409 error. To start such a session from a program, read [Create a
session](sessions-api.md#create-a-session).

### Choose single tools

Use `--tools` when a session needs an extension, but not all of its tools:

```bash
vis-agent tui --tools py.lint_code,py.repl_eval   # only these two tools of py
vis-agent tui --tools -py.repl_stop               # every tool except py.repl_stop
vis-agent tui --tools 'py.repl_*'                 # only the REPL tools of py
```

A tool name is the name that the agent calls, such as `py.lint_code`. A `*` matches any part of a
name, as in `py.repl_*`. A list of names limits only
the extensions that the names belong to, and other extensions keep all of their tools. Do not mix
names and `-names` in one list.

For a tool list, each MCP server is an extension of its own, and its tools have names such as
`linear.get_issue`. So `-linear.*` turns off each tool and prompt of `linear`, and no tool of
another server. Vis checks only the name of the server.

Vis refuses an unknown name before the session starts, and the error lists the tools of that
extension. For scripts, set `VIS_TOOLS` to a tool list for new sessions. The `--tools` flag wins
over the variable.

The choice applies only to tools of extensions with an **Auto**, **On**, and **Off** setting. A
tool that is off is not in the Python sandbox of the session. Vis refuses a call to it, also
through a saved handle. The name of an MCP server stays in the sandbox, and the prompt text of an
extension can still name the tool.

### Change the tools of a saved session

A saved session keeps its tool list when you resume it. To change the list, give `--tools` again
when you open the session:

```bash
vis-agent -p --session-id 6d78b3f2 --tools -py.repl_stop "Run the tests"
vis-agent tui --continue --tools py.lint_code,py.repl_eval
vis-agent tui --resume --tools '*'                # every tool on again
```

The new list replaces the saved list, and the session keeps it for later resumes. In the terminal,
the list applies to the first saved session that you open. That session is the session of
`--session-id` or `--continue`, or the session that you pick after `--resume`. `VIS_TOOLS` never
changes a saved session.

A program can change the list at any time, also during a turn. Read [Change the tools of a
session](sessions-api.md#change-the-tools-of-a-session). A busy session gets the new list at its
next step.

### Turn tools off for a project, group or session

When the same tools must stay off in every session of a scope, add a `tools` section to a global or
project file:

```yaml
# vis.yml
tools:
  deny:
    - py.repl_*          # no Python REPL tools
  allow:
    - clj.lint_code      # of clj, keep only these two tools
    - clj.run_tests
```

The entries use the names and the `*` of `--tools`. An `allow` list keeps only the named tools of
the extensions that it names. A tool that a later release adds to such an extension also stays off.
A `deny` list turns only the named tools off.

You can also set the lists in the **Allowed tools** and **Turned-off tools** rows of global,
project, group, and session settings. The rows suggest the tool names of the extensions of the
session. Vis refuses a name that matches no tool, and it writes nothing.

The lists of the configuration files, of each settings scope, and of `--tools` all apply. A more
specific scope cannot turn on a tool that a wider scope turns off. A session that another session
starts also gets the lists of that session.

New and resumed sessions get the lists at their start. A change during a session applies at its
next tool call.

## Providers and models

The UI has presets and sign-in for OpenAI, Anthropic, OpenAI Codex, GitHub Copilot, Z.AI, Ollama,
and LM Studio. For Anthropic, it has the API and the coding plan. In YAML, you can also declare an
endpoint that implements a supported API format, such as a local model:

```yaml
providers:
  - id: anthropic
    api_key: ${ANTHROPIC_API_KEY}
    models:
      - name: claude-sonnet-4-5-20250929
  - id: my-gateway
    compatibility: openai            # endpoint's API format
    base_url: https://gateway.example.com/v1
    api_key: ${LLM_TOKEN}
    models:
      - name: qwen3-coder-30b
        context: 262144          # input window
        output_limit: 32768      # max output tokens
        is_tool_calls_supported: true
  - id: my-responses-gateway
    compatibility: openai-responses
    base_url: https://gateway.example.com/v1
    api_key: ${GATEWAY_TOKEN}
    responses_path: /responses       # only when served off another path
    is_stateless: true               # load-balanced replicas
    models:
      - name: gpt-5.6
```

| Level | Keys |
| --- | --- |
| Provider | `compatibility`, `base_url`, `api_key`, `api_key_command`, `responses_path`, `api_style`, `llm_headers`, `extra_body`, `is_stateless`, `is_vision_supported` |
| Model | `context`, `input_limit`, `output_limit`, `is_tool_calls_supported`, `is_vision_supported`, `api_style` |

A provider with managed sign-in, such as Copilot or a coding plan, needs no `api_key`. Set the
limits of a model to make the context checks and the output cap accurate.

Vis also reads the old names `is_tool_call` and `is_image_input`. The new name wins, and Vis writes
only the new names.

Vis offers the models in the order that you list them, then the models that the provider reports.
Vis does not offer or save image, video, speech, or embedding models.

To add another provider, read [Provider extensions](provider-extensions.md).

### Set verified limits for a model

Before Vis sends a request, it checks the request against the input and context limits of the
model. Vis gets these limits from its model catalog or from the live model list of the provider. If
neither gives valid limits, the turn stops with **Model limits unavailable**, and Vis sends
nothing.

To continue, set the limits of that model in `vis.yml`. Use the values that the provider publishes
for your account. This example sets the limits of Claude Sonnet 5.5 on GitHub Copilot:

```yaml
providers:
  - id: github-copilot
    models:
      - name: claude-sonnet-5.5
        context: 1000000         # total window
        input_limit: 872000      # max input tokens
        output_limit: 128000     # max output tokens
```

Your values replace the discovered values of that model only. Keep the output limit below the
context. Keep the input limit at or below the context.

When the live model list gives no valid limits, the [gateway log](logging.md) gets a
`model-metadata-unavailable` warning. The warning shows the limit fields of the provider entry for
the model, or similar model IDs if no entry matches. It contains no credentials, so you can attach
it to a bug report.

### Reduce latency with Anthropic threads

If you require zero data retention (ZDR), turn threads off. Anthropic keeps encrypted conversation
data in memory for up to one hour after its last use. No API deletes a thread.

By default, Vis turns on Message Threads for conversations on the direct Anthropic API. To turn
threads off, set `extra_body.thread` to `null`. To turn them on again, remove the key.

```yaml
providers:
  - id: anthropic
    api_key: ${ANTHROPIC_API_KEY}
    extra_body:
      thread: null
```

Vis sends only new messages while a thread is available. After five idle minutes, or after
compaction, Vis creates a new thread from the full history. Prompt caching stays active. Token
prices and rate limits do not change.

Threads work with the direct Anthropic API, not with coding-plan OAuth or compatible proxies. Read
[Anthropic's Message Threads
documentation](https://platform.claude.com/docs/en/build-with-claude/message-threads).

### API format

`compatibility` selects the API format of the endpoint. `api_style` is the router name for the same
setting, and it wins when you set both. Set a value on a model only when the models of one endpoint
use different APIs.

| Format | Request | Aliases |
| --- | --- | --- |
| `anthropic` | `{base_url}/messages` | `claude`, `anthropic-messages`, `messages` |
| `openai` | `{base_url}/chat/completions` | `openai-chat`, `openai-compatible`, `chat`, `chat-completions` |
| `openai-responses` | `{base_url}` + `responses_path` | `responses`, `openai-compatible-responses` |
| `gemini` | Gemini `generateContent` | `google`, `google-gemini` |

Vis ignores case and reads `_` and `-` as one character. Vis refuses an unknown value when the
configuration loads. A `responses_path` without a format implies `openai-responses`.

Declare the API that you use. A gateway that serves both chat completions and Responses accepts
either API. But each API rejects tool-call IDs from the other, and the turn fails with a 400 error.

### Default and fallback

When you pick a model in the UI, Vis writes two keys:

```yaml
default_provider: zai-coding-plan
default_model: glm-5.3        # or one line: zai-coding-plan/glm-5.3
```

- The whole configuration has one default pair, not one pair for each provider.
- Vis looks up `default_model` in the catalog of that provider. For an unknown name, Vis uses the
  first model of the provider.
- Without a default, Vis uses the first provider and its first model.

For rate limits and provider failures, set a fallback provider and model:

```yaml
fallback_provider: anthropic-coding-plan   # must differ from default_provider
fallback_model: claude-sonnet-5
```

Vis tries the fallback provider immediately after the default. Other providers follow in the
configured order. When you log out of a provider, Vis clears its pair.

In a committed `<project>/vis.yml`, Vis ignores both pairs with a warning. Put them in
`~/.vis/config.yml`, `~/.vis/state.yml`, or the gitignored `<project>/.vis/config.yml`.

On the command line, `--model provider/model` selects both for one run and saves nothing. The
provider needs no configuration if it has a built-in preset with managed sign-in:

```bash
vis-agent -p --model zai-coding-plan/glm-5.3 "task"
vis-agent -p --model glm-5.3 "task"       # on the active provider
```

### Default thinking level

Sessions on a provider start with its default thinking level. Set the level in the menu of the
provider:

- In the Companion app, open **Settings**, then the row actions of the provider, and select
  **Default thinking**.
- In the terminal, open the provider list and select **Set Thinking Level...**.

The menu lists quick, balanced, and deep. With **Simplified thinking modes** off, it lists the exact
levels that the models of the provider offer. **Use the built-in level** removes the default. Vis
writes the choice to the entry of the provider:

```yaml
providers:
  - id: anthropic
    reasoning_level: deep       # quick, balanced or deep
  - id: openai
    reasoning_effort: high      # an exact level, used when simplified modes are off
```

The reasoning control in a session footer changes only that session. The global keys
`toggles.reasoning_level` and `toggles.reasoning_effort` have no effect.

### Environment references

A string value can use `${NAME}`. Vis does not recognise `$NAME`, and it does not interpolate map
keys.

An unset variable does not fail the load. The provider manager and `vis-agent doctor` report the
provider as unusable, and the router skips it. If you select that provider yourself, Vis returns an
error. When Vis saves configuration, a value that is one whole reference stays `${NAME}`, not the
resolved secret.

### Images

Attached pictures, matplotlib figures, and other `image/*` attachments go to the model only when
the model reads images. The model catalog decides for known models. To decide yourself, set
`is_vision_supported` on a model:

```yaml
providers:
  - id: my-gateway
    compatibility: openai
    base_url: https://gateway.example.com/v1
    models:
      - name: qwen2.5-vl-72b
        is_vision_supported: true     # reads images
      - name: qwen3-coder-30b
        is_vision_supported: false    # gets a text description of each picture
```

Use `true` for a local or private model that the catalog does not know.

`is_vision_supported: false` on a provider blocks images for all its models, also when a model says
`true`. A provider extension sets the same value with `ProviderModel(is_vision_supported=...)`.

### GitHub Copilot

Vis sends `X-Initiator: user` on the first call of each turn. It sends `X-Initiator: agent` on
tool-call continuations and on internal calls such as session titling. Copilot decides the billing,
and these headers do not guarantee a particular charge. To override them, set `X-Initiator` in
`llm_headers`. Vis sends a trivial message to a Claude model on Copilot, such as a greeting or a
thank-you, without a reasoning parameter.

### Evaluation runs

`--reasoning-effort LEVEL` sends one exact thinking level that the model offers, such as `high` or
`max`. It replaces the adaptive levels of Vis. The run exits with status `2` if the provider, the
model, or the value is not accepted, or if an iteration switched provider or model. The JSON output
includes an `eval` object that describes the run.

## System prompt

A string adds text to the built-in prompt:

```yaml
system_prompt: Prefer restructuredText docstrings. Do not edit generated/.
```

A map with `is_replace: true` replaces the built-in prompt:

```yaml
system_prompt:
  text: You are …
  is_replace: true
```

The files `.vis/SYSTEM.md` and `.vis/APPEND_SYSTEM.md`, in the project or in `~/.vis`, do the same
and win over these keys. Put repository conventions in `AGENTS.md`, not here. Read [System prompt
files](project-instructions.md#system-prompt-files).

## Environment

Vis loads the `.env` and `.env.local` files of the project for the shells, REPLs, test runners, and
extensions that it starts. The parser accepts `NAME=value`, `export NAME=value`, quotes, and
comments.

`environment:` declares variables that a dotenv file cannot, and it never holds a secret value:

```yaml
environment:
  OPENAI_API_KEY: {env: WORK_OPENAI_KEY}       # another process variable
  STRIPE_KEY: {dotenv: STRIPE_TEST_KEY}        # a dotenv entry under a new name
  EXA_API_KEY:
    keychain: vis-exa                          # macOS Keychain or secret-tool
    account: alice                             # optional
  GITHUB_TOKEN:
    command: [gh, auth, token]                 # trimmed stdout is the value
  VIS_MANAGED: {literal: "true"}               # non-secret marker
```

- An entry has exactly one source.
- A declared name never falls back to `.env` or to the environment that Vis started from. A blank
  value means unset.
- `literal` needs the wrapper. Vis refuses it for names that look like credentials: `*_KEY`,
  `*_TOKEN`, `*_SECRET`, and `*_PASSWORD`.
- Vis gets command and keychain values without a shell, caches them briefly, and never logs them.

Vis resolves a variable in this order everywhere: `environment:`, then `.env`, then `.env.local`,
then the environment that Vis started from.

With the jail on, child processes do not get the environment of the parent process, and
`{env: NAME}` includes one variable. With the jail off, child processes keep the full environment.
Vis always refuses `LD_*`, `DYLD_*`, `PERL*`, and `BASH_ENV`. Child processes never inherit
`NODE_OPTIONS` from the environment that Vis started from. If a project needs it, declare it under
`environment:`.

A shell call can add or override variables. Vis records literal values in the transcript, so use a
source reference for secrets:

```python
sh = await shell("npm test", {"env": {"NODE_ENV": "test"}})
sh = await shell("./deploy.sh", {"env": {"STRIPE_KEY": {"keychain": "vis-stripe"}}})
```

## Router

The `router` block sets retry delays, network timeouts, and spending limits. Omit the block to use
the defaults.

```yaml
router:
  rate_limit:
    same_provider_delays_ms: [2000, 3000, 6000]
    is_respect_retry_after: true
    is_fallback_provider: true    # may a rate-limited turn move to another provider
  network:
    timeout_ms: 300000
    idle_timeout_ms: 45000
  budget:
    max_tokens: 1000000
    max_cost: 5.0
```

## Jail, filesystem and network

The jail is off by default, and shells and language processes then run with your full permissions.
Turn it on when the model runs untrusted code. With `jail.enabled: true`, commands run under
Seatbelt on macOS or bubblewrap on Linux, and through the egress proxy of the gateway. An
unsupported host returns an error.

Declare directories in `workspace.filesystem`, then allow them by ID in `jail.filesystem.allow`.
The jail exposes no other root. To keep files out of every grant, list patterns under
`jail.filesystem.deny_read` or `jail.filesystem.deny_write`. The file tools of Vis obey these rules
with the jail on or off, but only `jail.enabled: true` keeps child processes out. Read [Deny
specific files](jail.md#deny-specific-files).

| Key | Meaning |
|---|---|
| `id` | Name used by the allow list and the UI |
| `path` | Absolute or `~`-relative directory |
| `description` | Optional. Tells the model what the root is for. |
| `python_name` | Optional Python variable for the path, for example `runtime_path` |
| `access` | `read-write` or `read-only` |
| `search` | Whether search indexes it |
| `when`, `optional` | Mount only on some hosts or when the path exists |

Each allowed, searchable root has a Python `Path` variable with the name of its directory, so
`vis-python-runtime` gets `vis_python_runtime_path`. `project_root_path` is the current project.
`~/.vis` is always allowed.

```yaml
# vis.yml
workspace:
  filesystem:
    - id: sibling
      path: ~/sibling-repository
    - id: reference
      path: ~/shared-reference
      access: read-only
    - id: npm
      path: ~/.npm
      description: Node build cache
      search: false
    - id: cuda
      path: /usr/local/cuda
      when:
        exists: /usr/local/cuda
    - id: scratch
      path: ~/scratch
      optional: true
jail:
  enabled: true
  environment: declared          # or inherit
  filesystem:
    allow: [sibling, reference, m2, cuda, scratch]
    deny_read:                   # patterns denied to every jailed reader
      - .env
      - "**/.env*"
    deny_write:
      - deploy/production
  keychain: true                 # let gh/git credential helpers reach the OS keychain
  network:
    allowed_domains:
      - github.com
      - npmjs.org
    denied_domains:
      - example.invalid
    allow_private: false
    inbound_ports:               # ports a confined server may listen on
      - 5273
```

`when.os` accepts `macos`, `linux`, `wsl`, or `windows`. `vis-agent doctor` reports an allowed path
that is missing.

With `jail.enabled: true`, Vis jails only the processes that it starts. A tool that attaches to a
running process cannot jail it, and a running process keeps the jail profile that it started with.
So if a native tool such as `bb` or `clj-kondo` fails with `CSunMiscSignal.open() failed` after an
upgrade, restart Vis. For the full policy, the network rules, and refusal diagnosis, read [Process
jail and network policy](jail.md).

Shared Python installs use `~/.vis/python/packages`, and `VIS_PYTHON_PACKAGES` overrides only that
location. Project installs stay in their uv environment. Bytecode caches use
`~/.vis/python/pycache`, or the path in `VIS_PYTHON_PYCACHE_PREFIX`. `VIS_PYTHON_HOME` and
`VIS_PYTHON_NATIVE_PATH` select another runtime. Vis reads these variables at startup.

## Python TLS validation

Both `~/.vis/config.yml` and the project `vis.yml` accept this setting, with the normal merge
order. The default is `true`, which keeps the TLS validation of Python:

```yaml
python:
  tls_strict: true
```

Set it to `false` only for a trusted corporate CA that fails strict X.509 checks. For example, a CA
fails these checks when its `Basic Constraints` extension is not marked critical:

```yaml
python:
  tls_strict: false
```

`false` clears only `ssl.VERIFY_X509_STRICT`. Python still verifies the trust chain, the certificate
signature, the expiry, and the host name. The setting adds no trusted certificate and does not retry
a failed handshake without verification. STRICT covers several X.509 requirements, so `false` is a
security trade-off, not a certificate repair. Prefer a correctly issued CA when your administrator
can provide one.

The merged value applies to `py3`, trusted Python extensions, and `vis-agent python`,
with or without `--shared`. Language extension REPLs that run `vis-agent python` get it too. It
affects the `ssl` contexts of Python, also those that stdlib clients and libraries create with
`ssl.SSLContext`. It does not affect JVM TLS, `gh`, `curl`, uv, external Python processes, or
libraries with a different TLS implementation.

Vis also sets `VIS_PYTHON_TLS_STRICT` to `true` or `false` in the environment of that Python
program. After you change the value, run `/reload` to rebuild the session workers. If the gateway
already runs a worker that registers extensions, restart Vis. Existing connections do not change.
Use the YAML boolean `false`, not the string `"false"`.

## Python package index

Set the primary package index of the embedded runtime in `vis.yml`:

```yaml
python:
  index_url: https://gateway.example.com/simple
```

The setting applies to pip installs that Vis manages, to `vis-agent python uv`, and to the automatic
preparation of extension projects.

- **pip.** `index_url` overrides the primary index from `PIP_INDEX_URL` or `pip.conf`.
- **uv.** Vis sets `UV_DEFAULT_INDEX` when neither `UV_DEFAULT_INDEX` nor `UV_INDEX_URL` is set.
  For a command-line override, use `--default-index`. The deprecated `--index-url` and `-i` do not
  override `UV_DEFAULT_INDEX`. uv still manages named indexes and package source pins.

Without `index_url`, both installers keep their inherited settings. Other installer settings, such
as extra indexes, proxies, and certificates, do not change. Prefer one company virtual index that
serves both private and public packages. Extra indexes are not ordered fallback sources, and they
can cause dependency confusion.

Use a literal HTTP or HTTPS URL without credentials, a query string, or a fragment. Keep
authentication out of committed YAML, for example in the `.netrc` of the gateway user. An invalid
value stops the installation, and Vis does not fall back to a public index. The setting does not
reinstall packages that are already installed.

## Python import roots

After `vis-agent python uv sync --project .`, run your package from the same project directory with
`vis-agent python -m your_package`. Before Python starts, the standalone CLI selects one dependency
environment:

- **Project mode.** A `.venv` or a `pyproject.toml` in the current directory, or
  `UV_PROJECT_ENVIRONMENT`, selects project mode. Only that environment supplies installed
  packages, distribution metadata, and editable `.pth` files or import hooks. The CLI does not load
  shared Vis packages or their startup hooks.
- **Shared packages.** Without a project, the CLI uses the shared packages in
  `~/.vis/python/packages`, or in `VIS_PYTHON_PACKAGES`.
- **Shared packages inside a project.** `vis-agent python --shared -m your_tool` selects shared
  packages. It skips project activation and the configured or inferred source roots. An explicit
  `PYTHONPATH` still applies.

`UV_PROJECT_ENVIRONMENT` selects another project environment. A relative path starts at the current
directory. On startup, the CLI does
not search parent directories, create an environment, or sync dependencies. A `pyproject.toml` with
no prepared environment reports a sync error.

The CLI uses the embedded Python of Vis, so the environment must match its Python version and
platform. If the matching site-packages directory is missing, the CLI reports a diagnostic. In
neither case does the CLI fall back to shared packages. To use the interpreter of the project, run
`vis-agent python uv run --no-sync python -m your_package`.

Activation happens inside the sandbox, so editable source and custom environments must be inside
allowed filesystem roots. Activate only environments that you trust, because editable import hooks
can run code. The standalone CLI does not add the environment of the project to agent
`py3`.

`vis-agent python` also puts declared source roots on `sys.path`. So
`vis-agent python -m pytest tests/` imports a `src/` layout without `PYTHONPATH`. Vis reads the
roots from `pyproject.toml`, `setup.cfg`, `pytest.ini`, and `tox.ini`. In `pyproject.toml`, it
reads the settings of setuptools, pdm, poetry, and hatch, and the pytest `pythonpath`. Without this
metadata, Vis infers no roots. To declare roots yourself:

```yaml
# vis.yml
python:
  source_paths: [src, lib/vendor, ~/shared/py]
```

`PYTHONPATH` comes first, then the configured paths, then the inferred paths. All these roots come
before the packages of the selected environment. An [editable package
install](extension-development.md) supplies its own import roots through `.pth` files or backend
hooks, so it needs no layout override. Import roots do not grant filesystem permissions or install
dependencies.

## Python extension environment

By default, each uv project of a project extension gets its own environment, normally `.venv`. If
your project installs its dependencies with `vis-agent python --shared uv sync`, use the shared
packages for its extensions too:

```yaml
# vis.yml
python:
  environment: shared
```

The value is `project`, the default, or `shared`. With `shared`, the workers of the project
extensions import `~/.vis/python/packages`. `py.run_tests` and `py.repl_start` then run project
Python with `vis-agent python --shared`, and they do not use the `.venv` of the project.

Run `/reload` after you change the value. Extensions read the value with
[`vis.python_environment()`](extension-api.md#python-environment). For the steps and the limits,
read [Use shared packages for project
extensions](extension-development.md#use-shared-packages-for-project-extensions).

All extensions of a session run in one Python worker. Each extension imports its own environment
first, but the extensions share one interpreter and its loaded modules. When two extensions need
different versions of one package, both use the version that loads first. Give such extensions
compatible dependencies.

## MCP servers

Servers that you add from the UI, the API, or `vis-agent mcp add --scope user` go to
`~/.vis/state.yml`. Vis also uses servers that you declare by hand in another file, but the UI
cannot edit them. Change the file that declares them.

### Add a server from the CLI

`vis-agent mcp` has the commands and arguments of `claude mcp` in Claude Code: `add`, `add-json`,
`add-from-claude-desktop`, `get`, `list`, and `remove`. Put `--` before the command of a local
server and its arguments.

```bash
vis-agent mcp add --transport http notion https://mcp.notion.com/mcp
vis-agent mcp add --transport sse asana https://mcp.asana.com/sse
vis-agent mcp add --env AIRTABLE_API_KEY=<KEY> airtable -- npx -y airtable-mcp-server
vis-agent mcp add-json weather '{"type":"stdio","command":"weather-mcp"}'
vis-agent mcp list
vis-agent mcp get notion
vis-agent mcp remove notion
```

The `--scope` option, or `-s`, selects where Vis keeps the server:

| Scope | File | Who uses the server |
|---|---|---|
| `local`, the default | `.vis/config.yml` in the current directory | You, in this project |
| `project` | `.mcp.json` in the current directory | All persons who use the project |
| `user` | `~/.vis/state.yml` | You, in all your projects |

When two scopes have the same name, `local` wins over `project`, and `project` wins over `user`.
In the project scope, a server in `vis.yml` wins over the same name in `.mcp.json`.

Vis reads `.mcp.json` in the Claude Code format, so a project that has this file for Claude Code
also works in Vis. Vis starts each server of `.mcp.json`. A stdio server runs a command of the
repository, so read `.mcp.json` before you start Vis in a repository that you do not trust.

A server that signs in with a pre-registered OAuth client takes these options:

```bash
vis-agent mcp add --transport http --client-id <CLIENT_ID> --client-secret \
  --callback-port 8080 my-server https://mcp.example.com/mcp
```

- `--client-secret` asks for the secret, or reads `MCP_CLIENT_SECRET`. Vis keeps the secret in
  `~/.vis/mcp-tokens`, not in a configuration file. `remove` deletes it. The gateway that signs
  in reads this folder, so Vis refuses the option with a remote gateway.
- `--callback-port` sets `auth.callback_port`. The sign-in then returns to
  `http://localhost:<PORT>/callback`, the address that you registered for the client.

`vis-agent mcp serve` runs Vis as an MCP server on standard input and output. Another MCP client,
such as Claude Desktop, can then use the tools of Vis:

```json
{"mcpServers": {"vis": {"command": "vis-agent", "args": ["mcp", "serve"]}}}
```

With `-d`, `--debug` or `--verbose`, `serve` writes each message to standard error.

A refused `vis-agent mcp` command ends with exit code 2, like each usage error of Vis. Claude Code
uses exit code 1. `--scope group` and `--scope session` with `--target-id` add a server to one
group or one session. Read [Scoped MCP servers](#scoped-mcp-servers).

### Connections and transports

The gateway keeps one connection for each enabled server, and all sessions share it. The gateway
reconnects a connection that crashed. **Kill** closes the connection until you select **Start** or
the gateway restarts. To keep a server off after a restart, set `enabled: false`.

Vis connects to a server that has a URL with Streamable HTTP. When an older server refuses that
request, Vis uses the older HTTP+SSE transport on the same URL. To use only HTTP+SSE, set
`transport: sse`.

Vis works with both generations of the MCP specification, with no setting. It first asks the server
which protocol versions it supports. A server of revision `2026-07-28` answers, and Vis then sends
each request on its own, without a session. Any other server gets a session. A local server that
does not answer within one second also gets the older start request.

If a server changes its generation while Vis runs, for example after an upgrade, Vis connects again
and sends the refused request one more time.

Vis keeps one stream open to each Streamable HTTP server. The server sends changes, such as a new
tool list, and its questions on it. To turn the stream off, set `listen: false`.

### Questions, roots, and sampling

A server can ask you for input in the middle of a tool call. Vis shows the question as a form in
the terminal and in the Companion app, and the tool call waits. Select **Send** to answer or
**Decline** to refuse. For how forms work, read [Forms and user input](human-input.md).

A local server gets the folders of your session as MCP roots: the workspace, and each other folder
that the session can search. A server at a URL does not get them. To change this for one server, set
`roots: true` or `roots: false`.

A server can also ask a model for an answer. MCP calls this request sampling, and it is off by
default. Set `sampling: true` for a server that you trust:

```yaml
mcp:
  servers:
    research:
      transport: stdio
      command: research-mcp
      sampling: true
```

Vis then shows you each request before a model sees it. Select **Allow** to send it or **Decline**
to refuse it. An allowed request costs tokens, and Vis sends only its text to a low-cost model.

The MCP specification deprecates roots and sampling from revision `2026-07-28`. Both still work
with present servers.

### Long tool calls

A server of revision `2026-07-28` can answer a slow tool call with a task. Vis then asks the server
for the state of the task at the interval that the server gives, from 100 ms to 60 seconds. The tool
call waits until the task completes, fails, or is cancelled. A question of the task shows as a form,
as in other tool calls. When you stop the turn, Vis tells the server to cancel the task. The server
can still complete the work.

### Tools, resources, and prompts

A server can have tools, resources, and prompts. The agent can list and read all of them. Ask in
plain words, for example: "Read the resource `file:///logs/app.log` of the server `files`."

- The agent calls a tool by the name of the server and the name of the tool.
- The agent reads a prompt as a help page. A prompt with no arguments comes with its text.
- The agent sees the resources of each connected server in its list of attachments. It reads a
  resource as it reads a file that you attached.

The agent can read the help page of a tool before the server is connected, and Vis then connects the
server.

### Sign in to a server

The gateway starts an OAuth flow when an HTTP MCP server asks for it with status `401`. In the
terminal, use **MCP Servers**. From the CLI, use these commands:

```bash
vis-agent mcp add --scope user --transport http linear https://mcp.linear.app/mcp
vis-agent mcp auth-start linear
# open the printed URL, approve, then paste the redirect URL or code:
vis-agent mcp auth-complete linear --flow-id <FLOW_ID> --input "<URL_OR_CODE>"
vis-agent mcp list
```

A static token skips sign-in:

```bash
vis-agent mcp add --transport http linear https://mcp.linear.app/mcp \
  --header "Authorization: Bearer <TOKEN>"
```

By default, Vis uses `Claude Code` as its OAuth registration name for all MCP servers. To send a
different name for one server, set `auth.client_name`. Figma needs no name override:

```yaml
mcp:
  servers:
    figma:
      transport: streamable_http
      url: https://mcp.figma.com/mcp
```

`auth.client_name` changes only the OAuth registration name, not the MCP handshake name. If you are
signed in, sign out and sign in again to register with the new name. The server can still refuse
registration.

If the provider gives you a registered client, set its identifier and secret. With
`auth.client_id`, Vis skips registration and does not send `auth.client_name`:

```yaml
mcp:
  servers:
    figma:
      transport: streamable_http
      url: https://mcp.figma.com/mcp
      auth:
        client_id: <CLIENT_ID>
        client_secret: ${FIGMA_CLIENT_SECRET}
```

Vis does not accept a secret as plain text. When sign-in fails, the error names the failed step,
the HTTP status of the server, and the reason that the server gives.

Some authorization servers read a client document and need no registration. To these servers, Vis
sends the URL of its own document, `https://vis.blockether.com/oauth/mcp-client.json`, as the
client identifier. The consent screen then shows the name `Vis`. Set `auth.client_metadata_url` to
the URL of your own document, or to `false` to register a client:

```yaml
mcp:
  servers:
    figma:
      transport: streamable_http
      url: https://mcp.figma.com/mcp
      auth:
        client_metadata_url: false
```

Vis uses `auth.client_id` first, then a client document, then registration.

### Get a token without a browser

Some servers give a token to a program, with no sign-in in a browser. Set `auth.grant` for such a
server:

| `auth.grant` | Use it when | Also set |
|---|---|---|
| `client_credentials` | The provider gave you a client for a service. | `client_id`, and `client_secret` or `private_key_file` |
| `jwt_bearer` | Your platform gives the workload a signed token. | `assertion_file` |
| `enterprise_managed` | The identity provider of your company decides the access. | `idp_issuer` or `idp_token_endpoint`, and `idp_id_token_file` |

```yaml
mcp:
  servers:
    reports:
      transport: streamable_http
      url: https://mcp.example.com/mcp
      auth:
        grant: client_credentials
        client_id: <CLIENT_ID>
        private_key_file: /etc/vis/reports-key.pem
```

With a private key, Vis signs in with a signed assertion and sends no secret. The key selects the
algorithm, `ES256` or `RS256`. Vis reads a token file again for each token request. For
`enterprise_managed`, also set `idp_client_id` and `idp_client_secret` when the identity provider
needs a client.

Each of `private_key`, `assertion`, `idp_client_secret`, and `idp_id_token` takes only an
environment reference, such as `${REPORTS_KEY}`. When the server refuses the grant, the server
shows as unhealthy and the error gives the reason of the server.

Some servers bind a token to a key of the client, so that a stolen token does not work elsewhere.
This method is DPoP, and Vis uses it when the server asks for it. Set `auth.dpop: true` to always
ask for such a token, or `auth.dpop: false` to never ask for one.

### Server names and other commands

Vis keeps server names in one form, with lowercase letters, digits, and hyphens. `Figma Dev`
becomes `figma-dev`. Vis refuses a name without a letter or a digit.

`vis-agent mcp` has these other commands: `test`, which connects and saves nothing, `enable`,
`disable`, `kill`, `start`, `auth-poll`, `auth-cancel`, and `auth-logout`. `test` takes the
arguments of `add`. When you save
a server without `env` or `headers`, Vis keeps the stored values.

## Feature toggles

Run `/reload` after you edit the `toggles` block.

```yaml
toggles:
  shell: false          # default true; removes shell(...) from the sandbox
  introspection: true   # default false; lets the agent read its own session data
  council: true         # default true; classified project messages, replies and explicit pings
  automations: true     # default false; lets schedules and webhooks start turns (see automations.md)
```

## Machine settings

Settings for one machine have these sections:

- **General** has the agent name, Council with Subagents, apps, sounds, voice, notifications, and
  experimental features.
- **Providers** has provider accounts and options for model requests, such as automatic fallback.
- **Permissions** has files, network, process access, and sandbox tools.
- **Tools** has MCP servers and extensions.
- **Automations** has prompts that run on a schedule or a webhook.

The terminal shows the same sections. Notifications, speech engines, and automations are only in
the Companion app.

In the terminal, the **Providers** header has an **Add provider** button. Below it, **Configured
providers** lists your accounts and **Configuration** holds the provider options. MCP servers have
their own **MCP servers** section after **Tools**, with an **Add** button in its header.

## Project, group and session settings

To open the settings of a scope, do one of these steps:

- In the Companion app, select **Settings** with the cog icon in the **…** menu of a session. For a
  project or a group, select **Settings** in its actions.
- In the terminal, use **Session settings**, **Group settings**, or **Project settings** from the
  command palette.

Each row shows the effective value and where it comes from. **Use inherited value** removes only
the override at the scope that you opened.

### Which value applies

Vis resolves each setting in the order global, project, group, session. A more specific value wins,
and a scope with no value does not count. `false` is an explicit value, not inheritance. So a
session that turns on a setting keeps it when its group turns the setting off.

When a more specific scope decides a row for the open session, Settings locks that row and names the
scope. For example, the `vis.yml` of your project sets `toggles.shell: false`. While a session of
that project is open, the global **Shell commands** row is locked. Turn the row on in **Project
settings**.

Each scope saves its changes in its own place:

- **Global** is this gateway, which all its connected clients share.
- **Project** changes go to the `.vis/config.yml` of the project. That file overrides `vis.yml`,
  and the committed `vis.yml` does not change.
- **Group and session** overrides go to the gateway database.

A session that you move keeps its own values and follows its new ancestors. New sessions and forks
inherit from their ancestors, not from the overrides of the source session.

Provider setup, credentials, OAuth, and the start and stop of shared MCP servers stay global. The
theme and other device preferences stay local to the Companion app or the terminal. Project access
settings cannot expand the global access policy of the host. Group and session settings can, for
example to turn off the process jail for one session.

### When a change applies

Paths and access rows use guided editors, and their changes apply on the next turn. To edit
configuration files directly, use a text editor outside the Companion app or the terminal.

Vis captures reasoning, verbosity, thinking summary, and fast mode when you submit a message. Later
edits do not change running or queued responses.

Skill and MCP availability applies to the next lookup or call, also to a call by a name that the
agent already knows. An external call that already runs can finish. Turn a skill off globally or
locally to remove it from discovery, `doc()`, prompt inventories, and slash entry points. The
change does not erase text that the agent already read, and it does not deny filesystem access.

### Thinking modes

**Simplified thinking modes** is on by default. Each tap or key press then moves the reasoning
control to the next of quick, balanced, and deep. Turn the setting off to choose from a list of
every thinking level that the current model offers. Vis saves that choice as `reasoning_effort`.
After a model change, each turn sends the nearest level that the new model offers.

The setting is in the **Application** section of **Settings** in the Companion app, and in the top
section of **Settings** in the terminal. A new session starts with the [default thinking
level](#default-thinking-level) of its provider.

### Extension activation

Optional tool extensions have an engine setting. **Auto** uses the applicability check of the
extension. **On** keeps the extension active. **Off** hides its tools and rejects new calls, also
through saved handles. You cannot switch core, provider, or channel infrastructure here.

The catalog of a scope shows only the settings that are declared for that scope, with one exception.
An extension setting that a wider scope decides shows as read-only. Its row names that scope and
opens its settings. Machine settings also name the project extensions of the current session. Reload
them in Project settings.

### Scoped MCP servers

Use the MCP panel in the same scoped dialog to add a server definition or to control its
availability.

- A local definition can shadow an inherited name and does not edit its parent. **Use inherited**
  removes only the local definition.
- A local definition cannot store environment credentials, HTTP headers, or authentication
  settings.
- When you turn off availability for one session, the shared connection does not stop.
- When you turn on availability, a server that the global administrator turned off does not start.

The CLI accepts a scope and a target for `list`, `add`, `remove`, `enable`, and `disable`:

```bash
vis-agent mcp list --scope session --target-id <session-id>
vis-agent mcp add --transport http docs https://gateway.example.com/mcp --scope group --target-id <group-id>
vis-agent mcp disable docs --scope session --target-id <session-id>
vis-agent mcp remove docs --scope group --target-id <group-id>
```

Without a target, `enable` and `disable` manage global servers. A project target accepts the project ID or its canonical
root. Group and session targets use their IDs. Authentication and lifecycle commands are global
only.

### Extension settings

The **Extensions** part of the **Tools** section holds the settings of all extensions. Each
extension with settings has its own group under its name. The `project` tag marks an extension of
the current project, and the `global` tag marks a machine extension. Extensions that ship with Vis
have no tag.

Settings never runs extension code when it opens. After you add, change, or remove an extension
file, reload the extensions:

- In the terminal, select **Reload** in the **Extensions** header line.
- In the Companion app, select the round arrows next to the **Extensions** heading.

Vis runs the extension files again, then reads the settings list again. In global settings, the
reload covers machine extensions, and Vis tells you to use project settings or `/reload` for project
extensions. In project, group, or session settings, the reload also covers the extensions of that
project. Vis then shows each scope with its directory and how many extensions loaded and failed
there. If a directory has no extensions, Vis says that it found none there.

If an extension fails to load, its group stays in the list and shows the error. If a reload fails
after an earlier load, Vis keeps the earlier version, and the group says so. Your stored values do
not change when an extension fails, goes away, or loads again.

Some extensions ask for a password or an access token. Settings shows such a secret setting only as
**Saved** or **Not set**, never its value:

- To save or change the value, select **Set** or **Replace**, type the value, and save.
- To delete the saved value, select **Remove** and confirm.

Vis keeps a secret value in `~/.vis/state.yml` on this computer and only in global settings. Only
your user account can read that file. A project `vis.yml` cannot change the value. Saved
trajectories show `[REDACTED]` in its place.

## Session titling

Vis names a session from its first message immediately. After the turn finishes, one short model
call improves the name.

```yaml
titling:
  mode: llm             # llm | first_sentence | first_words | disabled
  provider: zai-coding-plan   # optional: pin the title call
  model: glm-4.7
```

## Gateway pairing address

The pairing link for the Companion app has an address that Vis finds on this computer. When your
network needs another address, such as a port forward, a proxy, or the one address that it allows,
name that address here. Every pairing link then puts it first:

```yaml
gateway:
  advertise: 10.0.0.5
```

The value is a host, `host:port`, or a full URL. The detected addresses still follow in the same
link as fallbacks. `--advertise` on `vis-agent gateway start` or `vis-agent gateway pair` wins over
`VIS_GATEWAY_ADVERTISE`, and that variable wins over this key.

A gateway that launchd or systemd starts can read the key, because a service unit starts without
your shell profile. The key changes what the link says, not where the gateway listens. The address
must still reach this computer.

`gateway.proxy` lets the Companion app open links to company hosts through the gateway. To set it,
read [Private links](private-links.md).

## Database

Vis stores sessions in SQLite. It resolves the location in this order: the `--db` flag,
`VIS_DB_PATH`, `db_spec`, then `~/.vis/vis.mdb`. `--db :memory` uses an in-memory database.

```yaml
db_spec:
  backend: sqlite
  path: /somewhere/else/vis.db
```

## Grep

Search always obeys `.gitignore`. To search a gitignored subtree, such as vendored repositories,
include it again here:

```yaml
# vis.yml
grep:
  include_gitignored_paths: [repositories/]
```

The `include_gitignored_paths` and `always_exclude` lists use `.gitignore` pattern syntax. Omit
`always_exclude` to use the defaults. The defaults are `.git/`, `node_modules/`, `target/`,
`build/`, `dist/`, `__pycache__/`, `.venv/`, `.gradle/`, `vendor/`, `.next/`, `out/`, `.m2/`,
`.shadow-cljs/`, `cljs-runtime/`, `.cpcache/`, `.clj-kondo/`, `.calva/`, and `.lsp/`. If you set
`always_exclude`, it replaces that list and does not add to it. Run `/reload` after you edit these
lists.

## See also

- For the `jail` block in full, read [Process jail and network policy](jail.md).
- For `AGENTS.md`, `SYSTEM.md`, and prompt templates, read [Project
  instructions](project-instructions.md).
- To add your own providers, tools, and toggles, read [Extending Vis](extending.md).
- To connect clients and resolve token errors, read [Running a gateway](gateway-service.md).
- To read or change settings from a program, read [Configuration API](configuration-api.md).
