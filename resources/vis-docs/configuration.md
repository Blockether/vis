# Configuration

Most settings are managed from the terminal UI or the Companion app. Edit YAML
when you want settings shared across projects or checked into a repository.

## When to use

- **You want to use a model from OpenAI, Ollama, another provider or your own
  endpoint, or set its API key.** Add it from the app in [Quick setup](#quick-setup),
  or declare it in YAML under [Providers and models](#providers-and-models).
- **Your team should share the same settings.** Commit a project `vis.yml`.
  [Configuration files](#configuration-files) shows which file wins when several set
  the same key.
- **One session must not use your global or project setup, or some extensions.** Start it with
  the flags in [Leave out global or project configuration](#leave-out-global-or-project-configuration).
- **One session or group needs different behavior.** Open its
  [scoped settings](#project-group-and-session-settings) instead of changing the
  gateway defaults.
- **A provider hits rate limits or fails, or a task should stay within a budget.**
  Set a [fallback model](#default-and-fallback), and set retries and token and cost
  limits under [Router](#router).
- **New sessions think too much or too little on one provider.** Set its
  [default thinking level](#default-thinking-level).
- **Commands that Vis runs need your project's environment variables.** Put them in
  `.env`, as described in [Environment](#environment).
- **Vis should use tools from an MCP server.** Add the server as described in [MCP
  servers](#mcp-servers).
- **Search skips a directory that `.gitignore` excludes, such as vendored
  repositories.** Include it again under [Grep](#grep).
- **Python HTTPS requests fail a strict certificate check, or packages must come
  from a private index.** See [Python TLS validation](#python-tls-validation) and
  [Python package index](#python-package-index).
- **Your own program must read or change settings.** Read [Configuration API](configuration-api.md).

Use [Project instructions](project-instructions.md) for rules about working in your
codebase, and the [process jail](jail.md) to limit what commands can access.

## Quick setup

In the terminal, open the provider picker and choose **Add Provider**. In the Companion app, use
**Settings → Providers → Add provider**. Sign in, then pick a model.

For API keys, local models and custom endpoints, see
[Providers and models](#providers-and-models). For project instructions, see
[Project instructions](project-instructions.md).

## Coding-agent name

Set `agent_name` in the project's `vis.yml`:

```yaml
agent_name: Ada
```

The default name is `Vis`. You can also change it in **Settings → General → Agent
name** in the TUI, or **Settings → your gateway → General** in Companion. Saving in
Settings writes to the gateway's `~/.vis/state.yml`, not the client's disk. This
gateway-wide name overrides project names. Remove the key from `state.yml` to
use project defaults again.

A name can have up to 80 characters. It cannot be blank or contain control characters. Vis removes
spaces at the start and end.

A change in Settings updates open sessions immediately, also on remote clients. If you edit the YAML
file by hand, reopen the session to show the new name. The name changes the next default system
prompt. It does not change product branding, session titles or a full custom prompt, which keeps its
own identity.

Programs read and save the name like any other setting, with the [Configuration
API](configuration-api.md#change-one-setting). An invalid name returns 400 without changing the
saved value. The settings list shows the name as a `string` row in the `general` group. Session details
include the resolved `agent_name`, so reconnecting clients receive the current value.

## Configuration files

Files are read in this order. Later files override earlier ones. Nested maps
merge, scalars and lists are replaced. The gateway-wide `agent_name` saved in
`state.yml` is an exception: it overrides the project tiers. `extensions` merges
by package name, replacing each complete declaration rather than mixing source
and version fields from different files. See [declarative extension packages](extension-packages.md#declare-packages-in-configuration)
for global/project installation scopes and `vis-agent extension sync --trust`.

| File | Purpose |
| --- | --- |
| `~/.vis/config.yml` | Your global settings |
| `~/.vis/state.yml` | Settings and credentials that Vis writes. Change them in the UI. |
| `<project>/vis.yml` | Settings shared with the project |
| `<project>/.vis/config.yml` | Local project overrides, usually gitignored |

Global configuration in `~/.vis` accepts `config.yml`, `config.yaml`, `vis.yml` or `vis.yaml`. The
project root accepts `vis.yml` or `vis.yaml`. The `<project>/.vis` directory accepts `config.yml` or
`config.yaml`. The project is the directory where you start `vis-agent`.

Keys are `snake_case` strings. Boolean keys start with `is_`. Unknown keys are
rejected, and an invalid file prints every offending path and exits with status
2 instead of starting:

```text
Invalid Vis configuration in /project/.vis/config.yml:

  - grep.include-gitignored-paths: unknown key (config is closed) — did you mean "grep.include_gitignored_paths"?
  - mcp.servers.docs.transport: value rejected by the transport contract
```

Model names are free-form and are not validated. A wrong one fails at the
provider.

A small config:

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

Start the terminal or a one-shot run with a flag when a session must not use part of your setup:

```bash
vis-agent tui --no-global                 # only the project configuration
vis-agent tui --no-project                # only your global configuration
vis-agent tui --repro                     # neither: plain Vis with your providers
vis-agent tui --extensions gh,clj         # only these extensions
vis-agent tui --extensions -spel,-uplink  # every extension except these
vis-agent tui --extensions none           # no optional extension
```

| Flag | What the session leaves out |
| --- | --- |
| `--no-global` | `~/.vis/config.yml`, the settings in `~/.vis/state.yml`, `~/.vis/extensions/`, `~/.vis/AGENTS.md` and global skills |
| `--no-project` | `vis.yml`, `.vis/config.yml`, `.vis/extensions/`, `AGENTS.md` files and `.vis/skills` |
| `--repro` | Both of the rows above |

Providers, sign-in data and your default and fallback models stay in every case. Without them, a
session has no model.

The same flags work for a one-shot run, also with `--json-schema`:

```bash
vis-agent --repro --json-schema @city.json "Name the capital of Poland"
vis-agent --no-global --extensions -spel --persist "Fix the failing test"
```

In the terminal, the flags apply to each new session that it opens. A saved session keeps the choice
that it started with. Do not combine the flags with `--session-id`, `--resume` or `--continue`.

An extension name is the name in its manifest. A list of names keeps only those extensions. A list
of `-names` turns only those off. Do not mix the two forms in one list. Vis refuses an unknown name
before the session starts. Parts of Vis without an Auto/On/Off setting always stay on.

For scripts, set `VIS_SOURCES` to `global`, `project` or `none`, and set `VIS_EXTENSIONS` to an
extension list. A flag wins over its variable.

A session cannot change the files of a tier that it leaves out. Vis refuses the change with a 409
error. To start such a session from a program, see [Sessions API](sessions-api.md#create-a-session).

## Providers and models

The UI provides presets and sign-in for OpenAI, Anthropic (API and coding
plan), OpenAI Codex, GitHub Copilot, Z.AI, Ollama and LM Studio. You can also
configure endpoints that implement a supported API format, including local
models:

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
        is_tool_call: true
  - id: my-responses-gateway
    compatibility: openai-responses
    base_url: https://gateway.example.com/v1
    api_key: ${GATEWAY_TOKEN}
    responses_path: /responses       # only when served off another path
    is_stateless: true               # load-balanced replicas
    models:
      - name: gpt-5.6
```

Provider keys: `compatibility`, `base_url`, `api_key`, `api_key_command`,
`responses_path`, `api_style`, `llm_headers`, `extra_body`, `is_stateless`,
`is_image_input`. Providers with managed sign-in (Copilot, coding plans) need
no `api_key`.

Model keys: `context`, `output_limit`, `is_tool_call`, `api_style`. Filling in
the limits makes context checks and output capping accurate.

Models are offered in the order you list them. Models discovered from the
provider are appended after them.

### API format

`compatibility` selects the endpoint's API format. `api_style` is the router's
name for the same setting and takes precedence when both are set. Use a
per-model value only when models on one endpoint use different APIs.

| Format | Request | Aliases |
| --- | --- | --- |
| `anthropic` | `{base_url}/messages` | `claude`, `anthropic-messages`, `messages` |
| `openai` | `{base_url}/chat/completions` | `openai-chat`, `openai-compatible`, `chat`, `chat-completions` |
| `openai-responses` | `{base_url}` + `responses_path` | `responses`, `openai-compatible-responses` |
| `gemini` | Gemini `generateContent` | `google`, `google-gemini` |

Case, `_` and `-` are normalised. An unknown value is rejected when the config
loads. A `responses_path` without a format implies `openai-responses`.

Declare the API that you actually use. A gateway that serves both chat completions and Responses
accepts either API. But each API rejects tool-call IDs from the other, and the turn fails with a 400
error.

### Default and fallback

Picking a model in the UI writes two keys:

```yaml
default_provider: zai-coding-plan
default_model: glm-5.2        # or one line: zai-coding-plan/glm-5.2
```

- There is one default pair for the whole config, not one per provider.
- `default_model` is looked up in that provider's catalog. An unknown name
  falls back to the provider's first model.
- Without a default, the first provider and its first model are used.

Set a fallback provider and model for rate limits and provider failures:

```yaml
fallback_provider: anthropic-coding-plan   # must differ from default_provider
fallback_model: claude-sonnet-5
```

The fallback provider is tried right after the default. Other providers follow
in configured order. Logging out of a provider clears its pair.

Both pairs are per user. They are ignored with a warning in a committed
`<project>/vis.yml`. Put them in `~/.vis/config.yml`, `~/.vis/state.yml` or the
gitignored `<project>/.vis/config.yml`.

On the command line, `--model provider/model` selects both for one run without
saving anything. The provider does not have to be configured if it has a
built-in preset with managed sign-in:

```bash
vis-agent --model zai-coding-plan/glm-5.2 "task"
vis-agent --model glm-5.2 "task"          # on the active provider
```

### Default thinking level

Each provider can have its own default thinking level. Sessions on that provider start
with it. To set it, open the provider's menu:

- In the app, open **Settings**, then the provider's row actions, and choose **Default thinking**.
- In the terminal, open the provider list and choose **Set Thinking Level...**.

The menu lists quick, balanced and deep. With **Simplified thinking modes** off, it lists the
exact levels that the provider's models offer. Choose **Use the built-in level** to remove the
default. Vis then uses its built-in level. The choice goes to the provider's entry:

```yaml
providers:
  - id: anthropic
    reasoning_level: deep       # quick, balanced or deep
  - id: openai
    reasoning_effort: high      # an exact level, used when simplified modes are off
```

The reasoning control in a session footer changes only that session. Global
`toggles.reasoning_level` and `toggles.reasoning_effort` keys have no effect.
### Environment references

Any string value may use `${NAME}`. `$NAME` is not recognised. Map keys are
not interpolated.

An unset variable does not fail the load. The provider manager and
`vis-agent doctor` report the provider as unusable, and the router skips it.
Selecting it explicitly returns an error. When Vis saves config, a
whole-value reference remains `${NAME}`, not the resolved secret.

### Images

Images produced in a session — matplotlib figures and `image/*` attachments —
are replayed to the model only when its name is known to support vision. A
text-only or unknown model gets the text results and no image block.

### GitHub Copilot

Vis sends `X-Initiator: user` on the first call of each turn and
`X-Initiator: agent` on tool-call continuations and internal calls such as
session titling. Copilot determines billing. These headers do not guarantee a
particular charge. Setting `X-Initiator` in `llm_headers` overrides this
behavior. Trivial messages to Claude models on Copilot, such as a greeting or
a thank-you, are sent without a reasoning parameter.

### Evaluation runs

`--reasoning-effort LEVEL` sends one exact thinking level that the model offers, such as `high`
or `max`, instead of Vis's adaptive levels. The run exits `2` if the provider, model or value is not
accepted, or if any iteration switched provider or model. The JSON output
includes an `eval` object describing the run.

To add another provider, see
[Provider extensions](provider-extensions.md).

## System prompt

Add to the built-in prompt, or replace it:

```yaml
system_prompt: Prefer restructuredText docstrings. Do not edit generated/.
```

```yaml
system_prompt:
  text: You are …
  is_replace: true
```

`.vis/SYSTEM.md` and `.vis/APPEND_SYSTEM.md` in the project or `~/.vis` do the
same from files and take precedence over these keys. Repository conventions
belong in `AGENTS.md`, not here. See
[Project instructions](project-instructions.md#system-prompt-files).

## Environment

The project's `.env` and `.env.local` are loaded automatically for processes
Vis starts: shells, REPLs, test runners and extensions. `.env` takes precedence
over `.env.local`. The parser accepts `NAME=value`, `export NAME=value`,
quotes and comments.

`environment:` declares variables a dotenv file cannot, and never holds a
secret value itself:

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

Exactly one source per entry. A declared name never falls back to `.env` or the
ambient environment, and a blank value means unset. `literal` requires the
wrapper and is refused for credential-looking names (`*_KEY`, `*_TOKEN`,
`*_SECRET`, `*_PASSWORD`). Command and keychain values are fetched without a
shell, cached briefly and never logged.

Resolution order everywhere: `environment:` → `.env`, `.env.local` → the
environment Vis was started from.

With the jail enabled, the parent process environment is excluded.
`{env: NAME}` explicitly includes a variable, while `jail.environment: inherit`
includes the full environment. `LD_*`, `DYLD_*`, `PERL*` and `BASH_ENV` are
always refused. Child processes never inherit `NODE_OPTIONS` from the environment
Vis was started from. Declare it under `environment:` if a project needs it.

A shell call can add or override variables. Literal values are recorded
in the transcript, so use a source reference for secrets:

```python
sh = await shell("npm test", {"env": {"NODE_ENV": "test"}})
sh = await shell("./deploy.sh", {"env": {"STRIPE_KEY": {"keychain": "vis-stripe"}}})
```

## Router

Configure retry delays, network timeouts and spending limits. Omit this block to use defaults.

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

The jail is off by default. Enable it whenever the model runs untrusted code.
Without it, shells and language processes run with your full permissions. With
`jail.enabled: true`, commands run under Seatbelt (macOS) or bubblewrap (Linux)
and through the gateway's egress proxy. Unsupported hosts return an error.

Declare directories in `workspace.filesystem`, then allow them by ID in `jail.filesystem.allow`. The
jail does not expose unlisted roots. To keep specific files out of every grant, list patterns under
`jail.filesystem.deny_read` or `jail.filesystem.deny_write`. The file tools of Vis obey these rules
whether the jail is on or off. Only `jail.enabled: true` keeps child processes out. See [Deny
specific files](jail.md#deny-specific-files).

| Key | Meaning |
|---|---|
| `id` | Name used by the allow list and the UI |
| `path` | Absolute or `~`-relative directory |
| `description` | Optional. Tells the model what the root is for. |
| `python_name` | Optional Python variable for the path, for example `runtime_path` |
| `access` | `read-write` or `read-only` |
| `search` | Whether search indexes it |
| `draft` | `shared`, `copy-only`, `copy-and-apply` or `not-allowed` in an isolated session (see [Drafts](drafts.md)) |
| `when`, `optional` | Mount only on some hosts or when the path exists |

Allowed, searchable roots have a Python `Path` variable named after the
directory (`vis-python-runtime` → `vis_python_runtime_path`). Set `python_name`
to choose another name. `project_root_path` is the current project. Access to
`~/.vis` is always allowed.

You can ask Vis to create one draft across several added read/write repositories,
even when their policies are `shared`. `draft_create("task", roots=[project_root_path, sibling_path])`
selects participants without changing the catalog. The first root is primary.
See [Work in another repository](drafts.md#work-in-another-repository).

```yaml
# vis.yml
workspace:
  filesystem:
    - id: sibling
      path: ~/sibling-repository
      draft: copy-and-apply
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

`when.os` accepts `macos`, `linux`, `wsl` or `windows`. A missing admitted path
is reported by `vis-agent doctor`.

A tool that attaches to an already running process cannot jail it. Processes
Vis starts are jailed when `jail.enabled` is true.

[Process jail and network policy](jail.md) explains the full policy, including network rules and how
to diagnose a refusal. Running processes keep the jail profile that they started with. So if a
native tool such as `bb` or `clj-kondo` fails with `CSunMiscSignal.open() failed` after an upgrade,
restart Vis.

Shared Python installs use `~/.vis/python/packages`. Project installs stay in their
uv environment. `VIS_PYTHON_PACKAGES` overrides only the shared location. Bytecode
caches use `~/.vis/python/pycache`, overridden by `VIS_PYTHON_PYCACHE_PREFIX`.
`VIS_PYTHON_HOME` and `VIS_PYTHON_NATIVE_PATH` select another runtime. These variables
are read at startup.

## Python TLS validation

Both `~/.vis/config.yml` and project `vis.yml` accept this setting, using the
normal configuration merge order:

```yaml
python:
  tls_strict: true
```

The default is `true`, which keeps Python's TLS validation. Set it to `false` only for a trusted
corporate CA that fails strict X.509 checks. For example, a CA fails these checks when its
`Basic Constraints` extension is not marked critical:

```yaml
python:
  tls_strict: false
```

This clears only `ssl.VERIFY_X509_STRICT` when Python SSL contexts receive their
verification flags. Trust-chain, certificate-signature, expiry and hostname
verification remain enabled. It does not add trusted certificates or retry a
failed handshake with verification disabled. STRICT covers multiple X.509
requirements. Disabling it is a security trade-off, not a certificate repair.
Prefer a correctly issued CA when your administrator can provide one.

The same merged value applies to **`python_execution` and trusted Python
extensions**, including their separate workers. It affects Python's `ssl`
contexts, including contexts created by stdlib clients and libraries that use
`ssl.SSLContext`. It does not affect JVM TLS, `gh`, `curl`, uv, external Python
processes or libraries using a different TLS implementation.

Vis reads the value when a worker starts. After you change it, run `/reload` to rebuild session
workers. If the gateway already runs a worker that registers extensions, restart Vis. Existing
connections do not change. Use a YAML boolean: `false`, not the string `"false"`.

## Python package index

Set the embedded runtime's primary package index in `vis.yml`:

```yaml
python:
  index_url: https://gateway.example.com/simple
```

This setting applies to Vis-managed pip installs, `vis-agent python uv`, and
automatic extension project preparation. It is read from merged configuration
for each subprocess. For uv, Vis supplies `UV_DEFAULT_INDEX` when neither
`UV_DEFAULT_INDEX` nor `UV_INDEX_URL` is already set. Use `--default-index` for a
command-line override. uv's deprecated `--index-url` and `-i` do not override
`UV_DEFAULT_INDEX`. Named indexes and package source pins remain managed by uv.

`index_url` overrides pip's primary index from `PIP_INDEX_URL` or `pip.conf`.
When absent, both installers keep their inherited settings. Other installer
settings, including extra indexes, proxies and certificates, remain unchanged.
Prefer one company virtual index serving both private and public packages.
Extra indexes are not ordered fallback sources and can introduce dependency confusion.

Use a literal HTTP(S) URL without credentials, a query string or a fragment.
Keep authentication outside committed YAML, for example in the gateway user's
`.netrc`. Invalid index values stop installation rather than falling back to a
public index. Existing installed packages are not reinstalled by this setting.

## Python import roots

After `vis-agent python uv sync --project .`, run your package from the same
project directory with `vis-agent python -m your_package`. The standalone CLI
selects one dependency environment before Python starts:

- A current-directory `.venv`, `pyproject.toml`, or `UV_PROJECT_ENVIRONMENT`
  selects project mode. Only that environment supplies installed packages,
  distribution metadata and editable `.pth` files or import hooks. Shared Vis
  packages and their startup hooks are not loaded.
- Without a project, the CLI uses shared packages in `~/.vis/python/packages`
  (or `VIS_PYTHON_PACKAGES`).
- `vis-agent python --shared -m your_tool` explicitly selects shared packages,
  even inside a project. It skips project activation and configured or inferred
  source roots. An explicit `PYTHONPATH` still applies.

`UV_PROJECT_ENVIRONMENT` selects another project environment. Relative paths
resolve against the current directory. The CLI does not search parent directories,
create an environment or sync dependencies on startup. A `pyproject.toml` with no
prepared environment reports a sync error instead of borrowing shared packages.

The CLI still uses Vis's embedded Python, so the environment must match its
Python version and platform. If the matching site-packages directory is missing,
the CLI reports a diagnostic rather than silently falling back to shared packages.
To use the project's own interpreter instead, run
`vis-agent python uv run --no-sync python -m your_package`.

Environment activation happens inside the sandbox: editable source and custom
environments still need to be within allowed filesystem roots. Only activate
environments you trust. Editable import hooks can execute code. This standalone
CLI behavior does not add the project's environment to agent `python_execution`.

`vis-agent python` also puts declared source roots on `sys.path`, so
`vis-agent python -m pytest tests/` imports a `src/` layout without
`PYTHONPATH`. Roots are read from `pyproject.toml` (setuptools, pdm, poetry,
hatch, pytest `pythonpath`), `setup.cfg`, `pytest.ini` and `tox.ini`. No roots
are inferred without this metadata. To declare roots explicitly:

```yaml
# vis.yml
python:
  source_paths: [src, lib/vendor, ~/shared/py]
```

Configured paths come first, then inferred ones. `PYTHONPATH` comes before both. All these roots
come before the packages of the selected environment. Vis does not merge project and shared
packages. An [editable package install](extension-development.md) supplies its own import roots
through `.pth` files or backend hooks, so it does not need these layout overrides. Import roots do
not grant filesystem permissions or install dependencies.

## MCP servers

Servers you add from the UI, the API or `vis-agent gateway mcp` are written to
`~/.vis/state.yml`. Servers declared by hand elsewhere are used too, but the UI
cannot edit them. Change the file that declares them.

The gateway keeps one connection for each enabled server, which all sessions share. It reconnects a
connection that crashed. **Kill** closes the connection until you press **Start** or the gateway
restarts. To keep a server off after a restart, set `enabled: false`.

The gateway starts an OAuth flow when an HTTP MCP server requests it with
`401`. From the terminal, use **MCP Servers**. From the CLI:

```bash
vis-agent gateway mcp add linear --url https://mcp.linear.app/mcp
vis-agent gateway mcp auth-start linear
# open the printed URL, approve, then paste the redirect URL or code:
vis-agent gateway mcp auth-complete linear --flow-id <FLOW_ID> --input "<URL_OR_CODE>"
vis-agent gateway mcp list
```

A static token skips sign-in:

```bash
vis-agent gateway mcp add linear --url https://mcp.linear.app/mcp \
  --headers "Authorization=Bearer <TOKEN>"
```

Other verbs: `test` (connect without saving), `remove`, `enable`, `disable`,
`kill`, `start`, `auth-poll`, `auth-cancel`, `auth-logout`. Saving a server
without `env` or `headers` keeps the stored values.

## Feature toggles

```yaml
toggles:
  shell: false          # default true; removes shell(...) from the sandbox
  introspection: true   # default false; lets the agent read its own session data
  council: true         # default true; classified project messages, replies and explicit pings
  draft_backend: off    # default off; opt in with auto | worktree | rift (see drafts.md)
  automations: true     # default false; lets schedules and webhooks start turns (see automations.md)
```

Run `/reload` after editing.

## Machine settings

Settings for one machine have six sections:

- **General**: the agent name, Council and its rooms, notifications and experimental features.
- **Providers**: provider accounts and options for model requests, such as automatic fallback.
- **Voice**: speech engines, the voice model and the transcription of recordings.
- **Permissions**: files, network, process access and sandbox tools.
- **Tools**: MCP servers and extensions.
- **Automations**: prompts that run on a schedule or a webhook.

The TUI shows the same sections. Notifications, speech engines and automations are only in the
Companion app.

In the TUI, the **Providers** header has an **Add provider** button. Below it, **Configured
providers** lists your accounts and **Configuration** holds the provider options. MCP servers have
their own **MCP servers** section after **Tools**, with an **Add** button in its header.

## Project, group and session settings

In the app, select **Settings** with the cog icon in a session's **…** menu. For a project or a
group, select **Settings** in its actions. In the TUI, use **Session settings**, **Group
settings** or **Project settings** from the command palette. The settings rows show the
effective value and where it comes from. **Use inherited value** removes only the override at
the scope you opened.

Vis resolves each setting in this order: **global → project → group → session**. A scope with no
value is skipped. `false` is an explicit value, not inheritance. For example, turn a setting off in
a group and turn it on in one session. To make that session follow the group again, choose **Use
inherited value** in it. A change to the group does not overwrite an explicit choice in another
session.

A more specific value wins. When a more specific scope decides a row for the open session,
Settings locks that row. The row names the scope that decides it.

In this example, your project's `vis.yml` sets `toggles.shell: false`. While a session from that
project is open, the global **Shell commands** row is locked in TUI **Settings** and in the app.
Turning it on globally does not change that session. Open **Project settings** and turn it on
there. Vis writes the change to `.vis/config.yml`, which overrides `vis.yml` without editing it.
From a session outside that project, you can still edit the global row.

Global means this gateway, which all its connected clients share. Project settings use the canonical
project root, not the working-copy path of a draft. Edits go to the project's `.vis/config.yml`, and
the checked-in `vis.yml` does not change.

The gateway database keeps group and session overrides. A session that you move keeps its own values
and follows its new ancestors. New sessions and forks inherit from their ancestors, not the
overrides of the source session. Sessions without a group skip the group layer.

Response options, including reasoning, verbosity, thinking summary and fast mode, are captured
when you submit a message. Later edits do not change running or queued responses.

With **Simplified thinking modes** on, the default, the reasoning control switches to the next of
quick, balanced and deep with each tap or key press. Turn the setting off to choose from a list of
every thinking level that the current model offers. The setting is in the **Application** section of
the app's **Settings**, and in the top section of the terminal's **Settings**.
Vis saves that choice as `reasoning_effort`. After a model change, each turn sends the nearest level
that the new model offers.

The reasoning control changes only the current session. A new session starts with the
[default thinking level](#default-thinking-level) of its provider.

Paths and access rows use guided editors. Changes apply on the next turn.
To edit configuration files directly, use a text editor outside the app or TUI.

Local permissions cannot expand the host's global access policy. Draft settings
govern future operations. Changing them does not move or delete an existing draft.

Skill and MCP availability applies to the next lookup or call, including a call
by an already-known name. An ongoing external call can finish. Disable a skill
globally or locally to remove it from discovery, `doc()`, prompt inventories and
slash entry points. This does not erase text already read or deny filesystem access.

Optional tool extensions have **Auto**, **On** and **Off** engine settings.
Auto uses the extension's existing applicability check. On keeps it active. Off
hides its tools and rejects new calls, including saved handles. This is extension
activation, not model routing. Core, provider and channel infrastructure is not
switchable here.

Provider setup, credentials, OAuth and shared MCP start/stop operations stay
global. Theme and other device preferences remain local to the app or terminal.
Only settings declared for the selected scope appear in its catalog.

### Scoped MCP servers

Use the MCP panel in the same scoped dialog to add a server definition or control
its availability. Local definitions can shadow an inherited name without editing
its parent. **Use inherited** removes only the local definition. Local definitions
cannot store environment credentials, HTTP headers or authentication settings.
Disabling availability for one session does not stop a shared connection. Enabling
availability does not start a server disabled by the global administrator.

The CLI accepts the same explicit scope and target for `list`, `add`, `remove`,
`enable` and `disable`:

```bash
vis-agent gateway mcp list --scope session --target-id <session-id>
vis-agent gateway mcp add docs --url https://gateway.example.com/mcp --scope group --target-id <group-id>
vis-agent gateway mcp disable docs --scope session --target-id <session-id>
vis-agent gateway mcp remove docs --scope group --target-id <group-id>
```

Omit both flags to manage global servers. Project targets accept the project ID
or its canonical root. Group and session targets use their IDs. Authentication
and lifecycle commands remain global-only.

### Extension settings

The **Extensions** part of the **Tools** section holds the settings of all extensions. Each extension
with settings has its own group under its name. The `project` tag marks an
extension of the current project. The `global` tag marks a machine extension.
Extensions that ship with Vis have no tag.

Opening Settings never runs extension code. After you add, change or remove an
extension file, reload the extensions:

- In the TUI, select **Reload** in the **Extensions** header line.
- In the app, select the round arrows next to the **Extensions** heading.

Vis runs the extension files again, then reads the settings list again.

In global settings, the reload covers machine extensions. In project, group or
session settings, it also covers the extensions of that project. Vis then shows
each scope with its directory and how many extensions loaded and failed there. If
a directory has no extensions, Vis says that it found none there. A reload from
global settings also tells you to use project settings or `/reload` for project
extensions.

If an extension fails to load, its group stays in the list and shows the
error. If a reload fails after an earlier load, Vis keeps using the earlier
version. The group then says so. Your stored values do not change when an
extension fails, goes away or loads again.

If the gateway runs an older Vis, the reload shows a message. Update Vis on that
machine to reload extensions.

## Session titling

A session is named from its first message immediately, then improved by one
short model call after the turn finishes.

```yaml
titling:
  mode: llm             # llm | first_sentence | first_words | disabled
  provider: zai-coding-plan   # optional: pin the title call
  model: glm-4.7
```

## Gateway pairing address

The pairing link for the companion app has an address that Vis finds on this computer. Your network
can need a different address, for example a port forward, a proxy or the one address that the
network allows. Name that address here, and every pairing link puts it first:

```yaml
gateway:
  advertise: 10.0.0.5
```

The value is a host, `host:port` or a full URL, and the detected addresses still
follow in the same link as fallbacks. `--advertise` on `vis-agent gateway start`
or `vis-agent gateway pair` wins over this key, and `VIS_GATEWAY_ADVERTISE` sits
between the two. The config file is the source a gateway started by launchd or
systemd can read, because a service unit starts without your shell profile.
Setting it changes what the link says, not what the gateway listens on, so the
address still has to reach this computer.

## Database

Sessions are stored in SQLite. Resolution order: `--db` flag, `VIS_DB_PATH`,
`db_spec`, then `~/.vis/vis.mdb`. `--db :memory` uses an in-memory database.

```yaml
db_spec:
  backend: sqlite
  path: /somewhere/else/vis.db
```

## Grep

Search always honours `.gitignore`. To search a gitignored subtree such as
vendored repositories, re-include it here:

```yaml
# vis.yml
grep:
  include_gitignored_paths: [repositories/]
```

Both lists use `.gitignore` pattern syntax. Omit `always_exclude` to use the defaults. The defaults
are `.git/`, `node_modules/`, `target/`, `build/`, `dist/`, `__pycache__/`, `.venv/`, `.gradle/`,
`vendor/`, `.next/`, `out/`, `.m2/`, `.shadow-cljs/`, `cljs-runtime/`, `.cpcache/`, `.clj-kondo/`,
`.calva/`, `.lsp/` and `.rift/`. If you set `always_exclude`, it replaces that list. It does not add
to it. Run `/reload` after you edit these lists.

## See also

- [Process jail and network policy](jail.md) — the `jail` block in full.
- [Project instructions](project-instructions.md) — AGENTS.md, SYSTEM.md and prompt templates.
- [Extending Vis](extending.md) — configuring providers, tools and toggles.
- [Running a gateway](gateway-service.md) — connect clients and resolve token errors.
