# Configuration

Most settings are managed from the terminal UI or the Companion app. Edit YAML
when you want settings shared across projects or checked into a repository.

## Quick setup

Open the provider picker in the terminal and choose **Add Provider**, or use
**Settings → Providers → Add provider** in the Companion app. Sign in, then pick
a model.

For API keys, local models and custom endpoints, see
[Providers and models](#providers-and-models). For project instructions, see
[Project instructions](context-and-prompts.md).

## Configuration files

Files are read in this order. Later files override earlier ones; nested maps
merge, scalars and lists are replaced.

| File | Purpose |
| --- | --- |
| `~/.vis/config.yml` | Your global settings |
| `~/.vis/state.yml` | Settings and credentials written by Vis; manage them from the UI |
| `<project>/vis.yml` | Settings shared with the project |
| `<project>/.vis/config.yml` | Local project overrides, usually gitignored |

Global configuration in `~/.vis` accepts `config.yml`, `config.yaml`,
`vis.yml` or `vis.yaml`. The project root accepts `vis.yml` or `vis.yaml`;
`<project>/.vis` accepts `config.yml` or `config.yaml`. The project is the
directory where you start `vis-agent`.

Keys are `snake_case` strings; boolean keys start with `is_`. Unknown keys are
rejected, and an invalid file prints every offending path and exits with status
2 instead of starting:

```text
Invalid Vis configuration in /project/.vis/config.yml:

  - grep.include-gitignored-paths: unknown key (config is closed) — did you mean "grep.include_gitignored_paths"?
  - mcp.servers.docs.transport: value rejected by the transport contract
```

Model names are free-form and are not validated; a wrong one fails at the
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

Models are offered in the order you list them; models discovered from the
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

Declare the API you actually use. A gateway that serves both chat completions
and Responses accepts either, but tool-call ids from one are rejected by the
other and the turn fails with a 400.

### Default and fallback

Picking a model in the UI writes two keys:

```yaml
default_provider: zai-coding-plan
default_model: glm-5.2        # or one line: zai-coding-plan/glm-5.2
```

- There is one default pair for the whole config, not one per provider.
- `default_model` is looked up in that provider's catalog; an unknown name
  falls back to the provider's first model.
- Without a default, the first provider and its first model are used.

Set a fallback provider and model for rate limits and provider failures:

```yaml
fallback_provider: anthropic-coding-plan   # must differ from default_provider
fallback_model: claude-sonnet-5
```

The fallback provider is tried right after the default; other providers follow
in configured order. Logging out of a provider clears its pair.

Both pairs are per user. They are ignored with a warning in a committed
`<project>/vis.yml`; put them in `~/.vis/config.yml`, `~/.vis/state.yml` or the
gitignored `<project>/.vis/config.yml`.

On the command line, `--model provider/model` selects both for one run without
saving anything. The provider does not have to be configured if it has a
built-in preset with managed sign-in:

```bash
vis-agent --model zai-coding-plan/glm-5.2 "task"
vis-agent --model glm-5.2 "task"          # on the active provider
```

### Environment references

Any string value may use `${NAME}`; `$NAME` is not recognised. Map keys are
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
session titling. Copilot determines billing; these headers do not guarantee a
particular charge. Setting `X-Initiator` in `llm_headers` overrides this
behavior. Claude models on Copilot are capped at balanced reasoning, and
trivial messages are sent without a reasoning parameter.

### Evaluation runs

`--reasoning-effort high|max` sends the provider's exact effort value instead of
Vis's adaptive levels. The run exits `2` if the provider, model or value is not
accepted, or if any iteration switched provider or model; the JSON output
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
[Project instructions](context-and-prompts.md#system-prompt-files).

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
always refused.

A shell or REPL call can add or override variables. Literal values are recorded
in the transcript, so use a source reference for secrets:

```python
sh = await shell("npm test", {"env": {"NODE_ENV": "test"}})
r = await repl_start({"language": "python",
                      "env": {"STRIPE_KEY": {"keychain": "vis-stripe"}}})
```

Vis refuses to reuse a running REPL with different `env` values. Stop it before
starting one with a different environment.

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

The jail is off by default. Enable it whenever the model runs untrusted code;
without it, shells and language processes run with your full permissions. With
`jail.enabled: true`, commands run under Seatbelt (macOS) or bubblewrap (Linux)
and through the gateway's egress proxy. Unsupported hosts return an error.

Declare directories in `workspace.filesystem`, then allow them by id in
`jail.filesystem.allow`. The jail does not expose unlisted roots.

| Key | Meaning |
|---|---|
| `id` | Name used by the allow list and the UI |
| `path` | Absolute or `~`-relative directory |
| `description` | Optional; what the model is told the root is for |
| `python_name` | Optional Python variable for the path, e.g. `runtime_path` |
| `access` | `read-write` or `read-only` |
| `search` | Whether search indexes it |
| `draft` | `shared`, `copy-only`, `copy-and-apply` or `not-allowed` in an isolated session (see [Drafts](drafts.md)) |
| `when`, `optional` | Mount only on some hosts or when the path exists |

Allowed, searchable roots have a Python `Path` variable named after the
directory (`vis-python-runtime` → `vis_python_runtime_path`). Set `python_name`
to choose another name. `project_root_path` is the current project. Access to
`~/.vis` is always allowed.

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
    - id: m2
      path: ~/.m2
      description: Maven/Clojure dependency cache
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

`repl_connect` attaches to an existing process, which Vis cannot jail. Processes
Vis starts are jailed when `jail.enabled` is true.

[Process jail and network policy](jail.md) explains the policy in full, including
network rules and how to diagnose a refusal. If a native tool such as `bb` or
`clj-kondo` fails with `CSunMiscSignal.open() failed` after an upgrade, restart
Vis: the jail profile is inherited by running processes.

Python installs packages in `~/.vis/python/packages` and writes bytecode caches
in `~/.vis/python/pycache`. Override these locations with `VIS_PYTHON_PACKAGES`
and `VIS_PYTHON_PYCACHE_PREFIX`. `VIS_PYTHON_HOME` and `VIS_PYTHON_NATIVE_PATH`
select another runtime. All four variables are read at startup.

## Python package index

Set the embedded runtime's primary package index in `vis.yml`:

```yaml
python:
  index_url: https://gateway.example.com/simple
```

This setting applies to Vis-managed pip installs and to extension projects selected
with [`tool.vis.project`](extending.md#uv-projects). Vis passes it as uv's
`--default-index`; named indexes and `[tool.uv.sources]` retain uv semantics.
It is read from merged configuration for each install. It does not change a
project's own `.venv` or configure uv commands run outside Vis.

`index_url` overrides pip's primary index from `PIP_INDEX_URL` or `pip.conf`.
When absent, each installer keeps its inherited settings. Other pip settings,
including extra indexes, proxies and certificates, remain unchanged. Prefer one
company virtual index serving both private and public packages; extra indexes
are not ordered fallback sources and can introduce dependency confusion.

Use a literal HTTP(S) URL without credentials, a query string or a fragment.
Keep authentication outside committed YAML, for example in the gateway user's
`.netrc`. Invalid index values stop installation rather than falling back to a
public index. Existing installed packages are not reinstalled by this setting.

## Python import roots

`vis-agent python` puts the project's packages on `sys.path`, so
`vis-agent python -m pytest tests/` imports a `src/` layout without
`PYTHONPATH`. Roots are read from `pyproject.toml` (setuptools, pdm, poetry,
hatch, pytest `pythonpath`), `setup.cfg`, `pytest.ini` and `tox.ini`. No roots
are inferred without this metadata. To declare roots explicitly:

```yaml
# vis.yml
python:
  source_paths: [src, lib/vendor, ~/shared/py]
  runner: project     # default run_tests backend: project | vispython
```

Configured paths come first, then inferred ones; `PYTHONPATH` precedes both.
An [editable package install](extending.md#uv-projects) supplies its own import
roots through `.pth` files or backend hooks; it does not need these layout overrides.
Import roots do not grant filesystem permissions or install dependencies.
`runner: project` runs the project's own pytest with its installed
dependencies; `vispython` runs in the embedded sandbox.
An explicit `runner` argument on the call
(`run_tests({"language": "python", "runner": "project"})`) overrides this
default for one run.

The project interpreter is chosen automatically: uv, then Poetry, then `.venv`
or `venv`, then `python3`.

## MCP servers

Servers you add from the UI, the API or `vis-agent gateway mcp` are written to
`~/.vis/state.yml`. Servers declared by hand elsewhere are used too, but the UI
cannot edit them; change the file that declares them.

The gateway keeps one connection per enabled server, shared by every session,
and reconnects a crashed one. **Kill** closes the connection until **Start** or
a gateway restart; `enabled: false` persists.

The gateway starts an OAuth flow when an HTTP MCP server requests it with
`401`. From the terminal, use **MCP Servers**; from the CLI:

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
  council: false        # default false; project log and explicit active-session pings
  draft_backend: auto   # auto | worktree | rift | off; how the agent isolates a draft (see drafts.md)
```

Run `/reload` after editing.

## Session titling

A session is named from its first message immediately, then improved by one
short model call after the turn finishes.

```yaml
titling:
  mode: llm             # llm | first_sentence | first_words | disabled
  provider: zai-coding-plan   # optional: pin the title call
  model: glm-4.7
```

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

Both lists use `.gitignore` pattern syntax. Omit `always_exclude` to use the
defaults: `.git/`, `node_modules/`, `target/`, `build/`, `dist/`, `__pycache__/`,
`.venv/`, `.gradle/`, `vendor/`, `.next/`, `out/`, `.m2/`, `.shadow-cljs/`,
`cljs-runtime/`, `.cpcache/`, `.clj-kondo/`, `.calva/`, `.lsp/` and `.rift/`.
Setting `always_exclude` replaces, rather than extends, that list.
Run `/reload` after editing.

## See also

- [Process jail and network policy](jail.md) — the `jail` block in full.
- [Project instructions](context-and-prompts.md) — AGENTS.md, SYSTEM.md and prompt templates.
- [Extending Vis](extending.md) — configuring providers, tools and toggles.
- [Remote access and the Companion app](gateway.md) — gateway keys and tokens.
