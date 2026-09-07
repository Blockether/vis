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

`config.yaml`, `vis.yml` and `vis.yaml` are accepted in the `.vis` directories;
the project root accepts `vis.yml` or `vis.yaml`. The project is the directory
where you start `vis-agent`.

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

The UI knows the presets — OpenAI, Anthropic (API and coding plan), OpenAI
Codex, GitHub Copilot, Z.AI, and local Ollama and LM Studio — and handles
sign-in. Anything that speaks an OpenAI- or Anthropic-style API works,
including local models. The on-disk shape:

```yaml
providers:
  - id: anthropic
    api_key: ${ANTHROPIC_API_KEY}
    models:
      - name: claude-sonnet-4-5-20250929
  - id: my-gateway
    compatibility: openai            # wire dialect of the endpoint
    base_url: https://llm.internal/v1
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

### Wire dialect

`compatibility` names the API the endpoint speaks. `api_style` is the same
setting under the router's name and wins when both are set; use it per model
only when one endpoint routes models through different APIs.

| Dialect | Request | Aliases |
| --- | --- | --- |
| `anthropic` | `{base_url}/messages` | `claude`, `anthropic-messages`, `messages` |
| `openai` | `{base_url}/chat/completions` | `openai-chat`, `openai-compatible`, `chat`, `chat-completions` |
| `openai-responses` | `{base_url}` + `responses_path` | `responses`, `openai-compatible-responses` |
| `gemini` | Gemini `generateContent` | `google`, `google-gemini` |

Case, `_` and `-` are normalised. An unknown value is rejected when the config
loads. A `responses_path` with no dialect implies `openai-responses`.

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

A second pair names where Vis goes when the default provider is rate-limited
or failing:

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

Any string value may use `${NAME}`; bare `$NAME` is not recognised. Map keys
are left alone.

An unset variable does not fail the load. The provider is reported as unusable
in the provider manager and by `vis-agent doctor`, and is skipped by the
router. Selecting it explicitly is the one place that errors. When Vis re-saves
config, a whole-value reference is written back as `${NAME}`, never as the
resolved secret.

### Images

Images produced in a session — matplotlib figures and `image/*` attachments —
are replayed to the model only when its name is known to support vision. A
text-only or unknown model gets the text results and no image block.

### GitHub Copilot

Copilot bills per premium request, decided by the `X-Initiator` header. Vis
sends the first call of each turn as `user` and every tool-call continuation
as `agent`, so a long turn is one premium request. Calls Vis makes for itself
(titling, extension helpers) are always `agent`. Setting `X-Initiator` in
`llm_headers` overrides this. Claude models on Copilot are capped at balanced
reasoning, and trivial messages are sent without a reasoning parameter.

### Evaluation runs

`--reasoning-effort high|max` sends the provider's exact effort value instead of
Vis's adaptive levels. The run exits `2` if the provider, model or value is not
accepted, or if any iteration switched provider or model; the JSON output
carries an `eval` object with the evidence.

To add a provider Vis does not ship with, see
[Provider extensions](provider-extensions.md).

## System prompt

Add to the built-in prompt, or replace it:

```yaml
system_prompt: Prefer restructuredText docstrings. Never touch generated/.
```

```yaml
system_prompt:
  text: You are …
  is_replace: true
```

`.vis/SYSTEM.md` and `.vis/APPEND_SYSTEM.md` in the project or `~/.vis` do the
same from files and take precedence over these keys. Repository conventions
belong in `AGENTS.md`, not here. See
[Project instructions](context-and-prompts.md#system-prompt-files-system-md-append-system-md).

## Environment

The project's `.env` and `.env.local` are loaded automatically and reach every
process Vis starts: shells, REPLs, test runners and extensions. `.env` wins over
`.env.local`; `NAME=value`, `export NAME=value`, quotes and comments are
understood.

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

With the jail enabled, the ambient environment is dropped and `{env: NAME}`
re-admits a variable. `jail.environment: inherit` keeps the whole ambient
environment instead. `LD_*`, `DYLD_*`, `PERL*` and `BASH_ENV` are always
refused.

A single shell or REPL can add to this from the call itself. The map is a
delta; a literal value is recorded in the transcript, so secrets should name a
source:

```python
sh = await shell("npm test", {"env": {"NODE_ENV": "test"}})
r = await repl_start({"language": "python",
                      "env": {"STRIPE_KEY": {"keychain": "vis-stripe"}}})
```

A REPL's environment is part of its identity: starting one that is already
running with a different `env` is refused. Stop it and start it again.

## Router

Retry pacing, network timeouts and spend limits. Omit the block for defaults.

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
and through the gateway's egress proxy. Unsupported hosts fail loudly.

Directories are declared once in `workspace.filesystem`, then admitted by id in
`jail.filesystem.allow`. A root the list does not name is not visible.

| Key | Meaning |
|---|---|
| `id` | Name used by the allow list and the UI |
| `path` | Absolute or `~`-relative directory |
| `description` | Optional; what the model is told the root is for |
| `python_name` | Optional Python variable for the path, e.g. `runtime_path` |
| `access` | `read-write` or `read-only` |
| `search` | Whether search indexes it |
| `draft` | `shared`, `copy-only`, `copy-and-apply` or `not-allowed` in an isolated session |
| `when`, `optional` | Mount only on some hosts or when the path exists |

Admitted, searchable roots get a Python `Path` variable named after the
directory (`vis-python-runtime` → `vis_python_runtime_path`); `python_name`
chooses the name. `project_root_path` is the current project. `~/.vis` is
always granted.

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

`repl_connect` attaches to a process you started yourself and is not jailed.
Everything Vis starts is.

[Process jail and network policy](jail.md) explains the policy in full, including
network rules and how to diagnose a refusal. If a native tool such as `bb` or
`clj-kondo` fails with `CSunMiscSignal.open() failed` after an upgrade, restart
Vis: the jail profile is inherited by running processes.

The embedded Python writes only to `~/.vis/python/packages` (override
`VIS_PYTHON_PACKAGES`) and `~/.vis/python/pycache` (`VIS_PYTHON_PYCACHE_PREFIX`).
`VIS_PYTHON_HOME` and `VIS_PYTHON_NATIVE_PATH` point at a different runtime;
all four are read at startup.

## Python import roots

`vis-agent python` puts the project's packages on `sys.path`, so
`vis-agent python -m pytest tests/` imports a `src/` layout without
`PYTHONPATH`. Roots are read from `pyproject.toml` (setuptools, pdm, poetry,
hatch, pytest `pythonpath`), `setup.cfg`, `pytest.ini` and `tox.ini`. A project
without such metadata gets nothing. To declare roots yourself:

```yaml
# vis.yml
python:
  source_paths: [src, lib/vendor, ~/shared/py]
  runner: project     # default run_tests backend: project | vispython
```

Configured paths come first, then inferred ones; `PYTHONPATH` precedes both.
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

An HTTP server that answers `401` needs OAuth, which the gateway runs for you.
From the terminal, use the **MCP Servers** command; from the CLI:

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
`db_spec`, then `~/.vis/vis.mdb`. `--db :memory` gives a throwaway session.

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
  always_exclude: [.git/, node_modules/, target/]   # replaces the default list
```

Both lists use `.gitignore` pattern syntax. `always_exclude` defaults to
`.git/`, `node_modules/`, `target/`, `build/`, `dist/`, `__pycache__/`,
`.venv/`, `.gradle/`, `vendor/`, `.next/` and `out/`; setting the key replaces
that list. Run `/reload` after editing.

## See also

- [Process jail and network policy](jail.md) — the `jail` block in full.
- [Project instructions](context-and-prompts.md) — AGENTS.md, SYSTEM.md and prompt templates.
- [Extending Vis](extending.md) — providers, tools and toggles this config names.
- [Remote access and the Companion app](gateway.md) — gateway keys and tokens.
