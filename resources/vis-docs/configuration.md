# Configuration

Vis reads config from four YAML sources, deep-merged in order — later sources win, nested maps merge, scalars and vectors replace:

1. `~/.vis/config.yml` (or `config.yaml` / `vis.yml` / `vis.yaml`) — **global base**, hand-written, optional.
2. `~/.vis/state.yml` — **machine store**, written by Vis itself (provider setup, OAuth tokens, TUI-added providers). Kept separate from the hand-written base so the read-modify-write cycle never clobbers your file; wins over the base per key.
3. `<project>/vis.yml` (or `vis.yaml`) — **project root**, visible. The natural home for team-shared, committed settings.
4. `<project>/.vis/config.yml` (or `.vis/config.yaml`) — **project overlay**, hidden. The nested overlay wins over the root file: personal beats committed.

"Project" means the directory you launched `vis` from. Everything else Vis owns lives next to the global config: the session database at `~/.vis/vis.mdb` and the log at `~/.vis/vis.log`.

## Keys are snake_case strings

Config is YAML only, validated exactly as parsed:

1. **Configuration keys stay strings.** Canonical keys use snake_case. No recursive
   keywordization or kebab normalization occurs, so `system_prompt` is valid and
   `system-prompt` is rejected as an unknown key. Boolean flags use an `is_` prefix
   and never a `?` suffix, e.g. `is_replace`, `is_respect_retry_after`, `is_tool_call`.
2. **User-owned keys stay verbatim.** Environment variables, MCP server names and
   `env`/`headers`, HTTP headers, request-body fields, pricing/model ids, and toggle ids
   retain their exact spelling and case.

## Executable configuration contract

`com.blockether.vis.internal.config-spec/config` is the complete `clojure.spec`
contract for the original string-keyed YAML representation. It covers these closed
top-level blocks: `providers`, `router`, `system_prompt`, `workspace`, `jail`,
`environment`, `db_spec`, `search`, `toggles`, `tui_settings`, `mcp`, `python`, `titling`, and `message_queue`. Filesystem
admission is a closed block at `jail.filesystem`, and egress policy is a closed block at `jail.network`.

Nested maps are also closed except maps whose keys are user-defined, such as environment
variables, HTTP headers, toggle ids, MCP server names, pricing tables, and request bodies.
Unknown keys and invalid value types fail config loading with source-aware spec problems;
credentials are redacted from those problems.

The parser validates this string-keyed map before any internal adaptation. The same
namespace derives process-jail and network policies directly from that validated map, so
security enforcement and the YAML schema cannot maintain different key lists.

## Invalid config: what you see

A bad config is a *user error*, not a crash. Vis prints a panel that names every
offending field by its full path — no stack trace, no log file to open — and exits
with status 2:

```
  Invalid Vis configuration in /project/.vis/config.yml:

  - providers[0].models[0].contxt: unknown key (config is closed) — did you mean "providers[0].models[0].context"?
  - search.include-gitignored-paths: unknown key (config is closed) — did you mean "search.include_gitignored_paths"?
  - jail.filesystem.allow_reed: unknown key (config is closed)
  - mcp.servers.docs.transport: value rejected by the transport contract

  Fix the entries above and run vis again.
```

The most common cause is kebab-case: `search.include-gitignored-paths` must be
`search.include_gitignored_paths`. Kebab-case names such as
`:include-gitignored-paths` appear in the CHANGELOG and in engine internals — they
are the *internal keyword* mirrors of the YAML keys, never the YAML spelling.
Model names (`providers[].models[].name`) are free-form strings and are never
validated against a known-model list; a wrong one fails at the provider API, not here.

A small config:

```yaml
# vis.yml
system_prompt: Prefer restructuredText docstrings.
router:
  budget:
    max_cost: 5.0
environment:
  ANTHROPIC_API_KEY: "…"
```

## Providers and models

The `providers` vector holds your configured AI providers; **the first entry is the active one**. You normally manage this through the TUI (provider picker / "Add Provider"), which knows the presets — OpenAI, Anthropic (API and coding plan), OpenAI Codex, GitHub Copilot, Z.AI, plus local Ollama and LM Studio — and handles OAuth where needed. The on-disk shape, if you do edit it:

```yaml
providers:
  - id: anthropic
    api_key: sk-…
    models:
      - name: claude-sonnet-4-5-20250929
  - id: my-gateway
    compatibility: openai        # anthropic | openai | openai-responses
    base_url: https://llm.internal/v1
    api_key: ${LLM_TOKEN}
    models:
      - name: qwen3-coder-30b
        context: 262144          # input window
        output_limit: 32768      # max output tokens
        is_tool_call: true
```

Per-model keys Vis honors: `context` (input window — the override for servers that can't report one), `output_limit` (max output tokens), `is_tool_call`. Both limits are forwarded to the router, which uses them for pre-flight context checks and output capping, so filling them in makes routing decisions accurate instead of conservative.

Per-provider keys: `compatibility`, `base_url`, `api_style`, `llm_headers`, `extra_body`. **`compatibility`** is the wire dialect the endpoint speaks — `anthropic` (Anthropic Messages), `openai` (OpenAI chat completions), or `openai-responses` (OpenAI Responses API) — and is all a custom or self-hosted endpoint normally needs. `api_style` remains the raw escape hatch for anything outside those dialects (e.g. `gemini`) and wins if both are set. Note that the dialect is a property of the **endpoint**, so it is per provider; the per-model keys above are the only things Vis reads inside a model entry. Providers with managed auth (Copilot, coding plans) resolve tokens through their extension at runtime — no `api_key` needed in the file.

**Model order is your order.** Models are offered in exactly the order you wrote them under a provider in `vis.yml`; anything discovered from the provider's live catalog is appended after them, sorted. With nothing else set, the first provider is the active one and its first model is the default.

Vis is model-agnostic: anything that speaks an OpenAI- or Anthropic-style chat API works, including fully local models.

**A model may name its provider: `provider/model`.** Anywhere the CLI takes `--model`, a slash-qualified name is accepted and is the one-shot equivalent of passing `--provider` and `--model` together:

```bash
vis --model zai-coding-plan/glm-5.2 "task"      # same as --provider zai-coding-plan --model glm-5.2
vis --model glm-5.2 "task"                      # bare name: selects on the ACTIVE provider
```

The named provider is promoted to the router root for that run only, and it does not have to exist in `vis.yml` yet — an unconfigured one is synthesized from its built-in preset, so a provider that already has managed auth (a coding plan, Copilot) is usable without touching config. Nothing is persisted. A provider Vis cannot resolve is reported as a user error naming it, before the first request.

### The default selection is ONE pair

Picking a model in the TUI provider manager, the gateway, or the companion app writes exactly two top-level keys:

```yaml
default_provider: zai-coding-plan   # provider id
default_model: glm-5.2              # model name WITHIN that provider
```

There is one default in the whole config — one provider and one model — not one default per provider. Consequences worth knowing:

- `default_model` is resolved **inside `default_provider`'s catalog**. A name that provider does not offer is not an error: Vis falls back to that provider's **first** model — the named provider still wins.
- `default_model` also accepts the `provider/model` form, exactly like `--model`, and then its provider part wins over `default_provider`:

  ```yaml
  default_model: zai-coding-plan/glm-5.2   # one line, both halves of the pair
  ```
- Vis does **not** remember a model per provider. Change `default_provider` alone and you get that provider's first model until you set `default_model` too.
- Choosing a default never reorders `providers:` — order stays yours (above), the pair is just a pointer into it.
- Both keys are plain config, so a project `vis.yml` can pin a run's provider/model for a repository, and `--model provider/model` (above) overrides both for one run without persisting anything.

### Model capabilities: chat and vision

Capabilities are per **model**, not per provider, and come from the router's pinned model registry (svar) — the same source as context windows and pricing. Every model has `chat`; `vision` is the one Vis actually gates on.

Vis produces images on its own: matplotlib figures captured from the Python sandbox and anything `vis_attach`ed with an `image/*` media type are stored as durable session attachments (downsized first, so they stay legible without flooding the context). On later iterations and later turns they are replayed to the model as a canonical `image_url` data-URI block, emitted as its own message right after that iteration's `<results>`; the router translates it to Anthropic `image`, OpenAI `image_url`, or Gemini inline data, whichever the active provider speaks.

That replay is gated on the target model advertising `vision`:

- a vision model (`claude-opus-5`, `gpt-5.6-sol`, `gemini-3-pro-preview`, `glm-4.6v`) sees the figures it generated;
- a text-only model (`glm-4.7`, `glm-5.2`, Copilot models without vision) is silently sent the results text and no image block — a broken payload is never fabricated for it;
- non-image attachments (csv, json, wav) are never turned into image blocks; the filter is the stored media type, not the file name.

Model names are matched with version/date suffixes tolerated, so `claude-opus-5-20991231` still resolves to a vision model. A name the registry does not know at all resolves to `chat` only — conservative on purpose, but it means a brand-new model reached through a custom `base_url` gets no image replay until its name is known to the router.

### Environment references

Any string value may reference an environment variable as `${NAME}`. Vis resolves it while loading config, so a key never has to sit in the file:

```yaml
providers:
  - id: anthropic
    api_key: ${ANTHROPIC_API_KEY}
    base_url: https://${LLM_HOST}/v1
```

`${NAME}` is the only spelling — bare `$NAME` is deliberately not recognised, because it cannot be told apart from a value that legitimately starts with `$`. References work in any string, nested anywhere; map keys are left alone.

An **unset** variable is not a load failure. Vis is a long-lived gateway whose config is re-read live and on `/reload`, so one unused provider's missing key must never kill a session running happily on a healthy provider. The reference is left verbatim instead, and that provider is reported unusable in three places:

- the **provider manager** shows `NEEDS ENV · ANTHROPIC_API_KEY` instead of an authenticated verdict;
- **`vis doctor`** warns `can't use anthropic: ANTHROPIC_API_KEY is not set` and names the export to run;
- one warning is logged at load time, once per set of missing variables.

The provider is also dropped from the router fleet, so it can never be picked implicitly. Reaching for it **explicitly** — selecting it in the provider picker — is the one place that hard-errors, with the same message. Failure lands at the point of intent, never globally.

A value resolved from a whole-value `${NAME}` is written back as `${NAME}` whenever Vis re-saves config, so a theme flip or a toggle change cannot bake the plaintext secret into `~/.vis/state.yml`.

### Provider-native evaluation effort

Use `--reasoning-effort high|max` when a controlled evaluation must send the
provider's exact effort value instead of Vis's adaptive, provider-agnostic
reasoning levels:

```bash
vis --provider zai-coding-plan --model glm-5.2 \
  --reasoning-effort high --json "task"
```

Vis validates the selected provider and model before the first request. An
unsupported model or value returns the accepted values and exits `2`. A
completed run also exits `2` if any iteration changed provider or model;
same-model retries remain valid. Execution failures exit `1`, and a valid run
exits `0`.

Structured output contains an `eval` object with `valid?`, `invalid-reasons`,
and per-iteration reasoning evidence: requested/effective effort, actual
provider and model, the emitted wire fragment, and fallback status. The same
object appears in the final `--full-trace-json-stream` result frame. It is
run evidence only and is not added to the session database schema.

## System prompt

Append project house rules to the core prompt, or replace it outright:

```yaml
# addendum (string form)
system_prompt: Prefer restructuredText docstrings. Never touch generated/.
```

```yaml
# full replacement
system_prompt:
  text: You are …
  is_replace: true
```

Markdown files work too — `.vis/SYSTEM.md` replaces the core prompt,
`.vis/APPEND_SYSTEM.md` appends to it, in both the project and `~/.vis`. A
project `SYSTEM.md` beats a global one, and both beat the config `is_replace`
form. See [Context files & prompts](context-and-prompts.md).

Independently of this, Vis stacks **`AGENTS.md`** (or `CLAUDE.md` as fallback) context files into every turn as project-owned instructions: `~/.vis/AGENTS.md` (user-global), each ancestor directory of the workspace root, then the workspace root itself — those files, not config, are the right place for repo conventions. See [Context files & prompts](context-and-prompts.md).

## Router

The `router` block tunes the request pipeline — retry pacing, network timeouts, spend limits:

```yaml
router:
  rate_limit:
    same_provider_delays_ms: [2000, 3000, 6000]
    is_respect_retry_after: true
    is_fallback_provider: true
  network:
    timeout_ms: 300000
    idle_timeout_ms: 45000
  budget:
    max_tokens: 1000000
    max_cost: 5.0
```

Omit it and built-in defaults apply. Unknown keys are rejected by the configuration spec.

## Sandbox, filesystem, and network

The process jail is **off by default** and opt-in via `jail.enabled: true`.
**Strongly recommended** whenever the model runs untrusted code: without it,
managed shells and language processes run with the gateway user's full host
permissions. With it enabled, shell commands and managed language processes run
under the OS jail (Seatbelt on macOS, bubblewrap on Linux) and use the gateway
egress proxy. There is no separate shell or network toggle. Unsupported hosts
currently have no OS boundary and a requested `jail.enabled: true` fails loud.

Filesystem roots are declared once in the `workspace.filesystem` catalog (`id`,
`path`, optional `description`, `access` = `read-write`/`read-only`, `search`),
and `jail.filesystem.allow` lists the ids that enter the jail (deny-by-omission).
Vis's own session folder `~/.vis` is granted implicitly (read/write, `search: false`)
whatever the catalog and the allow list say; declare it to override that.

```yaml
# vis.yml
workspace:
  filesystem:
    - id: sibling
      path: ~/sibling-repository
    - id: reference
      path: ~/shared-reference
      access: read-only
    - id: m2
      path: ~/.m2
      description: Maven/Clojure dependency cache
      search: false            # granted but kept out of the default search sweep
jail:
  enabled: true
  filesystem:
    allow: [sibling, reference, m2]
  # Egress policy is one facet of the jail; jail.enabled is the single gate.
  network:
    allowed_domains:
      - github.com
      - npmjs.org
    denied_domains:
      - example.invalid
    allow_private: false
    # Extra local ports on which a confined shell child may accept connections.
    inbound_ports:
      - 5273
```

[Process sandbox and gateway egress](sandbox.md) is the single authoritative
reference for this boundary: the `workspace.filesystem` catalog and
`jail.filesystem.allow` admission model, the network model (HTTPS method/path
policy, MITM behavior, SSRF denial, programmable filters), `jail.network.inbound_ports`,
snapshot inheritance and `/reload`, and the read-only `session["access"]` view.
Every filesystem path must be absolute or home-relative (`~`); a bare-relative
path is rejected when the config is read.

One exception is called out there and worth repeating: **`repl_connect` is not
jailed.** It attaches to an already-running, user-owned external process that
Vis did not spawn, so Seatbelt cannot be applied retroactively; stopping the
resource only detaches. Everything Vis *starts* — shells, `subprocess`, managed
REPLs, test runners — is confined.

### macOS native-image startup failures

`CSunMiscSignal.open() failed` with `errno: 1` is not a domain-allowlist error.
GraalVM Native Image tools such as `spel`, `bb`, and `clj-kondo` create a named
POSIX semaphore while installing signal handlers, before their command runs.
The macOS profile permits that single IPC class with `ipc-posix-sem`; network
permissions remain unchanged.

Seatbelt policy is inherited and cannot be replaced inside an already confined
process. After upgrading from a Vis build without this permission, restart the
Vis client/gateway before retrying the native tool. An actual egress-policy
failure instead reports the rejected host (for example, `host not permitted`);
add that hostname to `jail.network.allowed_domains` when appropriate.

### GraalPy internal-resource cache

GraalPy extracts its Python standard library ("internal resources") on first
use into `$XDG_CACHE_HOME/org.graalvm.polyglot` (default
`~/.cache/org.graalvm.polyglot`). This happens at **runtime** on both the JVM
and the compiled native-image binary — the stdlib is not baked into the
executable. If that directory is unwritable (a confined process, a read-only
home, minimal CI), the very first Python block fails with
`ModuleNotFoundError: No module named 'ast'`.

Resolution order for the cache root:

1. The GraalVM system property always wins:
   `vis -J-Dpolyglot.engine.userResourceCache=/path` on the JVM launcher, or
   `VIS_OPTS`/`JAVA_TOOL_OPTIONS` style `-D` flags where applicable.
2. `python.resource_cache` in config (`~` expands to your home directory):

   ```yaml
   # vis.yml
   python:
     resource_cache: ~/.vis/cache/graal-resources
   ```

3. `~/.vis/cache/graal-resources` — always preferred when writable. vis redirects
   here unconditionally rather than reusing the default
   `~/.cache/org.graalvm.polyglot` root, so behavior is identical whether or not a
   sandbox happens to whitelist that root. This directory is git-ignored.
4. Final fallback: `./.graal-resources` under the working directory (also
   git-ignored) when even `~/.vis` is unwritable.

The property is read **once per process** when the polyglot engine
initializes, so changing it requires restarting the client and the gateway
daemon — `/reload` is not enough. An unusable configured path silently degrades
to steps 3–4 rather than failing startup.

## Python import roots

`vis python` puts a project's own packages on `sys.path` before running, so
`vis python -m pytest tests/` imports a `src/` layout the same way an explicit
`PYTHONPATH=src` invocation would.

The roots are read from the project's packaging metadata with Python's own
parsers — `tomllib` for `pyproject.toml`, `configparser` for `setup.cfg`,
`pytest.ini` and `tox.ini` — never by pattern-matching the file text. Inference
is strictly declarative: nothing is guessed from directory names, a project
without such metadata gets nothing, and a malformed file yields nothing instead
of a partial scrape. Recognized declarations:

```toml
[tool.setuptools.packages.find]      where       = ["src"]
[tool.setuptools]                    package-dir = {"" = "src"}
[tool.pdm.build]                     package-dir = "src"
[tool.poetry]                        packages    = [{include = "pkg", from = "src"}]
[tool.hatch.build.targets.wheel]     packages    = ["src/pkg"]   # parent wins
[tool.pytest.ini_options]            pythonpath  = ["src"]
```

plus `package_dir` under `setup.cfg`'s `[options]` and pytest's `pythonpath`
under `setup.cfg` `[tool:pytest]`, `pytest.ini` `[pytest]` and `tox.ini`
`[pytest]`.

When that is wrong, absent, or simply not how you lay a project out, say it
outright:

```yaml
# vis.yml
python:
  source_paths: [src, lib/vendor, ~/shared/py]
```

Configured paths come **first**, ahead of anything inferred. Relative entries
resolve against the working directory, `~` expands, and an entry that is not an
existing directory is dropped. An explicit `PYTHONPATH` in the environment still
precedes both.

## Extension environment overrides

Extensions declare the environment variables they read (API keys and the like). The `environment` map overrides the process environment per variable — set once in config instead of exporting in every shell:

```yaml
environment:
  ANTHROPIC_API_KEY: …
```

Config wins over the real environment; removing the entry reveals the process value again.

## Feature toggles

Built-in extensions can expose a boolean toggle under `toggles:`. Toggle values merge with the rest of the config and take effect after `/reload` (or the next environment build):

```yaml
toggles:
  # Default: true. Set false to remove the Exa/arXiv live-research extension.
  web_search: false
```

When `web_search` is false, Vis does not bind `search`, `search_web`, `search_code`, or `search_papers`; no request can be sent to Exa through that extension.

## Session titling

Vis names a session from its first request. The name is written locally and
instantly (a deterministic fallback title), then — by default — **upgraded once**
by a short LLM call. That upgrade is cosmetic, so it must never compete with your
own turn for a rate-limited gateway's slot: it runs *after* the foreground turn
finishes, not alongside it. The `titling:` block controls all of it:

```yaml
titling:
  # llm (default) | first_sentence | first_words | disabled
  mode: llm
  # after_turn (default) | idle (alias) | immediate
  scheduling: after_turn
  # optional: pin the title call instead of walking the provider fleet
  provider: zai-coding-plan
  model: glm-4.7
```

- `mode: llm` — local fallback first, then one LLM upgrade; generated once and
  never regenerated.
- `mode: first_sentence` / `first_words` — purely local titles. No provider call,
  no quota, no 429 on a trivial first message.
- `mode: disabled` — no auto-title at all.
- `scheduling: after_turn` (default, `idle` is an alias) — the LLM call is
  deferred past the user's turn. `immediate` restores the old concurrent
  behaviour.
- `provider` / `model` — pin the title call to one cheap endpoint. Without them
  Vis walks its own cheap-first provider order.

A broken or missing `titling:` block never costs the session its name: config
errors here fall back to the defaults.

## Database

Sessions, turns, and durable agent state live in SQLite. Resolution order: explicit `--db` flag → `VIS_DB_PATH` env var → `db_spec` in config → the default `~/.vis/vis.mdb`. Use `--db :memory` for a throwaway session.

```yaml
db_spec:
  backend: sqlite
  path: /somewhere/else/vis.db
```

## Grep

The `grep` block tunes what `grep` may see. `.gitignore` is ALWAYS
honored — there is no per-call opt-out — so this config block is the only
way to change what search sees. `include_gitignored_paths` re-includes
chosen gitignored subtrees: the walker descends them, bypassing every
nested `.gitignore` layer inside them, while the rest of
the workspace keeps honoring `.gitignore`. This is the fix for
intentionally-gitignored vendored or cloned repos (`repositories/**`): a
`.gitignore` `!` negation cannot re-include them (git never descends into
an excluded directory, so a negation on a child is dead code), but a
tool-side overlay can.

```yaml
# vis.yml
grep:
  include_gitignored_paths:
    - repositories/
  # pruned even inside re-included subtrees; setting it REPLACES the default list:
  always_exclude:
    - .git/
    - node_modules/
    - target/
    - build/
    - dist/
    - __pycache__/
    - .venv/
    - .gradle/
    - vendor/
    - .next/
    - out/
```

```yaml
# vis.yml
grep:
  include_gitignored_paths: [repositories/]
  always_exclude: [.git/, node_modules/, target/]
```

Semantics:

- Both lists speak **`.gitignore` pattern syntax** (`dir/`, `**`, `?`, char
  classes) — not a second glob dialect. `repositories/` and
  `repositories/**` both re-include the whole subtree.
- A path is searched when it is **not** gitignored, **or** it falls under
  an `include_gitignored_paths` pattern — unless `always_exclude` matches
  it. Formally: `excluded?(f) = always-exclude?(f) OR (gitignored?(f) AND
  NOT included?(f))`.
- A pattern also opens the directories **above** it: `repositories/**`
  makes the walker descend into `repositories/` itself even though
  `.gitignore` excludes it.
- `always_exclude` defaults to the denylist in the example above. Setting
  the key replaces the defaults (vectors replace on merge, like everywhere
  else in config). It guards the re-included subtrees; outside them
  `.gitignore` already governs.
- There is no per-call gitignore flag: edit `vis.yml` and `/reload` to
  change what search sees.
- Hidden files stay governed by `is_hidden`: re-including `repositories/`
  never surfaces the repos' `.git` internals (doubly guarded — `.git/` is
  also in the default `always_exclude`).
- The overlay is applied **natively by the fff index** (its ignore walker
  *and* its live file watcher), not by a second pass in vis — so a
  re-included subtree is indexed once, stays incrementally up to date, and
  costs nothing per search. The same mechanism registers `.rgignore`, the
  one ignore filename ripgrep's `ignore` crate does not pick up on its own
  (`.gitignore`, `.ignore`, `.git/info/exclude` and the global gitignore
  are native).
