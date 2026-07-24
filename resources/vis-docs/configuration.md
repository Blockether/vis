# Configuration

Vis reads config from four sources, deep-merged in order — later sources win, nested maps merge, scalars and vectors replace:

1. `~/.vis/config.yml` (or `config.yaml` / `vis.yml` / `vis.yaml`) — **global YAML base**, hand-written, optional.
2. `~/.vis/config.edn` — **global**. Providers, credentials, machine-wide defaults. Written by Vis itself (provider setup, OAuth flows); safe to edit by hand. Wins over its YAML twin per key.
3. `<project>/vis.edn` (or `vis.yml` / `vis.yaml`) — **project root**, visible. The natural home for team-shared, committed settings.
4. `<project>/.vis/config.edn` (or `.vis/config.yml`) — **project overlay**, hidden. The nested overlay wins over the root file: personal beats committed.

"Project" means the directory you launched `vis` from. Everything else Vis owns lives next to the global config: the session database at `~/.vis/vis.mdb` and the log at `~/.vis/vis.log`.

## EDN or YAML

Every tier can be written in YAML instead of EDN: `vis.yml` (or `vis.yaml`)
at the project root, `.vis/config.yml` for the hidden overlay, and
`~/.vis/config.yml` (or `config.yaml` / `vis.yml` / `vis.yaml`) globally.

At the **project** tiers the two formats are two spellings of the same file,
so if both exist (`vis.edn` **and** `vis.yml`) the EDN file wins and the
YAML file is ignored with a logged warning — never merged.

The **global** tier is different: `~/.vis/config.edn` is machine-written
(Vis persists provider setup and OAuth tokens there), while a
`~/.vis/config.yml` is yours. Different authors, not two spellings — so both
load, deep-merged, with the EDN file winning per key. Keep hand-written
global settings (system prompt, `:search`, `:router`, `:environment`) in the
YAML file; let Vis own the EDN file.

YAML is validated exactly as parsed:

1. **Configuration keys stay strings.** Canonical keys use kebab-case. No recursive
   keywordization or underscore normalization occurs, so `system-prompt` is valid and
   `system_prompt` is rejected as an unknown key.
2. **User-owned keys stay verbatim.** Environment variables, MCP server names and
   `env`/`headers`, HTTP headers, request-body fields, pricing/model ids, and toggle ids
   retain their exact spelling and case.

## Executable configuration contract

`com.blockether.vis.internal.config-spec/config` is the complete `clojure.spec`
contract for the original string-keyed YAML representation. It covers these closed
top-level blocks: `providers`, `router`, `system-prompt`, `sandbox`, `jail`, `network`,
`environment`, `db-spec`, `search`, `toggles`, `tui-settings`, `mcp`, and `python`. Filesystem
policy is a closed block at `jail.filesystem`.

Nested maps are also closed except maps whose keys are user-defined, such as environment
variables, HTTP headers, toggle ids, MCP server names, pricing tables, and request bodies.
Unknown keys and invalid value types fail config loading with source-aware spec problems;
credentials are redacted from those problems.

The parser validates this string-keyed map before any internal adaptation. The same
namespace derives process-jail and network policies directly from that validated map, so
security enforcement and the YAML schema cannot maintain different key lists.

The same config, both ways:

```clojure
;; vis.edn
{:system-prompt "Prefer restructuredText docstrings."
 :router {:budget {:max-cost 5.0}}
 :environment {"ANTHROPIC_API_KEY" "…"}}
```

```yaml
# vis.yml
system-prompt: Prefer restructuredText docstrings.
router:
  budget:
    max-cost: 5.0
environment:
  ANTHROPIC_API_KEY: "…"
```

## Providers and models

The `:providers` vector holds your configured AI providers; **the first entry is the active one**. You normally manage this through the TUI (provider picker / "Add Provider"), which knows the presets — OpenAI, Anthropic (API and coding plan), OpenAI Codex, GitHub Copilot, Z.AI, plus local Ollama and LM Studio — and handles OAuth where needed. The on-disk shape, if you do edit it:

```clojure
{:providers [{:id      :anthropic
              :api-key "sk-…"
              :models  [{:name "claude-sonnet-4-5-20250929"}]}
             {:id      :lmstudio
              :models  [{:name "qwen3-coder-30b" :context 262144}]}]}
```

Per-model keys Vis honors: `:context` (window override for local servers that can't report one), `:output-limit`, `:tool-call?`. Per-provider: `:base-url`, `:api-style`, `:llm-headers`, `:extra-body`. Providers with managed auth (Copilot, coding plans) resolve tokens through their extension at runtime — no `:api-key` needed in the file.

Vis is model-agnostic: anything that speaks an OpenAI- or Anthropic-style chat API works, including fully local models.

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

```clojure
;; addendum (string form)
{:system-prompt "Prefer restructuredText docstrings. Never touch generated/."}

;; full replacement
{:system-prompt {:text "You are …" :replace? true}}
```

Markdown files work too — `.vis/SYSTEM.md` replaces the core prompt,
`.vis/APPEND_SYSTEM.md` appends to it, in both the project and `~/.vis`. A
project `SYSTEM.md` beats a global one, and both beat the config `:replace?`
form. See [Context files & prompts](context-and-prompts.md).

Independently of this, Vis stacks **`AGENTS.md`** (or `CLAUDE.md` as fallback) context files into every turn as project-owned instructions: `~/.vis/AGENTS.md` (user-global), each ancestor directory of the workspace root, then the workspace root itself — those files, not config, are the right place for repo conventions. See [Context files & prompts](context-and-prompts.md).

## Router

The `:router` block tunes the request pipeline — retry pacing, network timeouts, spend limits:

```clojure
{:router
 {:rate-limit {:same-provider-delays-ms [2000 3000 6000]
               :respect-retry-after?    true
               :fallback-provider?      true}
  :network    {:timeout-ms 300000 :idle-timeout-ms 45000}
  :budget     {:max-tokens 1000000 :max-cost 5.0}}}
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
  # Extra local ports on which a confined shell child may accept connections.
  inbound-ports:
    - 5273
network:
  allowed-domains:
    - github.com
    - npmjs.org
  denied-domains:
    - example.invalid
  allow-private: false
```

[Process sandbox and gateway egress](sandbox.md) is the single authoritative
reference for this boundary: the `workspace.filesystem` catalog and
`jail.filesystem.allow` admission model, the network model (HTTPS method/path
policy, MITM behavior, SSRF denial, programmable filters), `jail.inbound-ports`,
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
add that hostname to `network.allowed-domains` when appropriate.

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
2. `python.resource-cache` in config (`~` expands to your home directory):

   ```yaml
   # vis.yml
   python:
     resource-cache: ~/.vis/cache/graal-resources
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

## Extension environment overrides

Extensions declare the environment variables they read (API keys and the like). The `:environment` map overrides the process environment per variable — set once in config instead of exporting in every shell:

```clojure
{:environment {"ANTHROPIC_API_KEY" "…"}}
```

Config wins over the real environment; removing the entry reveals the process value again.

## Database

Sessions, turns, and durable agent state live in SQLite. Resolution order: explicit `--db` flag → `VIS_DB_PATH` env var → `:db-spec` in config → the default `~/.vis/vis.mdb`. Use `--db :memory` for a throwaway session.

```clojure
{:db-spec {:backend :sqlite :path "/somewhere/else/vis.db"}}
```

## Search

The `:search` block tunes what `find_files` may see. By default
both honor `.gitignore`; `include-gitignored-paths` re-includes chosen
gitignored subtrees — the walker descends them as if
`is_respect_gitignore=False` were passed **for those subtrees only**,
bypassing every nested `.gitignore` layer inside them, while the rest of
the workspace keeps honoring `.gitignore`. This is the fix for
intentionally-gitignored vendored or cloned repos (`repositories/**`): a
`.gitignore` `!` negation cannot re-include them (git never descends into
an excluded directory, so a negation on a child is dead code), but a
tool-side overlay can.

```yaml
# vis.yml
search:
  include-gitignored-paths:
    - repositories/
  # pruned even inside re-included subtrees; setting it REPLACES the default list:
  always-exclude:
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

```clojure
;; vis.edn equivalent
{:search {:include-gitignored-paths ["repositories/"]
          :always-exclude [".git/" "node_modules/" "target/"]}}
```

Semantics:

- Both lists speak **`.gitignore` pattern syntax** (`dir/`, `**`, `?`, char
  classes) — not a second glob dialect. `repositories/` and
  `repositories/**` both re-include the whole subtree.
- A path is searched when it is **not** gitignored, **or** it falls under
  an `include-gitignored-paths` pattern — unless `always-exclude` matches
  it. Formally: `excluded?(f) = always-exclude?(f) OR (gitignored?(f) AND
  NOT included?(f))`.
- A pattern also opens the directories **above** it: `repositories/**`
  makes the walker descend into `repositories/` itself even though
  `.gitignore` excludes it.
- `always-exclude` defaults to the denylist in the example above. Setting
  the key replaces the defaults (vectors replace on merge, like everywhere
  else in config). It guards the re-included subtrees; outside them
  `.gitignore` already governs.
- An explicit per-call `is_respect_gitignore` — either value — wins over
  the overlay for that call.
- Hidden files stay governed by `is_hidden`: re-including `repositories/`
  never surfaces the repos' `.git` internals (doubly guarded — `.git/` is
  also in the default `always-exclude`).
