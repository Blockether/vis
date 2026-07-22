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

YAML maps onto the EDN shape with two rules:

1. **Keys become keywords.** `snake_case` and `kebab-case` are both
   accepted and normalize to the same kebab-case keyword:
   `system_prompt:` ≡ `system-prompt:` ≡ `:system-prompt`.
2. **String-keyed blocks stay verbatim.** Keys under `environment`,
   `llm-headers`, and `extra-body` are kept as strings —
   `ANTHROPIC_API_KEY` is never keywordized or case-mangled.

The same config, both ways:

```clojure
;; vis.edn
{:system-prompt "Prefer restructuredText docstrings."
 :router {:budget {:max-cost 5.0}}
 :environment {"ANTHROPIC_API_KEY" "…"}}
```

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

Omit it and built-in defaults apply. Unknown keys are dropped, never fatal.

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
