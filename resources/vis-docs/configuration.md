# Configuration

Vis reads EDN config from three places, deep-merged in order — later sources win, nested maps merge, scalars and vectors replace:

1. `~/.vis/config.edn` — **global**. Providers, credentials, machine-wide defaults. Written by Vis itself (provider setup, OAuth flows); safe to edit by hand.
2. `<project>/.vis/config.edn` — **project overlay**, hidden.
3. `<project>/vis.edn` — **project root overlay**, visible. The natural home for team-shared, committed settings.

"Project" means the directory you launched `vis` from. Everything else Vis owns lives next to the global config: the session database at `~/.vis/vis.mdb` and the log at `~/.vis/vis.log`.

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
{:environment {"TELEGRAM_BOT_TOKEN" "…"}}
```

Config wins over the real environment; removing the entry reveals the process value again.

## Database

Sessions, turns, and durable agent state live in SQLite. Resolution order: explicit `--db` flag → `VIS_DB_PATH` env var → `:db-spec` in config → the default `~/.vis/vis.mdb`. Use `--db :memory` for a throwaway session.

```clojure
{:db-spec {:backend :sqlite :path "/somewhere/else/vis.db"}}
```
