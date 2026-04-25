<h2 align="center">
  <br/>vis<br/>
</h2>

<div align="center">
<i>vis</i> — an <a href="https://arxiv.org/abs/2512.24601">RLM-inspired</a> coding agent built from the ground up. Works with any text-based model.
<br/>
<sub>Code-eval over tool-calls. O(1) context per iteration. Secure-by-default SCI sandbox.</sub>
</div>

<div align="center">
  <h2>
    <a href="https://clojars.org/com.blockether/vis"><img src="https://img.shields.io/clojars/v/com.blockether/vis?color=%23007ec6&label=clojars" alt="Clojars version"></a>
    <a href="https://github.com/Blockether/vis/blob/main/LICENSE">
      <img src="https://img.shields.io/badge/license-Apache%202.0-green" alt="License - Apache 2.0">
    </a>
    <a href="https://blockether.github.io/vis/">
      <img src="https://img.shields.io/badge/docs-mdBook-blue" alt="Docs">
    </a>
  </h2>
</div>

<div align="center">
<h3>

[Rationale](#rationale) • [Quick Start](#quick-start) • [Architecture](#architecture) • [Extensions](#extensions) • [Docs](https://blockether.github.io/vis/)

</h3>
</div>

## Rationale

Every mainstream coding agent accumulates messages into a growing context
window, then compacts when it overflows. Vis takes a different approach
inspired by [Recursive Language Models](https://arxiv.org/abs/2512.24601):
the context is an **external environment** the model interacts with
through code.

The model writes Clojure. A sandboxed [SCI](https://github.com/babashka/sci)
interpreter executes it. Results flow back as a compact journal. State
lives in named vars and SQLite, not in the token budget.

| | Tool-call agents | Vis |
|---|---|---|
| **Context growth** | O(n) — all messages accumulated | O(1) — fixed per iteration |
| **Compaction** | Required | Never needed |
| **Ops per LLM call** | N tools (harness-dispatched) | N code blocks (LLM-composed, async via futures) |
| **API requirement** | `function_calling` / `tool_use` | Any model that outputs JSON |
| **State** | Context window only | Named vars + SQLite |
| **Security** | Permission prompts | Deny-by-default sandbox |

→ **[Full rationale with diagrams](https://blockether.github.io/vis/rationale.html)**

## Quick Start

Add to `deps.edn`:

```clojure
com.blockether/vis {:mvn/version "0.1.0"}
```

### CLI

```bash
clojure -M:run "What is 2+2?"
clojure -M:run --json "Explain the auth flow"
clojure -M:run --model gpt-4o "Summarize X"
```

### Programmatic

```clojure
(require '[com.blockether.vis.core :as vis]
         '[com.blockether.vis.config :as config])

(let [router      (config/make-router)
      environment (vis/create-environment router {:db "~/.vis/vis.mdb"})]
  (vis/query! environment [{:role "user" :content "Fix the auth bug"}])
  ;; => {:answer "..." :iterations 5 :tokens {:input N :output N} :cost {...}}
  (vis/dispose-environment! environment))
```

## Architecture

```
Channels (Web, TUI, Telegram, CLI)
    ↓
Conversation Layer (lifecycle, locking)
    ↓
Environment (SCI sandbox + extensions + var index)
    ↓
Query Engine (one user turn)
    ↓
Iteration Engine (ask LLM → execute code → persist → loop)
    ↓
SQLite (versioned snapshots of everything)
```

→ **[Full architecture docs](https://blockether.github.io/vis/architecture/overview.html)**

## Extensions

Extensions are the **only** way to add capabilities to the sandbox.
Deny-by-default — no extension, no capability.

```clojure
(ns com.acme.ext.my-tool
  (:require [com.blockether.vis.loop.runtime.conversation.environment.extension :as ext]))

(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.acme.ext.my-tool
     :ext/version   "1.0.0"
     :ext/author    "Acme Corp"
     :ext/license   "Apache-2.0"
     :ext/doc       "My custom tool"
     :ext/group     "tools"
     :ext/requires  ['com.blockether.vis.ext.editing]
     :ext/prompt    "Use (my-tool query) to search."
     :ext/symbols   [(ext/symbol 'my-tool search-fn
                       {:doc "Search for things"
                        :arglists '([query])})]}))
```

Drop the jar on the classpath → namespace loads → extension self-registers
→ every new environment gets it, in dependency order.

→ **[Extension system docs](https://blockether.github.io/vis/extensions/overview.html)**

## Channels

| Channel | Command | Description |
|---------|---------|-------------|
| **TUI** | `clojure -M:run chat` | Lanterna terminal UI |
| **CLI** | `clojure -M:run "prompt"` | One-shot query |
| **Telegram** | `clojure -M:run telegram` | Bot polling |
| **Web** | `clojure -M:run web` | Jetty + SSE |

## Development

```bash
# REPL
clojure -M:dev

# Run
clojure -M:dev:run

# Test
clojure -M:test

# Docs (live reload)
cd resources/docs && mdbook serve --open
```

## License

Copyright 2025-2026 Blockether

Licensed under the [Apache License, Version 2.0](LICENSE).
