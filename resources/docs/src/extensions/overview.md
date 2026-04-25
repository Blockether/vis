# Extension System

> **Namespace:** `com.blockether.vis.loop.runtime.conversation.environment.extension`

Extensions are the **only** way to add symbols, classes, and documentation
to the SCI sandbox. An extension is a namespace-like bundle that groups
related tools, constants, prompt context, and per-iteration nudges into
a single validated unit.

## What an Extension Can Do

1. **Bind functions** into the sandbox — the LLM calls them from `:code`
2. **Bind constants** — data the LLM can reference by name
3. **Inject prompt context** — LLM-facing docs in the system prompt
4. **Emit per-iteration nudges** — situational hints (budget, errors, etc.)
5. **Expose Java classes** — enable `(LocalDate/now)` style interop
6. **Guard activation** — conditionally enable/disable based on env state

## Registration

Two ways to register extensions:

### Global Registry (recommended)

Call `register-global!` at namespace load time. When any environment
is created, all global extensions are automatically installed in
dependency order.

```clojure
(ns my.company.ext.git
  (:require [....extension :as ext]))

(ext/register-global!
  (ext/extension
    {:ext/namespace 'com.acme.ext.git
     :ext/requires  ['com.blockether.vis.ext.editing]
     :ext/doc       "Git integration"
     ...}))
```

Drop the jar on the classpath → namespace loads → extension
self-registers → every new environment gets it.

### Dynamic Loading

An extension can load other extensions at runtime:

```clojure
(ext/load-extension! 'my.company.ext.git)
;; => requires the ns, triggers register-global!, returns the ext
```

This is how meta-extensions (extension packs) work — one extension
`require`s others dynamically.

### Per-Environment (ad-hoc)

```clojure
(register-extension! environment my-ext)
```

For extensions that shouldn't be global.

## Lifecycle

```mermaid
flowchart TD
    Build["1. Build extension<br/>ext/extension {...}"] --> Global
    Global["2. register-global!<br/>or register-extension!"] --> Topo
    Topo["3. Topo-sort by ext/requires"] --> Deps
    Deps{"4. Dependencies met?"}
    Deps -->|yes| Install["5. Install into environment"]
    Deps -->|no| Fail(["Throws missing-dependencies"])
    Install --> Activate
    Activate{"6. Per-query<br/>activation-fn?"}
    Activate -->|active| Nudge["7. Per-iteration<br/>nudge-fn called"]
    Activate -->|inactive| Skip(["Symbols unbound<br/>nudge skipped"])
    Nudge --> Hooks["8. Per-call hooks<br/>before-fn, fn, after-fn"]
```

## Quick Example

```clojure
(require '[c.b.vis.loop.runtime.conversation.environment.extension :as ext])

(def my-ext
  (ext/extension
    {:ext/namespace     'com.acme.ext.my-tool
     :ext/doc           "My custom tool"
     :ext/group         "tools"
     :ext/requires      ['com.blockether.vis.ext.editing]
     :ext/prompt        "Use (my-tool query) to search things."
     :ext/symbols       [(ext/symbol 'com.acme.ext.my-tool search-fn
                           {:doc "Search for things"
                            :arglists '([query])})]
     :ext/nudge-fn      (fn [{:keys [iteration prev-expressions]}]
                          (when (and (> iteration 5)
                                    (some :timeout? prev-expressions))
                            "[system_nudge] my-tool is timing out. Use smaller queries."))}))

(register-extension! environment my-ext)
```

## Sections

- [Extension Spec](spec.md) — all keys, defaults, validation
- [Hook Protocol](hooks.md) — `:before-fn`, `:after-fn`, `:on-error-fn`
- [Environment Map](environment.md) — every key in the environment
- [Nudge System](nudges.md) — built-in + extension nudges
