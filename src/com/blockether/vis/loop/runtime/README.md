# loop/runtime — The Agent Runtime

> **MANDATORY**: Update this file whenever ANY file in `loop/runtime/` changes.
> This is the single source of truth for how the agent loop works.

## Architecture

```
conversation
  └─ query (one per user turn)
       └─ iteration (one per LLM call, up to N per query)
            └─ SCI execution (code blocks run in sandbox)
```

The directory structure mirrors the domain hierarchy:

```
runtime/
  conversation/           ← conversation lifecycle (owns env + query)
    core.clj                 create!, send!, close!, delete!
    shared.clj               reusable conversation helpers (cache, base-tools,
                             error->user-message, streaming chunk helpers)
    persistence.clj          vis_conversation sidecar table (CRUD + db-info +
                             sweep-orphaned-running-queries!)
    environment/          ← RLM environment lifecycle
      base.clj               SCI sandbox primitives: context construction,
                             bindings (sci-update-binding!, bind-and-bump!),
                             var-index (build-var-index)
      core.clj               orchestration: hook system, execute-tool pipeline,
                             tool registration, built-in tool wiring
      extension.clj          extension spec: namespace-like bundles that add
                             symbols, classes, and docs to the SCI sandbox
      query/              ← one turn: prep → loop → refine → finalize
        shared.clj           reusable: router, var snapshots, system-var lifecycle
        persistence.clj      DB boundary: store-query!, update-query!
        base.clj             CORE ENGINE: iteration-loop + run-query!
        core.clj             query-env! orchestration (context prep, refinement, Q-values)

        iteration/        ← one LLM call: assemble → ask → execute → format
          shared.clj         reusable: rlm-debug!, rlm-stage!, reasoning levels, answer-str
          persistence.clj    DB boundary: store-iteration!
          core.clj           run-iteration (single LLM call + response handling)
          execute.clj        SCI sandbox execution, timeouts, stdout capture
          validate.clj       syntax parsing + code-block + answer validation
          format.clj         journal rendering, error hints, prior-thinking, handover
          assemble.clj       message assembly: system + user + history + var-index + nudges

  shared.clj              ← realize-value, truncate, shape, date tools, doc tools, history tools
  prompt.clj              ← system prompt builder (ARCH rules, tool docs, response format)
  tool_diagnostics.clj    ← process-global tool telemetry (vis doctor)
```

## Flow: What happens when the user sends a message

```
1. CONVERSATION LAYER (conversation/core.clj)
   conversation/send! acquires the per-conversation lock, builds
   history messages, calls query-env!.

2. QUERY LAYER (conversation/environment/query/core.clj → conversation/environment/query/base.clj)
   query-env! does:
   a. Validate inputs, extract query-str from messages
   b. Per-turn tool activation pass (activation-fn per tool)
   c. Bind per-query SCI vars (request-more-iterations, etc.)
   d. Call run-query! which:
      - Stores a :query entity in DB
      - Calls iteration-loop
      - Updates the :query entity with results
   e. Optional refinement pass (cross-model verify if confidence=low)
   f. Q-value reward updates for cited pages
   g. Finalize: persist cost, duration, tokens

3. ITERATION LOOP (conversation/environment/query/base.clj)
   iteration-loop runs N iterations:
   a. Build system prompt (prompt.clj)
   b. Assemble messages (iteration/assemble.clj)
   c. Call LLM via run-iteration (iteration/core.clj)
   d. If :final → return answer
   e. If :code → execute blocks (iteration/execute.clj)
   f. Format results as <journal> (iteration/format.clj)
   g. Persist iteration + vars to DB
   h. Loop back to (b)
```

## State: What lives where

| State | Location | Lifetime |
|-------|----------|----------|
| Router (singleton) | `query/shared.clj/router-atom` | Process |
| SCI sandbox | `env :sci-ctx` | Conversation |
| Var-index cache | `env :var-index-atom` | Conversation (reset per fork) |
| Tool registry | `env :tool-registry-atom` | Conversation |
| Skill registry | `env :skill-registry-atom` | Conversation |
| Max-iterations budget | `env :max-iterations-atom` | Query (set by query-env!) |
| Current iteration ref | `env :current-iteration-ref-atom` | Query (reset per query) |
| Query context depth | `query-ctx :depth` (plain int) | Query (0 = top-level) |
| Token usage | `usage-atom` (local) | Query |
| Repetition counts | `call-counts-atom` (local) | Query |
| Tool diagnostics | `tool_diagnostics.clj/diagnostics` | Process |

## Key env map shape

```clojure
{:env-id              "uuid-str"
 :conversation-id     [:id uuid]        ;; entity ref
 :parent-iteration-id [:id uuid]|nil    ;; vestigial; always nil for top-level queries
 :db-info             {...}             ;; SQLite connection
 :router              <svar-router>     ;; LLM provider router
 :sci-ctx             <sci-context>     ;; SCI sandbox
 :sandbox-ns          <sci-ns>          ;; sandbox namespace
 :initial-ns-keys     #{sym...}         ;; built-in tool symbols
 :tool-registry-atom  (atom {sym → tool-def})
 :skill-registry-atom (atom {kw → skill})
 :var-index-atom      (atom {:index str :revision n :current-revision n})
 :max-iterations-atom (atom n)          ;; set per query
 :current-iteration-ref-atom (atom [:id uuid]|nil)
 :state-atom          (atom {:custom-bindings {} :custom-docs []})}
```

## Dependency graph

```
                    (leaves — no runtime/ deps)
                    shared.clj
                    tool_diagnostics.clj
                         │
              conversation/environment/
                    base.clj (SCI sandbox, var-index)
                    extension.clj (extension spec)
                    core.clj (hooks, execute-tool, tool registration)
                         │
                    prompt.clj (system prompt builder)
                         │
              ┌──── conversation/environment/query/iteration/*
              │       (validate → execute → format → assemble → core)
              │
         conversation/environment/query/base.clj (router + iteration-loop + run-query!)
              │
         conversation/environment/query/core.clj (query-env! orchestration)
              │
         conversation/core.clj (conversation lifecycle)
```

## DB entity tree

```
:conversation (one per conversation)
  └─ :query (one per user turn)
       └─ :iteration (one per LLM call)
            └─ :iteration-var (one per (def ...) in that iteration)
```
