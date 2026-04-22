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
      query/                ← one turn: prep → loop → refine → finalize
      shared.clj             reusable: router, var snapshots, system-var lifecycle
      persistence.clj        DB boundary: store-query!, update-query!
      base.clj               CORE ENGINE: iteration-loop + run-query!
      core.clj               query-env! orchestration (context prep, refinement, Q-values)
      subquery.clj           sub-RLM: fork SCI + parent handoff + run-query!

      iteration/          ← one LLM call: assemble → ask → execute → format
        shared.clj           reusable: rlm-debug!, rlm-stage!, reasoning levels, answer-str
        persistence.clj      DB boundary: store-iteration!
        core.clj             run-iteration (single LLM call + response handling)
        execute.clj          SCI sandbox execution, timeouts, stdout capture
        validate.clj         syntax parsing + code-block + answer validation
        filter.clj           supersession: skip redundant tool calls
        format.clj           journal rendering, error hints, prior-thinking, handover
        assemble.clj         message assembly: system + user + history + var-index + nudges

  shared.clj              ← realize-value, truncate, shape, date tools, doc tools, history tools
  prompt.clj              ← system prompt builder (ARCH rules, tool docs, response format)
  tool_diagnostics.clj    ← process-global tool telemetry (vis doctor)
  tools/
    core.clj              ← tool factories: search-documents, request-more-iterations, etc.
    git.clj               ← git tool bindings (search-commits, blame, diff)
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
   c. Bind per-query SCI vars (sub-rlm-query, request-more-iterations)
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
   f. Filter superseded blocks (iteration/filter.clj)
   g. Format results as <journal> (iteration/format.clj)
   h. Persist iteration + vars to DB
   i. Loop back to (b)

4. SUB-QUERIES (conversation/environment/query/subquery.clj)
   When agent code calls (sub-rlm-query "prompt" opts):
   a. Depth tracking + skill resolution (in query/base.clj)
   b. subquery.clj forks the SCI env, prepends parent handoff
   c. Calls run-query! → iteration-loop (same as main query)
   d. Returns result to parent sandbox
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
| Token usage | `usage-atom` (local) | Query |
| Repetition counts | `call-counts-atom` (local) | Query |
| Tool diagnostics | `tool_diagnostics.clj/diagnostics` | Process |

## Key env map shape

```clojure
{:env-id              "uuid-str"
 :conversation-id     [:id uuid]        ;; entity ref
 :parent-iteration-id [:id uuid]|nil    ;; set for sub-RLMs
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
 :state-atom          (atom {:custom-bindings {} :custom-docs []})
 :depth-atom          (atom 0)          ;; sub-rlm recursion depth
 :sub-rlm-query-fn    fn}
```

## Dependency graph

```
                    (leaves — no runtime/ deps)
                    shared.clj
                    tool_diagnostics.clj
                    tools/core.clj, tools/git.clj
                         │
                    core.clj (SCI sandbox, var-index, hooks)
                         │
                    prompt.clj (system prompt builder)
                         │
              ┌──── conversation/environment/query/iteration/*
              │       (validate → execute → filter → format → assemble → core)
              │
         conversation/environment/query/base.clj (router + iteration-loop + run-query!)
              │
    ┌─────────┼──────────────┐
    │         │              │
 subquery  base.clj    conversation/environment/query/core.clj
    │                        │
    └────────────────── conversation/core.clj
```

## DB entity tree

```
:conversation (one per conversation)
  └─ :query (one per user turn)
       └─ :iteration (one per LLM call)
            └─ :iteration-var (one per (def ...) in that iteration)
       └─ :query (sub-RLM, parented to invoking :iteration)
            └─ :iteration
                 └─ :iteration-var
```
