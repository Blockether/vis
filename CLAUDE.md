# Vis - Development Guide

## Clojure CLI

### JS Interop

Always use `^js` type annotations when interoperating with JavaScript:

```clojure
;; Good - specify JS types
(defn handler [^js key]
  ...)

;; Good - convert Clojure data to JS objects
(react/createElement Component #js {:props value})

;; Bad - will cause type inference issues
(defn handler [key]
  ...)
```

### Running

```bash
clojure -M:run
```

### Project Structure

- `src/com/blockether/vis/cli.clj` - CLI entrypoint (subcommands: chat, run, help)
- `src/com/blockether/vis/agent.clj` - Agent orchestration over svar RLM (Sandcastle-inspired)
- `src/com/blockether/vis/config.clj` - Config I/O, provider presets, svar-native helpers
- `src/com/blockether/vis/logging.clj` - TTY logging setup, stream redirection
- `src/com/blockether/vis/redact.clj` - Secret redaction for logs and tool output
- `src/com/blockether/vis/trace.clj` - ANSI trace renderer for CLI + structured sections for TUI
- `src/com/blockether/vis/languages/commons/read.clj` - Base READ tool (offset/limit)
- `src/com/blockether/vis/languages/commons/write.clj` - Base WRITE tool (full overwrite)
- `src/com/blockether/vis/languages/commons/edit.clj` - Base EDIT tool (old_string → new_string)
- `src/com/blockether/vis/tui/state.clj` - App state (re-frame pattern)
- `src/com/blockether/vis/tui/screen.clj` - TUI main loop and rendering
- `src/com/blockether/vis/tui/render.clj` - Rendering components (boxes, bubbles, text wrapping)
- `src/com/blockether/vis/tui/chat.clj` - LLM integration via SVAR RLM
- `src/com/blockether/vis/tui/dialogs.clj` - Interactive dialog screens
- `src/com/blockether/vis/tui/input.clj` - Keyboard input handling
- `src/com/blockether/vis/tui/provider.clj` - Provider setup wizard (TUI dialogs)
- `src/com/blockether/vis/tui/theme.clj` - Colors and visual constants

### Agent Module (agent.clj)

Sandcastle-inspired agent orchestration. Define agents as data, register tools, run one-shot queries with JSON/EDN output.

**Programmatic usage:**
```clojure
(require '[com.blockether.vis.agent :as ag])

;; Define an agent with tools
(def reviewer
  (ag/agent {:name "reviewer"
             :system-prompt "You are a senior Clojure engineer. Review code for bugs, performance, and style."
             :tools [(ag/tool 'read-file slurp
                       {:doc "Read file contents"
                        :params [{:name "path" :type :string :required true}]
                        :returns {:type :string :description "File contents"}})]
             :max-iterations 30}))

;; Run a query
(ag/run! reviewer "Review src/auth.clj for security issues")
;; => {:answer "..." :iterations 5 :duration-ms 2340 :tokens {...} :cost {...}}

;; JSON output
(ag/result->json result)
```

**CLI usage:**
```bash
vis run "What is 2+2?"                          # plain text output
vis run --json "Explain the auth flow"          # JSON output
vis run --model gpt-4o "Summarize X"            # override model
vis run --system-prompt "You are a code reviewer" "Review auth.clj"
```

### State Management (MANDATORY)

All application state lives in `tui/state.clj` using a re-frame dispatch pattern.

**Rules:**
- ALL app state must go through `state/app-db` — never create standalone atoms for app state
- State changes MUST use `(state/dispatch [:event-name args...])` — never `swap!`/`reset!` on app-db directly
- Event handlers registered with `reg-event-db` must be **pure functions**: `(fn [db event-vec] new-db)`
- Side effects (RLM calls, disk I/O, futures) go through `reg-event-fx` + `reg-fx`
- Local ephemeral state in dialogs (selection index, cursor position) may use local atoms — these are component-scoped and transient

```clojure
;; Good — dispatch an event
(state/dispatch [:send-message text])

;; Good — pure event handler
(reg-event-db :update-input
  (fn [db [_ new-input]]
    (assoc db :input new-input)))

;; Good — side effects via reg-event-fx
(reg-event-fx :send-message
  (fn [db [_ text]]
    {:db (-> db (update :messages conj msg) (assoc :loading? true))
     :fx [[:rlm-query (:conv db) text]]}))

;; Bad — direct atom mutation
(swap! some-atom assoc :key val)
```

**App-db shape:**
```clojure
{:config     nil              ;; provider config map: {:providers [{:id kw :label str :api-base str :api-key? str :model str} ...]}
 :conv       nil              ;; {:env ...} RLM environment
 :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
 :msg-scroll nil              ;; row offset, nil = auto-bottom
 :input      {:lines [] :crow 0 :ccol 0}
 :loading?   false            ;; true while RLM is working
 :dialog-open? false}         ;; dialog singleton guard
```

### Session Database (SQLite)

**ONE SQLite DB for everything.** Path: `~/.vis/vis.db` (`config/db-path`). Every vis entrypoint — TUI chat, web sessions, Telegram chats, CLI `run` — opens the same DB. Each entrypoint owns a distinct `:conversation` inside, addressed by the caller-supplied `:conversation/name`:

| Entrypoint | Conversation name | Helper                                  |
|------------|-------------------|-----------------------------------------|
| TUI chat   | `tui:default`     | `(config/conversation-name :tui)`       |
| Web session| `session:<uuid>`  | `(config/conversation-name :session id)`|
| Telegram   | `telegram:<chat-id>` | `(config/conversation-name :telegram chat-id)` |
| CLI run    | fresh each call (no name) unless caller passes `:conversation`| — |

svar resolves `{:conversation {:name s}}` as lookup-or-create: first call with a name creates the `:conversation` entity; subsequent calls resolve the existing one and rehydrate message history + var registry.

The SQLite DataSource is process-wide shared, so every env uses one pool. svar's `dispose-env!` leaves the shared DataSource open (`:owned? false`) so disposing one env never breaks sibling envs.

**Entity model:**
- `:conversation` — one per session/chat, holds `:conversation/name` (unique identity, caller-supplied), `:conversation/env-id`, `:conversation/model`, `:conversation/system-prompt`
- `:query` — one per user turn, parented to `:conversation` via `:entity/parent-id`, holds `:query/text`, `:query/answer`, `:query/status`, `:query/iterations`, `:query/duration-ms`, `:query/messages` (pr-str'd)
- `:iteration` — one per LLM iteration inside a query, parented to `:query`, holds `:iteration/code` (pr-str'd vec of source strings), `:iteration/results` (pr-str'd vec), `:iteration/thinking`, `:iteration/answer` (present iff this iteration emitted `:final`), `:iteration/duration-ms`
- `:iteration-var` — one per `(def …)` executed in an iteration, parented to `:iteration`, holds `:iteration.var/name`, `:iteration.var/value` (pr-str'd), `:iteration.var/code`

All ordering within a parent is by `:entity/created-at` (no explicit index attribute). The LLM uses `restore-var`/`restore-vars` SCI tools to rebind prior `:iteration-var`s on demand.

**Investigating DB state:**
```clojure
(require '[com.blockether.vis.rlm.db :as rlm-db]
         '[com.blockether.vis.config :as config])

(let [db-info (rlm-db/create-rlm-conn config/db-path)
      conv    (rlm-db/db-resolve-conversation-ref db-info {:name "telegram:12345"})]

  ;; List conversations by prefix
  (rlm-db/db-list-conversations-by-prefix db-info "session:")

  ;; Compact query summaries for one conversation
  (rlm-db/db-query-history db-info conv)

  ;; Full iteration entities for one query
  (let [[q & _] (rlm-db/db-list-conversation-queries db-info conv)]
    (rlm-db/db-list-query-iterations db-info [:entity/id (:entity/id q)]))

  ;; Latest {sym → {:value :code :query-id :query-ref :iteration-id :created-at}}
  (rlm-db/db-latest-var-registry db-info conv)

  (rlm-db/dispose-rlm-conn! db-info))
```

**Common DB issues:**
- `Sessions: 0` on startup — `load-sessions!` prints `[server] Failed to load session <id>: <msg>` when a `:conversation/name` is present but rehydration fails.
- WAL `.tmp` files — incomplete writes from hard kills. Safe to delete.

**Key DB functions (in `rlm.db` / `rlm.sqlite`):**
- `rlm-db/db-find-latest-conversation-ref` — lookup ref for the most-recent `:conversation`
- `rlm-db/db-find-named-conversation-ref` — lookup ref by `:conversation/name` (nil if absent)
- `rlm-db/db-resolve-conversation-ref` — resolves `:latest`, uuid, lookup ref, or `{:name s}`
- `rlm-db/db-list-conversations-by-prefix` — list conversations whose name starts with a prefix
- `rlm-db/db-list-conversation-queries` — ordered `:query` entities under a conversation
- `rlm-db/db-list-query-iterations` — ordered `:iteration` entities under a query
- `rlm-db/db-list-iteration-vars` — persisted `:iteration-var` entities
- `rlm-db/db-query-history` — compact `{:text :answer-preview :iterations :key-vars}` for rendering
- `rlm-db/db-latest-var-registry` — last-write-wins map of `sym → {:value :code ...}`
- `rlm-db/delete-entity-tree!` — delete entity + all descendants via parent_id chain

**svar RLM entrypoints vis uses (`com.blockether.svar.internal.rlm`):**
- `create-env router {:db path :conversation selector}` — selector is `:latest` | `{:name s}` | uuid | `[:entity/id uuid]` | nil. `{:name s}` is lookup-or-create — if the conversation exists it's resumed, otherwise a new one is created with that name.
- `register-env-fn!` / `register-env-def!` — wire tools/constants into the SCI sandbox
- `query-env! env [(llm/user "...")] opts` — **messages must be a vector of message maps**; `opts` accepts `:system-prompt :context :spec :model :max-iterations :on-chunk :verify? :debug? :eval-timeout-ms`
- `ingest-git! env {:repo-path path :n 100}` — JGit-backed, attaches `search-commits`/`commit-history`/`file-history`/`blame`/`commit-diff`/`commit-parents`/`commits-by-ticket` to the sandbox. `:repo/name` is unique identity — repeated calls for the same repo dedupe.
- `dispose-env!` — releases the env handle; on a persistent :db path, the shared SQLite DataSource stays open so sibling envs keep working.

**Iteration lifecycle:** The LLM does **not** call `(FINAL ...)` as a SCI fn. svar sends a spec-validated JSON response per provider capability: `ITERATION_SPEC_NON_REASONING` (includes `:thinking`) or `ITERATION_SPEC_REASONING` (no `:thinking`). Shared fields come from `ITERATION_SPEC_BASE` (`:code` vec + optional `:final {:answer :confidence :language :sources}` + `:next-optimize`). When `:final` is set, svar stops iterating and the answer is the RLM result. Observability is via the single `:on-chunk` callback — `{:iteration :thinking :code :final :done?}` — not the old `:hooks` map, which no longer exists.

**Web session lifecycle:**
- `server/create-session!` — opens a named `:conversation` `session:<uuid>` in the shared DB; no per-session directory.
- `server/load-sessions!` — on startup, queries every conversation whose `:conversation/name` starts with `session:` via `rlm-db/db-list-conversations-by-prefix` and rehydrates each into an env.
- `server/load-messages-from-db` — projects `:conversation` → `:query` → `:iteration` into the presenter's `{:role :text :result}` shape; no message entities involved.
- `server/delete-session!` — disposes env + deletes the conversation's entity tree via `rlm-db/delete-entity-tree!`. Caller (routes) checks in-flight status first.
- `executor/on-chunk-handler` — maps svar's streaming callback into `server/live-status` so the `/s/:id?check=N` polling endpoint can drive the live trace UI.
