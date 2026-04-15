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
 :conv       nil              ;; {:id conv-id} — handle into the conversations cache
 :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
 :msg-scroll nil              ;; row offset, nil = auto-bottom
 :input      {:lines [] :crow 0 :ccol 0}
 :loading?   false            ;; true while RLM is working
 :dialog-open? false}         ;; dialog singleton guard
```

### Conversations (`com.blockether.vis.conversations`)

**One module owns env lifecycle for every frontend.** Web + TUI share the `:vis` channel; Telegram has its own `:telegram` channel. Conversation IDs are plain UUIDs — the same `:entity/id` svar uses. No name prefixes, no string lookups.

```clojure
(require '[com.blockether.vis.conversations :as conv])

;; Create / lookup
(conv/create! :vis)                    ;; new web/TUI conversation
(conv/create! :vis {:title "…"})
(conv/by-id conv-id)                   ;; conv map or nil
(conv/by-channel :vis)                 ;; sidebar / list, recent first
(conv/for-telegram-chat! chat-id)      ;; find-or-create by chat-id

;; Mutate
(conv/set-title! conv-id "New title")
(conv/env-for conv-id)                 ;; raw rlm env (for presenter projections)

;; Turn
(conv/send! conv-id msgs opts)         ;; locked per conv-id; see docstring for every opt

;; Lifecycle
(conv/close! conv-id)                  ;; release env handle, keep DB data
(conv/delete! conv-id)                 ;; close + purge entity tree + sidecar row
(conv/close-all!)                      ;; process shutdown
```

### Storage

**ONE SQLite DB for everything.** Path: `~/.vis/vis.mdb` (`config/db-path`). Every frontend opens the same DB; svar's connection pool is shared across envs, so disposing one conversation's env never breaks siblings.

Two layers:

- **rlm layer** — `:conversation` / `:query` / `:iteration` / `:iteration-var` entity tree. Owned by `com.blockether.vis.rlm.*`. `conversation_attrs` holds only `entity_id`, `system_prompt`, `model` — no name, no env_id, no caller-facing lookup keys.
- **vis sidecar** — `vis_conversation(conversation_id, channel, external_id, title, created_at)`. Keyed by the rlm conversation's entity id. `UNIQUE(channel, external_id)` is how Telegram resolves a chat-id to its conversation. Installed lazily on first access from `com.blockether.vis.conversations.schema`.

**Entity model (unchanged):**
- `:conversation` — one per chat/session; holds `:conversation/system-prompt`, `:conversation/model`.
- `:query` — one per user turn, parented to `:conversation`. `:query/text`, `:query/answer`, `:query/status`, `:query/iterations`, `:query/duration-ms`, `:query/messages` (pr-str'd).
- `:iteration` — one per LLM iteration inside a query. `:iteration/code` (pr-str'd source vec), `:iteration/results`, `:iteration/thinking`, `:iteration/answer` (present iff `:final`), `:iteration/duration-ms`.
- `:iteration-var` — one per `(def …)` inside an iteration. `:iteration.var/name`, `:iteration.var/value` (pr-str'd), `:iteration.var/code`.

Ordering within a parent is by `:entity/created-at`. `restore-var` / `restore-vars` SCI tools rebind prior `:iteration-var`s on demand.

**Investigating DB state:**
```clojure
(require '[com.blockether.vis.conversations :as conv]
         '[com.blockether.vis.rlm.db :as rlm-db]
         '[com.blockether.vis.config :as config])

;; List vis/telegram conversations
(conv/by-channel :vis)
(conv/by-channel :telegram)

;; Raw rlm view via the conversation's env
(let [env      (conv/env-for conv-id)
      db-info  (:db-info env)
      conv-ref (:conversation-ref env)]
  (rlm-db/db-query-history db-info conv-ref)       ;; {:text :answer-preview :iterations :key-vars}
  (rlm-db/db-list-conversation-queries db-info conv-ref)
  (rlm-db/db-latest-var-registry db-info conv-ref));; {sym → {:value :code …}}
```

**Key rlm DB functions (`com.blockether.vis.rlm.db`):**
- `create-rlm-conn` / `dispose-rlm-conn!` — SQLite pool handle open/close.
- `db-find-latest-conversation-ref` — the most-recent `:conversation` entity.
- `db-resolve-conversation-ref` — accepts `nil` / `:latest` / uuid / `[:id uuid]`.
- `db-list-conversation-queries` / `db-list-query-iterations` / `db-list-iteration-vars` — ordered children.
- `db-query-history` — compact summaries for UI rendering.
- `db-latest-var-registry` — last-write-wins var map for a conversation.
- `delete-entity-tree!` — delete an entity and every descendant via parent_id.

**rlm entrypoints vis uses (`com.blockether.vis.rlm`):**
- `create-env router {:db path :conversation selector}` — selector is `nil` | `:latest` | uuid | `[:id uuid]`. Nil creates a fresh conversation; an id-ref resumes an existing one.
- `register-env-fn!` / `register-env-def!` — wire tools/constants into the SCI sandbox.
- `query-env! env [(llm/user "...")] opts` — messages must be a vector of message maps. See `conv/send!` docstring for every opt forwarded to `query-env!`.
- `ingest-git! env {:repo-path path :n 100}` — JGit-backed; attaches `search-commits`/`commit-history`/`file-history`/`blame`/`commit-diff`/`commit-parents`/`commits-by-ticket` to the sandbox. `:repo/name` is unique — repeated calls dedupe.
- `dispose-env!` — releases the env handle; the shared SQLite DataSource stays open for sibling envs.

**Iteration lifecycle:** The LLM does **not** call `(FINAL ...)` as a SCI fn. svar sends a spec-validated JSON response per provider capability: `ITERATION_SPEC_NON_REASONING` (includes `:thinking`) or `ITERATION_SPEC_REASONING` (no `:thinking`). Shared fields come from `ITERATION_SPEC_BASE` (`:code` vec + optional `:final {:answer :confidence :language :sources}` + `:next-optimize`). When `:final` is set, iteration stops and the answer is the RLM result. Observability: pass `{:hooks {:on-chunk (fn [{:iteration :thinking :code :final :done?}])}}` to `conv/send!`.

**Frontend wiring:**
- **TUI** — `chat/make-conversation` creates a fresh `:vis` conversation on every boot (history starts empty). Disposal on exit only closes the env; the conversation stays in the `:vis` channel so the web sidebar can see it.
- **Web** — `/new` → `conv/create! :vis`; URL `/s/<uuid>` is the conv-id directly; the server projects message lists lazily from the entity tree (cached in `server/messages-cache`) and generates a title from the first user message via `server/set-session-title!`. `delete-session!` → `conv/delete!` purges the entity tree + sidecar row. `executor/on-chunk-handler` maps svar's streaming callback into `server/live-status` so the `/s/:id?check=N` polling endpoint drives the live trace UI.
- **Telegram** — `conv/for-telegram-chat!` find-or-creates by chat-id; each incoming message becomes a `conv/send!` with the Telegram persona system prompt.
- **CLI `agent/run!`** — ephemeral one-shot. Creates a fresh rlm env (no sidecar row) and deletes the conversation entity tree in a `finally` after the query returns — CLI invocations don't accumulate in the `:vis` channel.
