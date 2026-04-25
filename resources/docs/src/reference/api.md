# Public API

> **Namespace:** `com.blockether.vis.core` (facade) and
> `com.blockether.vis.loop.core` (implementation)

## Environment Lifecycle

### `create-environment`

```clojure
(create-environment router opts) → environment
```

Creates an RLM environment for querying.

- `router` — LLM router from `config/make-router`
- `opts`:
  - `:db` — `nil` | `:temp` | path string | `{:path p}` | `{:datasource ds}`
  - `:conversation` — `nil` | `:latest` | uuid | `[:id uuid]`
  - `:channel` — `:vis` | `:telegram` | `:cli`
  - `:external-id` — external identifier (e.g. Telegram chat-id)
  - `:title` — conversation title

Returns the environment map. See [Environment Map](../extensions/environment.md).

### `dispose-environment!`

```clojure
(dispose-environment! environment) → nil
```

Releases the environment handle. The shared SQLite DataSource stays
open for sibling environments.

### `register-extension!`

```clojure
(register-extension! environment ext) → environment
```

Registers a validated extension. Appends to the environment's
`:extensions` atom. Returns the environment for chaining.

See [Extension Spec](../extensions/spec.md).

## Query Execution

### `query!`

```clojure
(query! environment messages opts) → result-map
```

Runs a query using iterative LLM code evaluation.

- `environment` — from `create-environment`
- `messages` — vector of message maps, e.g. `[(llm/user "...")]`
- `opts` (all optional):
  - `:max-iterations` — iteration budget (default 10)
  - `:reasoning-default` — base reasoning level (default `:balanced`)
  - `:debug?` — enable verbose logging
  - `:hooks` — `{:on-iteration fn, :on-chunk fn, :on-cancel fn}`
  - `:routing` — svar routing overrides
  - `:cancel-atom` — atom; set to `true` to cooperatively cancel

Returns:

```clojure
{:answer      "..."           ;; final answer string
 :iterations  5               ;; iterations used
 :duration-ms 2340            ;; wall-clock time
 :tokens      {:input N :output N :reasoning N :cached N :total N}
 :cost        {:input-cost N :output-cost N :total-cost N}
 :confidence  :high           ;; :high | :medium | :low
 :trace       [...]           ;; per-iteration trace entries
 ;; On failure:
 :status      :max-iterations ;; or :error-budget-exhausted, :cancelled
 :status-id   :rlm.status/max-iterations}
```

## Conversation Layer

> **Namespace:** `com.blockether.vis.loop.runtime.conversation.core`

Higher-level API used by channels. Wraps environment lifecycle + query
execution with per-conversation locking and caching.

```clojure
(create! channel)                    ;; → {:id "uuid" :channel :vis ...}
(create! channel {:title "..."})
(send! id messages opts)             ;; → result-map (locked per conversation)
(close! id)                          ;; dispose env, keep DB data
(delete! id)                         ;; close + purge entity tree
(by-id id)                           ;; → conversation map or nil
(by-channel :vis)                    ;; → [conversation ...] recent first
(env-for id)                         ;; → raw environment (for projections)
(for-telegram-chat! chat-id)         ;; find-or-create by chat-id
(set-title! id "New title")
(close-all!)                         ;; process shutdown
```
