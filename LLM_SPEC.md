# LLM Routing, Retry, Fallback Persistence Spec

## Goal

Make provider routing observable, durable, queryable. 429 retry/fallback must be visible live in TUI, persisted in SQLite core schema, replayed on resume, configurable from Vis config.

## Non-goals

- No opaque routing data only in JSON metadata.
- No collapsed retry trace that only shows final fallback.
- No generic catch-all routing metadata or ultra-wide workaround table for core facts.
- No out-of-band migration file for this branch; update core V1 schema inline until migration policy changes.
- No TUI-only state as source of truth.
- No retry after streamed content starts.
- No silent provider fallback.
- No `vis conversations routing <turn-id>` CLI command for this feature. TUI shows routing state; DB rows preserve it.

## Terms

- **selected**: provider/model chosen before request.
- **actual**: provider/model that produced final response.
- **retry event**: same provider/model retry after transient failure, e.g. 429.
- **fallback event**: router changes provider/model after retry budget or hard transient failure.
- **routing trace**: ordered retry/fallback events for one LLM request.

## Config

### Change

Add router retry/fallback policy to Vis config and pass through to svar router creation.

```clojure
{:router
 {:rate-limit
  {:same-provider-delays-ms [2000 3000 6000]
   :fallback-after-ms 30000
   :respect-retry-after? true
   :fallback-provider? true}}
 :providers
 [{:id :anthropic-coding-plan
   :models [{:name "claude-opus-4-7"}]}
  {:id :openai-codex
   :models [{:name "gpt-5.3-codex"}]}]}
```

### Rationale

Retry policy belongs to routing, not TUI. User needs tune latency/cost/reliability without code change.

## svar behavior

### Change

429 handling becomes same-provider-first, fallback-after-budget.

```clojure
;; effective defaults
{:same-provider-delays-ms [2000 3000 6000]
 :fallback-after-ms 30000
 :respect-retry-after? true
 :fallback-provider? true}
```

Algorithm:

```clojure
;; on 429 before first streamed content:
;; 1. retry same provider after 2s
;; 2. retry same provider after 3s
;; 3. retry same provider after 6s
;; 4. if total wait budget remains, wait until fallback-after-ms boundary
;; 5. fallback to next provider
;; 6. persist/emit full trace
```

### Rationale

429 often clears fast. Immediate cross-provider fallback hides quota pressure and burns wrong account/model. Waiting forever hangs tab. Budgeted same-provider retries + bounded fallback gives predictable behavior.

## svar work plan

Target repo: `../svar`. svar owns routing truth. Vis only consumes svar routing trace.

### Contract

One shape only:

```clojure
:routed/trace [event event event]
```

`event` is same map everywhere:

- appended to final `:routed/trace`
- emitted live to caller/TUI when available
- persisted by Vis into `llm_routing_event`
- rendered by TUI progress/resume

No second event shape. No separate durable `fallback-trace`. Current `:routed/fallback-trace` becomes temporary compatibility alias only during migration; consumers must move to `:routed/trace`.

### Current svar state

- `src/clj/com/blockether/svar/internal/router.clj` has `fallback-trace` atom in `with-provider-fallback`. It records provider/format fallback only and returns `:routed/fallback-trace`.
- `src/clj/com/blockether/svar/internal/llm.clj` has `with-retry`, used by `chat-completion-with-retry`. It silently retries 429/502/503/504 with logs only. These retries are not in any trace and not visible to TUI.
- streaming fallback notifications use `on-chunk {:reset? true ...}`. That is live control state, not durable event contract.

### Svar steps

1. Define routing event normalizer in `internal/router.clj`.
   - event keys use final trace schema: `:event/id`, `:event/type`, provider/model fields, status/reason, delay/elapsed/attempt, `:at-ms`.
   - event types: `:llm.routing/provider-retry`, `:llm.routing/provider-fallback`, optionally `:llm.routing/format-fallback`.
2. Replace `fallback-trace` atom with `trace` atom in `with-provider-fallback`.
   - append normalized event maps only.
   - return `:routed/trace`.
   - add `:routed/fallback-trace` only as deprecated alias while Vis migrates.
3. Move 429 retry policy to router layer.
   - add router rate-limit policy defaults: `:same-provider-delays-ms`, `:fallback-after-ms`, `:respect-retry-after?`, `:fallback-provider?`.
   - on 429 before streamed content, append `:llm.routing/provider-retry`, sleep configured delay, retry same provider/model.
   - when budget exhausted and provider changes, append `:llm.routing/provider-fallback`.
4. Stop opaque 429 retry in `llm/with-retry`.
   - either remove 429 from primitive retry set when call is routed, or make `with-retry` accept trace callback/policy from router.
   - invariant: every retry that can affect user wait appears in `:routed/trace`.
5. Preserve streaming safety.
   - track first content chunk before retry/fallback.
   - if any content reached caller, throw stream error; do not retry or fallback.
6. Tests in `test/com/blockether/svar/internal/*`.
   - p1 429 retry retry retry then p2 success.
   - trace order equals live event order.
   - final result has `:routed/trace` with same event maps.
   - no retry/fallback after first streamed content.
   - `Retry-After` behavior covered.

Do not implement Vis CLI. Vis consumes svar `:routed/trace` through TUI/live callbacks and persistence.

## svar trace shape

### Change

Every retry/fallback produces structured event. Trace is ordered vector of those event maps; no separate trace/event schemas and no trace collapse.

```clojure
{:event/id "uuid"
 :event/type :llm.routing/provider-retry
 :status 429
 :reason :rate-limit
 :provider "anthropic-coding-plan"
 :model "claude-opus-4-7"
 :attempt 1
 :delay-ms 2000
 :at-ms 1779093990000}
```

```clojure
{:event/id "uuid"
 :event/type :llm.routing/provider-fallback
 :status 429
 :reason :rate-limit-budget-exhausted
 :from-provider "anthropic-coding-plan"
 :from-model "claude-opus-4-7"
 :to-provider "openai-codex"
 :to-model "gpt-5.3-codex"
 :elapsed-ms 30000
 :at-ms 1779094020000}
```

Final ask result carries one trace:

```clojure
{:routed/provider-id :openai-codex
 :routed/model "gpt-5.3-codex"
 :routed/selected {:provider "anthropic-coding-plan"
                   :model "claude-opus-4-7"}
 :routed/actual {:provider "openai-codex"
                 :model "gpt-5.3-codex"}
 :routed/fallback? true
 :routed/trace [retry-event retry-event retry-event fallback-event]}
```

Live routing notifications reuse the exact event maps later present in `:routed/trace`. Consumers must not read or write a separate `:fallback-trace` shape.

### Rationale

Flat event maps are durable, renderable, testable. Namespaced `:event/type` avoids guessing. Trace is the canonical event log; summary avoids re-deriving selected/actual from trace.

## Streaming rule

### Change

Retry only if no content chunk reached caller.

```clojure
(if (and retryable? (zero? content-acc-len))
  (retry-same-provider)
  (throw stream-error))
```

### Rationale

Retry after partial stream duplicates assistant content and corrupts transcript. Before first content, retry is safe.

## SQLite core schema

### Change

Add core routing columns/tables. Do not store primary routing facts only in metadata.

Edit `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql` inline. Fresh DB rebuild is acceptable; this spec shows target table shape, not stepwise migration statements.

Target `conversation_turn_state` excerpt:

```sql
CREATE TABLE conversation_turn_state (
  id                     TEXT PRIMARY KEY NOT NULL,
  conversation_turn_soul_id TEXT NOT NULL REFERENCES conversation_turn_soul(id) ON DELETE CASCADE,
  version                INTEGER NOT NULL CHECK (version >= 0),
  status                 TEXT NOT NULL CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  answer                 BLOB,
  llm_selected_provider  TEXT,
  llm_selected_model     TEXT,
  llm_actual_provider    TEXT,
  llm_actual_model       TEXT,
  llm_fallback           INTEGER NOT NULL DEFAULT 0 CHECK (llm_fallback IN (0, 1)),
  created_at             INTEGER NOT NULL,
  UNIQUE (conversation_turn_soul_id, version)
);
```

Target `conversation_turn_iteration` excerpt:

```sql
CREATE TABLE conversation_turn_iteration (
  id                         TEXT PRIMARY KEY NOT NULL,
  conversation_turn_state_id TEXT NOT NULL REFERENCES conversation_turn_state(id) ON DELETE CASCADE,
  position                   INTEGER NOT NULL CHECK (position >= 1),
  status                     TEXT NOT NULL CHECK (status IN ('running', 'done', 'error', 'interrupted')),
  llm_provider               TEXT,
  llm_model                  TEXT,
  llm_selected_provider      TEXT,
  llm_selected_model         TEXT,
  llm_actual_provider        TEXT,
  llm_actual_model           TEXT,
  llm_fallback               INTEGER NOT NULL DEFAULT 0 CHECK (llm_fallback IN (0, 1)),
  created_at                 INTEGER NOT NULL,
  UNIQUE (conversation_turn_state_id, position)
);
```

```sql
CREATE TABLE llm_routing_event (
  id TEXT PRIMARY KEY,
  conversation_turn_state_id TEXT NOT NULL REFERENCES conversation_turn_state(id) ON DELETE CASCADE,
  conversation_turn_iteration_id TEXT REFERENCES conversation_turn_iteration(id) ON DELETE CASCADE,
  position INTEGER NOT NULL,
  event_type TEXT NOT NULL,
  status INTEGER,
  reason TEXT,
  provider TEXT,
  model TEXT,
  from_provider TEXT,
  from_model TEXT,
  to_provider TEXT,
  to_model TEXT,
  attempt INTEGER,
  delay_ms INTEGER,
  elapsed_ms INTEGER,
  error TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(conversation_turn_state_id, position)
);

CREATE INDEX idx_llm_routing_event_turn
  ON llm_routing_event(conversation_turn_state_id, position);

CREATE INDEX idx_llm_routing_event_iteration
  ON llm_routing_event(conversation_turn_iteration_id, position);
```

### Rationale

Core columns answer common questions cheaply:

```sql
SELECT llm_selected_provider, llm_selected_model,
       llm_actual_provider, llm_actual_model,
       llm_fallback
FROM conversation_turn_state
WHERE id = ?;
```

Event table preserves full timeline:

```sql
SELECT *
FROM llm_routing_event
WHERE conversation_turn_state_id = ?
ORDER BY position;
```

Metadata remains extension/supplement field only, not canonical routing store.

## Persistence API

### Change

Extend turn update payload.

```clojure
(db-update-conversation-turn! db turn-id
  {:status :success
   :answer answer
   :tokens tokens
   :cost cost
   :llm-routing {:selected {:provider "anthropic-coding-plan"
                            :model "claude-opus-4-7"}
                 :actual {:provider "openai-codex"
                          :model "gpt-5.3-codex"}
                 :fallback? true
                 :trace [retry-event fallback-event]}})
```

Persist summary to `conversation_turn_state` columns. Persist trace to `llm_routing_event` rows.

### Rationale

Call site already knows final result. One atomic turn update prevents DB/view mismatch.

## Iteration persistence

### Change

`db-store-iteration!` may receive same routing trace for per-iteration event attachment.

```clojure
(db-store-iteration! db
  {:conversation-turn-id turn-id
   :code code
   :result result
   :llm-provider actual-provider
   :llm-model actual-model
   :llm-routing {:selected selected
                 :actual actual
                 :fallback? fallback?
                 :trace trace}})
```

Events with known iteration id store `conversation_turn_iteration_id`; final turn summary also stores turn-state id. All retry rows attach to iteration when known; TUI reads these rows, not iteration metadata.

### Rationale

TUI progress is iteration-oriented. Debugging needs answer: which iteration hit 429?

## TUI live rendering

### Change

Render routing events as first-class progress rows. Render every retry row in order; do not collapse retries into only final fallback/footer.

```text
↻ retry same provider: anthropic-coding-plan/claude-opus-4-7 — 429, retry in 2s
↻ retry same provider: anthropic-coding-plan/claude-opus-4-7 — 429, retry in 3s
↪ provider fallback: anthropic-coding-plan/claude-opus-4-7 → openai-codex/gpt-5.3-codex — 429 budget exhausted
```

Assistant footer:

```text
fallback anthropic-coding-plan/claude-opus-4-7 → openai-codex/gpt-5.3-codex — 3 retries, 429
```

### Rationale

Live user sees hang reason. Footer preserves compact final truth after progress collapses.

## TUI resume rendering

### Change

Conversation rebuild reads core routing fields/events. Resume path must use summary columns plus `llm_routing_event`; metadata fallback is transitional diagnostics only.

```clojure
{:llm-selected {:provider llm_selected_provider
                :model llm_selected_model}
 :llm-actual {:provider llm_actual_provider
              :model llm_actual_model}
 :llm-fallback? (pos? llm_fallback)
 :llm-routing-trace (db-llm-routing-events db turn-state-id)}
```

### Rationale

Reopened transcript must match live transcript. DB core facts must reconstruct same bubble/footer without parsing metadata.

## Tests

### svar tests

```clojure
(it "retries 429 on same provider before fallback"
  ;; p1 429 three times, p2 success
  ;; expect calls [:p1 :p1 :p1 :p2]
  ;; expect trace retry retry retry fallback
  )
```

```clojure
(it "does not retry streaming after content chunk"
  ;; first chunk emitted, then stream error
  ;; expect throw, no duplicate retry
  )
```

### Vis persistence tests

```clojure
(it "persists routing summary in core columns and trace rows"
  ;; db-update-conversation-turn! with :llm-routing
  ;; assert conversation_turn_state llm_* columns
  ;; assert llm_routing_event rows ordered by position
  )
```

### TUI tests

```clojure
(it "renders resumed fallback footer from core routing rows"
  ;; rebuild-history reads DB routing rows
  ;; assistant message has :llm-selected/:llm-actual/:llm-routing-trace
  )
```

### Rationale

Need unit coverage at each seam: router policy, DB durability, TUI live/resume rendering.

## Rollout

1. Edit V1 schema inline; destructive dev DB rebuild is acceptable.
2. Add persistence API + tests.
3. Implement svar `:routed/trace` event contract first.
4. Replace TUI metadata forwarding with core routing rows + resume from core rows.
5. Add config pass-through from Vis to svar.
6. No CLI command; TUI + persisted routing rows are enough.

## Open decisions

- Default `same-provider-delays-ms`: `[2000 3000 6000]` vs `[2000 4000 8000]`.
- Whether `Retry-After` caps, overrides, or maxes with configured delay.
- Whether fallback-after timer starts at first 429 or request start.
- Whether non-429 transient statuses use same policy or immediate fallback.

## Related specs

Metadata usage audit lives in `METADATA_SPEC.md`. Keep this file focused on LLM routing/retry/fallback behavior.
