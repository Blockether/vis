# LLM Routing, Retry, Fallback Persistence Spec

## Goal

Make provider routing observable, durable, queryable. 429 retry/fallback must be visible live in TUI, persisted in SQLite core schema, replayed on resume, configurable from Vis config.

## Non-goals

- No opaque routing data only in JSON metadata.
- No TUI-only state as source of truth.
- No retry after streamed content starts.
- No silent provider fallback.

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

## svar trace shape

### Change

Every retry/fallback produces structured event.

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

Final ask result carries summary:

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

### Rationale

Flat event maps are durable, renderable, testable. Namespaced `:event/type` avoids guessing. Summary avoids re-deriving selected/actual from trace.

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

```sql
ALTER TABLE conversation_turn_state
  ADD COLUMN llm_selected_provider TEXT;
ALTER TABLE conversation_turn_state
  ADD COLUMN llm_selected_model TEXT;
ALTER TABLE conversation_turn_state
  ADD COLUMN llm_actual_provider TEXT;
ALTER TABLE conversation_turn_state
  ADD COLUMN llm_actual_model TEXT;
ALTER TABLE conversation_turn_state
  ADD COLUMN llm_fallback INTEGER NOT NULL DEFAULT 0;
```

```sql
CREATE TABLE IF NOT EXISTS llm_routing_event (
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

CREATE INDEX IF NOT EXISTS idx_llm_routing_event_turn
  ON llm_routing_event(conversation_turn_state_id, position);

CREATE INDEX IF NOT EXISTS idx_llm_routing_event_iteration
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

Events with known iteration id store `conversation_turn_iteration_id`; final turn summary also stores turn-state id.

### Rationale

TUI progress is iteration-oriented. Debugging needs answer: which iteration hit 429?

## TUI live rendering

### Change

Render routing events as first-class progress rows.

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

Conversation rebuild reads core routing fields/events.

```clojure
{:llm-selected {:provider llm_selected_provider
                :model llm_selected_model}
 :llm-actual {:provider llm_actual_provider
              :model llm_actual_model}
 :llm-fallback? (pos? llm_fallback)
 :llm-fallback-trace (db-llm-routing-events db turn-state-id)}
```

### Rationale

Reopened transcript must match live transcript. DB core facts must reconstruct same bubble/footer without parsing metadata.

## CLI / diagnostics

### Change

Add turn routing inspect helper.

```bash
vis conversations routing <turn-id>
```

Output:

```text
selected: anthropic-coding-plan/claude-opus-4-7
actual:   openai-codex/gpt-5.3-codex
fallback: yes

1 retry anthropic-coding-plan/claude-opus-4-7 429 delay=2000ms
2 retry anthropic-coding-plan/claude-opus-4-7 429 delay=3000ms
3 retry anthropic-coding-plan/claude-opus-4-7 429 delay=6000ms
4 fallback anthropic-coding-plan/claude-opus-4-7 -> openai-codex/gpt-5.3-codex elapsed=30000ms
```

### Rationale

TUI not enough for postmortem. CLI gives grep-able support path.

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
  ;; assistant message has :llm-selected/:llm-actual/:llm-fallback-trace
  )
```

### Rationale

Need unit coverage at each seam: router policy, DB durability, TUI live/resume rendering.

## Rollout

1. Add schema to V1 migration inline until migrations policy changes.
2. Add persistence API + tests.
3. Add TUI metadata forwarding + resume from core rows.
4. Add svar policy + trace events.
5. Add config pass-through from Vis to svar.
6. Add CLI diagnostics.

## Open decisions

- Default `same-provider-delays-ms`: `[2000 3000 6000]` vs `[2000 4000 8000]`.
- Whether `Retry-After` caps, overrides, or maxes with configured delay.
- Whether fallback-after timer starts at first 429 or request start.
- Whether non-429 transient statuses use same policy or immediate fallback.
