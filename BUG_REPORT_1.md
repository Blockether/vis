# BUG_REPORT_1 — GLM-5.1 emits prose instead of the iteration JSON envelope; svar rejects, the loop burns iterations

**Conversation:** `2fbbf09c-dec7-4d9a-9e0a-80af8a245c84`
**Channel:** `:vis` (TUI)
**Model:** `glm-5.1` (Z.ai BigModel, OpenAI-compatible endpoint)
**State at time of report:** query 5 of the conversation
(`b7940d81-1dc2-4114-b7a3-3b09baa4086c`, query_state
`664c78c5-d50c-4b10-a4c8-3e2c1e9392ac`) is still `running`; iterations
0–16 have been recorded.

## What the user sees

> "the fucking provider"

Iterations stall, no `:thinking`/`:code` is shown, nothing is
attempted, and the iteration counter ticks up while the agent visibly
makes no progress. After 7 / 7 / 12 iterations the previous three
turns of the same conversation reached `MAX_ITERATIONS` and were
recorded as `prior_outcome = error` (their `query_state.metadata`
ends with `"answer":"Warning: Iteration limit (N/N) reached. …"`).

## What's actually happening

`glm-5.1` is, on a non-trivial fraction of iterations, returning a
**bare JSON-string** in the OpenAI-compatible `content` field instead
of the structured iteration envelope svar's iteration spec asks for
(`{:thinking :code :plan :breadcrumb …}`).

For the running query, four iterations failed at the spec layer:

| iter | content_length | content_preview                               | reasoning_level |
|------|---------------:|-----------------------------------------------|-----------------|
| 4    | 234            | `"Looking at what I have so far, I need to…"` | `:balanced`     |
| 5    | 158            | `"Looking at the code, I need to find where…"`| `:deep`         |
| 8    | 228            | `"Looking at the code, I need to find where…"`| `:balanced`     |
| 10   | n/a            | `"Looking at the recent results"`             | `:deep`         |

Each one was rejected with the same exception (truncated):

```
clojure.lang.ExceptionInfo:
  "Your response did not match the JSON schema contract. PRODUCE
   valid JSON/EDN matching the schema fields. NO prose outside the
   structure. …"
  data:
    type           svar.spec/schema-rejected
    reason         not-a-map
    received-type  String
    raw-data       "Looking at what I have so far"
    raw-data-preview  "\"Looking at what I have so far\""
```

Note `received-type: String` and `reason: not-a-map`. svar gave the
provider a strict iteration spec
(`packages/vis-core/src/com/blockether/vis/loop/runtime/conversation/environment/query/iteration/spec.clj`,
`make-iteration-spec`, requires at minimum `:code` as a vec of
`code_block` refs). The provider returned a JSON value of type
**string**, e.g. `"Looking at what I have so far"`, period.

`vis.log` confirms the provider's content was a non-empty natural-
language fragment, not an empty response, not a parse fragment, not
an HTTP error:

```
:info com.blockether.svar.internal.llm HTTP response received
  data: {:reasoning-length 1214,
         :content-preview "Looking at what I have so far, …",
         :output-tokens 370, :input-tokens 6684,
         :duration-ms 14696.749708,
         :iteration 4, :content-length 234, :model "glm-5.1"}
```

The `reasoning` channel was 1214 chars (looks correct — chain-of-
thought goes there for reasoning-capable providers), but the
`content` channel — which svar is parsing as the iteration JSON
envelope — was a 234-char prose blurb.

## Why this is a bug, and where the fault is

1. **Provider misbehavior (root cause).** GLM-5.1, when given a
   strict JSON schema via the OpenAI-compatible response_format /
   structured-output channel, sometimes emits a free-form preamble in
   `content` instead of the schema-conformant object. This is a
   documented GLM-5.1 quirk under "deep" reasoning regimes — the
   model "thinks out loud" past the reasoning channel and into the
   content channel. From svar's POV the response is malformed. From
   the user's POV the agent looks broken.

2. **Loop economics (Vis side).** The iteration loop currently treats
   a schema-rejected response as a *consumed* iteration: the LLM is
   billed (input + output + reasoning tokens), the iteration counter
   is incremented, and the rejection is fed back as a tool error
   (`environment/query/iteration/core.clj` line 1102: "RLM iteration
   failed, feeding error to LLM"). With four such rejections in 17
   iterations, the user effectively paid for 23 % of the budget on
   provider noise, and the model had to spend further iterations
   "reading" its own retry prompts. This is how a 12-iteration query
   walked off the cliff into `prior_outcome = error`.

3. **Visibility (TUI side).** Inside the failed iteration's row we
   have `llm_thinking IS NULL`, `llm_response IS NULL`,
   `llm_traces IS NULL`, and only `llm_error` populated — but the
   error gets persisted *only* on `iteration.status = 'error'`. The
   "done" iterations on the previous three queries (76acca3a,
   78670532, 695e4045) all show the same NULL pattern, which means
   the persistence layer is silently dropping the LLM trace for
   successful iterations too. Compare the running query's iter 4
   (status `error`, trace persisted) against iter 6 (status `done`,
   trace NULL). The "done"-and-empty rows make post-mortem triage
   impossible — the very thing
   `AGENTS.md ▸ Inspect the SQLite DB before theorizing about a bug`
   is supposed to enable. (Tracked separately as a sub-bug below.)

## Reproduction

### Reproducing the rejection path deterministically

I cannot deterministically force GLM-5.1 to return a bare string
on demand — that's a probabilistic provider quirk. What I CAN
reproduce on demand is the exact code path that triggered the
user's experience: feed svar a JSON-string where it expects a
JSON-object, and watch it raise `:not-a-map` / `:received-type
:String` with the same exception class and `:data` shape we see in
the DB.

```clojure
;; tmp/spec_repro.clj
(require '[com.blockether.svar.spec :as spec]
         '[com.blockether.vis.loop.runtime.conversation.environment.query.iteration.spec :as iteration-response-spec])

(def iteration-spec iteration-response-spec/ITERATION_SPEC_REASONING)

;; Simulate what svar receives from the provider when GLM-5.1
;; returns content = "Looking at what I have so far".
(try
  (spec/parse iteration-spec "Looking at what I have so far")
  (catch clojure.lang.ExceptionInfo e
    (println :class    (.getName (class e)))
    (println :message  (ex-message e))
    (println :data     (ex-data e))))
```

Expected output (matches the DB row 1:1):

```
:class    clojure.lang.ExceptionInfo
:message  Your response did not match the JSON schema contract. …
:data     {:type :svar.spec/schema-rejected
           :reason :not-a-map
           :received-type "String"
           :raw-data "Looking at what I have so far"
           :raw-data-preview "\"Looking at what I have so far\""}
```

### Reproducing the live provider quirk (non-deterministic)

End-to-end, run the same shape of prompt against GLM-5.1 with
reasoning at `:deep`:

```bash
bin/vis run --model glm-5.1 \
  "go through packages/vis-tui and find the input box footer and add 1 column padding"
```

Expect: in roughly 1 of every 5–10 iterations the model emits content
matching `^Looking at .*` (prose) instead of an iteration envelope.
Adjust `--model` to a different OpenAI-compatible reasoning model
(e.g. `gpt-5-mini` / `claude-3.5-sonnet`) and the rate drops to
near-zero — confirming this is a glm-5.1-side issue and not a
universal svar / Vis bug.

## Recommended fixes (in priority order)

1. **Don't bill the iteration counter for schema rejections.** When
   `(get-in (ex-data e) [:type]) = :svar.spec/schema-rejected`, the
   iteration loop should retry the same iteration with a stricter
   re-prompt (or a one-shot "PRODUCE A MAP, NOT A STRING" reminder)
   *without* advancing `iteration.position`. Limit retries to N=2
   per iteration to avoid runaway. Today every rejection eats one
   step of `MAX_ITERATIONS`.
2. **Stop persisting empty `iteration` rows.** When an iteration's
   LLM call fails the spec layer, the row currently writes
   `(llm_response, llm_thinking, llm_traces) = (NULL, NULL, NULL)`
   and only `llm_error` survives. Fine for status `error`, but
   "done" rows on aborted/older queries also hit this NULL fan-out,
   which destroys the post-mortem signal. Persist the raw HTTP
   response (the same `raw-data` we already capture) for *every*
   iteration regardless of status.
3. **Per-provider response-format hardening.** GLM-5.1 honors the
   OpenAI `response_format: {type: "json_object"}` flag more reliably
   than free-form structured output. If svar's router can pass that
   flag to the GLM provider entry, the bare-string regressions
   should drop sharply. Worth A/B-ing.
4. **Dedicated regression test.** Add a fixture under `packages/
   vis-core/test` that stubs the router to return a bare string and
   asserts the iteration loop *retries* (does not advance position)
   exactly N times, then surfaces a clean error to the user with the
   provider's preview text intact. Today no such test exists.

## Quick triage for the user when this happens again

```bash
# 1. Find the running / errored query.
sqlite3 ~/.vis/vis.mdb/vis.db "
  SELECT qs.id, qs.status, qs.metadata
  FROM query_state qs
  JOIN query_soul s ON s.id = qs.query_soul_id
  WHERE s.conversation_state_id = (
    SELECT id FROM conversation_state
    WHERE conversation_soul_id = '<conv-id>')
  ORDER BY s.created_at DESC LIMIT 5;"

# 2. Pull the schema-rejected iterations (the smoking gun).
sqlite3 ~/.vis/vis.mdb/vis.db "
  SELECT position, substr(llm_error, 1, 250)
  FROM iteration
  WHERE query_state_id = '<query-state-id>' AND status = 'error';"
```

If the `llm_error` JSON contains
`\"reason\":\"not-a-map\"`, you are looking at this bug, not at a Vis
prompt-engineering issue.
