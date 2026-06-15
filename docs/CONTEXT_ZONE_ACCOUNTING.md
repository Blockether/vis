# Context Zone Accounting Design

## Goal

Persist enough provider-request structure to answer, after the fact:

- What exact bytes were sent to the provider for each iteration?
- Which semantic zone did each byte belong to?
- What was cacheable, newly-written-to-cache, and cache-read?
- How much did the provider charge for the request and, where possible,
  how should that spend be attributed to zones?

The report generator should not reconstruct prompts from current code. It
must read the persisted provider payload and metadata that existed at spend
time.

## Current State

`session_turn_iteration` currently persists:

- `llm_user_prompt`: JSON vector of provider messages sent on the wire.
- Token columns: `input_tokens`, `input_regular_tokens`,
  `input_cache_write_tokens`, `input_cache_read_tokens`, `output_tokens`,
  `output_reasoning_tokens`.
- `cost_usd`: total provider-call cost estimate at write time.
- Provider/model routing columns.

This is enough to show verbatim provider messages and real per-iteration
spend. It is not enough to show authoritative per-zone token/cost accounting.
Message boundaries exist, but zones such as `stable-system`, `project`,
`frozen-ledger`, `current-turn-ledger`, and `mutable-context` are only inferable
from message content.

## Zone Model

Every outbound provider message should carry a zone descriptor before it is
persisted. A zone is not a renderer concern only; it is part of the provider
request audit record.

Suggested zone enum:

- `stable-system`: core Vis system prompt.
- `project-instructions`: AGENTS.md / CLAUDE.md project prompt.
- `capability-system-context`: active extension prompt fragments and runtime
  capability metadata. The provider marker may still say
  `TURN-SYSTEM-CONTEXT`; accounting treats it as a capability epoch that should
  stay byte-stable until tools, channel policy, workspace backend, or similar
  executable affordances change.
- `previous-turn-context`: previous user request and answer bridge.
- `current-user-request`: current user prompt.
- `frozen-ledger`: historical append-only `<results scope="...">` ledger before
  the current user request.
- `current-turn-ledger`: current-turn assistant/reasoning replay and result
  ledger after the current user request.
- `dag-ledger`: frozen DAG `advance` receipt/result entries containing
  `graph_diff`, `resolved_evidence`, or transaction metadata.
- `compaction-ledger`: explicit folded/summarized ledger replacement entries.
- `mutable-context`: final `<context>` tail.
- `provider-extra`: provider-specific request additions, if any.
- `unknown`: explicit fallback; should be treated as a bug in new writes.

Each zone instance should also carry:

- `zone_id`: stable string, unique within one provider request.
- `cache_class`: one of `stable-prefix`, `append-only-prefix`, `turn-prefix`,
  `mutable-tail`, `non-cacheable`, `unknown`.
- `source`: short code path label, e.g. `prompt/core`, `ctx/trailer`,
  `ctx/render-mutable`.
- `scope`: optional Vis scope, e.g. `t2/i4/f1` for frozen ledger entries.
- `turn`, `iteration`, `message_index`.

The desired provider wire shape is:

```text
[stable-system,
 project-instructions,
 capability-system-context,
 frozen-ledger-before-turn,
 current-user-request,
 current-turn-ledger...,
 mutable-context]
```

The invariant is byte-prefix stability: for iteration `N+1`, the sent provider
messages for iteration `N` should be a byte prefix of `N+1`, except when a
system/project/capability epoch changes or when explicit compaction rewrites
ledger pins. Historical frozen ledger must sit before the new user request;
otherwise each new request busts cache reuse for old DAG/result context.

## Persistence

Add a new table instead of overloading `llm_user_prompt`.

```sql
CREATE TABLE provider_request_zone (
  id                         TEXT PRIMARY KEY NOT NULL,
  session_turn_iteration_id  TEXT NOT NULL
                             REFERENCES session_turn_iteration(id)
                             ON DELETE CASCADE,
  message_index              INTEGER NOT NULL CHECK (message_index >= 0),
  zone_index                 INTEGER NOT NULL CHECK (zone_index >= 0),
  zone                       TEXT NOT NULL,
  zone_id                    TEXT NOT NULL,
  cache_class                TEXT NOT NULL,
  role                       TEXT NOT NULL,
  scope                      TEXT,
  source                     TEXT,
  content_sha256             TEXT NOT NULL,
  char_count                 INTEGER NOT NULL CHECK (char_count >= 0),
  byte_count                 INTEGER NOT NULL CHECK (byte_count >= 0),
  estimated_tokens           INTEGER CHECK (estimated_tokens IS NULL OR estimated_tokens >= 0),
  provider_input_tokens      INTEGER CHECK (provider_input_tokens IS NULL OR provider_input_tokens >= 0),
  provider_cache_write_tokens INTEGER CHECK (provider_cache_write_tokens IS NULL OR provider_cache_write_tokens >= 0),
  provider_cache_read_tokens INTEGER CHECK (provider_cache_read_tokens IS NULL OR provider_cache_read_tokens >= 0),
  cost_usd                   REAL CHECK (cost_usd IS NULL OR cost_usd >= 0),
  content                    TEXT NOT NULL,
  created_at                 INTEGER NOT NULL,
  UNIQUE (session_turn_iteration_id, message_index, zone_index)
);
```

Keep `llm_user_prompt` as the canonical whole-request JSON. The zone table is
an index/audit projection over the same request, not a replacement.

## Runtime Assembly Contract

Introduce an internal request part shape before lowering to provider messages:

```clojure
{:role "user"
 :content "<context>..."
 :zone :mutable-context
 :zone-id "t2/i5/context"
 :cache-class :mutable-tail
 :source :ctx/render-mutable
 :scope "t2/i5"}
```

The loop should assemble `provider-request-parts`, then lower to the plain
provider message vector immediately before `router/generate`.

Persist both:

- `llm_user_prompt`: lowered plain provider messages, exactly as today.
- `provider_request_zone`: one row per request part after lowering.

This avoids parsing XML-ish tags later to recover intent.

## Token Accounting

Provider APIs generally return aggregate input/cache/output usage, not
per-message or per-zone tokens. Therefore zone-level accounting has two tiers:

1. Exact structural counts:
   `char_count`, `byte_count`, `content_sha256`.

2. Token estimates:
   `estimated_tokens` from Vis tokenizer for every runtime-classified zone when
   the tokenizer can run. This is a Vis estimate, not a provider invoice field.
   Historical inferred report rows compute the same estimate in memory but do
   not persist it back into `provider_request_zone`.

Exact provider token allocation by zone should only be recorded when a provider
actually returns per-message or per-part metadata that can be mapped to a zone
without approximation. Otherwise `provider_input_tokens`,
`provider_cache_write_tokens`, `provider_cache_read_tokens`, and zone-level
`cost_usd` must stay `NULL`.

Column population contract:

- `estimated_tokens`: populated on new runtime rows from Vis tokenization;
  nullable for defensive writes, failed tokenization, and future non-text parts.
- `provider_input_tokens`: `NULL` unless the provider reports exact input tokens
  for this zone.
- `provider_cache_write_tokens`: `NULL` unless the provider reports exact cache
  creation/write tokens for this zone.
- `provider_cache_read_tokens`: `NULL` unless the provider reports exact cache
  read tokens for this zone.
- `cost_usd`: `NULL` unless provider pricing can be attributed exactly to this
  zone. Never store proportional estimates in this column.

The HTML report can then clearly distinguish:

- Real spend: iteration-level provider usage/cost.
- Estimated zone weight: tokenizer estimate, chars, bytes.
- Real cache totals: iteration-level cache read/write tokens.
- Cache eligibility: zone `cache_class`.

Do not apportion cost across zones unless the report labels it as estimated.

## Cache Classes

Cache class should be assigned at assembly time:

- `stable-prefix`: core system, project instructions, and capability system
  context when byte-stable.
- `append-only-prefix`: frozen/current/DAG/compaction ledger messages that only
  append unless explicit compaction happens.
- `turn-prefix`: previous-turn bridge and current user request; stable within
  one turn but expected to change across turns.
- `mutable-tail`: rendered `<context>` tail.
- `non-cacheable`: anything known to be unstable or provider-specific.
- `unknown`: migration/default only.

The report should show cache classes separately from provider cache usage.
Provider cache usage is aggregate; cache class is Vis intent.

## HTML Report Shape

`dev/provider-report SESSION_ID [OUT.html]` should render:

- Session summary: id, title, provider/model, turns, iterations, totals.
- Turn sections: request, answer status, turn token/cost totals.
- Iteration sections:
  - provider/model actually used
  - status and duration
  - real input/output/cache/cost columns
  - zone summary table
  - exact provider message JSON
  - zone content blocks in wire order

Each zone block should include:

- role
- message index
- zone
- cache class
- scope/source
- chars/bytes/estimated tokens
- content SHA-256
- verbatim content in `<pre>`

For current historical rows without zone metadata, the report should use a
separate `inferred` mode:

- Infer zones from message role and known markers.
- Show chars/bytes.
- Label all per-zone token/cost as unavailable.
- Keep real spend only at iteration level.

## Migration / Backfill

No backfill can produce authoritative zones for old rows. Backfill can only
create inferred rows. Prefer not to persist inferred rows into
`provider_request_zone`; infer them in the report to avoid mixing historical
guesses with runtime-truth data.

## Implementation Steps

1. Add request-part helpers in the loop/prompt boundary.
2. Tag all current message producers:
   stable prompt assembly, previous-turn bridge, frozen trailer messages,
   preserved assistant replay, mutable context tail.
3. Persist request parts into `provider_request_zone` from
   `db-store-iteration!`.
4. Add tests:
   - every outbound provider message has a non-unknown zone in new writes
   - `llm_user_prompt` still equals the lowered request exactly
   - ledger zones are `append-only-prefix`
   - mutable context is `mutable-tail`
   - completed task evidence appears in ledger entries and not in mutable
     context
5. Add `dev/provider-report`:
   - exact zone mode when `provider_request_zone` rows exist
   - inferred historical mode otherwise
6. Use the report on an existing DAG session to verify:
   - provider cache-read grows across iterations
   - frozen DAG receipts are `dag-ledger` prefix zones
   - mutable context keeps compressed evidence references only

## Non-Goals

- Do not claim exact per-zone spend when the provider only returns aggregate
  usage.
- Do not replace `llm_user_prompt`; it remains the canonical whole-wire
  request snapshot.
- Do not rely on current prompt code to recreate historical requests.
