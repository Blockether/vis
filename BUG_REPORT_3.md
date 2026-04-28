# BUG_REPORT_3 — Dead `iteration` / `query_state` columns: REMOVED

**Status:** RESOLVED — columns dropped from `V1__schema.sql` instead
of populated.
**Source:** carved out of BUG_REPORT_1 §"Recommended fixes" item 2,
after reading
`packages/vis-persistance-sqlite/src/com/blockether/vis/persistance/sqlite/core.clj:559`
and confirming the original framing was over-confident.
**Related:** BUG_REPORT_1 (schema-reject retry) — already shipped via
`ask-with-schema-retry!` in `iteration/core.clj`.

## What was wrong

The schema reserved three LLM-trace columns on `iteration` and one
on `query_state` that `store-iteration!` (and every other writer)
**never wrote**:

| table         | column         | schema doc                                                                  | written? |
|---------------|----------------|------------------------------------------------------------------------------|----------|
| `iteration`   | `llm_response` | "final selected LLM response for this iteration"                             | ❌ NULL  |
| `iteration`   | `llm_traces`   | "all LLM attempts/traces (fallback chain, failures, timings)"                | ❌ NULL  |
| `iteration`   | `llm_provider` | provider id (e.g. `:openai`, `:zai`)                                         | ❌ NULL  |
| `query_state` | `llm_provider` | same — sibling to `query_state.llm_root_model`                               | ❌ NULL  |

Verifiable on any live `~/.vis/vis.mdb/vis.db` from before this
change:

```bash
sqlite3 ~/.vis/vis.mdb/vis.db "
  SELECT COUNT(*) AS total,
         SUM(llm_response IS NULL) AS null_response,
         SUM(llm_traces   IS NULL) AS null_traces,
         SUM(llm_provider IS NULL) AS null_provider
  FROM iteration;"
```

Output: `null_response = total`, etc. Same story for
`query_state.llm_provider`. Zero readers, zero writers,
load-bearing nowhere.

## Decision: remove rather than populate

The earlier draft of this report (visible in git history) advocated
**populating** these columns to enable post-mortem replay. After
reviewing the trade-offs (disk growth without a prune story, PII
duplication, projection-split work, config-flag surface, test
fixtures, doc updates) the cost outweighed the diagnostic gain — the
schema-reject retry from BUG_REPORT_1 already absorbs the common
"I can't see what the provider returned" pain. Vis logs (`~/.vis/vis.log`)
already carry the raw provider content for ad-hoc post-mortems.

The simpler, cheaper, cleaner answer is to **rip the dead columns
out** so future readers of `V1__schema.sql` aren't misled into
thinking they carry data, and so future PRs don't get tempted to
populate them piecemeal.

## What changed

1. **`packages/vis-persistance/resources/db/sqlite/migration/V1__schema.sql`**
   — dropped 4 column declarations: `query_state.llm_provider`,
   `iteration.llm_provider`, `iteration.llm_response`,
   `iteration.llm_traces`. Edit was made **in-place** in V1, no new
   migration file. The product is local-first and pre-1.0; users
   carrying an existing `~/.vis/vis.mdb/vis.db` upgrade by deleting
   it (Flyway's checksum will detect the in-place edit and refuse to
   start otherwise).
2. **`docs/src/architecture/database.md`** — removed the 4 rows from
   the `query_state` and `iteration` tables.
3. **No code changes.** Confirmed via
   `rg ':llm[-_]provider|:llm[-_]response|:llm[-_]traces' packages/ extensions/`
   — the only hit was a `log-stage!` tag (`:llm-response`) which
   names a logging *stage*, not a column. Left alone.

## Upgrade path for existing users

```bash
rm -rf ~/.vis/vis.mdb
```

Vis recreates the directory and applies the modified V1 cleanly on
next boot. All conversation history stored in that DB is lost; this
was acknowledged and accepted at the time of removal because the
product is pre-1.0 and the surviving `~/.vis/vis.log` retains
enough context for any active post-mortem.

## Why this is now closed, not "open follow-up"

- Zero readers were affected (no Clojure code referenced the
  columns).
- Zero writers were affected (no INSERT statement set them).
- The earlier "follow-up" items in the populate-them draft (DB
  lifecycle / projection split / config flag / replay subcommand)
  no longer have a motivating bug. Re-open them only if a future
  feature genuinely requires per-iteration provider trace storage.

## Lessons captured

1. Schema columns without a writer are dead weight. Add them only
   when the writing call site lands in the same PR.
2. "Add a column for future use" is the same anti-pattern as
   "leave a TODO comment for future me" — both rot in place.
3. When a "recommended fix" turns out to require a small design
   doc to defend, that's the signal to reconsider whether the fix
   is actually wanted, not to escalate. This report's first draft
   sat for two minutes before being flipped to a removal.
