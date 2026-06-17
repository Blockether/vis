# Derived wire — one consistent provider view over the DB

**Goal:** the messages we send to the provider are a **pure, deterministic
function of persisted state**, so the wire is byte-identical whether we are in
the same long-lived process or a brand-new one. No in-memory structure is ever
a *richer* source than what the DB can reconstruct. (pi/Codex/Claude Code get
this for free from an append-only JSONL event log; vis keeps its relational
store and enforces the same invariant at the renderer.)

vis's `session_turn_iteration` rows ALREADY are an append-only, ordered,
immutable event log (one row per iteration, carrying its `forms` blob with
`:scope`/`:src`/`:result`/`:result-pickle`). So we do not need a new storage
format **and we do not materialize a JSONL file** — the DB IS the log. We need
ONE renderer that folds that log (plus `:session/summaries` from the ctx blob)
into provider messages, ON DEMAND. Any process re-derives the identical wire from
the DB; there is nothing extra to keep in sync.

## The pure function

```
derive-wire(db, session-id, current-turn-id, current-user-content)
  -> [ {role, content}, … ]     ; the exact provider message list
```

Reads ONLY persisted state. Same inputs ⇒ same output ⇒ process-invariant.

Message order (unchanged from today):
1. system: CORE prompt, project instructions, extensions, `ctx = {…}` embed
2. user: `CONVERSATION-SO-FAR` — every prior answered turn (Q/A + r[] scope index)
3. user: `CURRENT-USER-MESSAGE`
4. trailer: the CURRENT turn's iterations, as `[assistant-replay, # tN/iN + r[…]=…]`
   pairs — **derived from the current turn's persisted iterations**, not from an
   in-memory-only trailer.

## No materialized file — the DB is the log

We deliberately do NOT write a `.jsonl` (or any other) materialization. It would
be pure duplication of state the DB already holds, and a second thing to keep in
sync / let go stale. The invariant we want — "same wire regardless of process" —
comes from `derive-wire` being a PURE FUNCTION of the DB, not from a file. If we
ever want to *inspect* the events, we call the derivation on demand; the rows are
the source. (This is the one place we diverge from pi/Codex, who persist the
JSONL because it IS their store; ours is already in SQLite.)

## The compaction policy (deterministic ⇒ consistent)

`derive-wire` applies a fixed policy as it folds events, so the same turn renders
identically whether it is "current" or "prior":

| state of a form's result | rendered as |
|---|---|
| current turn, live value | `r["tN/iN/fN"] = <full value>` |
| prior turn | scope-index line `r["tN/iN/fN"] = <src>` (value fetchable via rebind) |
| `summarize`d (any turn) | `# -- tN/iN -- summarized: <gist>` |
| `drop`ped (any turn) | omitted (`# -- tN/iN -- dropped` where relevant) |

Because the policy is pure and its inputs (iterations + summaries) are persisted,
the output cannot depend on process lifetime.

## How this closes the two edges

- **Edge 1 — mid-turn kill.** The current turn's *completed* iterations are
  already db-stored per iteration. `derive-wire` builds the trailer from the
  current turn's persisted iterations, so a resume reconstructs them identically
  to the live render. (Today `seeded-trailer-iters` excludes the current turn and
  the live trailer is in-memory-only — that divergence goes away when both render
  through `derive-wire`.)
- **Edge 2 — summaries vs scope index.** `:session/summaries` is an input to the
  one renderer, applied to current AND prior turns, so a dropped/summarized scope
  is reflected everywhere — including a prior turn's scope index.

## Build order (staged, each verifiable)

1. **Summary-aware scope index** — thread `:session/summaries` into
   `previous-turn-context` so the prior-turn index reflects summarize/drop. Small,
   closes Edge 2. *(first)*
2. **`derive-wire` pure function** — reconstruct the trailer from the current
   turn's persisted iterations; build messages = system + conversation-so-far +
   current-user + DB-derived trailer. Verify it equals the live wire byte-for-byte
   on a real turn before switching the live path to it. Closes Edge 1, makes the
   invariant structural. (No file is written — the function IS the view.)

## Non-goals
- No materialized JSONL / no JSONL-as-source-of-truth. SQLite rows ARE the event
  log; the wire is derived on demand, never duplicated to a file.
- No loss of recall / FTS / introspection.
- No new in-memory structure that can be richer than the DB derivation.
