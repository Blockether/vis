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
format — we need ONE renderer that folds that log (plus `:session/summaries`
from the ctx blob) into provider messages.

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

## The derived-event JSONL (materialized view)

The JSONL is a projection of the derivation — "events for the purpose of the
provider send". One line per conversational event, folded into messages by the
renderer. Keeping it event-shaped (not just dumped messages) makes compaction a
first-class event, exactly like pi's `{type:"compaction"}`.

```jsonl
{"type":"turn","n":1,"user_request":"Read deps.edn.","answer":"Read `deps.edn`."}
{"type":"result","scope":"t1/i1/f1","src":"cat(\"deps.edn\")"}           // prior-turn → scope index
{"type":"summary","scopes":["t2/i1/f1"],"gist":"…"}                        // compaction event
{"type":"drop","scopes":["t2/i3/f1"]}
{"type":"user","content":"…current message…"}
{"type":"assistant","turn":3,"iter":1,"thinking":"…","code":"cat(\"x\")"}  // current-turn replay
{"type":"result","scope":"t3/i1/f1","src":"cat(\"x\")","value":"{…}"}      // current-turn → full value
```

- It is a read-only projection: SQLite stays the source of truth (recall / FTS /
  introspection unchanged). The JSONL never diverges because it is *derived*.
- Written per session (`~/.vis/sessions/<id>.jsonl`) on each send for debug /
  inspection / optional resume cache.

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
   invariant structural.
3. **Derived JSONL projection** — emit the event stream from `derive-wire` to
   `~/.vis/sessions/<id>.jsonl` per send. The literal browseable log.

## Non-goals
- No JSONL-as-source-of-truth rewrite (keep SQLite; the rows are the event log).
- No loss of recall / FTS / introspection.
- No new in-memory structure that can be richer than the DB derivation.
