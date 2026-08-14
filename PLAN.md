# PLAN — Let a session speak to the other sessions in its tree

*The daemon already delivers the message; nobody ever told the agent the others exist.*

## Context

### State before

**The fleet is real, and it is in this checkout.** Measured through the canonical client
(`gateway-client/request! :get "/v1/sessions?root=/Users/fierycod/vis"`) while this plan was
restored: **1139 sessions on this daemon, 1127 in this root, 2 running at the same instant** —
`6447232f` *"Analysis of fold token saving"* and `238c9645` *"Atomic File Patch Implementation
Plan"* (this one). Both are turns against the same working tree, and neither is told the other
exists. Every field Phase 1 needs is already on the row the daemon hands back: `live`, `status`,
`running_request`, `running_started_at`, `current_turn_id`, `workspace`.

**Blindness has already cost a commit in this tree.** While the session-surface rename was
being verified, `foundation/language_surface.clj`, `foundation/surface_contract.clj` and
`internal/test_contract.clj` went dirty underneath the test run — another live session rewriting
the `run_tests` selector. Neither agent was told the other existed; the collision was found by
reading `git status`, after the fact.

**The transport already runs — end to end, with no code change.** A probe through the same client
— no client attached to the target and no window open — recorded when this plan was written:

| Step | Result |
| --- | --- |
| `POST /v1/sessions` | new session `beb1a4ac…` |
| `POST /v1/sessions/:sid/turns` | accepted in **7 ms**, `status "streaming"` |
| the daemon's own worker | ran a full agent turn with **nobody attached** |
| ~6.2 s later | `status idle`, `turn_count 1` |
| `GET /v1/sessions/:sid/transcript` | one prose block, `"PONG"` — 1.8 s engine, 12.4k→5 tokens, ≈ $0.073 |
| `DELETE /v1/sessions/:sid` | `204` |

The machinery it used already exists and is already public:

- `state/submit-turn!` — `src/com/blockether/vis/internal/gateway/state.clj:3476` — starts
  immediately when idle, queues when busy; queued record `:3564-3585`, running record
  `:3600-3626`, `turn.queued` event `:3637-3651`, worker launch `:3656`.
- `state/submit-turn-sync!` — `state.clj:3746` — subscribes BEFORE submitting and buffers the race
  (`:3764-3769`), so no sibling turn's terminal is ever handed to the caller.
- `client/terminal-event->result` — `src/com/blockether/vis/internal/gateway/client.clj:1530` —
  content blocks plus `status` ∈ `done | failed | cancelled | needs_input` (`:1556-1572`: "done"
  rides in on the turn row's meta, only the three exceptions are set here).
- Both are exported already: `vis/gateway-submit-turn!` and `vis/gateway-submit-turn-sync!` —
  `src/com/blockether/vis/core.clj:119` and `:121`.

**What does not exist is the introduction.** The agent-facing surface is single-session and
read-only: `read_session` / `get_session` / `list_sessions`, all `:tag :observation`
(`src/com/blockether/vis/internal/foundation/introspection.clj:1418`, `:1421`, `:1424`), all behind
the `introspection` toggle whose default is `false` (`:1438-1447`). And nothing in the model's world
mentions another session at all: the entire model-facing view is nine keys —
`ctx-engine/model-facing-keys`, `src/com/blockether/vis/internal/ctx_engine.clj:323-327` — and the
per-turn boundary emits exactly `session["turn"]` and `session["utilization"]`
(`src/com/blockether/vis/internal/ctx_renderer.clj:154-172`).

**A submitted turn carries no provenance.** `submit-turn!` destructures its opts explicitly
(`state.clj:3483-3484`); neither the record nor `turn.queued` / `turn.started` says who asked. The
seam for it is already cut: `display_request` (`state.clj:3585`, `:3626`, `:3650`, worker
`:3076-3077`) already separates *what the UI shows* from *what the engine reads*.

**Three constraints the design does not get to choose.**

1. **Concurrency is finite and shared.** `MAX_CONCURRENT_TURNS` (`state.clj:245-257`) backs a
   process-wide semaphore (`:261-262`) that every worker acquires before it runs (`:3108`) and releases
   at `:3132`. A turn that BLOCKS on another session's answer holds its permit the whole time, so a
   chain of blocking asks consumes execution slots for the whole daemon. (The docstring says the
   default is 2 while the code says 50 — `:249` vs `:257`; the guard must not depend on either.)
2. **The dependency direction is already fixed.** `gateway.state` requires `loop` (`:as lp` in its
   `ns`), so `loop.clj` can never require the gateway, and this repo does not permit `declare`.
   Peers must be PUSHED into the engine call the way `:turn-features` / `:workspace` /
   `:engine-opts` already are (`state.clj:3118-3131`).
3. **The fleet page is too slow to ask per turn, and the registry is the wrong index.** ONE
   root-filtered `/v1/sessions` page costs **653 ms** (measured, 200 rows of 1128 in this root).
   But the in-memory `registry` (`state.clj:141`) is PROCESS-LOCAL — a sibling vis process's turn is
   mirrored into it only once somebody subscribes — and answering liveness from it is the bug
   `state.clj:4315-4321` records: "two apps talking to the SAME gateway reported two different
   fleets". The machine-wide index is `bus/live-turns` (`bus.clj:101-169`): one cached scan of
   `~/.vis/gateway/live` markers, `LIVE_CACHE_MS` 200, dead pids reaped — measured **0 ms**, and
   correct when called from a JVM that is not the daemon. The marker carries `session_id`,
   `turn_id`, `pid`, `started_at` and nothing else (`bus.clj:182-187`), so a peer's ROOT still costs
   `resolve-workspace` (`state.clj:770`, two SQLite reads per session — **9 ms for 3 live peers**,
   measured) and its request TEXT is only readable where the turn runs.

### The root problem

The daemon is a fleet manager; the agent's world model is a single session. Every fact needed to
cooperate — who is live, in which root, on which request — exists in the gateway and stops at the
HTTP boundary. So coordination is performed by the human, by hand, by copy-paste — and when the
human is not looking, two agents edit the same file and find out afterwards.

The second half of the same problem: a message that arrives with no provenance is indistinguishable
from the operator's own. An agent that cannot tell *the human asked* from *another agent asked*
cannot weigh the request, cannot refuse it, and cannot answer "who wants this?".

### What we solve

- The agent sees the live peers in its own tree at every turn start, with the address needed to
  reach one.
- One verb sends a request into a named peer, and optionally blocks for its settled answer.
- The receiving side can tell a peer's request from its human's, and the surfaces a human watches
  say so.
- Cycles and pile-ups are bounded *before* the verb that can cause them exists.

### What we explicitly do not solve

- **No broadcast, no room, no group.** One target per call.
- **No shared lock, merge or ownership protocol.** Two agents in one checkout can still collide;
  after this they can NOTICE and negotiate, which is all this plan claims.
- **No new transport.** Nothing here opens a socket or hand-builds an HTTP call; every
  agent-initiated gateway call goes through the canonical Clojure client.
- **No cross-machine fleet.** `root=` is local to this daemon.
- **Not a scheduler.** `ask_session` never re-orders the target's queue; a busy peer queues exactly
  like a human's message and stays cancellable from the strip.
- **Not human-input plumbing.** A peer suspended on its own human returns `needs_input` and that is
  the end of it; answering another session's question is separate work.

### Alternatives considered

- **Reuse `agent("name", "task")`** (`src/com/blockether/vis/internal/foundation/harness/core.clj:206-224`,
  `:tag :mutation`, *"EXPENSIVE full LLM turn"* at `:210`). Lost: that is a child loop running in
  MY context and MY workspace whose edits merge back to me — delegation downward. A peer is a
  sibling with its own workspace, model, transcript and human. Reusing it would silently hand a
  peer's workspace to the caller.
- **Keep the human as the relay** (today). Lost: measured above — the collision happened while the
  human was in the loop, and the human is not present at 03:00.
- **A file in the tree (`.vis/inbox/<sid>.md`)**. Lost: no delivery and no turn. It requires the
  peer to poll, and a peer has no reason to poll.
- **MCP between sessions.** Lost: a second transport for a fleet we already own end to end; auth,
  lifecycle and replay would be reinvented next to `submit-turn!`.
- **The verb without the context line** (let the model call `list_sessions()` when curious). Lost:
  a model does not call a verb it has no reason to believe applies. The trigger IS the feature.
- **Sandbox-side `httpx` POST to the gateway.** Lost: forbidden — agent-initiated gateway calls go
  through the canonical client — and it would bypass provenance entirely.
- **Hang it on the existing `introspection` toggle.** Lost: that toggle means *let the agent read
  its own history*, is off by default for good reason, and is an observation. Peer messaging is a
  different policy and a mutation; sharing one switch makes both undecidable in the settings dialog.

---

## Phase 1 — Put the live peers in the turn context

**Rationale.** Without this line every other phase is dead weight: the model never calls a verb it
has no reason to believe applies, and today the only way to learn a peer exists is a toggle-gated
`list_sessions()` nothing prompts it to run. This phase ships alone as a working product — with the
peers visible, the EXISTING reads (`get_session`, `read_session`) already answer "what is that one
doing", and the collision measured above becomes noticeable before the edit instead of after.

**Data.**

```clojure
;; One live peer as the model reads it. Engine side is kebab keywords; the ctx stamp and every wire
;; echo are the mechanical snake_case of these keys (`wire/->wire`), so the Python the model sees is
;; `session["peers"][0]["running_request"]`.
(s/def :ext.peer/id string?)                            ; full session id — the target `ask_session` takes
(s/def :ext.peer/title (s/nilable string?))
(s/def :ext.peer/status #{"running"})                   ; liveness only — an idle session is not listed
(s/def :ext.peer/running-request (s/nilable string?))   ; ONE line; absent for a foreign process
(s/def :ext.peer/root string?)                          ; its workspace root, so "same tree" is checkable
(s/def :ext.peer/is-draft boolean?)                     ; a draft clone under ~/.vis/drafts, not this checkout
(s/def :ext.peer/peer
  (s/keys :req-un [:ext.peer/id :ext.peer/status :ext.peer/root :ext.peer/is-draft]
          :opt-un [:ext.peer/title :ext.peer/running-request]))
(s/def :ext.peer/peers (s/coll-of :ext.peer/peer :kind vector?))
```

It is specced because it crosses two boundaries: the Python `session` dict, and
`session_turn_state.ctx` (persisted Nippy).

**Acceptance criteria.**
- `src/com/blockether/vis/internal/gateway/state.clj` — `live-peers` answers from `bus/live-turns`
  (`bus.clj:148-169`): the MACHINE-WIDE index, minus self, keeping the sids whose
  `resolve-workspace` (`:770`) root equals this session's. O(live turns), never O(fleet). `title`
  from `lp/by-id`; `running_request` from this process's registry entry when it has one, omitted
  otherwise — a sibling process's request text is not on the marker.
- `src/com/blockether/vis/internal/gateway/state.clj` — the worker passes `:peers` in the same opts
  map that already carries `:turn-features` / `:workspace` (`:3118-3131`), computed once the permit
  is in hand so the list is the fleet at EXECUTION time, not at submit time.
- `src/com/blockether/vis/internal/loop.clj` — beside `_initial-utilization` (`:6748`), stamp
  `"session_peers"` into ctx, and DISSOC it when the fleet is empty, so a resumed session never
  shows a peer that has gone.
- `src/com/blockether/vis/internal/ctx_engine.clj` — `"session_peers"` joins `model-facing-keys`
  (`:323-327`), so `session-view` keeps it instead of dropping it as engine bookkeeping.
- `src/com/blockether/vis/internal/ctx_renderer.clj` — `project-ctx` (`:52-77`) maps
  `"session_peers"` → `"peers"`. That ORDERED WHITELIST, not `model-facing-keys`, is what makes
  `session["peers"]` exist in the bound Python dict (`env/bind-ctx!`), and it must stay OUT of
  `static-context-keys` (`:79-82`): peers change every turn and the cached system prefix must not.
- `src/com/blockether/vis/internal/ctx_renderer.clj` — `render-turn-boundary` (`:154`) emits
  `session["peers"] = […]` beneath the utilization line, and emits nothing when there are none.
- `src/com/blockether/vis/internal/foundation/peer.clj` (new) — registers the `session_peers`
  toggle (`toggles.clj:214`, snake_case id, `:group :sandbox`, `:persist? true`, hydrated from
  merged config at `toggles.clj:495` so `/reload` honours a project override) and owns the pure
  peer-row projection. The stamp is gated on it.
- Test that proves it done: `test/com/blockether/vis/internal/foundation/peer_test.clj` —
  `live-peers` excludes self and other roots; `render-turn-boundary` prints the peers line with a
  peer and omits it with none; the ctx key is absent, not empty, when alone.

**Unknowns.**
- Is a DRAFT peer (its own clone under `~/.vis/drafts/<repo>/<label>`) listed at all — it cannot
  collide with my files, but it is the same work?
- How wide is `running_request` before it is truncated, and is the title alone enough — given that a
  peer running in a SIBLING vis process has no request text here at all?
- Should an IDLE session in this root be listed too? The live index cannot see one; that needs the
  653 ms fleet page or a second index, and "who has this tree open" may matter more than "who is
  mid-turn".
- Does any channel already render unknown `session_*` keys (the companion context viewer) — does an
  added key break a client that mirrors the shape?

---

## Phase 2 — Stamp provenance on the turn record and cap the chain

**Rationale.** A peer request that looks exactly like the operator's is worse than no message: the
receiving agent cannot weigh or refuse it, and the human watching that terminal sees a request they
never typed. The guard must exist BEFORE any verb can loop — every running turn holds one of
`MAX_CONCURRENT_TURNS` permits (`state.clj:3108`), so a chain of blocking asks holds a permit per
hop and a cycle drains the daemon's execution slots. This phase lands as a working product on its
own: every HTTP client (companion, CLI, tests) gains a provenance-carrying submit and the refusals
that bound it.

**Data.**

```clojure
;; Provenance rides the turn record and every event that carries a turn id. Engine kebab → wire
;; snake through `wire/->wire`: :origin-session-id → "origin_session_id".
(s/def :ext.peer.turn/origin-session-id string?)        ; the ASKING session
(s/def :ext.peer.turn/origin-chain                      ; oldest first, target appended before the check
  (s/coll-of :ext.peer.turn/origin-session-id
             :kind vector? :min-count 1 :max-count 3 :distinct true))
(s/def :ext.peer.turn/origin
  (s/keys :req-un [:ext.peer.turn/origin-session-id :ext.peer.turn/origin-chain]))
;; ABSENT on a human turn — that absence is the signal, so it is optional on the record and is never
;; defaulted to a placeholder. The chain is every session the request has passed THROUGH, the target
;; appended before the check, so `:distinct true` fails exactly on a cycle and `:max-count 3` exactly
;; on depth. The spec only DOCUMENTS them: the refusals are explicit `{:error :peer-cycle}` /
;; `{:error :peer-depth}` values, because a spec failure cannot name which rule it broke.
```

**Acceptance criteria.**
- `src/com/blockether/vis/internal/gateway/state.clj` — `submit-turn!` accepts `:origin`
  (`:3483-3484`) and stores it on both the queued (`:3564-3585`) and running (`:3600-3626`) records.
- `src/com/blockether/vis/internal/gateway/state.clj` — `turn.queued` (`:3637`) and `turn.started`
  echo `origin_session_id` / `origin_chain`.
- `src/com/blockether/vis/internal/gateway/state.clj` — the DAEMON, never the caller, prefixes the
  engine `request` with the single provenance line and keeps the clean text in `display_request`
  (`:3076-3077`), so UI rows stay readable and a caller cannot forge the sentence.
- `src/com/blockether/vis/internal/gateway/state.clj` — refusal before any work is done: target
  already in `origin-chain`, or chain already 3 deep → `{:error :peer-cycle | :peer-depth}`; no
  record, no event, no permit taken.
- `src/com/blockether/vis/internal/gateway/server.clj` — `POST /v1/sessions/:sid/turns` accepts
  `origin_session_id` and derives the chain from the CALLER's own turn, never from the body alone.
- Test that proves it done: gateway state test — a cycle and an over-deep chain are refused with no
  event appended; a human turn carries no origin key at all; `wire/->wire` round-trips both fields.

**Unknowns.**
- Is depth 3 right, or is 2 (ask, answer back) all anyone needs?
- Queued work is deliberately memory-only across restarts (`state.clj:3671-3676`): must a blocked
  asker see an explicit failure rather than silence when the daemon restarts under it?
- Should the provenance reach the receiving agent as ctx (like peers) instead of inside the request
  text — is a sentence in the request the right channel for a machine-readable fact?

---

## Phase 3 — `ask_session`: one verb that spends another session's turn

**Rationale.** The verb is the only part the model can call; everything before it is visibility and
safety. It lands after the guard on purpose — the phase that can loop arrives when looping is
already refused.

**Data.**

```clojure
;; What `ask_session` hands back into Python. It crosses the Clojure→GraalPy boundary, so it is ONE
;; closed shape, never "whatever the terminal event happened to carry".
(s/def :ext.peer.ask/turn-id string?)
(s/def :ext.peer.ask/status #{"queued" "running" "done" "failed" "cancelled" "needs_input"})
(s/def :ext.peer.ask/content (s/coll-of map? :kind vector?))  ; the peer's blocks, only once settled
(s/def :ext.peer.ask/usage (s/nilable map?))                  ; model, tokens, cost as the peer reports them
(s/def :ext.peer.ask/result
  (s/keys :req-un [:ext.peer.ask/turn-id :ext.peer.ask/status]
          :opt-un [:ext.peer.ask/content :ext.peer.ask/usage]))
;; Refusals RAISE, following `patch`: unknown or ambiguous target, cycle, depth, toggle off, `wait`
;; elapsed — each with the recovery in the message. A refusal is never a status value.
```

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/peer.clj` — `ask_session(target, request, wait=180)`,
  `:tag :mutation`, `:inject-env? true`; `target` resolves by the same ONE-argument rule as
  `get_session` (exact id or unambiguous prefix), keyword and positional calls bind identically.
- `src/com/blockether/vis/internal/foundation/peer.clj` — `wait=0` → `vis/gateway-submit-turn!`
  returning `{turn_id, status}`; `wait>0` → `vis/gateway-submit-turn-sync!` (`core.clj:119`, `:121`).
  The docstring states EXPENSIVE in the same voice as `agent` (`harness/core.clj:210`), with the
  measured floor: a full peer turn, ~12.4k prompt tokens and ≈ $0.07 for a one-word answer.
- `src/com/blockether/vis/internal/gateway/state.clj` + `.../gateway/client.clj` — the ceiling does
  not exist yet and is part of this phase: BOTH sync paths block unbounded today —
  `state.clj:3841` derefs the terminal promise with no timeout, and `client.clj:2083-2111`
  reconnects forever with no deadline. `wait` becomes a deadline on that deref (releasing through
  the `unsubscribe!` the `finally` already runs) and on the SSE read; a lapsed `wait` raises WITH
  the peer's `turn_id`, so the caller can read the settled answer later instead of losing it.
- `src/com/blockether/vis/internal/foundation/peer.clj` — the extension's `:ext/activation-fn` is
  `vis/toggle-enabled?` on `session_peers`, so the ctx line and the verb hang off ONE switch, and
  `:ext/prompt-fn` contributes its guidance only when the toggle is on.
- `src/com/blockether/vis/internal/doc_corpus.clj` — `doc("ask_session")` already answers from the
  LIVE extension registry (`:35`), so the only decision here is whether the verb earns a slot in
  `curated` (`:244-253`), the hand-ordered list `doc()` prints with no argument.
- Test that proves it done: `test/com/blockether/vis/internal/foundation/peer_test.clj` — target
  resolution (exact, prefix, ambiguous → raises), `wait=0` returns a queued row, the probe above
  becomes a real round trip through the daemon, and the symbol is absent when the toggle is off.

**Unknowns.**
- Is `wait=180` the honest default? It parks a whole turn of mine on another agent's model. The
  alternative default is `wait=0` plus a read for the answer later. **This is the one open call.**
- Does an ask into a session whose human is actively typing deserve a refusal instead of a queue?
- Does `ask_session` need an `idempotency_key` passthrough for a retried block, or is the existing
  dedupe on the HTTP path enough?

---

## Phase 4 — Show the peer where the human is watching

**Rationale.** The moment an agent can send, a human can see a request in their own session that
they never typed. Unlabelled, that is indistinguishable from the agent going rogue; labelled, the
strip reads "from <peer title>" and the whole feature becomes auditable after the fact. It lands
last because Phases 1-3 are already a working product without it — this is the phase that makes it
trustworthy.

**Data.** None. It renders `origin_session_id` / `origin_chain` from Phase 2 and adds no field; the
change is rendering only.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/loop.clj` — the TUI queue-strip row for a peer-origin turn
  carries the asking session's short id and title; paint contract per `doc("tui-rendering")`.
- `apps/vis-companion` — the tray / turn row shows the same label from the same wire keys; pull
  `doc("companion-ui")` first, verify with `npm run lint` and `npm run build`, never edit generated
  `ios/` or `android/`.
- The transcript needs no second mechanism: the provenance line is part of the request the engine
  read, and the test asserts exactly that rather than adding another path.
- Test that proves it done: a TUI paint test for the labelled strip row, and the companion build
  green with the row rendered against live DOM numbers.

**Unknowns.**
- Is the companion tray the right surface, or does an incoming peer request deserve its own
  notification?
- Does the label belong on the transcript row as well once the session is read back days later?

---

## State of the plan

**ACCEPTED** — nothing has landed yet.

Two decisions the plan takes, so they are not re-litigated in review:

1. **One toggle, `session_peers`, default ON.** It costs nothing when a tree has a single session:
   the context line is silent and the verb is never called. OFF makes the feature undiscoverable,
   which is how a capability with no advertisement dies. Overturning this is one keyword.
2. **Refusals raise; statuses describe the peer.** `queued | running | done | failed | cancelled |
   needs_input` are what the PEER did; a cycle, a bad target or a lapsed `wait` are the CALLER's
   error and raise with the recovery in the message.

TODO, in order:

1. **Phase 1** — `live-peers` from the in-memory registry, pushed through the worker opts, stamped
   beside `_initial-utilization`, projected by `model-facing-keys`, rendered by
   `render-turn-boundary`; `session_peers` toggle registered in the new `foundation/peer.clj`.
2. **Phase 2** — `:origin` on `submit-turn!`, both records, both events, daemon-side request
   framing, cycle and depth refusals, `origin_session_id` on the HTTP route.
3. **Phase 3** — `ask_session` in `foundation/peer.clj` behind the same toggle, `wait=0` async and
   `wait>0` blocking, doc corpus entry, peer tests.
4. **Phase 4** — the TUI strip label and the companion row label.

**Lineage.** This plan superseded *"Bring back `cat` and `patch` as positional verbs over one
anchored line, and make `grep` speak the same text"*, which is **DONE** (preserved at
`git show 906b091be:PLAN.md`). It was then itself superseded, unstarted, by *"Make `patch` take one
file's whole batch of anchored edits and write once"* — also **DONE**: the batch verb landed in
`22b36784f` and the cross-validation it earned in `40c9ec904` (preserved at
`git show 22b36784f:PLAN.md`). This text was preserved verbatim at `git show fbca3dd3a:PLAN.md` and
is restored here with every `file:line` claim re-verified against HEAD and the fleet re-measured.
Nothing from either superseding plan is carried into this one.
