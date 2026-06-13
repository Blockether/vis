# Trailer / context-compaction redesign — handoff

**Author:** Karol (+ Claude investigation, 2026-06-13)
**For:** whoever picks up the trailer/compaction work
**Status:** investigated + designed, not yet implemented
**Related memory:** frozen-trailer (E1), trailer-prompt-diet, RLM architecture

---

## 0. The ask in one paragraph

vis sends *much* bigger prompts than pi/opencode on comparable work, and after a long
turn the model starts reading its own history as a tool-call transcript — it gets
confused and even suggests "maybe use tools." We want a trailer/context structure that
is **(a) prefix-cache friendly (append-only, like today), (b) bounded/compressed (like
pi's sliding window), and (c) 100% text-based — NO native tool API** (this is a hard
product constraint: svar must drive any text model). This doc explains why it happens,
how pi and vis each do it today, the fee math, and exactly where to touch vis.

---

## 1. Symptoms

1. **"Mashup."** After ~15–26 iterations in one turn, the prompt is a long alternation
   of `assistant: <python>` / `user: <results>…</results>` pairs plus a pile of raw
   read outputs. It reads as garbage.
2. **Tool-drift.** The model starts suggesting native tool use. Root cause is
   structural (see §3) — the wire shape *is* a tool transcript, so the model
   pattern-matches to it.

---

## 2. Measured evidence (real bytes)

Decomposed the actual 244KB prompt vis sent at **iteration 26** of session
`372994ce` (turn `40fcf7`), pulled from `session_turn_iteration.llm_user_prompt`
(vis persists the full wire per iteration — no reconstruction needed):

| Section | Bytes | % |
|---|---|---|
| System (3 msgs: 30KB core + 1.3KB AGENTS.md + **15.3KB extensions block**) | 46,622 | 19% |
| `<results>` pins (26 raw, alternating with replays) | ~125,000 | **51%** |
| Assistant replays (thinking/text) | 44,655 | 18% |
| `<context>` mutable tail (once, end — lean, NOT duplicated) | 4,784 | 2% |
| **total** | **244,499** | **≈61K tokens** |

- The `<results>` bulk is a handful of **stale raw reads**: `t1/i12`=31.6KB,
  `t1/i18`=19KB, `t1/i3`=11.3KB, `t1/i4`=9.6KB, `t1/i11`=7.6KB, `t1/i8`=7.2KB.
  **6 reads ≈ 86KB ≈ 35% of the whole prompt**, most already superseded (the i12 file
  was edited by i17; the i3 rg was consumed by i12).
- This was a single 26-iteration turn → vis's turn-boundary fold never fired (it only
  folds turns ≤ pos−2), and the mid-turn budget-guard fold only fires under window
  pressure. So everything stayed raw.

**Conclusion: the problem is RETENTION, not encoding.** The `<results>` text format is
already compact (gutters, `$ cmd → exit N`). We keep too much, too long.

---

## 3. The three architectures

| | **vis** | **pi** (`@mariozechner/pi-coding-agent`) | **opencode** |
|---|---|---|---|
| Execution surface | **text** (RLM: model writes Python, no tool API) | native `toolCall` (1,206 in one vis-repo session) | native `tool_use` + server-side `code_execution_20260120` |
| System prompt | ~46KB | ~5.7KB | large, baked into binary |
| Context strategy | **append-only frozen trailer**; raw results linger | **hard sliding window** + recursive summary | provider-native `compaction` content-type |
| Prompt caching | **prefix-optimal** (append-only never moves old bytes) | prefix cache, **busts on every recompaction** | provider-managed |
| Compression trigger | turn boundary + budget guard | every call (window) | provider |

**Why the model drifts to tools:** vis's wire is
`[system, user_initial, asst_iter1, <results t/i1>, asst_iter2, <results t/i2>, …, <context tail>]`
(`loop.clj` ~1725). `assistant(code) → user(result)` repeated 15× is *structurally
identical to* `assistant(tool_call) → tool(tool_result)`. pi/opencode get the compact,
un-confusing framing **for free** because they actually use the tool API — the thing we
refuse. So we must (a) stop the shape from looking like tools, and (b) compress
retention ourselves.

---

## 4. How fees/tokens are calculated (why caching dominates)

Per iteration vis persists canonical token columns on `session_turn_iteration`
(and aggregates on `session_turn_state`):

```
input_tokens  = input_regular + input_cache_write + input_cache_read   (TOTAL, additive)
output_tokens (incl output_reasoning_tokens)
cost_usd      = router pricing table @ write time
```

Anthropic prompt-cache economics (roughly): **cache_write ≈ 1.25× base input price;
cache_read ≈ 0.1× base.** So:

- **vis (append-only):** the big stable prefix (system + old pins) is **cache_read**
  (cheap) on every call. Only the new tail + the moving cache breakpoint pays
  cache_write/regular. This is exactly why E1 froze pins into permanent `<results>`
  messages — re-rendering the trailer inside the mutable tail re-billed the whole pile
  **uncached** every call (measured: cached pinned at ~8k while prompts grew past 22k;
  ~91% of uncached spend).
- **pi (window):** when it recompacts, the cut point moves → the cached prefix is
  invalidated → it pays **cache_write again** on the new prefix. Bounded size, but
  periodic cache busts.

The cache breakpoint in vis is set on the **last** `<results>` pin via `:svar/cache true`
(`loop.clj` ~1803–1813, `mark-last-pin`); it advances as pins append so each call hits
the previously-cached prefix incrementally. **Any redesign must preserve this** — only
fold OLD pins, never rewrite the recent tail mid-call.

---

## 5. How pi's compaction works ("the key")

Source: `@mariozechner/pi-coding-agent/dist/core/compaction/compaction.js` (+ `branch-summarization.js`).

- Config: `keepRecentTokens: 20000`, `reserveTokens: 16384`.
- `findCutPoint(entries, …, keepRecentTokens)`: walk **backwards** from newest,
  accumulate estimated message sizes, stop once ≥ `keepRecentTokens`, cut there.
  May cut at a user OR assistant message, **never mid tool-result**.
- Everything before the cut → replaced by **one markdown summary** (a `compaction`
  event with a `summary` string; confirmed in a real pi-on-vis session).
- Net shape: `[system/prefix] + [one summary blob] + [~20K recent verbatim]`.
  Bounded, but the summary blob + shifted cut bust the cache.

## 6. How vis's compaction works today

Two mechanisms in `src/com/blockether/vis/internal/ctx_engine.clj`:

1. **`fold-stale-turn-pins` (~1656)** — turn-START policy: FORM pins from turns
   ≤ `turn-pos − 2` collapse into one mechanical stub per stale turn. The turn boundary
   is the cache-free moment (prefix busts at the new user message anyway), so this is
   free. Previous turn stays verbatim.
2. **Budget-guard mid-turn fold** — `pick-oldest-batch-for-summarization` (~1555) +
   `apply-trailer-summarize` (~1440) + `summarize-trailer-with-companion` (~1689).
   Folds oldest pins (keeping ≥2 most recent, W5 safety) into a stub when
   `trailer-total > target`. "Blunt budget guard."
3. Model can also explicitly call `summarize({...})`.

Stubs are built by `make-summary-stub` (~1634) + `dummy-summary-text` (~1618), carry
`recall(...)` pointers (`recall-call` ~1598) and a `compact-src` (~1612, 90-char cap)
listing of what ran — so **folding is lossless-recoverable** (raw blob stays in DB).

**The gap:** within one long turn, #1 doesn't apply (same turn) and #2 only fires under
window pressure. So a 26-iteration turn keeps everything raw → §2.

---

## 7. What we should do better — the design

Port pi's "keep recent, summarize older" **into vis's cache-aware append-only model**:

### 7a. Sliding raw-pin window, fired IN-TURN (primary win)
- Keep the most recent **N pins (or ~K recent tokens, like pi's 20K)** raw — this is the
  cache-stable suffix.
- Fold everything older into existing summary stubs (reuse `make-summary-stub` +
  `dummy-summary-text`) **eagerly, mid-turn**, not only on budget pressure.
- Cache-safe because we fold only the OLD prefix → one deliberate bust, then the suffix
  re-stabilizes (same model as today's `summarize`). Bonus: bias folding toward
  **superseded** reads (file later edited / rg hits consumed) — those are pure dead weight.
- Expected effect on the §2 prompt: ~35% smaller.

### 7b. Kill tool-drift (cheap, do alongside)
- Reframe `<results>` so the model reads it as *its own prior call*, not a tool result.
  The scope tag already carries the address (`scope="t1/i12"`) and the assistant code is
  position-paired right before it — make the ownership explicit in `render-trailer-pin`
  and/or the system-prompt contract copy.

### 7c. Shrink the fixed system (secondary)
- The **15.3KB extensions block** (`extensions-prompt-block`, `prompt.clj` ~711) is the
  only soft target in the 46KB system. pi's whole system prompt is ~6KB.

### Non-goals / constraints (do NOT violate)
- **No native tool API.** Text-only; must work for any text model. This is the whole point.
- **Keep prefix caching.** Append-only; never rewrite the recent tail mid-call.
- **No consolidation** of pins into one rolling message (kills caching — rejected).
- Folding must stay lossless-recoverable via `recall(...)`.

---

## 8. Where to touch (file : line — why)

| Location | Why |
|---|---|
| `internal/ctx_engine.clj:1656` `fold-stale-turn-pins` | Add an **in-turn sliding-window analog**: keep recent N pins, fold older. Primary change. |
| `internal/ctx_engine.clj:1555` `pick-oldest-batch-for-summarization` | Reuse / generalize for the window policy (it already keeps ≥2 recent — extend to keep N / K-tokens). |
| `internal/ctx_engine.clj:1440` `apply-trailer-summarize`, `:1689` `summarize-trailer-with-companion` | The fold appliers — wire the new trigger through here. |
| `internal/ctx_engine.clj:1634` `make-summary-stub`, `:1618` `dummy-summary-text`, `:1598` `recall-call` | Stub format (recoverable). Reuse as-is; maybe add "superseded" hint. |
| `internal/loop.clj:1746` `frozen-trailer-messages` | Where the append-only suffix + cache breakpoint (`:svar/cache`, ~1803) is built. Verify the window fold keeps the breakpoint on the recent tail. |
| `internal/ctx_renderer.clj:531` `render-trailer-pin` | Reframe `<results>` ownership wording (7b). |
| `internal/ctx_renderer.clj:184` `prompt-trailer-form-noise-keys` / `presentation-form` / `project-trailer-pin` | Pin projection / noise stripping — confirm nothing extra rides; cost is NOT here (good). |
| `internal/prompt.clj:711` `extensions-prompt-block` (`turn-system-context`) | Shrink the 15KB extensions block (7c). |
| `internal/prompt.clj` ~153–206 | The `<results>` contract system-prompt copy — reframe ownership (7b). |
| `internal/safe_guards.clj:57` | Fold-round sizing (over-budget delta) — interacts with the new trigger. |

Token/fee columns to validate against: `session_turn_iteration` (`input_tokens`,
`input_regular/cache_write/cache_read_tokens`, `output_tokens`, `cost_usd`) and
`session_turn_state` aggregates.

---

## 9. Validation plan

1. Replay session `372994ce` (turn `40fcf7`) through the new fold in the live REPL;
   measure before/after wire bytes per iteration (target ≥30% cut at iter 26).
2. Confirm the cache breakpoint still lands on the recent tail (no extra busts) — check
   `input_cache_read_tokens` stays high across iterations.
3. Confirm recoverability: a folded read is still reachable via `recall(...)`.
4. (Optional, billable) live vis-vs-pi head-to-head on the z.ai/GLM coding plan, same
   task, compare `input_tokens` trajectories.

## 10. Reference paths for the comparison
- pi compaction: `@mariozechner/pi-coding-agent/dist/core/compaction/{compaction,branch-summarization}.js`
- pi system prompt: `.../dist/core/system-prompt.js` (~5.7KB)
- pi real sessions on vis: `~/.pi/agent/sessions/--Users-fierycod-vis--/*.jsonl`
- opencode: `~/.opencode/bin/opencode` (binary; native `tool_use`/`server_tool_use`/`code_execution_20260120`/`compaction`)
