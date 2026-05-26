# PLAN — Canonical token model + prompt cache + title-hint enforcement

Hard-cut philosophy: **JEBAC LEGACY, JEBAC PARITY, JEBAC OPTS**.

- **JEBAC LEGACY** — no migration, no forward compat, no deprecation
  cycle, no shim, no alias, no rename-and-preserve. SQLite schema
  rewritten inline in `V1__schema.sql`. Existing `~/.vis/vis.mdb/`
  gets **deleted** on next boot when Flyway detects the checksum
  mismatch — not renamed, not stashed, not preserved — just `rm -rf`.
  svar gets a breaking minor; canonical usage is the ONLY usage shape,
  legacy `:prompt_tokens` / `:prompt_tokens_details` deleted from every
  return. Every consumer (vis loop, vis chat projection, persistance
  writes/reads, cost estimator) is migrated in the SAME patch.
- **JEBAC PARITY** — no per-entry-point parity matrix, no
  parameterized parity regression tests, no "identical opts across
  six entry points" audit. The fix lives in ONE middleware fn that
  `ask!*` and `ask-code!*` both call. The wrappers (`abstract!*` /
  `eval!*` / `refine!*` / `sample!*`) inherit by the existing
  `llm-passthrough` merge — no test guards that, it just works
  because there is no other code path. If a wrapper later diverges,
  that's a different problem; not paying parity test maintenance
  upfront.
- **JEBAC OPTS** — no `:cache true|false` flag, no
  `-Dsvar.cache.default=...` JVM kill switch, no `:cache-system?` /
  `:cache-tools?` / `:cache-ttl` opt menu. Cache is **always on**.
  Hard-coded. Anthropic auto-mode emits `cache_control` every
  request; OpenAI gets `prompt_cache_key` every request. The only
  caller-side parameter is the optional `:cache-key` (vis passes
  `session-id`; everyone else passes whatever; missing key on OpenAI
  is fine, just no routing stickiness). To debug a suspected cache
  issue, edit svar source. The 30 lines of "opt-out plumbing" buy
  zero value when the answer is always "cache it".

Trigger: session `dc606bf8-39ee-4555-85e6-782e31797f62`. Three coupled
problems from one short Polish chat with `claude-opus-4-7`:

1. TUI footer shows `tok 324→36` for a `siema`/`hi` turn while
   Anthropic's own usage records `cache_creation_input_tokens: 15036`.
   System prompt (~15K tokens) invisible in the displayed counter.
2. Cache write fires every turn (`turn 1: 15036`, `turn 2: 15212`) but
   `cache_read_input_tokens` stays **0**. Cache writes cost 1.25×
   base input rate; reads cost 0.1×. We pay write premium twice
   instead of write-once-read-many.
3. First turn never emitted `(set-session-title! …)`. Model only
   acknowledged the missing title in turn 2 after the user complained.
   No deterministic engine-side mechanism enforces a session title.

Root cross-cutting issue: **provider token-usage semantics differ and
vis treats them as identical**. Anthropic is additive (`input_tokens`
excludes cached); OpenAI / Gemini / Z.ai / OpenRouter are inclusive
(`prompt_tokens` already includes cached). Same prompt across two
providers ⇒ vis footer reads `324` vs `15360` for the same physical
work. Cost rollup at `loop.clj:2987-3016` accumulates raw
`:prompt_tokens` without normalizing.

---

## Live-validated svar bugs (cross-checked via nREPL against
`claude-haiku-4-5` through `:anthropic-coding-plan`, May 2026)

Five real bugs in svar `0.5.10` proven live. **Feature parity across
the six public entry points is THE root issue** — caching only works
in `svar/ask!`, and even there only partially. Vis uses
`svar/ask-code!`, so vis gets zero caching despite the docstring
claiming `:cache-system?` works.

### Feature-parity matrix (verified via REPL inspection)

| Entry point | `:cache-system?` | `:cache-tools?` | `:prompt-cache-key` | top-level `:system` |
|---|---|---|---|---|
| `ask!` / `ask!*` | ✅ works | ❌ missing | ❌ missing | ❌ missing |
| `ask-code!` / `ask-code!*` | ❌ **flag in opts — silently dropped** | ❌ | ❌ | ❌ |
| `abstract!` / `abstract!*` | ❌ | ❌ | ❌ | ❌ |
| `eval!` / `eval!*` | ❌ | ❌ | ❌ | ❌ |
| `refine!` / `refine!*` | ❌ | ❌ | ❌ | ❌ |
| `sample!` / `sample!*` | ❌ | ❌ | ❌ | ❌ |

Methodology: scanned `clojure.repl/source-fn` for each `*` impl,
grep'd for `cache-system?` / `cache-tools?` / `prompt-cache-key`
tokens. None of the higher-level wrappers (`abstract!` etc.)
themselves call the LLM — they all delegate to `ask!*`, so fixing
parity at `ask!*` + `ask-code!*` propagates to every wrapper via
the existing `llm-passthrough` merge pattern (line 4224, 4538,
4637, 4758, 4877, 5029, 5177, 5276, 5541).

### Bug S1 — top-level `:system` opt silently dropped (ALL entry points)

Repro:
```clojure
(svar/ask-code! r
  {:routing {:provider :anthropic-coding-plan :model "claude-haiku-4-5"}
   :system "<5249-token instructions>"
   :messages [{:role "user" :content "hi"}]
   :max-tokens 32 :lang "text"})
;; reported by anthropic: input_tokens 51  (just the user msg)
;; expected:                 input_tokens ≈ 5300
```

Root cause: `build-anthropic-request-body` (line 660) ONLY reads
system from `messages` filtered by `(= "system" (:role m))`.
Top-level `:system` opt is never consulted. svar accepts the key
(no spec validation), drops it.

Docs confirm the gap: no entry-point docstring even mentions
`:system` as a valid top-level opt.

### Bug S2 — `:role :system` (keyword) crashes; only `"system"` (string) works

Repro: `[{:role :system :content ...}]` → svar passes role through
verbatim → Anthropic returns HTTP 400 `"Unexpected role 'system'.
The Messages API accepts a top-level system parameter, not 'system'
as an input message role."`

Root cause: same line 660 — string-compare against `"system"`; a
keyword `:role :system` doesn't match the filter, so svar lets it
through as-is to the wire. Anthropic rejects.

### Bug S3 — `:cache-system?` only wired in `ask!*`; `ask-code!*` silently ignores

**REPL-verified parity (the bug):**

```clojure
;; ask!*       : (source-fn) contains "cache-system?" → TRUE
;; ask-code!*  : (source-fn) contains "cache-system?" → FALSE
;; abstract!*  : FALSE
;; eval!*      : FALSE
;; refine!*    : FALSE
;; sample!*    : FALSE
```

**Wire-body verification (clean repro):**

```clojure
;; UNIQUE 48K-char system prompt prefixed with a timestamp
;; so no earlier test could have primed the cache.
(svar/ask-code! r
  {:messages [{:role "system" :content huge}
              {:role "user" :content "hi"}]
   :cache-system? true   ;; ← IGNORED in ask-code! body builder
   ...})
;; tokens: {:input 9657, :cached 0, :cache-created 0}   ← NO CACHE
```

Direct inspection of `(build-anthropic-request-body messages model
extra-body)` for the same `:messages` (without `:svar/cache true`
on blocks) returns:

```clojure
{:system "STRING 48023 chars"}     ;; plain string, NO cache_control
```

Vs with `:svar/cache true` set manually on the block:

```clojure
{:system [{:type "text" :cache_control {:type "ephemeral"}}]}
```

The `:cache-system?` opt is propagated through `resolve-opts` (line
2864 `assoc :cache-system? cache-system?`) but consumed ONLY by
`ask!*` (line 3424, 3462). The `ask-code!*` body builder never
reads it.

### Bug S4 — no `:cache-tools?` opt anywhere

Anthropic's prefix hierarchy is `tools → system → messages`. Caching
the system without caching tools means any tools-array change
invalidates the system cache too. svar has zero support for tool
caching as a top-level convenience opt — callers must manually
attach `cache_control` to the last tool entry.

Not a blocker for vis (vis ships no tool defs to Anthropic), but
the parity story would be: ANY caller → single opt → cached.

### Bug S5 — no `:prompt-cache-key` opt for OpenAI / Responses

OpenAI's automatic cache is hash-of-first-256-tokens-keyed.
`prompt_cache_key` increases routing stickiness (OpenAI docs case
study: 60% → 87% hit-rate). svar exposes nothing for it. Vis can't
use it without poking the OpenAI extra-body manually.

Live-verified (Q1): when caller passes `:extra-body
{:prompt_cache_key "k"}` today, svar correctly carries it onto the
OpenAI wire body. The bug is just that there's no top-level
`:cache-key` opt; callers must know to use `:extra-body`. Phase 0.5
lifts it.

### Bug S6 — `:svar/cache-ttl :1h` emitted on wire WITHOUT the
`extended-cache-ttl-2025-04-11` beta header (Anthropic silently downgrades to 5min)

Live wire-body verification (Q7):

```clojure
;; Caller sets:
{:type "text" :text "..." :svar/cache true :svar/cache-ttl :1h}

;; svar emits on wire:
{:type "text" :text "..." :cache_control {:type "ephemeral" :ttl "1h"}}
```

Wire body is correct. BUT svar's `anthropic-oauth-headers` only
adds `"anthropic-beta" "claude-code-20250219,oauth-2025-04-20"`.
The `extended-cache-ttl-2025-04-11` header REQUIRED to honor 1h TTL
is **NEVER added**. Anthropic receives the `ttl: "1h"` field, ignores
it, falls back to 5min, no warning to caller.

Fix in svar Phase 0.5b: when ANY block in the request carries
`:svar/cache-ttl :1h`, append `extended-cache-ttl-2025-04-11` to the
`anthropic-beta` header for that request.

### Bug S7 — OpenAI Codex requires `:cache-key` for BOTH sticky
routing AND `cached_tokens` reporting

**Live verification (May 2026, two test runs):**

Without `:cache-key`:
```
Call 1: input 4844, cached 0, latency 7.40s
Call 2: input 4844, cached 0, latency 2.18s
Call 3: input 3644, cached 0, latency 1.49s
```
Latency drops prove cache fires server-side. `cached_tokens` field
stays 0 across all three calls.

With `:cache-key "stick-key-xyz"`:
```
Call 1: input 8443, cached 0,     latency 3.24s   (cold; cache write)
Call 2: input 8443, cached 7936,  latency 6.39s   (warm; field surfaces!)
```

**Resolution:** when caller passes `:cache-key`, svar forwards as
`prompt_cache_key` (Phase 0.5). This (a) pins routing to a stable
engine across calls and (b) makes the Codex backend surface the
`cached_tokens` field. Both effects in one opt.

Not a svar-side fix — Phase 0 already handles it via the
`apply-prompt-cache-key` chokepoint. Vis Phase C's one-line
`:cache-key (str session-id)` addition is THE thing that unlocks
Codex cache visibility + reuse. Without it, Codex sessions look like
they have zero cache hits even though the cache fires under the
hood (worst of both worlds: no visibility + no engine stickiness).

### Confirmed working: cache DOES fire end-to-end when blocks are tagged

Live test J above proves the full chain works at the API level the
moment a block carries `:svar/cache true` AND the prompt exceeds
the model's minimum (4096 tok for Haiku/Opus, 1024 for Sonnet):

```clojure
;; Call 1: {:input 37, :cache-created 19224, :cached 0}
;; Call 2: {:input 37, :cache-created 0,     :cached 19224}
;; ⇒ 92% input-token discount on call 2.
```

So: bug surface is svar's **plumbing**, not the wire protocol or
the Anthropic side. Fix the entry-point parity → vis cache wins for
free.

---

## Provider docs cross-check (May 2026)

Direct quotes / facts from official Anthropic + OpenAI docs that
drive the cache plan in Phase C below.

### Anthropic prompt caching

- **Max 4 `cache_control` breakpoints per request.**
- **Cache key hierarchy:** `tools` → `system` → `messages`. A change
  at one level invalidates that level AND everything after it.
- **Min cacheable prefix per model (this is what tripped my
  earlier 3.6K-token test):**
  - Sonnet 4.x: **1024 tokens**
  - Opus 4.7, Haiku 4.5: **4096 tokens**
  - Sub-minimum prompts: `cache_control` accepted silently, never
    written.
- **Lookback window:** 20 blocks per breakpoint. Growing
  conversations > 20 blocks since the last write miss the lookback;
  need a second breakpoint closer to the new position.
- **TTL:** 5min default ephemeral. 1h tier requires beta header
  `extended-cache-ttl-2025-04-11`. **1h breakpoints MUST appear
  before 5min breakpoints** in the prefix order.
- **Pricing:** cache write = 1.25× input, cache read = 0.1× input.
  Break-even = 2 reuses within TTL (1 write + 1 read ≈ 2.35× input,
  same as 2 uncached writes). After that pure win.
- **Cacheable block types:** tool definitions, system blocks, user/
  assistant text blocks, images, documents, tool_use, tool_result.
- **Auto-caching mode:** top-level `cache_control` field auto-places
  the breakpoint on the last cacheable block and **moves it forward
  as the conversation grows**. Uses 1 of the 4 breakpoint slots.
- **What invalidates the cache (full table from docs):**
  - Tool def changes → entire cache (tools+system+messages)
  - Toggling web search / citations → system + messages
  - Changing `tool_choice` → messages
  - `disable_parallel_tool_use` change → messages
  - Image add/remove → messages
  - Thinking params change → messages

### OpenAI prompt caching (Chat + Responses APIs)

- **Fully automatic.** No client signal needed.
- **Min cacheable:** 1024 tokens. Cache hits land in 128-token
  increments above that.
- **Cache discount:** 50% (gpt-4o) up to **90% (gpt-5.x family)**.
- **Cacheable:** entire messages array, tools array, structured
  output schemas, images.
- **Retention:** in-memory default for older models; gpt-5.5+ are
  24h-only (in-memory unsupported). Other gpt-5.x can opt into 24h
  via `service_tier` / `extended` retention.
- **`prompt_cache_key`** (optional but valuable): combined with the
  first-~256-token prefix hash to route requests to the same
  inference engine. One OpenAI customer's docs case study went from
  60% → 87% hit rate after adopting it.
- **Usage field:** `usage.prompt_tokens_details.cached_tokens`
  (Chat) / `usage.input_tokens_details.cached_tokens` (Responses).
  Inclusive: subset of `prompt_tokens` / `input_tokens`.
- **OpenAI Codex (`openai-compatible-responses`):** same `*details`
  shape, accessed via Responses-API envelope.

### GitHub Copilot (NOT live-validated this session)

Q4 test attempted but `:github-copilot-individual` token had
expired (`IDE token expired: unauthorized: token expired`). Token
refresh required before next validation pass. Defer test to first
post-merge regression run when the user is freshly authenticated.

Known shape (from docs): copilot proxies multiple upstream
providers (anthropic / openai / gemini / grok) over an OpenAI-style
endpoint. Cache reporting depends on the upstream:
- Anthropic-family via copilot → should surface `cache_creation` /
  `cache_read` if copilot's proxy passes them through (unconfirmed).
- GPT-5.x via copilot → `cached_tokens` per OpenAI shape.

Provider extension `provider_anthropic.clj` handles oauth-specific
limits; copilot has its own `provider-limits-fn` exposing premium
interactions remaining (verified via Q12, 215/300 remaining on the
test account).

### Z.ai (live-validated, May 2026)

Live nREPL test against `:zai-coding-plan` `glm-5.1`, 48K-char
system prompt with a unique timestamp tag (so no prior call could
have primed the cache):

```
Call 1: tokens {:input 8444, :cached 0,    :cache-created 0}
Call 2: tokens {:input 8444, :cached 8384, :cache-created 0}
         ↑ ~99% cache hit-rate, zero client config
```

Z.ai docs confirm: implicit auto-cache, no `cache_control` needed,
surfaces in `usage.prompt_tokens_details.cached_tokens` (OpenAI
wire shape). ~50% discount on cache hits. Wide model support
(GLM-5, GLM-5.1, GLM-4.7, GLM-4.6).

GLM-5.1 (coding-plan default) is *Anthropic-API-compatible* via
`https://api.z.ai/api/anthropic` for Claude Code clients, but vis
uses the OpenAI-shape coding endpoint (`/api/coding/paas/v4`). svar
already maps `:zai-coding-plan` to `:openai-compatible-chat`
api-style → cache fields surface correctly through the standard
OpenAI normalizer.

Net: **`:cache-key` forwarding on z.ai is harmless and might help
routing stickiness on multi-replica deployments** (z.ai docs don't
guarantee but don't forbid). Vis passes the session-id key on
z.ai too — same as OpenAI — no special case.

### Takeaway for vis

With Anthropic vis pays 1.25× write per turn forever and 0× reads
because no breakpoint is set. With OpenAI/Z.ai vis MIGHT be hitting
the implicit cache on stable system prompt prefixes but has no
`prompt_cache_key` to stick the routing, so hit rate is leaky.

Live measurements confirm cache works as documented on all three
providers we use: Anthropic (with marker), OpenAI/Codex (auto +
key), Z.ai (fully auto). All three fixed by Phase 0+C below.

---

## THE unified cache model: ALWAYS ON, NO OPTS

User-stated requirements: **"po prostu z automatu kurwa cachowac"**
followed by **"jebac parity, jebac opty"**. Cache is hard-coded ON
in svar. No flag. No kill switch. No per-call escape hatch. The only
parameter is the optional `:cache-key` string (forwarded as
`prompt_cache_key` on OpenAI for sticky routing).

Why always-on is the right answer:
- **Anthropic** has an official "automatic caching" mode — set
  top-level `cache_control` ONCE and the API auto-places the
  breakpoint on the last cacheable block AND moves it forward as the
  conversation grows. Below the model minimum prompt size, the marker
  is silently ignored. Zero downside to always emitting it.
- **OpenAI / Codex / Gemini / Z.ai** auto-cache server-side. Adding
  `prompt_cache_key` is silently ignored when absent, boosts hit-rate
  60% → 87% when present. Zero downside to always passing it.
- A `:cache true/false` flag costs 30 lines of plumbing (opts
  destructure, default-merge, conditional emission, docstring
  paragraph in 6 entry-points, regression tests) and buys zero
  value: when would we ever want it off? Cache-bisection debugging
  comes from grepping production logs, not from a per-call flag.

### The hard-coded contract (svar `0.6.0`)

```clojure
;; Every direct LLM call path emits:
;;   - Anthropic api-style: top-level cache_control: {type: ephemeral}
;;   - OpenAI / Responses api-style: prompt_cache_key (when :cache-key passed)
;;   - Everyone else: no client signal (provider auto-caches anyway)
;;
;; The ONLY caller-side parameter:
{:cache-key  "some-stable-string-or-nil"}    ;; for OpenAI routing stickiness
```

### Fine-grained overrides (still possible, just not a documented opt)

Callers who genuinely need a non-default placement hand-build blocks
with `:svar/cache true` (existing svar `0.5.10` shape, unchanged):

```clojure
{:role "system"
 :content [{:type "text" :text stable   :svar/cache true :svar/cache-ttl :1h}
           {:type "text" :text per-turn}]}    ;; not cached

;; Sugar that already exists:
(svar/cached stable-text {:ttl :1h})
```

When ANY manually-tagged block is present, svar's auto-mode steps
aside (avoids burning two of the 4 anthropic breakpoint slots on the
same position). The manual placement wins.

This is an EDGE CASE. Not part of the documented happy path. The
90% case is: call `ask-code!`, get cached output. The 10% case is:
do the hand-tagging if your use case demands per-block TTL or split
placement — svar already supports the shape, no new opt needed.

### The five places, one vocabulary

```
      caller opts (svar)              wire body (provider)
  -------------------------       -------------------------
  (always-on, no opt)            -> top-level cache_control (anthropic)
  :cache-key  "abc"              -> prompt_cache_key (OpenAI only)
  :system     "..."              -> system param (anthropic) /
                                     system message (openai)

      provider usage              canonical svar :tokens
  -------------------------    -------------------------
  anthropic input_tokens     \
  + cache_creation_input_tokens } -> :input-tokens (TOTAL, always inclusive)
  + cache_read_input_tokens  /     :input-tokens-details {:regular     N
                                                          :cache-write W
                                                          :cache-read  R}
  openai prompt_tokens          ->  :input-tokens (already TOTAL)
  + prompt_tokens_details          :input-tokens-details {:regular     N
      .cached_tokens                                      :cache-read  R
                                                          :cache-write 0}

      DB column (sqlite)         display (TUI footer)
  -------------------------    -------------------------
  input_tokens                 "tok <total>→<out>"
  input_regular_tokens         when read or write > 0:
  input_cache_write_tokens     "... (cached R, cache-write W)"
  input_cache_read_tokens      cache 87% ↓75k +360   (optional segment)
  output_tokens                ctx 15360/200000 (8%)  (optional segment)
  output_reasoning_tokens
```

**The invariant runs end-to-end:**
`regular + cache-write + cache-read = input-tokens` at every layer.
SQLite CHECK enforces it at write time; vis tests assert it at read
time; svar tests assert it at provider-normalization time.

### One way to use it from vis (zero chokepoint changes)

File: `src/com/blockether/vis/internal/prompt.clj`.
Fn: `assemble-stable-prompt-messages` (the existing one).

**No change needed.** svar's auto-cache default means vis doesn't
have to tag blocks at all. The assembler already emits a stable
system prompt as the first message; svar's top-level
`cache_control` will auto-place its breakpoint at the end of system,
which is exactly what we want.

Only thing vis adds: `(assoc opts :cache-key (str session-id))` in
the `ask-code!` call inside `loop.clj` so OpenAI hits its sticky
cache too. ONE LINE.

Vis can still hand-build cache markers for the rolling conversation
breakpoint (Phase C.1 slot 3) when there's measurable win to take
the 2nd cache slot. Default = auto-mode only; opt in to
fine-grained when telemetry justifies.

### One way to read it back (vis loop + chat projection)

Loop (`loop.clj:2987-3016` post-rewrite) reads CANONICAL svar shape
`(:input-tokens api-usage)` + `(get-in api-usage [:input-tokens-details
:cache-read])` directly. No `(or :prompt_tokens :cached :input-cached
...)` fallback chains; the canonical fields are the only fields.

Persistance write (`persistance_sqlite/core.clj`) maps canonical
`:input-tokens-details` keys 1:1 to schema columns. No remapping.

Chat history projection (`chat.clj/rebuild-history`) reads the same
columns back into the same canonical shape. No remapping.

### One way to monitor (telemetry + footer)

One signal: `cache-hit-rate = cache-read / input-tokens` per turn.

- `tel/with-ctx+ {:cache-hit-rate r}` stamped on every iteration log.
- `vis report` aggregates by session.
- TUI footer optional `cache 87% ↓75k +360` segment.
- Alert when ratio < 50% on `iteration-count >= 3` (matches Claude
  Code team's published SEV threshold).

One metric. Same definition from svar's response → vis loop → DB →
footer → alert.

### Cross-provider semantics table

| Provider | What svar emits | Min prompt | TTL | Canonical shape fields populated |
|---|---|---|---|---|
| Anthropic (Claude 4.x) | top-level `cache_control: {type: ephemeral}` always | 4096 (Opus/Haiku), 1024 (Sonnet) | 5min default | full split: regular + cache-write + cache-read |
| OpenAI (gpt-4o+, gpt-5.x) | `prompt_cache_key` when caller passes `:cache-key` | 1024 | 24h (gpt-5.x default), 5min (gpt-4o) | regular + cache-read (no cache-write field) |
| OpenAI Codex (Responses API) | same as OpenAI | 1024 | same as above | regular + cache-read |
| Z.ai (GLM 5.x coding-plan) | nothing (server auto-caches) | unknown (assume 1024) | unknown | regular + cache-read when surfaced |
| OpenRouter → Anthropic | passes Anthropic markers + extras | per underlying model | per underlying | full split via pydantic-extras |
| Gemini | nothing (server auto-caches) | 1024 | variable | regular + cache-read |

Unified API: **cache is always on.** Caller adds `:cache-key (str
session-id)` for one extra string to boost OpenAI sticky routing.
svar handles every api-style-specific wire detail; canonical shape
comes back with whichever fields the provider populates.

---

## Industry consensus (May 2026) — informs the canonical shape

| Source | Convention |
|---|---|
| Vercel AI SDK V3 spec (vercel/ai#9921) | `inputTokens` = TOTAL; `inputTokensDetails { regular, cacheWrite, cacheRead }` |
| OpenTelemetry `gen_ai.usage.input_tokens` (≥ v1.37) | "SHOULD include all types of input tokens, including cached" |
| Claude Code official statusline JSON | `context_window.total_input_tokens = input + cache_creation + cache_read`; per-component detail under `current_usage` |
| opencode TUI status-line vars | `{tokens_input}`, `{tokens_output}`, `{tokens_cache_read}`, `{tokens_cache_write}`, `{total_tokens}`, `{context_used_pct}` |
| latitude-dev #2558 | Documents the exact double-count / under-report bug across inclusive vs additive providers |
| Anthropic "Lessons from building Claude Code" | System prompt + tools + CLAUDE.md cached as 4-layer prefix; alert on cache-hit-rate as SEV |
| Piebald `claude-code-system-prompts/data-prompt-caching-design-optimization.md` | Max 4 `cache_control` breakpoints; place on LAST block of stable region |

**Adopted shape:** canonical `:input-tokens` = TOTAL (inclusive). Subset
breakdown under `:input-tokens-details {:regular :cache-write
:cache-read}`. Invariant: `regular + cache-write + cache-read = total`.

---

## Phase 0 — svar bug fixes + always-on cache (BREAKING)

Goal: every direct LLM call path emits `cache_control` on Anthropic
and forwards `prompt_cache_key` on OpenAI when the caller passes one.
Hard-coded. No flag. Ship in svar `0.6.0` together with Phase A
canonical-usage shape.

### 0.1 — single `apply-llm-opts` middleware (the chokepoint)

One function runs before EVERY direct LLM call. `ask!*` and
`ask-code!*` both call it. Wrappers (`abstract!*` / `eval!*` /
`refine!*` / `sample!*`) inherit via the existing `llm-passthrough`
merge — not by parity test, by the fact that there is no other code
path that builds a request body.

```clojure
(defn- apply-llm-opts
  "Caller-opt → wire-body bridge. Runs once before each LLM call."
  [messages {:keys [system cache-key] :as opts}]
  (-> messages
    ;; S1+S2: top-level :system + keyword/string role normalization
    (apply-top-level-system system)
    ;; AUTO-CACHE always on. Anthropic body builder reads the meta
    ;; and emits top-level cache_control. OpenAI body builder no-ops.
    (vary-meta assoc :svar/auto-cache true)
    ;; S5: prompt_cache_key forwarding for OpenAI / Responses
    (apply-prompt-cache-key cache-key opts)))
```

### 0.2 — fix S1 + S2 (system-message handling)

In `apply-top-level-system`:
- Accept top-level `:system` (string OR vec-of-content-blocks).
  Prepend as the FIRST `:role "system"` message; any existing
  role-system messages append after.
- Normalize role at the head of `build-anthropic-request-body`:
  `(filter #(= "system" (some-> (:role %) name)))` so keyword
  `:role :system` matches string `"system"`.

### 0.3 — fix S3 via always-on cache (the merge)

`:cache-system?` opt is GONE. Auto-cache fires unconditionally on
Anthropic. Body builder (`build-anthropic-request-body`):

```clojure
(cond-> body
  ;; Always emit top-level cache_control on Anthropic. Provider
  ;; auto-places the breakpoint on the last cacheable block and
  ;; moves it forward as the conversation grows. Below model minimum
  ;; size the marker is silently ignored — no penalty.
  system-wire
  (assoc :cache_control {:type "ephemeral"})

  ;; When a manually-tagged :svar/cache block is present, the manual
  ;; placement wins (avoids burning two of the 4 anthropic
  ;; breakpoint slots on the same position). Drop the top-level
  ;; auto field in that case.
  (any-explicit-cache-marker? messages)
  (dissoc :cache_control))
```

Result: every call vis (or anyone) makes auto-caches on Anthropic.
Zero caller boilerplate.

### 0.4 — drop `:cache-tools?` (auto-mode covers it)

Anthropic's prefix hierarchy is `tools → system → messages`. The
auto-placed breakpoint at end-of-system caches `tools + system`
together as ONE prefix. Separate tools flag = unnecessary.

Edge case: caller wants tools-only cache (force system to re-process
every turn). Hand-build a `:svar/cache true` marker on the last tool
entry. svar already supports the shape. No documented opt.

### 0.5 — fix S5 (`:cache-key` for OpenAI sticky routing)

`apply-prompt-cache-key`: when caller passes
`:cache-key "some-string"` AND api-style is OpenAI / Responses /
openai-compatible → merge into `:extra-body` as
`{:prompt_cache_key "some-string"}`. No-op on Anthropic.

This is the ONE opt that has any caller-side semantics. Vis derives
the key from `session-soul-id` (one stable key per session).

Live-verified (Q1): when the value lands on `:extra-body`, svar
already carries it onto the OpenAI wire body. Phase 0.5 just lifts
it to a top-level opt so callers don't need to know the magic
extra-body key.

### 0.5b — fix S6 (auto-add 1h-cache beta header)

When ANY block in the request carries `:svar/cache-ttl :1h`:

- Append `extended-cache-ttl-2025-04-11` to the `anthropic-beta`
  header for that single request.
- For OAuth coding-plan keys, the existing static value
  `"claude-code-20250219,oauth-2025-04-20"` becomes
  `"claude-code-20250219,oauth-2025-04-20,extended-cache-ttl-2025-04-11"`.
- For raw API keys, the header gets added even if it was absent
  before.

Hook lives in `build-anthropic-request-body` (or wherever headers
are composed for the wire call) — scan the request body for any
`:cache_control` block with `:ttl "1h"` after `cache-control-for`
translates the marker.

Docstring on `cache_control` opt mentions the beta header as a
"requires" note today but doesn't enforce it. Phase 0.5b makes it
automatic.

Live-verified bug (Q7+Q8): svar 0.5.10 emits the `ttl: "1h"` field
on wire but the beta header stays missing → Anthropic silently
degrades to 5min.

### 0.6 — tests (svar)

Minimal set. No parity matrix. Wire-body verification is enough.

- `(build-anthropic-request-body messages model)` for any messages
  vec with a non-empty system → wire body contains
  `:cache_control {:type "ephemeral"}`. Single assertion.
- Same call with a manually `:svar/cache`-tagged block → wire body
  has the marker on the block AND top-level `cache_control` is
  absent (manual placement wins).
- OpenAI body builder + `:cache-key "k"` in opts → wire body
  contains `prompt_cache_key "k"`.
- Live test (when ANTHROPIC_API_KEY present): single-call → turn 1
  shows `cache-write > 0`. Second call within 5min → turn 2 shows
  `cache-read > 0`. This is the dc606bf8 reproduction inverted.

No per-entry-point test (jebac parity). The middleware is the only
code path — if `(apply-llm-opts ...)` works, every entry point works.

---

## Phase A — svar canonical usage shape (BREAKING)

Single chokepoint for normalization. Today svar emits a hybrid:
`anthropic-extract-response-data` sets `:prompt_tokens` to anthropic's
RAW `input_tokens` (excludes cached); `normalize-openai-usage` passes
through OpenAI's `prompt_tokens` (already inclusive). Same field name,
two different meanings — origin of bug.

### A.1 — new ns `com.blockether.svar.internal.usage`

Single canonical shape every provider extractor produces:

```clojure
{:input-tokens          <long>   ;; TOTAL prompt tokens (always inclusive)
 :output-tokens         <long>   ;; TOTAL completion tokens
 :input-tokens-details  {:regular     <long>   ;; not from cache, not written
                         :cache-write <long>   ;; written this request (1.25×)
                         :cache-read  <long>}  ;; served from cache (0.1×)
 :output-tokens-details {:reasoning   <long>}  ;; subset of output-tokens
 :total-tokens          <long>   ;; input-tokens + output-tokens convenience
 :raw                   <map>}   ;; original provider envelope, preserved
```

Invariant tests:
- `regular + cache-write + cache-read = input-tokens`
- `output-tokens-details.reasoning ≤ output-tokens`

### A.2 — provider normalizers

- `anthropic-canonical-usage`: `input-tokens = input_tokens +
  cache_creation_input_tokens + cache_read_input_tokens`. Details map
  filled directly. Replaces `anthropic-extract-response-data` usage
  block + `anthropic-stream-usage` so stream and non-stream paths
  share one canonicaliser.
- `openai-canonical-usage`: `input-tokens = prompt_tokens` (already
  total). `cache-read = prompt_tokens_details.cached_tokens` (or 0).
  `cache-write = prompt_tokens_details.cache_creation_tokens` (only
  when OpenRouter-proxied Anthropic surfaces it; otherwise 0).
  `regular = input-tokens - cache-write - cache-read`.
- `gemini-canonical-usage`: `input-tokens = promptTokenCount`.
  `cache-read = cachedContentTokenCount`. `cache-write = 0`.
  `regular = input-tokens - cache-read`.
- `zai` / `openrouter` go through `openai-canonical-usage` (OpenAI
  wire shape).

### A.3 — delete legacy fields (hard cut)

`:prompt_tokens` / `:completion_tokens` / `:prompt_tokens_details` /
`:completion_tokens_details` are GONE from every svar return. The
canonical `:input-tokens` etc. fields replace them in-place. NO
alias keys, NO `^:deprecated` markers, NO emit-both-for-one-release
transitional window. Bump svar to `0.6.0`. Every external svar
consumer (vis is the only known one) breaks at the same commit
that ships the canonical shape — if a downstream wants the old keys
it reads `(:raw u)` and pulls them itself.

### A.4 — pricing path

`router/usage-cache-tokens` + the cost estimator currently re-derive
splits from the legacy `prompt_tokens_details`. Inline-read the new
canonical `:input-tokens-details` directly. Drop the
`:cache-tokens-in-input?` flag — obsolete because canonical totals are
inclusive across the board.

### A.5 — tests (svar)

- Invariant table per provider: raw envelope → canonical shape;
  asserts `regular + write + read = total` for every fixture.
- Round-trip fixtures from current production traffic: anthropic +
  cache write, anthropic + cache read, anthropic + no cache, openai
  + cached, openai + no cache, gemini + cached.
- Cost regression: cache-write rate 1.25×, cache-read rate 0.1×;
  totals match published Anthropic pricing for `claude-opus-4-7`,
  `claude-sonnet-4-6`, `claude-haiku-4-5`.

---

## Phase B — vis canonical wiring (SCHEMA REWRITE, NO MIGRATION)

### B.1 — rewrite `V1__schema.sql` token columns inline

Replace token columns on `session_turn_state` AND
`session_turn_iteration` with the canonical shape. Drop legacy column
names entirely; no `ALTER TABLE`, no V2, no shim view.

**`session_turn_state` (lines 264-267 currently):**

```sql
  -- Canonical input-token shape. `input_tokens` is ALWAYS the total
  -- across providers (Anthropic additive raw values are summed into
  -- this column at write time; OpenAI / Gemini already total). The
  -- detail columns are subsets of `input_tokens` and obey the
  -- invariant:
  --   input_regular_tokens + input_cache_write_tokens
  --     + input_cache_read_tokens = input_tokens
  input_tokens                INTEGER NOT NULL DEFAULT 0
                              CHECK (input_tokens >= 0),
  input_regular_tokens        INTEGER NOT NULL DEFAULT 0
                              CHECK (input_regular_tokens >= 0),
  input_cache_write_tokens    INTEGER NOT NULL DEFAULT 0
                              CHECK (input_cache_write_tokens >= 0),
  input_cache_read_tokens     INTEGER NOT NULL DEFAULT 0
                              CHECK (input_cache_read_tokens >= 0),
  output_tokens               INTEGER NOT NULL DEFAULT 0
                              CHECK (output_tokens >= 0),
  output_reasoning_tokens     INTEGER NOT NULL DEFAULT 0
                              CHECK (output_reasoning_tokens >= 0),
  total_cost_usd              REAL    NOT NULL DEFAULT 0
                              CHECK (total_cost_usd >= 0),
  -- Cost breakdown (writeable subset of total). All optional so a
  -- provider with no per-component pricing leaves them NULL.
  input_regular_cost_usd      REAL CHECK (input_regular_cost_usd IS NULL OR input_regular_cost_usd >= 0),
  input_cache_write_cost_usd  REAL CHECK (input_cache_write_cost_usd IS NULL OR input_cache_write_cost_usd >= 0),
  input_cache_read_cost_usd   REAL CHECK (input_cache_read_cost_usd IS NULL OR input_cache_read_cost_usd >= 0),
  output_cost_usd             REAL CHECK (output_cost_usd IS NULL OR output_cost_usd >= 0),
```

**`session_turn_iteration` (lines 360-378 currently):** same column
shape as above, all nullable (per-iter usage may be absent when the
provider call errored before usage landed). Drop the legacy
`llm_input_tokens` / `llm_output_tokens` / `llm_reasoning_tokens` /
`llm_cached_tokens` / `llm_cache_created_tokens` / `llm_cost_usd`
columns entirely.

Add per-iter CHECK that enforces the invariant when ALL fields are
non-null:

```sql
CHECK (
  input_tokens IS NULL OR
  (input_regular_tokens + input_cache_write_tokens
     + input_cache_read_tokens = input_tokens)
)
```

### B.2 — DB nuke on boot (no rename, no preservation)

Current state (verified): `persistance_sqlite/core.clj:150-173` already
throws on Flyway checksum mismatch with a user-facing message telling
the user to manually `rm -rf ~/.vis/vis.mdb` and restart. That manual
step dies.

New behaviour: when `migration-checksum-mismatch?` fires, the
handler ITSELF deletes `~/.vis/vis.mdb/` recursively then re-runs
`install-schema!` on a fresh empty directory. One stderr line emitted:

```
vis: DB schema changed; old DB at ~/.vis/vis.mdb deleted, recreating fresh.
```

No backup. No `vis.mdb.legacy.<ts>/` stash directory — those existing
dirs in `~/.vis/` are leftovers from earlier manual interventions, NOT
from code. Code creates none. The user's historical session rows are
gone; they were broken anyway (wrong token semantics, missing
cache-write column). Acceptable cost for clean shape.

### B.3 — persistance core rewrite

`extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj`:

- Delete `llm_input_tokens` / `llm_output_tokens` / `llm_reasoning_tokens` /
  `llm_cached_tokens` / `llm_cache_created_tokens` / `llm_cost_usd`
  references everywhere.
- New write payload reads canonical shape directly:
  ```clojure
  {:input_tokens             (:input-tokens tokens)
   :input_regular_tokens     (get-in tokens [:input-tokens-details :regular])
   :input_cache_write_tokens (get-in tokens [:input-tokens-details :cache-write])
   :input_cache_read_tokens  (get-in tokens [:input-tokens-details :cache-read])
   :output_tokens            (:output-tokens tokens)
   :output_reasoning_tokens  (get-in tokens [:output-tokens-details :reasoning])}
  ```
- Read projection returns the same shape (no field-name remapping at
  the boundary; the canonical shape IS the wire format).

### B.4 — vis loop migration

`src/com/blockether/vis/internal/loop.clj:2987-3016` currently reads
`:prompt_tokens` / `:prompt_tokens_details`. Replace with
`(svar.usage/canonical api-usage)` and pull typed fields. Delete the
two `(or ... :input_cached_tokens ...)` legacy-alias fallbacks
(`loop.clj:2993`, `3016`) — canonical shape is the only shape.

`usage-atom` aggregation simplifies to:

```clojure
(-> acc
  (update :input-tokens             + (:input-tokens api-usage))
  (update :input-regular-tokens     + (get-in api-usage [:input-tokens-details :regular]))
  (update :input-cache-write-tokens + (get-in api-usage [:input-tokens-details :cache-write]))
  (update :input-cache-read-tokens  + (get-in api-usage [:input-tokens-details :cache-read]))
  (update :output-tokens            + (:output-tokens api-usage))
  (update :output-reasoning-tokens  + (get-in api-usage [:output-tokens-details :reasoning])))
```

### B.5 — chat / restored-bubble projection

`extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj:444-445`
currently builds the `:tokens` map for `format-meta-line` from
legacy fields. Replace with:

```clojure
tokens {:input          (:input-tokens q)
        :input-regular  (:input-regular-tokens q)
        :cache-write    (:input-cache-write-tokens q)
        :cache-read     (:input-cache-read-tokens q)
        :output         (:output-tokens q)
        :reasoning      (:output-reasoning-tokens q)}
```

### B.6 — `format-tokens` rewrite

`src/com/blockether/vis/internal/format.clj/format-tokens` reads the
new keys directly. Display contract:

```
tok <input-total>→<output>                            ;; minimum
tok <input-total>→<output> (cached R)                 ;; cache hit
tok <input-total>→<output> (cached R, cache-write W)  ;; cache hit + new write
tok <input-total>→<output> (cache-write W)            ;; first turn / no hit yet
```

Always show the TOTAL up front. Detail in parens only when non-zero.
Behavior identical across providers because canonical totals are
inclusive everywhere.

### B.7 — TUI context-window % (parity with Claude Code statusline)

New optional footer slot `ctx <input-total>/<model.context-limit>
(<pct>%)` driven by `:vis/show-context-pct` toggle (default off so
the current footer width stays the same unless opted in). Model
context limit pulled from svar's router catalog.

### B.8 — tests (vis)

- `format_test`: feed canonical-shape `:tokens` map → assert footer
  string for cache-miss, cache-hit, cache-write-only, and first-turn
  shapes.
- `chat_test/rebuild-history`: persisted row with non-zero
  `input_cache_write_tokens` projects into the rebuilt `:tokens`
  map and renders correctly.
- `loop_test/usage-accumulation`: stream a synthetic 3-iter session
  with mixed cache shapes; final aggregate equals provider truth.
- `persistance/core-test`: round-trip session_turn_state +
  session_turn_iteration with non-zero cache fields; assert invariant
  CHECK trips on bad payload.

---

## Phase C — vis cache wiring (one line + observability)

Mantra (user-stated): **"po prostu z automatu kurwa cachowac"** +
**"jebac opty"** — so the big lever is svar Phase 0 making
auto-cache hard-coded. After that lands, vis gets the 90% cost win
without changing call sites. This phase is the ~10% — the one
session-id-keyed `:cache-key` pipe + observability.

Live nREPL repro (test J in this session) confirms the chain works
end-to-end the moment ANY breakpoint is set — `cache-created 19224`
on call 1, `cached 19224` on call 2. That's a ~92% discount on every
cached token after the first turn.

### C.0 — the freebie + the one line (the entire vis change)

Once svar `0.6.0` lands in `deps.edn`, every `(svar/ask-code! ...)`
call vis makes auto-tags Anthropic with top-level `cache_control`.
Last cacheable block (usually end of system message) becomes the
auto-placed breakpoint. Conversation grows → breakpoint moves
forward automatically. **Vis gets cache for free on every Anthropic
turn with zero call-site change.**

The ONLY vis edit: add `:cache-key (str (:session-id env))` to the
`ask-code!` opts in `src/com/blockether/vis/internal/loop.clj`:

```clojure
(svar/ask-code! router
  (assoc opts
    :cache-key (str (:session-id env))))   ;; ← the only line
```

One line. That's the whole Phase C user-facing implementation. The
rest of this phase is verification + observability scaffolding.

Why `:cache-key`: OpenAI's automatic cache fires server-side anyway,
but routing stickiness via `prompt_cache_key` boosts hit-rate from
~60% → ~87% per OpenAI's own docs case study. Session-soul-id is
stable per session, so every call within a session lands on the same
inference engine → maximum reuse.

No-op on Anthropic (svar drops the key from extra-body before wire).

Verification: post-upgrade, re-run a session like `dc606bf8` and
assert turn 2+ shows `cache-read > 0` instead of `cache-write` only.

### C.2 — prefix-stability hygiene audit (one-time, no recurring work)

Both providers' caches are byte-identical-prefix hashes. The single
biggest risk is interpolated timestamps / per-turn counters / random
UUIDs inside the system prompt or CTX EDN block — every such byte
difference invalidates the cache for the rest of the prefix.

Audit before merging the deps bump:

- `prompt/build-system` — grep for `(java.util.Date.)`, `Instant/now`,
  `(random-uuid)`, `(quot now ...)`, anything that varies per call.
  Move every such interpolation OUT of the cached region. If a value
  truly must reach the model fresh, append it as a separate trailing
  user-message block that lands AFTER the auto-placed breakpoint.
- `prompt/build-ctx-edn-block` — same audit. Session-level state
  (specs, tasks, facts) is stable within a session-state version, so
  by construction it lives behind the breakpoint cleanly. Per-iter
  counters in the EDN preamble (e.g. iteration-position) are the
  risk.
- The `(set-session-title! "...")` recap in the system block is
  stable within a session once set; first-call writes the cache,
  subsequent calls read it. No fix needed.

Add a `vis report cache-prefix-stability` command later that diffs
the rendered prompt bytes between two consecutive turns of a session
and highlights any diff inside the cached region. Out of scope for
this PR but worth listing as a follow-up.

### C.3 — optional: hand-built 1h TTL on the system prompt (deferred)

Default auto-cache uses 5min TTL. For typical interactive sessions
that's enough — each turn refreshes via lookup access. But long
breaks (lunch, meeting) evict the cache; next turn pays cache-write
again.

If telemetry shows >30% of multi-turn sessions hit the eviction,
opt the system prompt into 1h TTL by hand-building the system
message in the vis assembler:

```clojure
;; src/com/blockether/vis/internal/prompt.clj
[{:role "system"
  :content [(svar/cached system-text {:ttl :1h})]}]
```

When ANY explicit `:svar/cache true` marker is present, svar's
auto-mode steps aside (avoids burning a 2nd breakpoint slot on the
same position). The manual 1h tag takes the slot instead.

Requires `extended-cache-ttl-2025-04-11` beta header — svar already
adds OAuth beta headers for `:anthropic-coding-plan`; extend the
header list when ANY `:svar/cache-ttl :1h` is observed on the
request.

Deferred until telemetry justifies. Default 5min covers the common
case.

### C.4 — minimum-prompt-size awareness (cosmetic, non-blocking)

From live testing AND docs:
- Sonnet 4.x: 1024 tokens minimum for cache to fire
- Opus 4.7, Haiku 4.5: **4096 tokens** minimum
- OpenAI all models: 1024 tokens minimum

Vis system prompt today is ~15K tokens — above every threshold. But
the FALLBACK / consult path uses a much smaller prompt; auto-cache
there is a no-op (anthropic silently accepts + ignores).

Add a `model.cache-min-tokens` field to svar's router catalog so
the telemetry hook (Phase 0.8) can stamp `:cache-decision
:below-min-size` instead of a false-positive `:auto-cache-emitted`
when the prompt won't actually cache. Cosmetic only.

### C.5 — (DELETED) per-slot breakpoint placement

The earlier draft of this section spent slots 1-4 hand-placing
breakpoints on system / CTX / conversation prefix / rolling reserve.
Auto-cache mode handles slot 1 for free; slot 3 (conversation
prefix) is implicitly covered because anthropic's auto-placement
moves the breakpoint to the LAST cacheable block of the request —
in a multi-turn conversation that's the last assistant turn. Slot 2
(CTX preamble) becomes a follow-up if telemetry shows the CTX block
varying per turn would otherwise waste tokens — a separate explicit
breakpoint splits the user message into cached + uncached halves.
Slot 4 is on-demand and stays caller-driven.

Net: this phase ships with ZERO hand-placed breakpoints. Auto-mode
does the work.

### C.6 — cache hit observability

Canonical `:input-tokens-details {:cache-read R :cache-write W
:regular N}` is first-class telemetry:

- `tel/log!` `::cache-hit-rate` per turn with
  `cache-read / input-tokens` ratio. Alert (warn-level) when ratio
  drops below 50% on a session with `iteration-count >= 3` (matches
  Anthropic Claude Code's "alert on cache breaks as SEVs" advice).
- `vis report` rolls up cache hit rate per session.
- TUI footer optional `cache 87% ↓75k +360` segment behind
  `:vis/show-cache-stats` toggle (parity with the
  `claude-code-statusline` community reference).
- Cost breakdown surfaces in `format-cost`: detail map shows
  `in / cached / write / out` so the user can see WHERE the
  money went per turn.

### C.7 — (DELETED) guardrails / kill switches

Deleted per **jebac opty**. No `:vis/prompt-cache` toggle, no
`--no-cache` CLI flag. Cache is always on. Model-not-cacheable
detection: svar already no-ops markers on incompatible api-styles;
vis doesn't need a second gate.

Auto-fallback wreckage (Anthropic → OpenAI mid-session wastes the
cached prefix): log + accept. Routing-policy revision is out of
scope for this PR.

### C.8 — tests

Live nREPL only — mocks lie about provider behavior, the bug-hunt
this whole PR exists to fix proved that.

- Anthropic: same prompt across two turns ⇒ turn 2 canonical usage
  shows `cache-read > 0`. Repro of the test-I/J pair in the
  live-validated bug section above.
- Anthropic: changing the system prompt between turns ⇒ turn 2
  `cache-read = 0`, `cache-write ≈ system prompt size`.
- Anthropic: wire body contains exactly one top-level
  `cache_control` field (auto-mode places its single breakpoint,
  manual placement off in the default path).
- OpenAI / Codex: `prompt_cache_key` present on wire when
  `:cache-key` passed; turn 2 of a stable-prompt session shows
  `cached_tokens > 0` from `input_tokens_details`.
- Cost regression: 5-turn session with stable system prompt costs
  ~80-90% less in input than the same 5-turn session run against a
  svar with the auto-cache disabled (temporarily, in a test
  fixture). Matches Anthropic's published "Lessons" numbers.

---

## Phase E — cache-aware routing policy (in-scope)

User-stated: routing policy revision is NOT out of scope. svar's
default `:on-transient-error :hybrid` strategy tries cross-provider
fallback on first transient error, which is poison for the cache
prefix — the warm Anthropic cache (~15K tokens) becomes worthless
the moment the next call lands on OpenAI/Z.ai, and the OpenAI cache
is empty so we pay full input rate to bootstrap.

Live svar `0.5.10` defaults `(or on-transient-error :hybrid)`
at `router.clj:1770`. Vis currently passes `:routing (or routing {})`
— empty map → svar default → cross-provider switch on error.

The revision: make the default **provider-sticky within a session**
and factor cache state into the fallback decision.

### E.1 — sticky-routing default

In vis `loop.clj`, set the default routing to keep all turns within
a session on the SAME provider/model:

```clojure
;; src/com/blockether/vis/internal/loop.clj (ask-code! call site)
(svar/ask-code! router
  (assoc opts
    :cache-key (str (:session-id env))
    :routing   (merge {:on-transient-error :fallback-model-in-the-same-provider}
                (or routing {}))))
```

`:fallback-model-in-the-same-provider` keeps fallback within the
active provider — if `claude-opus-4-7` rate-limits, svar tries
`claude-sonnet-4-6` or `claude-haiku-4-5` next; cache prefix
survives if the system prompt is identical across models (it is in
vis, because the model id doesn't go into the system prompt).

Override via per-call opts (consult engine already does this with
explicit `:provider :model` overrides; no behavior change there).

### E.2 — cross-provider escape hatch

When ALL same-provider fallbacks exhausted (whole provider down,
like Anthropic 503 across all models), svar falls through to
cross-provider as the LAST resort. svar already does this when
`:hybrid` is set; we want the same as the SECOND tier of
`:fallback-model-in-the-same-provider`.

This is svar-side work — add a new strategy keyword
`:same-provider-then-cross` (or extend `:hybrid` to try ALL
same-provider models before crossing). Plumbed through to
`with-provider-fallback`. Default for vis becomes the new value.

### E.3 — cache-wasted telemetry on cross-provider switch

When vis observes `(not= (:llm-actual-provider iter)
(:llm-actual-provider prev-iter))` AND `prev-iter` had `cache-write
> 0`, log `::cache-wasted` with the dollar value of the orphaned
cache. Emits one warn-level line per cross-provider hop. Telemetry
only; no automated recovery (cache TTL is 5min, just wait it out).

### E.4 — routing-decision factors (future-proofing)

Document what the routing decision SHOULD consider (deferred to a
later PR, but list here so we don't forget):

- **Cost per cache-miss tier** (already in svar router via
  `:input-cost` / `:cached-input` / `:cache-write-5m` etc.).
- **Latency / TTFT** (svar has timeout knobs but no routing
  preference yet).
- **Cache locality (NEW):** "we wrote 15K to Anthropic 90 seconds
  ago—cache still hot for 4 more minutes—prefer Anthropic this
  turn even if a cheaper option exists." Requires tracking cache
  state per (session, provider, model) tuple in vis or svar.

Out of scope for this PR but a follow-up worth tagging.

### E.5 — quota-aware fallback (all 4 providers — data confirmed live)

Live Q12 confirms ALL configured providers expose limits via
`vis/provider-limits`:

| Provider | Live values today | Window |
|---|---|---|
| `:anthropic-coding-plan` | Claude 5h 32% used, Claude 7d 76% used, Sonnet 7d 0% used | rolling 5h / 7d |
| `:zai-coding-plan` | tokens 1/100 (5h), requests 0/4000 (month) | rolling 5h / calendar month |
| `:openai-codex` | Codex 5h 8% used, Codex 7d 3% used | rolling 5h / 7d |
| `:github-copilot-individual` | Premium interactions 85/300 (this month) | calendar month |

All plumbing exists. E.5 just wires a router hook:

```clojure
;; src/com/blockether/vis/internal/router.clj (or wherever vis picks
;; the default provider for a new session)
(defn- quota-pressure-score [provider-id]
  ;; Returns 0.0-1.0 where 1.0 = quota fully exhausted in any window.
  (let [{:keys [limits]} (vis/provider-limits provider-id)]
    (apply max 0.0 (keep (fn [{:keys [used limit]}]
                           (when (and (pos? limit) used)
                             (/ (double used) (double limit))))
                     limits))))

(defn- pick-provider-for-new-session [candidates]
  ;; Sort by lowest quota pressure (highest remaining headroom).
  ;; Tie-break by configured priority order.
  (->> candidates
    (sort-by (juxt quota-pressure-score :priority))
    first))
```

Key rules:
- **In-session: NEVER switch** even if quota tips past 80%. Cache
  loss outweighs quota saving for ONE session. Quota-aware routing
  applies ONLY to new sessions.
- **Hysteresis:** flip-flop avoided by requiring >10% delta between
  providers before switching the default. Otherwise stick with the
  current default.
- **User override always wins.** If user pins
  `{:routing {:provider :anthropic-coding-plan}}` explicitly, vis
  honors it even if Anthropic is at 99% quota — they'll get the
  429 and decide to retry.

### E.6 — tests

- Live: 3-turn session with `:fallback-model-in-the-same-provider`
  default, force the first model to 429 → svar fallback within
  Anthropic, turn 2+ still shows `cache-read > 0`.
- Live: simulate full-provider outage (point at bad base-url) →
  vis logs `::cache-wasted` and falls back cross-provider.
- Unit: `:llm-actual-provider` change between iters + previous
  `cache-write > 0` → telemetry stamp emitted.

---

## Phase D — title-hint enforcement

User complaint #3 from `dc606bf8`: model skipped `(set-session-title!)`
on a trivial first turn. Today the prompt mentions it as one option in
the Control section but never enforces it.

### D.1 — engine post-turn check

After `(done …)` lands but BEFORE persisting the turn as `:done`,
inspect `(:session-title-atom env)`. If blank AND `turn-position = 1`,
stamp `::missing-title?` on the env for the next turn assembler.

### D.2 — next-turn prompt nudge

When `::missing-title?` is true, the next iteration's prompt assembler
injects a FIRST-PRIORITY reminder block:

```
=== FIRST-PRIORITY ===
The session has no title. Before any other action this turn, emit:
  (set-session-title! "<6-8 word summary of the user request>")
on its own line in the code block. Title is mandatory; this is
non-negotiable.
```

Reminder clears once `(set-session-title! …)` is observed.

### D.3 — hard refusal (toggle, default off → flip later)

`:vis/require-session-title` (default `false`): when on, an empty
title at `(done …)` time refuses the turn with a structured
preflight error directing the model to emit the call. Flip to `true`
after a week of telemetry confirming the soft nudge resolves most
cases.

### D.4 — tests

- Turn 1 without `set-session-title!` ⇒ env carries
  `::missing-title? true`; turn 2 prompt assembler injects the
  FIRST-PRIORITY block.
- Turn 2 with `set-session-title!` ⇒ flag clears; subsequent turns
  carry no nudge.
- Toggle on + done without title ⇒ preflight error matches expected
  message.

---

## Rollout sequencing

Hard cut. No deprecation cycles. Each phase ships as one PR; phases
are coupled because the schema rewrite touches everyone.

1. **Phase 0** (svar bug fixes S1-S5 + always-on cache) → svar `0.6.0`,
   shipped in the SAME release as Phase A canonical usage shape.
2. **A** (svar canonical usage, breaking) → svar `0.6.0`.
3. **B** (vis: schema rewrite + persistance + loop + chat + format) →
   single vis PR that bumps `com.blockether/svar` to `0.6.0`,
   rewrites `V1__schema.sql` inline, and deletes the old
   `~/.vis/vis.mdb/` on first boot when Flyway sees the checksum
   mismatch. NO V2 migration, NO rename, NO stash.
4. **C** (cache wiring, the cost lever) → separate vis PR; depends
   on Phase 0 (svar auto-cache + `:cache-key` opt) + Phase A
   (canonical usage to read cache-write/cache-read). Always on for
   every Anthropic / OpenAI / Codex / Z.ai / OpenRouter call.
5. **E** (cache-aware routing policy) → svar `0.6.1` (new
   `:same-provider-then-cross` strategy) + vis PR (default-on
   sticky routing + cache-wasted telemetry). Ships AFTER Phase A/C
   land so the cache-wasted telemetry has canonical fields to read.
6. **D** (title-hint enforcement) → independent vis PR; soft nudge
   default on, hard refusal default off.

Each phase: failing tests first (reproducing the gap from session
`dc606bf8` or the svar bug-hunt section above), then patch, then
verify via nREPL against the live persisted session row AND against
live providers (Anthropic + Codex configured today).

---

## Verification cadence (live providers, every phase)

This session ALREADY validated the core hypotheses against live
Anthropic. Every phase carries the same protocol:

1. **Pre-patch nREPL probe** — reproduce the bug against a live
   provider. Save the result as a failing test fixture.
2. **Patch.**
3. **Post-patch nREPL probe** — same call shape against the same
   provider, assert the bug is gone. Add to test suite as
   regression.
4. **Cost smoke test** for Phase C only: 3-turn session against
   `claude-haiku-4-5`, assert turn 2+3 `cache-read > 0` and total
   cost is ≤ 30% of the same session run with cache disabled.

Providers configured today in `~/.vis/config.edn`:
- `:anthropic-coding-plan` (claude-opus/sonnet/haiku 4.x family)
- `:openai-codex` (gpt-5.x family via Responses API)
- `:github-copilot-individual` (mixed claude / gpt-5 / grok / gemini)
- `:zai-coding-plan` (glm-5.1)

Min set per phase: ANTHROPIC + CODEX. Phase C also requires
github-copilot for cross-provider cache-hit comparison.

---

## Forensics scripts (reproducible against existing DB)

Pre-rollout (current schema, legacy column names):

```clojure
;; Cache write/read pattern per session (legacy column shape)
(let [sid #uuid "dc606bf8-39ee-4555-85e6-782e31797f62"
      d   (vis/db-info)]
  (->> (vis/db-list-session-turns d sid)
    (mapcat (fn [t] (vis/db-list-session-turn-iterations d (:id t))))
    (reduce (fn [acc it]
              (-> acc
                (update :input-uncached + (:input-tokens it))
                (update :cache-read     + (:cached-tokens it))
                (update :cache-write    + (:cache-created-tokens it))))
      {:input-uncached 0 :cache-read 0 :cache-write 0})))
```

Post-Phase-B (canonical schema):

```clojure
(let [sid #uuid "dc606bf8-39ee-4555-85e6-782e31797f62"  ;; (or fresh session)
      d   (vis/db-info)]
  (->> (vis/db-list-session-turns d sid)
    (mapcat (fn [t] (vis/db-list-session-turn-iterations d (:id t))))
    (reduce (fn [acc it]
              (-> acc
                (update :input-total       + (:input-tokens it))
                (update :input-regular     + (:input-regular-tokens it))
                (update :input-cache-read  + (:input-cache-read-tokens it))
                (update :input-cache-write + (:input-cache-write-tokens it))))
      {:input-total 0 :input-regular 0 :input-cache-read 0 :input-cache-write 0})))
```

Pre-Phase-C target metric: `cache-write` ≈ `regular` per turn (each
turn re-writes), `cache-read` ≈ 0.
Post-Phase-C target: `cache-read` dominates from turn 2 onward;
cache hit ratio (`cache-read / input-total`) ≥ 80% on multi-turn
sessions with stable system prompt.

---

## Open questions

- ~~**B.2 DB blow-away UX:**~~ **CLOSED**: nuke (`rm -rf`), one
  stderr warning, no rename, no `[y/N]` prompt. JEBAC LEGACY. Vis is
  pre-1.0; user data is not precious yet and the broken token columns
  in historical rows make those sessions useless anyway.
- **C.2 CTX-block breakpoint placement:** verify the `;; ctx` EDN
  preamble lives in the user message (or system block) and that its
  bytes are STABLE within a session. Any drift between turns = cache
  miss on every turn. Audit `prompt/assemble-initial-messages` before
  tagging.
- **C.4 model autorouting:** if vis falls back from Anthropic to
  OpenAI mid-session, the cached prefix is wasted. Should the
  fallback decision factor in "we've already paid the cache-write"
  cost recovery time? Probably out of scope for this PR; defer to a
  routing-policy revision.
- **D.3 hard-refusal toggle default:** start opt-in (default off)
  and flip to opt-out after a week of telemetry, or ship opt-out
  immediately to enforce on day one?
