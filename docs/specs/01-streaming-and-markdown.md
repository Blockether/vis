# Specification — Telegram Channel: Streaming + Markdown Rendering Overhaul

Status: design-locked, pre-implementation.
Scope owner: `extensions/channels/vis-channel-telegram` + `src/com/blockether/vis/internal/markdown.clj` + `:channel/messages-renderer-fn` registry slot in core.
Companion file: this spec is the source of truth for the work; AGENTS.md S5 (English-only, runtime truth) applies throughout.

---

## 1. Problem statement

Two user-visible defects in the Telegram channel:

### 1.1 No streaming

`bot.clj` `handle-user-text!` calls `(vis/send! id text opts)` blocking, then ships the full answer in one `tg/send-message!`. For multi-iteration turns or deep-reasoning models, the user stares at a `typing…` indicator for 10–60 s with **zero progress signal**. Cancellation requires `/cancel` text command — not discoverable, not one-tap.

### 1.2 Broken Markdown → Telegram-HTML rendering

`api.clj` `convert-md-to-html` is a regex pipeline. Concrete defects observed in real LLM output:

1. **Defect #10 (catastrophic):** any unbalanced inline marker (`*lonely`, stray `` ` ``) yields invalid HTML → Telegram rejects → silent fallback to plain text → **all formatting nuked for the whole message**, including parts that were perfectly fine.
2. Lists (`- item`, `1. item`) pass through literally — Telegram has no `<ul>`/`<ol>`, so users see raw `- ` prefix prose.
3. Code-fence language tag (` ```clojure `) leaks the word `clojure` as the first line of the `<pre>` block.
4. Multi-line blockquotes (`> a\n> b`) emit two adjacent `<blockquote>` elements instead of one.
5. Nested emphasis (`**bold _ital_**`) breaks the regex; tags either miss or cross.
6. `~~strike~~`, bare URLs, `__underline__` unsupported.
7. No paragraph spacing around block elements (heading→para, table→para) — blocks visually fuse.
8. Tables → `<pre>` aligned text works, but cell-internal `|` escapes and inline formatting in cells are dropped.
9. No GFM autolink; `[](...)` only.
10. Headings collapse to `<b>` with no trailing spacing → adjacent headings glue.

The same broken regex would also be hit mid-stream during live edits, where partial markdown is *guaranteed* unbalanced.

### 1.3 Why fix together

Streaming live-edits the same outbound text path that produces the final answer. Both rely on the same renderer. Designing them as one change closes the gap once.

---

## 2. Goals / non-goals

### Goals

- Visible progress within ~1 s of sending a message; user can see the model is working.
- One-tap cancellation from the chat UI.
- Markdown that "looks nice" in Telegram for the constructs LLMs actually emit.
- Mid-stream rendering robust against partial / unbalanced input.
- Architecture extensible to other channels (TUI, future CLI agent) without copy-paste.
- Zero new third-party deps.

### Non-goals

- Per-token typewriter streaming of the assistant's *answer* prose (architecturally not available — see §3.2). We stream **reasoning** + form lifecycle, not answer deltas.
- Full CommonMark spec compliance. We render the subset our LLM actually emits.
- Streaming voice synthesis (audio is post-completion, one-shot, unchanged).
- Backporting streaming to legacy turns / DB schema changes.

---

## 3. Architectural context

### 3.1 Existing streaming surface (already in core, unused by Telegram)

`src/com/blockether/vis/internal/progress.clj` — `make-progress-tracker`. Public via `vis/make-progress-tracker`. Wraps `:on-chunk` hook; accumulates a `timeline` of per-iteration entries with `:thinking`, `:code`, `:results`, etc.; calls `(on-update timeline chunk)` on every event. TUI already consumes it. Telegram does not. **Track 1 below = wire it up.**

Phases the tracker sees (from `loop.clj`):

| Phase | Carries | Notes |
|---|---|---|
| `:reasoning` | `:thinking` text deltas, `:done?` | Real token stream from LLM |
| `:form-start` | `:form-idx`, `:code` | Discrete event |
| `:form-result` | `:form-idx`, `:result`/`:error`, `:stdout`/`:stderr`, `:execution-time-ms` | Discrete event |
| `:iteration-final` | `:final` (nil mid-turn), `:done?` | Iteration boundary |
| `:iteration-error` | `:thinking`, `:error` | Aborted iteration |

### 3.2 What is NOT streamable

Vis is a code-as-output system. The LLM writes Clojure forms; one form is `(vis/answer "…full literal string…")`. The answer string is **fully assembled in source** before any form executes. There is no per-token answer-prose stream below `svar/ask-code!` short of bypassing Vis's whole architecture.

Implication: "ChatGPT typewriter on the answer" is **not** what we're building. We stream the *thinking + tool activity* phase live; the answer arrives atomically at the end.

### 3.3 Markdown utilities today

`src/com/blockether/vis/internal/markdown.clj` (1155 LOC) is **only** a conversation→Markdown-document exporter (DB → MD doc). No parser, no renderer. Each channel rolls its own output formatting (Telegram regex; TUI styled-string).

---

## 4. Locked design decisions

Every decision below is a Q/A from the design grilling. Each lists the choice, the alternatives considered, and the rationale.

### 4.1 (Q1) Streaming UX shape — **(A) single live-edited bubble**

**Chosen:** post placeholder bubble; `editMessageText` updates it as state evolves; on 4096-char overflow, freeze and roll over per §4.2.

**Rejected:**
- (B) Append-only chunks per phase — spammy, breaks "one turn = one bubble" mental model.
- (C) Hybrid (one bubble per phase) — multiplies edits across N bubbles, hits Telegram's 1 edit/sec/chat limit and 429s fast.

**Rationale:** matches every Telegram LLM bot users have already seen (least surprise); Telegram rate-limit math only works with one bubble; existing `chunk-text` already solves the 4096 boundary.

### 4.2 (Q3) Overflow / multi-bubble shape — **(B3) sliding-window thinking, separate answer bubble(s)**

**Chosen:**

```
[💭 Thinking…
 <blockquote expandable>{last ~3500 chars of CoT, sliding window}</blockquote>
 ⏳ Running: tool-name                                  ← swapped per form-start/result
 [⊘ Cancel]                                            ← inline keyboard
]
                                              ← stream end:
[💭 <blockquote expandable>{collapsed thinking summary}</blockquote>]   ← keyboard removed

[{final answer markdown rendered to Telegram-HTML}
 _🤖 model · in/out · duration · cost_]   ← footer on last bubble (§4.6)
```

If the answer itself >4096 chars, split into N answer bubbles via existing `chunk-text`; footer on last chunk.

**Rejected:**
- (B1) Truncate visible thinking — loses signal.
- (B2) Multi-bubble thinking parts — clutter; no reader benefit (DB has full CoT).

**Rationale:** answer is the user-valuable artifact and must never be truncated by overflow rollover; thinking is a live signal not an archive (sliding window is honest); single answer bubble preserves clean copy/quote/forward UX.

### 4.3 (Q2) Phase routing — what each phase does to the bubble

| Phase | Effect on live bubble |
|---|---|
| `:reasoning` (delta) | Append to thinking blockquote (sliding window, max ~3500 chars displayed) |
| `:form-start` | Update status line to `⏳ Running: {tool/form-id}`; refresh `chat-action "typing"` (Telegram clears it after ~5 s) |
| `:form-result` | Status line back to `⏳ Thinking…` (or removed if next is `:iteration-final`) |
| `:iteration-final` mid-turn | Flush pending edit immediately; status `⏳ Iteration N+1…` |
| `:iteration-final` `:done?=true` | **Stream end.** Final-edit thinking bubble to collapsed-summary state, remove keyboard. Render answer into separate bubble(s) with footer. |
| `:iteration-error` | Final-edit bubble to error state with collapsed thinking; surface `vis/format-error` text |

### 4.4 (Q7) Throttle policy

- **Min interval between edits:** **1200 ms** (Telegram soft-limit ≈ 1 msg/sec/chat; 200 ms headroom).
- **Edit triggers:** edit when `(elapsed ≥ 1200ms)` AND `(buffered Δ ≥ 40 chars OR newline arrived)`.
- **Stream end:** flush immediately, ignore interval.
- **First paint:** post initial bubble immediately on first `:on-chunk` event so user sees the bubble + Cancel button within ~1 s of sending.
- **`message is not modified` (HTTP 400):** swallow silently.
- **HTTP 429 backoff:** read `retry_after`, double current interval for next two edits, then decay back to 1200 ms.
- **Idle:** if `:on-chunk` silent for 3 s mid-stream, do not edit (avoids spurious not-modified errors and rate-limit waste).

### 4.5 (Q9) Voice-mode interplay — **(I2) stream thinking only**

**Chosen:** in voice-output chats, the live thinking bubble streams normally. After turn completion: existing voice-flow runs unchanged (`record_voice` chat-action → synth WAV → send voice note → optional text-answer bubble gated by `:telegram-send-answer-text?`).

**Rejected:**
- (I1) Skip streaming in voice mode — leaves voice-mode users with zero progress signal during the slow part.
- (I3) Stream answer text on top of voice — redundant with existing optional text-answer bubble.

**Rationale:** streaming's primary value (liveness signal) applies regardless of output modality; voice synth is post-completion anyway, so during the slow part there is no audio to compete with.

### 4.6 (Q12) Footer placement — **(K2) on last answer bubble**

Footer (`format-footer`) renders at the end of the last answer bubble (single bubble in the common case; last chunk in the multi-chunk case). Thinking bubble's collapsed-summary state has **no** footer.

**Cancelled mid-stream exception:** thinking bubble's `⊘ Cancelled by user.` final state gets a short footer (`_⊘ Cancelled · {duration} · {model}_`) since there is no answer bubble.

**Rejected:** (K1) footer on thinking bubble (dissociated from answer); (K3) footer on both (redundant).

### 4.7 (Q10 + add-on) Cancellation — **(J2) edit-in-place + inline `⊘ Cancel` button**

**Inline keyboard on the live bubble:**

```clojure
{:inline_keyboard [[{:text "⊘ Cancel" :callback_data (str "cancel:" chat-id)}]]}
```

- Attached on initial post; re-attached on each throttled edit; removed on stream end / cancel-final-edit.
- `callback_data = "cancel:<chat-id>"` (fits Telegram's 64-byte cap; bot guarantees one in-flight turn per chat via `set-in-flight!` so no token-id needed).
- `handle-callback-query!` matches `cancel:` prefix → looks up turn-token in `chat-state` atom → fires `vis/cancellation-token` cancel.

**Cancel outcome:** thinking bubble final-edited to `⊘ Cancelled by user.` + collapsed thinking summary + cancel-flavored footer. Keyboard removed. No new message.

**Edge — pre-first-paint cancel** (turn killed in <1 s, before the bubble exists): fall back to current behavior — send fresh `Cancelled by user.` message.

**`/cancel` text command:** kept alongside the button (accessibility, scriptability, voice-mode users).

**Rejected:**
- (J1) status quo (orphan bubble + new message) — looks abandoned.
- (J3) edit + new message — noisy.

### 4.8 (Q4 + Q5 + E1 + F3) Markdown rendering — hand-rolled, in `markdown.clj`, exposed as pure fns + `:channel/messages-renderer-fn`

#### 4.8.1 Hand-roll vs commonmark-java — **hand-roll**

**Rejected (F1/F2):** `org.commonmark/commonmark` ~200 KB jar.

**Rationale:**
- LLMs emit a *known subset*. CommonMark's hard parts (setext headings, ref-link definitions, HTML blocks, lazy continuations, intra-word emphasis edge cases) are 60% of the spec and we don't need them.
- Hand-roll runs in **SCI sandbox** too (commonmark-java is host-Java-only), enabling future extension code to render markdown.
- Defect #10 (unbalanced) is *easier* hand-rolled — inline tokenizer emits literal text on unmatched delimiters; parse never fails.
- ~350 LOC owned > 200 KB of someone else's edge-case opinions.
- AGENTS.md S2 ethos: boundaries observable, no surprise transitive deps.

**Estimated effort:** ~1 day implementation + ~1 day tests. Not free, owned forever.

#### 4.8.2 Code location — **(E1) inside existing `markdown.clj`**

**Rejected:**
- (E2) sibling `markdown_render.clj` — cleaner concern split, but two files for "all markdown stuff".
- (E3) sub-namespace tree — most refactor churn.

**Rationale:** user preference for one-stop discovery; `markdown.clj`'s docstring will be widened from "conversation → MD exporter" to "Vis markdown utilities (export + render)". File grows from 1155 → ~1500 LOC; acceptable.

#### 4.8.3 API shape — pure fns, no dynamic var

```clojure
(ns com.blockether.vis.internal.markdown
  ...)

;; existing public:
(defn conversation->markdown ...)        ;; unchanged

;; new public:
(defn parse [text] ...)                  ;; text → AST (plain Clojure maps)
(defn render-plain [ast opts] ...)       ;; AST → plain text  (default fallback)
(defn render-html  [ast opts] ...)       ;; AST → Telegram-HTML
(defn render-markdown-v2 [ast opts] ...) ;; AST → Telegram MarkdownV2  (future, stub OK)

(defn render
  "Convenience: parse + render with named flavor.
   flavor ∈ #{:plain :html :markdown-v2}.
   opts:
     :partial?   bool   - mid-stream lenient parse
     :context    #{:thinking :answer :status :error}
     :max-length int    - hard cap, render truncates with ellipsis"
  ([text flavor]      (render text flavor nil))
  ([text flavor opts] ...))
```

Exposed via `core.clj`:
```clojure
(def md-parse    markdown/parse)
(def md-render   markdown/render)
;; specific renderers also re-exported for direct use
```

**Rejected dynamic var `vis/*markdown-renderer*`:** spooky action at distance, awkward `binding` ceremony, SCI compat concerns, harder to test. Pure fn dispatch is honest.

#### 4.8.4 Channel registration — `:channel/messages-renderer-fn`

New optional key in the extension registration map (`vis/register-extension!`):

```clojure
{:ext/channels
 [{:channel/id      :telegram
   ...
   :channel/messages-renderer-fn  #'render-for-telegram}]}

;; in vis-channel-telegram:
(defn render-for-telegram
  [text opts]
  (md/render text :html opts))
```

**Contract:** `(fn [text opts] → renderer-output)` where `renderer-output` type is **channel-defined** (Telegram: `String`; TUI: `String` today, possibly styled-segment vector later). Bot/screen code never calls renderer manually — see §4.8.5.

#### 4.8.5 (H1) Single chokepoint per channel

**Telegram:** `tg/send-message!` looks up the registered `:channel/messages-renderer-fn` and calls it **internally**. No caller passes pre-rendered text. Replaces the existing `convert-md-to-html` call site.

**TUI (follow-up commit, §6):** screen-emit boundary does the same. Initially registers `(fn [text opts] (md/render text :plain opts))`.

**Rejected (H2):** explicit caller-side render — footgun (forgotten render → raw markdown leaks to Telegram → parse error → defect #10 cascade).

#### 4.8.6 (Q13) Renderer mapping table for `:html` flavor

Telegram-HTML allowlist: `<b>`, `<i>`, `<u>`, `<s>`, `<code>`, `<pre>`, `<a href>`, `<blockquote>`, `<blockquote expandable>`, `<tg-spoiler>`, `<pre><code class="language-...">`. Anything else → parse error.

| Construct | HTML output |
|---|---|
| `# H1` … `###### H6` | `<b>{escaped text}</b>\n\n` (no level distinction; always one trailing blank line) |
| `**bold**` | `<b>...</b>` |
| `*ital*` / `_ital_` | `<i>...</i>` |
| `~~strike~~` | `<s>...</s>` |
| `__underline__` | `<u>...</u>` |
| `` `code` `` | `<code>...</code>` |
| ` ```lang\n...\n``` ` | `<pre><code class="language-{lang}">...</code></pre>`; if no lang → `<pre>...</pre>` |
| `[txt](url)` | `<a href="{url}">{escaped txt}</a>` |
| Bare URL `https://...` | `<a href="...">{url}</a>` (autolink) |
| `- item` / `* item` | `• {item}\n`; nested = two-space indent + `• ` per level |
| `1. item` | `1. {item}\n` (preserve numbering as text) |
| `> quote` (multi-line `>` run) | one `<blockquote>{joined inline}</blockquote>` per *contiguous* `>` run |
| `---` / `***` / `___` line | `─────────` (em-dashes; Telegram has no `<hr>`) |
| GFM table | `<pre>{aligned-text-table}</pre>` (preserves current behavior) |
| HTML in source | `&<>"` escaped always; never trust LLM raw HTML |
| Unmatched `**` / `*` / `` ` `` / `~~` (defect #10) | emit literal char as text, never half-tag |

**Special — `:context :thinking`:** entire output wrapped in `<blockquote expandable>...</blockquote>` automatically. Otherwise blockquotes only from explicit `>` syntax.

**Special — `:context :status`:** rendered as `<i>...</i>` single-line, no block elements emitted.

**Special — `:partial? true` (mid-stream):**
- Drop trailing unclosed code-fence (do not emit half `<pre>`).
- Drop trailing unclosed inline run (`**`, `*`, `` ` ``, `~~`) — emit text up to last safe boundary.
- Always close every tag opened in the output (invariant: result is well-formed HTML).

**Special — `:max-length`:** if rendered length exceeds, truncate at last paragraph boundary ≤ limit-32, append `…`. Caller (Telegram chunker) typically does this at a higher level; renderer just exposes the option.

---

## 5. Implementation plan

Two tracks, can ship in either order; designed independent.

### 5.1 Track 1 — Markdown rendering (no behavior change, refactor + new renderer)

1. **`src/com/blockether/vis/internal/markdown.clj`**: add block tokenizer (~120 LOC), inline tokenizer (~150 LOC), AST node defs (~30 LOC), `render-plain`, `render-html`, `render-markdown-v2` (stub), `render` dispatch. Update ns docstring to reflect widened scope.
2. **`test/com/blockether/vis/internal/markdown_test.clj`**: extend with ~80 cases — every construct + adversarial inputs (lonely `*`, nested emphasis, code-fence-no-language, table-no-separator, multiline quote, unbalanced-mid-stream).
3. **`src/com/blockether/vis/core.clj`**: re-export `md-parse`, `md-render`, individual `md-render-plain` / `md-render-html`.
4. **`src/com/blockether/vis/internal/extension.clj`**: add `:channel/messages-renderer-fn` to channel registration spec; update validator.
5. **`extensions/channels/vis-channel-telegram/src/.../bot.clj`**: register `:channel/messages-renderer-fn #'render-for-telegram`.
6. **`extensions/channels/vis-channel-telegram/src/.../api.clj`**: `send-message!` looks up renderer-fn from channel registry; replaces `convert-md-to-html` call. Remove `convert-md-to-html` + helpers; keep `chunk-text`, `escape-markdown-v2` (still used elsewhere).
7. Test: existing telegram tests must still pass; new tests for renderer dispatch through registry.

### 5.2 Track 2 — Streaming + Cancel button (new behavior)

1. **`extensions/channels/vis-channel-telegram/src/.../bot.clj`**: introduce `live-bubble!` helper that owns one `chat_id` + `message_id` + render state + throttle. Internally calls `tg/edit-message!` (new wrapper in api.clj) with throttle policy from §4.4.
2. **`extensions/channels/vis-channel-telegram/src/.../api.clj`**: add `edit-message!` (HTTP `editMessageText`), `delete-message!` (for edge cleanup if needed).
3. **`bot.clj` `handle-user-text!`**: replace blocking `vis/send!` flow with:
   - Build `progress-tracker` whose `:on-update` drives the live-bubble's render.
   - Pass tracker's `:on-chunk` via `:hooks {:on-chunk ...}` to `vis/send!` opts.
   - On stream end / `:iteration-final :done?=true`: final-edit thinking bubble (collapsed), remove keyboard, render answer into new bubble(s) per §4.2.
4. **`bot.clj` `handle-callback-query!`**: handle `cancel:<chat-id>` callback → fire cancellation on stored turn-token.
5. **Voice path** (`handle-user-text!` voice-output branch): stream thinking bubble (§4.5); leave audio path unchanged after stream end.
6. **Tests**: new namespace `bot_streaming_test.clj` covering: throttle obeys 1200 ms; final flush ignores interval; `not_modified` swallowed; 429 backoff doubles interval; cancel button removes keyboard + edits in place; pre-first-paint cancel falls back to fresh message; sliding-window thinking truncation; multi-iteration boundary flush.

### 5.3 TUI follow-up (deferred, separate PR)

`vis-channel-tui`: register `:channel/messages-renderer-fn (fn [t o] (md/render t :plain o))`; route screen-emit boundary through it. Later: introduce `:tui-styled` flavor (renderer returns styled-segment vector) without touching markdown.clj's contract. Out of scope for this PR.

---

## 6. Risks & mitigations

| Risk | Mitigation |
|---|---|
| Hand-rolled parser misses an LLM idiom we hadn't catalogued | Renderer's `:partial?` / unbalanced-tolerance paths fall back to literal text — never produces invalid HTML. Add test cases as we observe. |
| Telegram changes rate-limits | Throttle constants centralized; 429 backoff already in spec. |
| `progress-tracker` semantics drift in core | TUI uses same tracker; any change shakes both channels' tests. Cross-channel safety net. |
| Inline button spammed | Telegram dedupes callback-query rapid taps; bot's `cancellation-token` is idempotent. |
| Renderer-fn registry slot conflicts with future channel design | Keep slot optional; absent → channel handles its own rendering (back-compat for any third-party channel). |
| Markdown rendering breaks existing tests for `convert-md-to-html` | Deletion is intentional; replace tests with renderer-equivalent tests asserting same outputs for the in-spec subset. |

---

## 7. Out of scope (explicitly)

- Per-token answer-prose streaming (§3.2 — architectural).
- Streaming voice synth.
- Telegram MarkdownV2 parse-mode (renderer stubbed, parse_mode flip is a one-line change later).
- Reasoning-as-permanent-transcript (CoT remains live signal; full transcript stays in Vis DB unchanged).
- TUI renderer wiring (deferred to follow-up PR).
- DB schema changes (none).
- Configuration UX for throttle constants (constants live in `bot.clj`; tunable later if needed).

---

## 8. Open items

None at spec time. All Q1–Q13 resolved in §4. Implementation proceeds.
