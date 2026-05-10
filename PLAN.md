# Plan — Answer IR + Streaming + Channel Rendering

Implementation plan companion to `docs/specs/01-streaming-and-markdown.md`.
Spec captures design rationale and alternatives; this file tracks the
concrete work and is the source of truth for what ships.

Status: pre-implementation, design-locked.

---

## 1. What ships

A single canonical Hiccup-EDN IR for `(vis/answer ...)`, three pure-Clojure
walkers (`html` / `markdown` / `plain`), a per-channel renderer registry slot,
streaming + one-tap cancel for Telegram, and a `vis run --code` flag that
extracts code from any answer.

Net change:
- **Removes** ~1500 LOC of `v/` markdown DSL + regex MD→HTML pipeline +
  `needs-input` prompt-flow gate.
- **Adds** ~400 LOC: IR types + normalization + three walkers + registry slot
  + Telegram streaming/cancel.
- **Removes** zero deps.
- **Adds** zero deps. (Pure Clojure walkers; no commonmark.)

---

## 2. Locked design

### 2.1 IR — 21 tags, pure HTML/MDAST subset

```
ROOT             :ir
BLOCKS    (11)   :p
                 :h          {:level 1-6}
                 :code       {:lang String?}     ; one string child
                 :ul
                 :ol         {:start Int?}
                 :li
                 :quote
                 :table
                 :tr
                 :th
                 :td
INLINES   (9)    :strong
                 :em
                 :c          inline code
                 :a          {:href String}
                 :img        {:src String :alt String?}
                 :kbd
                 :mark
                 :sup
                 :sub
```

Bare strings are text nodes. Attrs map is optional in the source the LLM
emits; normalization (§2.2) inserts `{}` so renderers always see attrs at
position 1.

`:li` content rule: all-blocks OR all-inlines, not mixed. Loose inlines
get wrapped in `[:p ...]` at normalization.

No `:vis/*` namespace. No domain extension slot in v1. No `:doc`/`:answer`
roots — only `:ir`.

### 2.2 `(answer ...)` contract — soft normalization

Soft-coerce, never throw. The LLM's prompt instructs it to always emit
`[:ir & nodes]`; coercion handles legacy data and forgetful models silently.

| LLM passes | Becomes after `answer-value->ast` |
|---|---|
| `[:ir ...]` | used as-is (after attrs normalization, `:li`-content discriminator) |
| `[:tag ...]` (Hiccup, non-`:ir` root) | `[:ir <node>]` |
| `"text"` (bare string) | `[:ir "text"]` |
| Anything else | `[:ir [:code {:lang "edn"} (pr-str x)]]` (debug surface) |

The auto-wrap behavior is **not advertised in the prompt**. Prompt always
shows the canonical `[:ir & nodes]` form so the LLM emits it consistently;
the wrapping is a runtime safety net.

### 2.3 LLM prompt — replaces the 660-char `v/` catalogue

```
Answers use IR (Hiccup-EDN). Always emit (answer [:ir & nodes]).

Block tags: :p :h{:level 1-6} :code{:lang} :ul :ol{:start} :li :quote
            :table :tr :th :td
Inline tags: :strong :em :c (inline code) :a{:href} :img{:src :alt}
             :kbd :mark :sup :sub

Bare strings are text nodes. Attrs map is optional. :li content is
either all blocks or all inlines (not mixed).

For pure-code answers: [:ir [:code {:lang "clojure"} "..."]].

Examples:

(answer
 [:ir
  [:p "Recurses on the rest until the empty case."]
  [:p "Complexity: " [:c "O(n)"] "."]])

(answer
 [:ir
  [:code {:lang "clojure"}
   "(defn reverse-list [xs] (reduce conj '() xs))"]])

(answer
 [:ir
  [:h {:level 2} "Comparison"]
  [:table
   [:tr [:th "Approach"]   [:th "Big-O"]]
   [:tr [:td "reduce"]     [:td [:c "O(n)"]]]
   [:tr [:td "loop+recur"] [:td [:c "O(n)"]]]]
  [:p "Both are linear; " [:strong "reduce"] " is more idiomatic."]])
```

Total ~800 chars. Net vs current 660-char `v/` catalogue: +140 chars,
gains: schema-correct examples, no DSL ceremony, models work in their
trained lingua franca (Hiccup).

### 2.4 Channel renderer registry

New optional key in `vis/register-extension!` channel registration:

```clojure
{:ext/channels
 [{:channel/id      :telegram
   :channel/main-fn #'channel-main
   :channel/messages-renderer-fn  #'render-for-telegram   ;; ← NEW
   ...}]}

(defn render-for-telegram [ir-or-string opts]
  (md/render ir-or-string :html opts))
```

Contract: `(fn [AnswerInput RenderOpts] -> RenderOutput)`. `AnswerInput`
is whatever the DB returned (`[:ir ...]`, legacy string, etc.); the
renderer normalizes via `md/->ast`. Output type channel-defined
(Telegram: String; TUI: String today).

Single chokepoint per channel. `tg/send-message!` looks up + calls the
fn; bot code never calls renderer manually.

### 2.5 `vis run --code` flag

Default `vis run "..."` renders to `:markdown` flavor and prints.

`vis run "..." --code` extracts and concatenates `[:code]` block contents
in source order, no fences. Multi-`[:code]` answer joins with `\n\n`.
No `[:code]` in answer → exits 1 with:

```
Error: --code expects answer to contain at least one [:code] block;
got prose only. Run without --code for rendered output.
```

`--code` is mutually exclusive with `--json` / `--edn` (existing flags
not removed; `--code` joins them as a fourth output mode).

### 2.6 Streaming UX (Telegram)

#### 2.6.1 Streaming transport — `sendMessageDraft` with edit-based fallback

Primary: **`sendMessageDraft`** (Bot API 9.3+, opened to all bots in 9.5,
March 2026). Purpose-built for streaming partial messages while the model
generates. Properties:

- **No notification** fires during streaming (draft is not a real message).
- **No `(edited)` tag** on the final message (draft converts to real
  message via final `sendMessage`).
- **Higher update frequency** allowed than `editMessageText`'s ~1 msg/sec
  ceiling.
- UX feel: "someone typing in real-time" rather than "message updated".

Fallback: legacy **`sendMessage` + `editMessageText` loop** for older
self-hosted Bot API servers that don't have 9.3+. Same throttle (§2.6.3).

**Capability detection** at bot startup:

1. On `channel-main` startup, attempt `sendMessageDraft` once with a probe
   (e.g., draft a placeholder to the bot's own admin chat or use a no-op
   target if the API allows; otherwise lazy-detect on first user turn).
2. On success → store `:streaming-mode :draft` in `chat-state`.
3. On `400 Bad Request: method not found` or `TEXTDRAFT_PEER_INVALID` →
   store `:streaming-mode :edit`.
4. Per-turn: read flag, dispatch to `live-bubble!` variant.

**No periodic re-probing** — Bot API version doesn't downgrade. Once
detected, stay.

If the user is on Telegram cloud (`api.telegram.org`), they're always on
the latest Bot API → draft path always succeeds. Self-hosted users on
<9.3 stay on legacy path automatically. Zero config knob.

#### 2.6.2 Live-bubble shape (transport-independent)

One live-edited bubble during turn:

```
[💭 Thinking…
 <blockquote expandable>{last ~3500 chars of CoT, sliding window}</blockquote>
 ⏳ Running: tool-name
 [⊘ Cancel]                                 ← inline keyboard
]
```

Phase routing of `:on-chunk`:
- `:reasoning` → append to thinking blockquote, sliding window
- `:form-start` / `:form-result` → status line swap
- `:iteration-final` mid-turn → flush + status update
- `:iteration-final :done?=true` → collapse thinking, render answer-IR,
  drop keyboard

#### 2.6.3 Throttle

Identical for both transports (`sendMessageDraft` is more permissive
but we keep the budget conservative to avoid surprise 429s):

Throttle: 1200 ms min interval; ≥40 chars Δ OR newline; idle 3 s resets
clock; first chunk after idle fires immediately; HTTP 400 not-modified
swallowed; HTTP 429 doubles next two intervals then decays.

Cancel button: `callback_data = "cancel:<chat-id>"`; on tap →
cancellation-token fires; thinking bubble final-edits to `⊘ Cancelled by
user.` + collapsed thinking + short footer; keyboard dropped.
Spam-tap during cancel-in-flight: `answerCallbackQuery "Already cancelling…"`.
Pre-first-paint cancel falls back to fresh `Cancelled.` message.

### 2.7 Persistence

Zero schema migration. `iteration.blocks` Nippy-encoded; vectors+keywords+
maps+strings roundtrip natively. Legacy strings + needs-input maps stay
readable via normalization in `db-turn-answer-ast`.

---

## 3. Phases

Phases 0–3 ship as **one atomic PR** to avoid the cosmetic-regression
window in Telegram. Phase 4 (streaming) and Phase 5 (voice) are separate
PRs that depend on Phases 0–3.

### Phase A — atomic foundation PR (Phases 0+1+2+3 of spec)

Single PR. Scope:

#### A.1 Demolish

- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/markdown.clj`
  — remove all `v/` helpers (`h h1 h2 h3 h4 h5 h6 p ul ol checklist
  blockquote code-block code bold italic i kbd strike underline link
  image file-link anchor hr br details table join lines section escape`).
  Keep ns; ns-doc rewrites to "this namespace is now empty; markdown
  surface lives in `vis.internal.markdown`."
- `src/com/blockether/vis/internal/markdown.clj` — strip everything
  except `conversation->markdown` and its private helpers. File goes
  ~1155 → ~400 LOC.
- `extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/api.clj`
  — remove `convert-md-to-html`, `markdown-table-*` helpers,
  `escape-markdown-v2`, `markdown-quote`, `html-inline`. Keep
  `escape-html`, `chunk-text`, HTTP wrappers, voice/file fns.
- `src/com/blockether/vis/internal/loop.clj`
  — remove `needs-input-answer?`, `answer-value-string`. Replace with
  `answer-value->ast` (§2.2). Update every call site of
  `answer-value-string` and `answer-str` to use the new shim.
- `src/com/blockether/vis/internal/prompt.clj`
  — remove the `v/` catalogue section; replace with the prompt in §2.3.
- `extensions/common/vis-foundation/resources/META-INF/vis-extension/vis.edn`
  — remove SCI exports for the deleted helpers.
- All call-sites of `vis.internal.markdown` / `foundation.markdown`
  (17 files per `rg`) — convert to plain markdown strings inline OR
  remove if the call only existed to construct prompt verbiage.

#### A.2 Build the IR core

New code in `src/com/blockether/vis/internal/markdown.clj`:

```clojure
(ns com.blockether.vis.internal.markdown
  "Vis answer IR — Hiccup-EDN with 21-tag MDAST-equivalent shape.

   Public surface:
     (parse text)           ; legacy string → [:ir text]  (no commonmark; verbatim)
     (->ast input)          ; soft-normalize any input → [:ir & nodes]
     (render input flavor opts)  ; one of :html :markdown :plain
     (extract-code ast)     ; for vis run --code
     (extract-text ast)     ; for voice TTS
     conversation->markdown ; existing exporter, unchanged

   Internal:
     normalize-attrs        ; insert {} where attrs missing
     normalize-li-content   ; wrap loose inlines in :p
     ir-walker-html         ; ~120 LOC, plain case dispatch
     ir-walker-markdown     ; ~120 LOC
     ir-walker-plain        ; ~80 LOC")
```

No commonmark dep. `(parse text)` is a one-liner that wraps a string as
`[:ir text]` for back-compat with persisted string answers. The IR is the
canonical shape; nothing parses markdown into IR (LLM emits IR directly).

Walker dispatch: plain `case` on `(first node)` per target. One function
per flavor. ~400 LOC total across the three walkers.

`core.clj` re-exports as `vis/md-render`, `vis/md->ast`,
`vis/md-extract-code`. SCI sandbox bindings under `md/`.

#### A.3 Channel registry slot

`src/com/blockether/vis/internal/extension.clj`:
- Add `:channel/messages-renderer-fn` to channel registration spec.
- Optional. Validator update.
- Lazy resolution at emit time (var deref) so dev-time redefs are picked
  up.

#### A.4 Telegram + TUI chokepoints + answer-AST + Telegram attachments

- `vis-channel-telegram/.../bot.clj` registers
  `:channel/messages-renderer-fn #'render-for-telegram`.
- `tg/send-message!` looks up the registered fn, calls it, ships result
  with `parse_mode HTML`.
- `vis-channel-tui/.../core.clj` registers
  `:channel/messages-renderer-fn #'render-for-tui` where
  `(render-for-tui [input opts] (md/render input :markdown opts))`.
- `vis-channel-tui/.../screen.clj` screen-emit boundary calls the
  registered fn instead of any inline rendering. Identifies the boundary
  by reading where messages currently get appended to the transcript pane.
- `bot.clj/handle-user-text!` reads
  `(db-turn-answer-ast (:turn-id result))`, passes to renderer; renderer
  output is the HTML string for the bubble.
- `[:img]` blocks: renderer emits inline placeholder `<i>🖼 alt</i>`
  text; bot's post-render walk detects `:img` blocks in the AST and
  schedules `tg/send-photo!` with `reply_to_message_id` = text-bubble
  message id. Source-order traversal.
- New `tg/send-photo!` in `api.clj` (multipart, similar to existing
  `send-voice!`).

#### A.5 CLI `--code` flag

`src/com/blockether/vis/internal/main.clj` `cli-run!` and the `run`
spec's `:cmd/args`:
- Add `{:name "code" :kind :flag :type :boolean :doc "Print only [:code] block contents."}`.
- After result: if `--code`, call `(md/extract-code (:answer-ast result))`
  → concat with `\n\n` → stdout. If empty, exit 1 with the error
  message in §2.5.
- If neither `--code` nor `--json` nor `--edn`, default render is
  `(md/render answer-ast :markdown)` → stdout.

#### A.6 Tests (per AGENTS.md S3, every touched ns gets coverage)

Tests in matching `test/` paths:

- `test/com/blockether/vis/internal/markdown_test.clj` (replaces existing)
  - 21-tag normalization roundtrip
  - soft-coerce: each entry in §2.2 table maps to expected canonical AST
  - `:li` content discriminator: loose inlines wrapped in `:p`
  - walker output for each flavor, every tag (skeletal smoke + one
    realistic mixed answer per flavor)
  - `extract-code` returns concatenated `[:code]` content in source order
  - adversarial: malformed Hiccup, unknown tag, missing required attr,
    empty `[:ir]`
- `test/com/blockether/vis/internal/loop_test.clj` (extend existing)
  - `answer-value->ast` covers every entry in §2.2 table
- `extensions/channels/vis-channel-telegram/test/.../api_test.clj`
  - Removed: tests for `convert-md-to-html`, `markdown-table-*`,
    `escape-markdown-v2`. Replaced with HTML-output assertions that go
    through the registered renderer.
- `extensions/channels/vis-channel-telegram/test/.../bot_test.clj`
  - Renderer dispatch via registry
  - `:img` block triggers `send-photo!` after text bubble
  - Multi-`:img` answers ship images in source order
- `extensions/common/vis-foundation/test/.../markdown_test.clj`
  - Deleted (the helpers are deleted).
- `extensions/common/vis-foundation/test/.../core_test.clj`
  - Drop tests for removed `v/` exports.

#### A.7 Verify gate

`./verify.sh` (full, not `--quick`) must pass before the PR merges:
- All Clojure tests green
- LLM smoke test: send a prompt, receive a `[:ir ...]` answer, verify
  Telegram-HTML rendering doesn't fail parse_mode

---

### Phase B — streaming + cancel button (own PR, depends on A)

Scope:

- `tg/send-message-draft!`, `tg/edit-message-draft!`,
  `tg/finalize-message-draft!`, `tg/edit-message!`, `tg/delete-message!`
  in `vis-channel-telegram/.../api.clj`.
- `bot.clj/detect-streaming-mode!` — capability probe at startup
  (§2.6.1); caches `:draft` or `:edit` per channel instance.
- `bot.clj/live-bubble!` helper — abstracts over transport. Owns
  `chat_id` + `message_id` + render state + throttle (§2.6.3). Two
  internal implementations behind one interface:
  - `live-bubble-draft!` — uses `sendMessageDraft` /
    `editMessageDraft` / final `sendMessage`. Bot API ≥ 9.3.
  - `live-bubble-edit!` — legacy `sendMessage` + `editMessageText`
    loop. Fallback for older self-hosted Bot API.
- `bot.clj/handle-user-text!` rewrite:
  - Read detected `:streaming-mode` flag; pick `live-bubble-draft!`
    or `live-bubble-edit!`.
  - Build `vis/make-progress-tracker` whose `:on-update` drives
    the picked live-bubble.
  - Pass tracker's `:on-chunk` via `:hooks {:on-chunk ...}` to
    `vis/send!` opts.
  - On `:iteration-final :done?=true`: collapse thinking bubble,
    drop keyboard, render answer-AST through registered renderer-fn,
    finalize bubble (transport-specific: draft → real `sendMessage`;
    edit → final `editMessageText`), post additional answer bubbles
    if multi-chunk, walk attachments per A.4.
- `bot.clj/handle-callback-query!` matches `cancel:<chat-id>` → fires
  cancellation on stored turn-token; updates bubble (transport-specific
  cancel-edit); spam-tap protection.
- Per-turn fallback: if `:draft` mode and a draft call returns an
  unexpected error (e.g., capability silently revoked), drop to `:edit`
  for the rest of the turn. Sticky downgrade.

Tests:
- `bot_streaming_test.clj`:
  - capability detection: probe success → `:draft`; probe 400 →
    `:edit`; cached across calls.
  - both transports: throttle 1200 ms; final flush ignores interval;
    not-modified swallowed; 429 backoff doubles interval; cancel
    button removes keyboard + edits in place; pre-first-paint cancel
    → fresh message; sliding-window thinking truncation;
    multi-iteration boundary flush.
  - per-turn fallback: simulated draft error mid-stream →
    transparent switch to edit transport for remaining chunks of
    that turn.
  - draft-specific: verify final message has no `(edited)` tag
    (assertion on `Message.edit_date` being nil).

---

### Phase C — voice integration (own PR, depends on B)

Scope:
- Voice path streams thinking bubble (Phase B) normally.
- After turn completion: extract `(md/extract-text answer-ast)` →
  synthesize → send voice note. Existing audio flow downstream
  unchanged.
- `:img` attachments in voice mode: still ship per A.4 attachment walk.
- `:code` / `:table` blocks: voice TTS skips them with a "see chat for
  code/table" announcement marker (or just drops them — UX call during
  implementation).

Tests:
- `bot_voice_test.clj`: voice + streaming + IR mixed answer ships
  thinking bubble during turn, voice note + optional text bubble after,
  attachments shipped.

---

### Phase D removed — TUI chokepoint folded into Phase A

TUI registry wiring is no longer a separate phase. It ships in Phase A:
- TUI extension (`vis-channel-tui`) registers
  `:channel/messages-renderer-fn (fn [input opts] (md/render input :markdown opts))`
  in `vis-channel-tui/.../core.clj`.
- TUI's screen-emit boundary in
  `vis-channel-tui/.../screen.clj` calls the registered fn instead of
  inline rendering. (TUIs in modern terminals render markdown reasonably
  via `:markdown` flavor; explicit `:tui-styled` styled-segment renderer
  remains future work.)
- Falls under Phase A's atomic-PR umbrella so the chokepoint contract is
  exercised by both consumers from day one.

---

## 4. Risks

| Risk | Mitigation |
|---|---|
| LLM regresses without `v/` DSL guidance | Few-shot examples in prompt (§2.3) ground it; auto-wrap soft-coerces forgetful output. |
| `sendMessageDraft` capability changes between Bot API versions | Detect at startup (§2.6.1); cache result; per-turn sticky downgrade on unexpected error. Self-hosted users on <9.3 transparently use legacy edit path. |
| Draft path silently dropped messages on edge cases (cf. OpenClaw #19001) | Per-turn fallback to edit path on first draft-call error; final-edit always re-confirms terminal state. |
| LLM emits malformed IR (unknown tag, missing required attr) | Walker `:default` arm renders children, drops wrapper. `->ast` normalizes attrs. Renderer never throws. |
| Soft-coerce hides bugs (LLM keeps emitting strings, we silently wrap) | Ship a debug counter in `tel/log!` for "auto-wrapped non-IR answer" so we can detect prompt drift. |
| Telegram `<blockquote expandable>` requires Bot API ≥ 7.0 | Documented requirement. Older Bot API silently shows non-expandable blockquote — degraded but functional. |
| `:img` with local-path src ships gigabyte file | Renderer/sender enforces 20 MB Telegram photo cap; oversize → degrade to file path text in placeholder. |
| Multi-iteration `(answer)` attachments | Only terminal-iteration's attachments ship. Mid-turn `(answer)` calls' images dropped. Spec §8.6. |
| Walker output edge cases (escape rules, nested lists, table alignment) | Per-flavor walker test coverage in A.6 catches them; ~80 cases. |
| Persistence size growth | Nippy compact; KB-scale. |
| LLM emits structural table (`:table`/`:tr`/`:td`) but with cell strings containing markdown | Cell rule pinned: `Cell = String \| Inline-node`. String cells rendered verbatim text; if LLM wants emphasis, it nests `[:strong ...]`. Tested explicitly. |
| `vis run --code` extracts wrong block when answer has multiple `:code` blocks | Concatenate in source order; if user wants only one, prompt LLM more precisely. Documented behavior. |

---

## 5. Out of scope

- Per-token answer-prose streaming (architectural).
- Streaming voice synth.
- Telegram MarkdownV2 parse-mode.
- `:vis/*` namespace / domain extensions.
- `needs-input` semantic (removed).
- Hand-rolled or library-based markdown parser (LLM emits IR; no parsing).
- DB schema changes (zero — Nippy roundtrips).
- Image generation pipelines.
- Layout primitives.
- Grammar-constrained LLM decoding.
- TUI styled-segment renderer (deferred to follow-up PR after D).

---

## 6. Sequencing & merge order

1. **PR A** (this plan §3 Phase A). Foundation — includes both Telegram
   AND TUI chokepoints wired through `:channel/messages-renderer-fn`.
   Atomic.
2. **PR B** depends on A merged. Streaming + cancel (Telegram only).
3. **PR C** depends on B merged. Voice.
4. _(was Phase D)_ TUI registry wiring is in PR A; no separate PR.

---

## 7. Open items

None at plan time. Implementation can begin Phase A immediately.

## 8. References

Industry research informing this plan:

- Bot API changelog — `expandable_blockquote` (2024), `sendMessageDraft`
  (Bot API 9.3 Dec 2025, opened to all bots in 9.5 March 2026). Source:
  https://core.telegram.org/bots/api-changelog
- OpenClaw / Clawdbot streaming migration to `sendMessageDraft`
  (issues #32041, #32180): no notification, no `(edited)` tag, faster
  update frequency.
- anything-llm production rate-limit data (issue #5447): edit-based
  streaming hit 429s, settled on 1000-1500ms intervals. Validates our
  1200 ms throttle ceiling.
- Industry consensus on content blocks (Anthropic, OpenAI Responses
  API, LangChain 1.0 `content_blocks`): typed structured blocks for
  mixed prose + non-text. Vis IR matches this shape via Hiccup-EDN.
- `telegramify-markdown` (sudoskys, Python): MessageEntity-based
  rendering as defect-#10-proof alternative to parse_mode. Noted as
  potential future evolution (out of scope for this PR).
