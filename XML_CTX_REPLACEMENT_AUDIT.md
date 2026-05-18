# XML → `ctx` Replacement Audit

## Goal

Find every remaining XML-ish prompt-control surface, explain why it exists, what is wrong with it, and how to replace it with Clojure/EDN `ctx` data.

`ctx` is real model-visible sandbox data. Host binds it before each model call:

```clojure
(env/bind-and-bump! environment 'ctx current-ctx-map)
```

Model can read:

```clojure
(:conversation ctx)
(:iteration ctx)
(:hints ctx)
(:extensions ctx)
(:defs ctx)
```

`ctx` is host-owned and hidden from `(:defs ctx)`.

## Principle

Use XML/HTML only for final human rendering formats where needed. Do **not** use XML-ish tags as prompt-control protocol.

Prompt-control data should be:

- EDN in `ctx` for dynamic runtime state.
- Comment-section text (`;; -- NAME --`) for static prompt prose.
- Structured DB/metadata for transcript classification, not tag sniffing.

## Current state

Hints already moved from XML:

```xml
<iteration_hints>
<iteration_hint importance="high">...</iteration_hint>
</iteration_hints>
```

into EDN:

```clojure
(:hints ctx)
;; => [{:id :vis.foundation/conversation-title
;;      :importance :high
;;      :text "..."
;;      :satisfy-with '(satisfy-hint! :vis.foundation/conversation-title)}]
```

Remaining XML-ish places below.

## Audit table

| Area | Files | Current XML-ish thing | Model-facing? | Why it likely existed | Problem | Better shape |
|---|---|---:|---:|---|---|---|
| Current user wrapper | `src/com/blockether/vis/internal/prompt.clj`, transcript tests | old `<current_user_message>` fixtures / labels | yes, via stored older prompts and maybe history | visually section current request; transcript classifier can label message | duplicates user role + `ctx`; teaches tag protocol; model may emit tags | user role carries request; `ctx[:conversation :user-request]`; transcript labels by message position/metadata |
| Current turn context | `extensions/common/vis-foundation/.../transcript.clj`, transcript tests | `<current_turn_context>` fixture/detection | yes in stored older prompt envelopes | identify per-iteration trailer; pack turn/iteration metadata | duplicates `ctx`; transcript parser sniffs text; XML+Clojure mixing | trailer starts with `;; ctx =`; transcript labels by role+position or persisted `message-kind` |
| Provider error nudges | `src/com/blockether/vis/internal/loop.clj` | `<error>...</error>` | yes, sent after provider failure / incomplete output | force salience; clear non-code error block | XML in Clojure prompt; model may copy tags; no structured fields | `;; ! provider-error ...` comment or `ctx[:last-error]` EDN |
| Extension prompt wrapper docs | `src/com/blockether/vis/internal/prompt.clj`, `extensions/common/vis-foundation/src/.../core.clj` | docstrings say `<extensions>`, `<turn_system_context>`, `<system_prompt>` | mostly docs stale; output is now `;; -- ... --` | old prompt-block tags | confusing docs; future code may reintroduce XML | update wording to "comment sections" / "extension prompt section" |
| Environment prompt docs | `extensions/common/vis-foundation/src/.../environment/core.clj`, `agents.clj`, tests | docs mention `<environment>`, `<project-guidance>`, `<scan-warnings>` | maybe model-facing concept, but tests assert tags absent | old environment was XML block; later moved to comment/EDN style | docs lie; agents think XML still exists | rename docs to `ctx.runtime`, `ctx.project-guidance`, `ctx.scan-warnings` / comment sections |
| Manifest scan warnings | `src/com/blockether/vis/internal/manifest.clj`, `main.clj` | comments mention `<scan-warnings>` | no direct output | old foundation renderer contract | stale comment hides actual surface | update comments to foundation environment warning section |
| Transcript markdown | `extensions/common/vis-foundation/src/.../transcript.clj` | `<details><summary>...</summary>` | human markdown only | collapsible sections in GitHub markdown | acceptable; not prompt-control | keep; clearly mark as markdown rendering, not model prompt |
| IR HTML renderers | `src/com/blockether/vis/internal/render.clj` | `<pre>`, `<code>`, `<blockquote>`, etc. | final answer renderer, not control prompt | HTML/Telegram/Markdown output formatting | acceptable if escaped | keep; not part of `ctx` migration |

## Detailed findings

### 1. `<current_user_message>`

Observed in tests/fixtures:

```clojure
:content "<current_user_message>\nUSER_TURN_TEXT_FIXTURE\n</current_user_message>"
```

Likely old live output came from `prompt-block "current_user_message"` in `prompt.clj`. Today `prompt-block` renders comment headers:

```text
;; -- CURRENT-USER-MESSAGE --
...
```

Wrong part: transcript tests and classifier still recognize old tag bodies.

Recommended path:

- Do not wrap current user message in XML.
- Keep provider role = `user` as main semantic marker.
- Put request into `ctx`:

```clojure
(get-in ctx [:conversation :user-request])
```

- Update transcript classifier to prefer persisted message role/index/kind, with fallback for old transcripts:

```clojure
(cond
  (= idx 0) "stable system prompt"
  (and (= role "user") first-user?) "current user message"
  (and (= role "user") (str/includes? content ";; ctx =")) "per-iteration trailer"
  ...)
```

Keep backwards detection for old `<current_user_message>` only in transcript rendering.

### 2. `<current_turn_context>`

Observed in transcript classification:

```clojure
(str/includes? content "<current_turn_context>")
```

Old fixture carries:

```text
<current_turn_context>
engine_state: turn.iteration/start
engine_phase: model_think
conversation_id: conv-fixture
engine_turn_id: turn-fixture
engine_turn_position: 1
current_engine_iteration_id: turn/turn-fix/iteration/1
engine_iteration_position: 1
prompt_role: user
</current_turn_context>
```

Most of this belongs in `ctx`:

```clojure
{:conversation {:id ... :turn-id ... :user-request ...}
 :iteration {:id ... :position ...}}
```

Wrong part: dual source of truth. Model sees `ctx` and tag block. Transcript depends on tag string.

Recommended path:

- Delete `<current_turn_context>` emission if any live path remains.
- Ensure per-iteration trailer always contains:

```clojure
;; ctx =
{...}
```

- Transcript classifier should detect `;; ctx =` for new messages.
- Persist explicit message kind later:

```clojure
:llm-message/kind :vis.message/per-iteration-trailer
```

Fallback only for old transcripts.

### 3. `<error>...</error>` provider failure messages

Observed in `src/com/blockether/vis/internal/loop.clj`:

```clojure
"<error>Provider stopped the response as incomplete because output budget was exhausted (max_output_tokens).</error>\n"
"<error>LLM call failed: ...</error>\n"
```

Why it existed: strong salience. Tell model previous provider call failed.

Wrong part: XML tag inside Clojure loop. Model may copy `<error>` or treat as markup instead of data.

Better comment style:

```clojure
;; ! provider-error :incomplete-output
;; ! message Provider stopped the response as incomplete because output budget was exhausted (max_output_tokens).
```

Better `ctx` style:

```clojure
{:last-error {:phase :provider-call
              :type :provider/incomplete-output
              :message "Provider stopped ..."
              :hint "Retry with shorter answer or lower output demand."}}
```

Recommendation: use `ctx[:last-error]` if error should affect next model step. Use comment transcript if it is only prior iteration tape.

### 4. `<extensions>` / `<turn_system_context>` / `<system_prompt>` docs

`prompt-block` no longer emits XML. It emits:

```text
;; -- SYSTEM-PROMPT --
;; -- TURN-SYSTEM-CONTEXT --
;; -- EXTENSIONS --
```

But docstrings still say XML-style names.

Wrong part: docs stale. Code is mostly okay.

Recommended cleanup:

- Replace backticked XML names in docstrings with comment-section names.
- Rename "block" docs to "section".
- Do not change runtime unless tests prove old tags still emitted.

### 5. `<environment>`, `<project-guidance>`, `<scan-warnings>` docs

Foundation environment docs/comments still describe XML blocks. Tests already assert tags are absent:

```clojure
(expect (not (string/includes? out "<environment>")))
(expect (not (string/includes? out "<project-guidance")))
(expect (not (string/includes? out "<scan-warnings")))
```

Wrong part: comments/docstrings describe retired protocol.

Recommended cleanup:

- Rename docs to foundation environment prompt sections.
- If data is dynamic and useful in code, consider `ctx` keys:

```clojure
(:runtime ctx)
(:project-guidance ctx)
(:scan-warnings ctx)
```

But beware: large project guidance may bloat every `ctx`. Static extension prompt may be better.

Decision needed:

- Small dynamic state -> `ctx`.
- Long static prose -> extension prompt section.
- Warnings relevant every iteration -> maybe `ctx[:warnings]` with small summaries.

### 6. Transcript markdown `<details>`

This is human-facing Markdown, not model prompt protocol:

```html
<details><summary>LLM messages (...)</summary>
...
</details>
```

Keep. It is output format, not `ctx` replacement target.

### 7. Answer render HTML tags

`render.clj` emits HTML-ish tags for final output targets:

```html
<pre><code>...</code></pre>
<blockquote>...</blockquote>
```

Keep if escaped. Not prompt-control XML.

## Proposed migration order

### Phase 1 — docs/comments truth pass

Low risk. No runtime change.

- Update stale references:
  - `<extensions>` -> extension prompt section
  - `<environment>` -> foundation environment section
  - `<scan-warnings>` -> scan warnings section
  - `<project-guidance>` -> project guidance section

### Phase 2 — transcript classifier fallback

Keep old transcript support, add new detection.

Current:

```clojure
(str/includes? content "<current_turn_context>")
```

Better:

```clojure
(or (str/includes? content ";; ctx =")
    (str/includes? content "<current_turn_context>")) ; legacy fallback
```

Then change tests to fixture new form.

### Phase 3 — provider error tags

Replace:

```xml
<error>...</error>
```

with comment or EDN. Preferred:

```clojure
;; ! provider-error {:type :provider/incomplete-output
;;                   :message "..."}
```

or in next `ctx`:

```clojure
:last-error {:type ... :message ...}
```

Need inspect exact control path before editing.

### Phase 4 — remove remaining prompt-control tags

Search target:

```bash
rg -n "<current_user_message>|<current_turn_context>|<error>|</error>|<system_prompt>|<turn_system_context>|<extensions>|<environment>|<scan-warnings>|<project-guidance>" src extensions test
```

Allowed leftovers:

- old transcript fallback tests only, marked `legacy`
- Markdown/HTML output renderers
- examples documenting legacy behavior

## Open design questions

1. Should `ctx` gain `:last-error`?
   - Pro: structured recovery.
   - Con: errors already visible in iteration transcript comments.

2. Should `ctx` gain environment/project guidance?
   - Pro: unified state access.
   - Con: large static docs inside `ctx` every iteration wastes tokens.

3. Should transcripts persist message kind?
   - Pro: no tag/string sniffing.
   - Con: DB/schema/API change.
   - Middle path: infer now, persist later.

4. Should prompt block names use comments forever?
   - Proposed yes:

```text
;; -- CURRENT-USER-MESSAGE --
;; -- EXTENSIONS --
```

No XML-like brackets.

## Acceptance criteria

- No model-facing prompt-control XML except legacy transcript fallback.
- Dynamic per-iteration facts live in `ctx`.
- Static extension prose lives in comment sections.
- Human markdown/HTML rendering remains allowed.
- Tests assert absence of retired tags in live prompt/trailer paths.

## Next concrete edits

1. Update transcript classification to detect `;; ctx =` and mark old tags legacy.
2. Update transcript fixtures away from `<current_user_message>` / `<current_turn_context>`.
3. Replace `<error>...</error>` in loop with comment/EDN form.
4. Clean stale docstrings/comments for environment/extensions scan warning blocks.
