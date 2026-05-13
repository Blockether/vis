# Analysis: conversation c97db662-9ac1-4cbb-aea9-e4dbac57a7c4

Scope: inspected Vis transcript via `com.blockether.vis.ext.foundation.transcript/transcript` and checked current working tree.

## Summary

Main problems were not Clojure syntax skill. They were control-flow and evidence problems:

1. Tool API guesses caused early errors.
2. The first real change targeted the wrong UI surface.
3. `z/patch` was used with stale/string-level replacement instead of robust locator/AST identity.
4. A no-op patch was treated like success.
5. The loop tried to answer in same iteration as mutation; host correctly rejected it.
6. Later work spent many iterations observing source but did not finish cleanly before interruption.

## 1. Tool API misuse

Transcript turn 3 had repeated tool arity failures:

```clojure
(v/rg "bubble footer|footer|provider|fallback|codex|anthropic|model"
      {:glob "extensions/channels/vis-channel-tui/src/**/*.clj"})
```

Error:

```text
Wrong number of args (2) passed to: com.blockether.vis.ext.foundation.editing.core/rg-tool
```

Resolution: `v/rg` now takes one spec map only (`{:all|:any [...] :paths [...] :include [...]}`); the legacy `query+opts` arity throws `:ext.foundation.editing/invalid-rg-arity` with a directive message. `v/glob` was removed entirely — path discovery now lives inside `v/rg` via `:paths`/`:include`/`:exclude` and `v/ls` for directory walks.

Pinpoint: agent guessed tool signatures instead of checking actual tool contract or falling back to `v/bash`/known working commands.

## 2. Wrong target: app footer vs bubble footer

User asked for fallback info in the TUI bubble footer. The agent changed:

```text
extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/footer.clj
```

But `footer.clj` is the global TUI footer/status row, not the assistant message bubble footer. Current correct surface is in `render.clj` around `draw-chat-bubble!` / `bubble-height*`:

```clojure
(defn- assistant-meta-line
  [{:keys [iteration-count duration-ms tokens cost] :as message}]
  (let [line (vis/format-meta-line ...
               {:suffix (when-let [fallback (fallback-summary message)]
                          [fallback])})]
    ...))
```

Transcript confirms user caught it:

```text
You changed it in the wrong place randomly :D
```

Pinpoint: agent saw “footer” and patched `footer.clj` by filename, not by runtime UI ownership.

## 3. Invalid `z/patch` search fragment

Turn 4 attempted this:

```clojure
{:search "(defn- build-segments\n  [db now-ms]\n  (let [m ..."
 :replace "(defn- build-segments\n  [db now-ms]\n  (let [m ..."}
```

Error:

```text
z/patch :search string must be parseable Clojure/EDN source
```

Pinpoint: `z/patch` search strings are parsed as Clojure/EDN forms. Partial forms are invalid. Use a full form from `z/forms`, a locator row from `z/locators`, or `v/patch` for raw text.

## 4. No-op patch misread as success

The self-analysis in turn 6 shows the exact failure:

```clojure
(str/replace
  footer-build-before
  "[provider-seg model-seg reasoning-seg verbosity-seg git-segs]"
  "[provider-seg ... git-segs]")
```

But the current form no longer contained that exact vector. Result: `footer-build-after` was effectively same bytes.

Transcript output:

```text
Patched 1 Clojure file(s). z/patch preflight validated exact matches before writing.
...
footer.clj — unchanged
```

Pinpoint: preflight proved the old full form matched; it did not prove replacement changed bytes. Need assert `:total-changes > 0` and reread changed source before claiming success.

## 5. Proposed inserted code had missing bindings

The failed snippet referenced:

```clojure
(some-> llm-selected str str/trim)
(some-> llm-actual str str/trim)
```

But those locals were not bound at that insertion point. The later self-analysis says:

```text
The injected snippet also referenced `llm-selected` / `llm-actual` symbols that were not introduced in that form at that stage
```

Pinpoint: patch planned local use before verifying local binding context.

## 6. Host correctly blocked answer-with-mutation

> **Note (post P0.2, commit 87acfe1c):** the gate described here has since
> been broadened. It used to fire only when a **mutating** tool call
> (`v/patch`, `z/patch`, ...) coexisted with `(turn-answer! ...)`. The
> stricter `answer-with-extension-preflight-mismatch` now fires when **any**
> registered extension call (observation OR mutation, e.g. `v/cat`, `v/rg`,
> `z/locators`) shares an iteration with the answer. Pure Clojure forms
> (`def`, `set-conversation-title!`, host primitives) still coexist with
> `(turn-answer! ...)` freely. The original section below documents the
> turn-11 incident under the old name; the underlying invariant
> ("observe before answer") is identical — the scope just widened.

Turn 11 hit a structural guard:

```text
Answer/mutation preflight rejected this iteration before evaluation:
(turn-answer! ...) ... and a mutating tool call ... appeared together
```

Why this matters: if mutation and final answer happen in one iteration, the answer cannot observe whether the mutation succeeded. The host forced the right workflow:

```text
keep the mutating call in THIS iteration, drop the (turn-answer! ...).
... next iteration's <journal> will carry the mutation's :success?/:error
```

Pinpoint: agent tried to claim success before the mutation result was observable.

## 7. Non-Clojure files need `v/patch`, not `z/patch`

Current working tree now includes tests referencing this regression:

```clojure
(it "rejects z/patch on a non-Clojure file before any rewrite-clj parse (regression: ANALYSIS.md §1)"
  (let [md-path (write-temp! "ext-check/AGENTS.md"
                  "# heading\n\nbody with infinity ∞ symbol.\n")]
    ...))
```

Pinpoint: `z/` tools parse rewrite-clj source. Markdown like `AGENTS.md` can contain prose/unicode/fences that are valid Markdown, not valid Clojure. Guard by extension and tell agent to use `v/patch`/`v/write` for text files.

## 8. Process issue: too much observation, no clean finish

Turn 12 had 9 iterations, mostly source reads and status checks, then ended interrupted. Examples:

```clojure
(def patch-focus-before
  (v/bash "sed -n '1,120p;180,460p;520,780p;900,940p' .../patch.clj"))
```

```clojure
(def awaiting-focused-source-output
  {:observed [...]
   :next :mutate})
```

Pinpoint: planning/inspection loop did not converge to mutate -> observe result -> verify -> answer before interruption.

## Current working tree signal

At inspection time there are uncommitted changes in:

```text
extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj
extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/render_test.clj
extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/editing/core.clj
extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/introspection.clj
extensions/languages/clojure/src/com/blockether/vis/ext/lang_clojure/patch.clj
extensions/languages/clojure/src/com/blockether/vis/ext/lang_clojure/repair.clj
extensions/languages/clojure/src/com/blockether/vis/ext/lang_clojure/path.clj
```

No full verification result was visible in the transcript. Before keeping this work, run at least:

```bash
./verify.sh --quick
```

Then full:

```bash
./verify.sh
```

## Fix rules for future edits

- Reproduce/locate runtime owner first; do not patch by filename guess.
- For Clojure: use `z/forms` / `z/locators` rows, not partial-form strings.
- For Markdown/plain text: use `v/patch` / `v/write`, never `z/patch`.
- After every mutation: reread bytes and check positive change count.
- Never combine mutation and final `turn-answer!` in same iteration.
- Verify behavior, not only source substring presence.

---

# Analysis: Z.ai preserved-thinking replay failure in conversation a9389e1d-d5b6-400e-a23d-3b4d6f1b53c5

Scope: inspected `~/.vis/vis.mdb/vis.db` for conversation `a9389e1d-d5b6-400e-a23d-3b4d6f1b53c5`, then cross-checked Z.ai/GLM behavior with Exa results from Z.ai docs and related issue reports.

## Summary

The token/time blow-up in this conversation was mostly **not** `v/cat`. The larger problem was that preserved-thinking replay is likely being assembled incorrectly for Z.ai/GLM:

1. Prior assistant messages are appended as a block of consecutive `assistant` messages.
2. The matching per-iteration `user` journal/context messages are **not** replayed between those assistant turns.
3. Replay crosses turn boundaries, so the new turn receives assistant content from the previous turn.
4. The replay budget counts mostly preserved-thinking chars, but the actual provider request carries full assistant text/code as well.

This produced replay contamination: on the first iteration of the new user turn, GLM replied as if the previous answer had already been accepted.

## Transcript/database facts

For turn 2 of conversation `a9389e1d-d5b6-400e-a23d-3b4d6f1b53c5`, SQLite showed:

```text
iterations:       8
LLM duration:     ~336.6s
input tokens:     125,841
output tokens:    13,062
reasoning tokens: 5,605
cached tokens:    85,760
prompt JSON:      ~608KB
```

The important per-iteration failures were:

| Iter | Result |
|---:|---|
| 1 | Model said: `The answer was already accepted in the previous iteration. No further action needed.` It emitted no code. This is strong evidence of cross-turn replay contamination. |
| 2 | Tried to answer immediately. Gate rejected: `<journal> contains no evidence for this turn yet — probe before turn-answer!`. |
| 3 | Tried to test two block patterns but used `(v/cat "deps.edn" 0 3)`. `v/cat` offsets are 1-based, so both reads failed. Later block then hit unresolved `lines-a`. |
| 4 | Corrected offset but mixed tool calls and `(turn-answer! ...)` in one iteration. Preflight rejected with `:answer-with-extension`. |
| 5 | Ran evidence but used wrong payload shape: `(:lines (:result r-a))`. In current Vis, observed tool wrappers return the payload directly, so the correct access is `(:lines r-a)`. This produced bogus zero/nil conclusions. |
| 6 | Corrected payload shape but emitted 26 blocks, wandered through wrong `v/conversation-state` paths, and final answer block failed with `Invalid symbol: bindings:`. |
| 7 | Tried again but mixed extension/tool evidence with `(turn-answer! ...)`. Preflight rejected with `:answer-with-extension`. |
| 8 | Finally emitted a clean answer with no extension calls. Accepted. |

## Actual message shape sent to GLM looked wrong

For iteration 8, the stored `llm_user_prompt` decoded to roughly:

```text
0  system
1  system
2  user       current user request
3  assistant  previous turn assistant message
4  assistant  previous turn assistant message
5  assistant  this turn iter 2
6  assistant  this turn iter 3
7  assistant  this turn iter 4
8  assistant  this turn iter 5
9  assistant  this turn iter 6
10 assistant  this turn iter 7
11 user       current journal/context
```

That is **not** a normal multi-turn chat transcript. It is a pile of assistant replies followed by one trailing user context.

The implementation comment in `src/com/blockether/vis/internal/loop.clj` says the desired shape is:

```text
[system, user_initial,
 asst_iter1, user_journal_after_iter1,
 asst_iter2, user_journal_after_iter2,
 ...]
```

But `append-preserved-thinking-replay` only appends assistant messages. The corresponding user journal/context messages are not replayed between them. This contradicts the comment and likely breaks the intended Z.ai preserved-thinking sequence.

Relevant code paths:

```text
src/com/blockether/vis/internal/loop.clj
  preserved-thinking-replay-messages
  append-preserved-thinking-replay
  compatible-preserved-thinking-journal-iters

src/com/blockether/vis/internal/prompt.clj
  build-iteration-context
  format-journal-block
```

## Exa cross-check

Exa results support the suspicion that the current replay shape is wrong:

1. Z.ai GLM-5.1 docs show normal chat history as alternating user/assistant/user messages, with deep thinking enabled via:

   ```json
   "thinking": {"type": "enabled"}
   ```

   Source: `https://docs.z.ai/guides/llm/glm-5.1`

2. Z.ai Deep Thinking docs show reasoning as `message.reasoning_content` on the assistant response, and streaming reasoning as `delta.reasoning_content`.

   Source: `https://docs.z.ai/guides/capabilities/thinking`

3. Z.ai migration docs say GLM-5.1 supports deep thinking and instruct clients to handle both `delta.reasoning_content` and `delta.content` in streaming mode.

   Source: `https://docs.z.ai/guides/overview/migrate-to-glm-new`

4. The GLM-4.5/4.7 README describes preserved thinking as retaining thinking blocks across multi-turn coding-agent conversations, with SGLang config:

   ```json
   {"chat_template_kwargs": {"enable_thinking": true, "clear_thinking": false}}
   ```

   Source: `https://github.com/zai-org/GLM-4.5/blob/main/README.md`

5. A related GLM-5 issue report says multi-turn coherence requires returning `reasoning_content` in each assistant message and using `clear_thinking: false`; otherwise GLM can repeat actions / loop after a few messages.

   Source: `https://github.com/can1357/oh-my-pi/issues/517`

Taken together: preserved thinking should preserve the assistant's reasoning payload **inside a coherent transcript**, not append a chain of orphaned assistant messages separated from the observations/user contexts that caused them.

## Root cause hypothesis

The current code preserves only assistant replay messages:

```clojure
(provider-messages (append-preserved-thinking-replay
                     messages journal-iters (replay-context pre-resolved-model)))
(effective-messages (cond-> provider-messages
                     (not (str/blank? iteration-context))
                     (conj {:role "user" :content iteration-context})))
```

This means each prior iteration contributes assistant text/thinking, but not the immediately following journal/context that made that assistant turn meaningful.

Also, `preserved-thinking-replay-token-budget` is based on `replay-reasoning-chars`, but the actual assistant replay includes:

- reasoning/thinking/signature
- assistant text
- generated Clojure code blocks
- previous final answers / failed answer attempts

So the budget underestimates the true prompt footprint.

## Recommended fixes / implemented policy

Implemented immediately after this analysis:

1. **Do not replay preserved thinking across user turns.** Cross-turn persisted iterations still seed `<journal>`, but carry `:preserved-thinking/replay? false`, so their assistant messages are not re-sent as hidden provider reasoning.
2. **Replay only the immediately previous compatible iteration inside the same live user turn.** No long run of assistant messages is replayed anymore; older iterations remain visible through `<journal>`.
3. **Keep provider/model compatibility checks.** Z.ai reasoning still cannot be replayed into Anthropic/OpenAI/etc., and poisoned Anthropic signatures are still filtered.
4. Add regression tests for both invariants: only newest compatible replay, and no replay for cross-turn journal seeds.

Still recommended follow-ups:

- Cap by serialized provider-message size if we ever replay more than one step again.
- Preserve `reasoning_content` as a provider-native assistant field where the serializer supports it, instead of duplicating reasoning into generic `thinking`/`thinking-signature` blocks.
- Consider replaying coherent assistant/user journal pairs only if we intentionally re-enable multi-step replay.

## Immediate diagnostic test

Create a fixture with two prior iterations and assert that the effective messages sent to Z.ai are not shaped like:

```text
user, assistant, assistant, assistant, user
```

Expected shape should be closer to:

```text
user_initial,
assistant_iter1_with_reasoning_content,
user_journal_after_iter1,
assistant_iter2_with_reasoning_content,
user_journal_after_iter2,
user_current_context
```

or, if we choose safer behavior:

```text
user_initial,
user_current_context
```

with no preserved-thinking replay across turn boundaries.
