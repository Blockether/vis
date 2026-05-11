# Plan: Answer Guardrails, Objective Binding, and Prompt Pruning

## Status

This plan is based on runtime transcript inspection through nREPL plus local hook work already started in the working tree.

Checked with nREPL: **yes**.

Commands used:

```bash
clj-nrepl-eval --discover-ports
clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.core :as vis] '[com.blockether.vis.ext.foundation.transcript :as tr] :reload)"
```

Conversation inspected:

```text
ac0da8ae-82c1-4255-80b1-94dd90a540ed
```

Important turn IDs:

```text
221e9d3f-75e2-4be1-9adc-49e5ae9bfb89  slash-suggestion task, 11 iterations, 7 failures
2330e0f6-c976-4076-a905-82f1d9bcc4a0  "do it" drift turn, 2 iterations, 1 failure
```

## Reproduction

Runtime transcript showed this sequence:

1. User asked for `/skill:` prefix + scroll in TUI command popup.
2. Agent attempted slash-popup work.
3. That turn had heavy failed history:

```text
turn: 221e9d3f-75e2-4be1-9adc-49e5ae9bfb89
iterations: 11
failures: 7
```

4. Later user asked a different task: code-block/header spacing in TUI renderer.
5. Assistant offered to patch `render_ir.clj` / markdown-to-lines path.
6. User said: `do it`.
7. Agent should have bound `do it` to the immediately previous offer.
8. Instead it resumed stale slash-popup work:

```clojure
screen.clj
render.clj
skill-commands
draw-slash-command-suggestions!
```

9. Patch failed in iteration 1:

```text
z/patch source string must be parseable Clojure/EDN source
cause: Unmatched delimiter: ]
```

10. Same iteration still ran:

```text
cljfmt fix ...
./verify.sh --quick
```

11. `verify.sh --quick` passed, but it did not verify the requested feature.
12. Iteration 2 answered:

```text
Done. I implemented...
```

That answer was false: the feature requested by current user intent was not implemented, and the previous iteration still contained a failed patch.

Reduced failure patterns:

### Repro A: answer after unresolved error

```text
iter 1: z/patch failed
iter 1: ./verify.sh --quick passed
iter 2: answer "Done"
```

Expected guard behavior:

```text
Reject answer. Journal contains unresolved failure obligation.
```

### Repro B: zero-action fix claim

```text
user: "please fix it now"
iter 1: (answer "fixed")
```

Expected guard behavior:

```text
Reject answer. User requested action but turn contains no observation/action/proof.
```

### Repro C: stale objective drift

```text
older task: slash popup failed, huge tool history
latest assistant offer: patch render_ir spacing
user: "do it"
model resumes older slash popup task
```

Expected behavior:

```text
Bind short imperative to immediate previous actionable assistant proposal.
```

## Root causes

### 1. No first-class failure obligation lifecycle

Journal records failures, but does not turn them into stateful obligations.

Current raw shape is like:

```clojure
{:success? false
 :form "(z/source ...)"
 :error {:message "z/patch source string must be parseable..."}}
```

This is evidence, but not lifecycle:

```text
open -> resolved -> acknowledged -> ignored
```

So the model can accidentally treat later unrelated clean commands as success.

### 2. Weak success invariant

Bad invariant seen in transcript:

```text
verify passed => work done
```

Correct invariant:

```text
requested feature diff exists
+ failed tool obligations resolved
+ targeted proof passed
=> work done
```

### 3. Ambiguous short imperative not resolved to latest objective

`do it` was not explicitly bound to the immediate prior assistant proposal.

### 4. Old failed task dominated context

Huge slash-popup failure trace polluted later prompt. Latest user intent was present but competed against stale unresolved task history.

### 5. No action-request evidence gate

If user asks to fix/implement/change/add/remove/do it, answer-only should not be allowed unless answer is explicitly blocked/needs-input/partial.

## Proposed architecture

Do this in layers. Each layer has a direct test seam.

## Layer 1: hard answer-validation hook surface

Already started.

Canonical hook phase:

```clojure
:turn.answer/validate
```

Contract:

```clojure
;; accept
nil

;; reject
{:reject true
 :message "why rejected"
 :hint "how to recover"}
```

Context:

```clojure
{:environment env
 :phase :turn.answer/validate
 :iteration 2
 :blocks current-iteration-blocks
 :answer [:ir ...]
 :previous-iterations [[1 {:blocks [...]}] ...]
 :previous-blocks [...]}
```

Rationale:

- Core owns hard acceptance/rejection.
- Foundation owns policy.
- Extension hook is explicit, named, testable.
- No hidden model prompt magic.

Tests:

```clojure
com.blockether.vis.internal.loop-test/answer-validation-hook-test
```

## Minimal extension spec changes

Keep the extension spec change as small as possible. Do **not** add a new extension surface if `:ext/hooks` can express the lifecycle.

### Existing spec before this work

```clojure
:ext/hooks [{:id keyword?
             :doc non-blank-string?
             :phase #{:session/start
                      :turn/start
                      :turn.iteration/start
                      :turn.iteration/stop
                      :turn/stop}
             :fn fn?}]
```

### Minimal spec delta proposed

Add one phase only:

```clojure
:turn.answer/validate
```

Keep the hook declaration shape unchanged:

```clojure
{:id :my-ext/answer-policy
 :doc "Reject unsupported final answers."
 :phase :turn.answer/validate
 :fn (fn [ctx] nil-or-reject)}
```

Add return-shape specs so the contract is explicit:

```clojure
::system-nudge-hit
;; {:hint non-blank-string?
;;  :importance :low|:normal|:high|:critical} ; importance optional

::answer-validation-reject
;; {:reject true
;;  :message non-blank-string? ; optional but recommended
;;  :hint non-blank-string?}   ; optional recovery guidance

::answer-validation-result
;; nil | ::answer-validation-reject
```

### Why this is minimal

No new top-level extension key:

```clojure
;; rejected
:ext/answer-guards
:ext/validators
:ext/preflight-hooks
```

No callback registry parallel to hooks. No legacy phase aliases. No broad schema migration.

Only required core spec changes:

1. Add `:turn.answer/validate` to `canonical-hook-phases`.
2. Add specs for pre-phase nudge return and answer-validation reject return.
3. In answer-validation dispatch, accept only values matching `::answer-validation-reject`; log invalid reject-looking maps.

### Why specs matter here

Without return specs, a hook can accidentally return shapes like:

```clojure
{:reject false :message "..."}
{:text "..."}
{:hint ""}
```

and the runtime has to guess. With specs:

- pre-phase nudges require `:hint` string.
- answer validators require `:reject true`.
- invalid hard-reject maps are observable via warning log and ignored.

### Test seam

Specs are checked by unit tests in `com.blockether.vis.internal.extension-test`:

```clojure
(s/valid? ::ext/system-nudge-hit {:hint "Focus now." :importance :high})
(s/valid? ::ext/answer-validation-reject {:reject true :message "No proof."})
```

Runtime dispatch is checked in `com.blockether.vis.internal.loop-test`:

```clojure
final-answer-gate-error rejects valid {:reject true ...}
final-answer-gate-error ignores invalid {:reject false ...}
```

## Layer 2: failure obligations in foundation

Implement in foundation first without schema migration.

Function names proposed:

```clojure
failure-obligations
proof-events
open-error-obligations
unresolved-error-answer-guard-check
```

### Failure obligation shape

```clojure
{:id "iter/1/block/3/journal/1"
 :kind :failure/journal
 :iteration 1
 :block 3
 :journal 1
 :tool :z/source
 :form "(z/source ...)"
 :message "z/patch source string must be parseable Clojure/EDN source"}
```

Block error shape:

```clojure
{:id "iter/1/block/3"
 :kind :failure/block
 :iteration 1
 :block 3
 :tool :z/patch
 :form "(z/patch ...)"
 :message "..."}
```

### Proof event shape

```clojure
{:iteration 2
 :block 1
 :journal 1
 :tool :v/bash
 :form "(v/bash \"./verify.sh --quick\" ...)"
 :verification? true}
```

### Closure rules, v1

An obligation is closed only by a later iteration proof.

Same-iteration proof does not count because the model could not observe the failure before composing the answer.

Rules:

```clojure
later successful verification command closes all prior failures
later successful same-tool call closes that tool's prior failure
```

Verification command examples:

```text
./verify.sh
./verify.sh --quick
clojure -M:test
```

### Gate behavior

Foundation hook:

```clojure
{:id :foundation/unresolved-errors-before-answer
 :phase :turn.answer/validate
 :fn unresolved-error-answer-guard-check}
```

Rejects:

```text
Open failure obligations remain in the journal: iteration 1/block 3/journal 1 :z/source `(z/source ...)`: z/patch source string must be parseable...
```

Hint:

```text
Do not answer yet. Read failed journal entry, fix or prove it, then run a later proof step before answering.
```

Rationale:

This replaces heuristic "latest iteration clean?" with:

```text
are all known failures resolved by later proof?
```

That is the real invariant.

Tests:

- extracts block-level failures
- extracts failed journal sink entries
- includes `iteration/block/journal/form/tool/message`
- rejects open failures
- accepts after later successful verify
- accepts after later successful same-tool retry
- does not accept same-iteration verify as proof

## Layer 3: action-request evidence gate

Status: **implemented in foundation v1**.

Foundation hook:

```clojure
{:id :foundation/action-request-needs-evidence
 :phase :turn.answer/validate
 :fn action-request-needs-evidence-check}
```

### Action request detector

User request is action-like when it contains verbs such as:

```text
fix, implement, patch, change, add, remove, do it, make it, verify, run, commit, push
```

Current v1 treats `do it` as action-like directly. Better v2 should make it context-sensitive:

```text
"do it" -> action-like if previous assistant made actionable offer
```

### Evidence detector

Evidence exists if current turn previous iterations contain at least one successful non-answer work event:

```clojure
successful journal entry
successful block with non-answer code
successful observation/action/proof tool
```

Answer-only is rejected when:

```text
action-request? true
and evidence? false
and answer is not blocked/needs-input/partial
```

Reject message:

```text
User asked for action, but this turn contains no observed tool/code work.
Do the work, inspect state, or answer blocked/needs-input explicitly.
```

Rationale:

This catches:

```clojure
(answer "fixed")
```

on a fix request with no tool calls.

It avoids dumb "always call tool first" for conceptual questions.

Tests implemented:

- conceptual request allows answer-only
- `"fix it"` rejects answer-only
- `"do it"` rejects answer-only with no turn evidence
- action request accepts after successful prior work evidence
- action request accepts blocked/partial answer

## Layer 4: current-turn objective pin

Status: **partially implemented (deterministic objective map + prompt block + hook ctx threading)**.

Added prompt/runtime helper that derives current objective once per turn.

### Rule

If user says short imperative:

```text
do it
please do it
fix it now
yes
ok do that
```

bind it to the immediately previous actionable assistant proposal, not older backlog.

Objective shape:

```clojure
{:source :previous-assistant-proposal
 :user-request "do it"
 :objective "Patch TUI code-block/header spacing in render_ir.clj / markdown->lines path"
 :confidence :high}
```

Current rendered shape:

```xml
<current_objective>
source: previous-user-request
confidence: high
objective: Patch TUI code-block/header spacing in render_ir.clj / markdown->lines path
</current_objective>
```

Rationale:

This directly prevents the transcript drift where `do it` resumed old slash-popup work.

Alternative implementation:

- Do not derive objective in code.
- Add a prompt-only rule saying latest offer wins.

Rejected because prompt-only rule is weaker and not testable against transcripts.

Tests implemented now:

- short follow-up `do it` binds objective to previous turn user request
- default objective source is current user request
- `<current_objective>` renders in initial user message
- action-evidence guard uses objective source so bare `do it` without binding is not treated as action request

Still missing:

- explicit switch-back (`go back to slash popup`) objective override regression test
- objective text narrowing to previous assistant actionable proposal (currently previous user request)

## Layer 5: stale trace pruning / summarization

Do not delete transcript. Only prune prompt projection.

### Current problem

Old failed tool-heavy turn can dominate model context.

### Proposed prompt projection

Keep:

```text
latest user turn verbatim
latest assistant proposal/answer verbatim
last 1-2 turns mostly verbatim
```

Compress older tool-heavy turns into digest:

```clojure
{:turn-id "221e9d3f"
 :user-request "Please add /skill prefix + scroll"
 :objective "slash suggestion popup"
 :outcome :partial
 :files-touched [...]
 :tool-count 40
 :failures 7
 :last-errors ["z/patch parse fail" "verify quick fail"]
 :unresolved ["skill prefix not landed" "scrollbar not landed"]}
```

Render low priority:

```xml
<inactive_backlog>
- Slash popup task partially failed. Do not resume unless user asks.
</inactive_backlog>
```

Render current objective high priority:

```xml
<current_objective>...</current_objective>
```

Rationale:

Old failures stay visible, but stop competing with current user intent.

Alternative ideas:

1. Drop old tool traces entirely.
   - Rejected: loses forensic truth.
2. Keep everything verbatim.
   - Rejected: caused observed drift.
3. Summarize all old turns with LLM.
   - Maybe later, but risky/non-deterministic. Start with deterministic digest from DB rows.

Tests:

- old turn with many tool blocks renders digest, not raw block dump
- latest turn still renders raw needed journal
- unresolved backlog marked inactive unless user references it

## Alternative designs considered

### Alternative A: core-only hard gates

Put all policies in `loop.clj`.

Pros:

- simple dispatch
- fewer extension boundaries

Cons:

- core becomes policy-heavy
- harder for extensions to add domain gates
- violates current design where foundation owns nudges/policy

Decision: reject. Keep hook surface, foundation policy.

### Alternative B: prompt-only nudges

Only tell model:

```text
Do not answer after errors.
```

Pros:

- easy
- no runtime blocking

Cons:

- model can ignore it
- transcript shows model did ignore similar instructions

Decision: reject for critical correctness. Use hard validator.

### Alternative C: latest-iteration-clean heuristic

Accept if latest previous iteration has no errors.

Pros:

- simple
- avoids old errors blocking forever

Cons:

- clean no-op iteration can falsely close an error
- no proof requirement

Decision: use only as temporary step. Long-term obligation resolver is better.

### Alternative D: require tool call on first iteration always

Pros:

- Pi-like simple heuristic
- catches answer-only fix claims

Cons:

- bad for conceptual questions
- adds pointless tool calls for chat
- still does not prove correct feature

Decision: reject blanket rule. Use semantic action-request evidence gate.

## Proposed implementation order

1. Finish `:turn.answer/validate` hook surface and docs.
2. Implement foundation failure obligations + proof closure.
3. Add action-request evidence gate.
4. Add objective pin.
5. Add stale trace pruning/digest.
6. Add end-to-end `vis run` smoke with `zai-coding/glm-5.1` if credentials/quota are available.

## Verification plan

### Unit / integration tests

Run targeted:

```bash
clojure -M:test \
  -n com.blockether.vis.internal.loop-test \
  -n com.blockether.vis.internal.extension-test \
  -n com.blockether.vis.internal.prompt-test \
  -n com.blockether.vis.ext.foundation.nudges-test
```

Run quick:

```bash
./verify.sh --quick
```

Run full before final handoff:

```bash
./verify.sh
```

Known current caveat: full verify has unrelated existing failures in broad suite. Record exact failures if still present.

### Runtime nREPL checks

Check hook registration:

```clojure
(require '[com.blockether.vis.ext.foundation.nudges :as nudges] :reload)
(mapv :id nudges/hooks)
```

Check hard gate:

```clojure
(require '[com.blockether.vis.internal.loop :as lp]
         '[com.blockether.vis.ext.foundation.nudges :as nudges]
         :reload)
(let [ext {:ext/namespace 'com.blockether.vis.ext.foundation.core
           :ext/hooks nudges/hooks}]
  (lp/final-answer-gate-error
    {:extensions (atom [ext])}
    2
    [{:code "(answer ...)"}]
    [:ir [:p "Done"]]
    [ext]
    {:previous-iterations
     [[1 {:blocks [{:code "(z/patch ...)"
                    :error {:message "z/patch failed"}}]}]]}))
```

Expected: rejection string.

Check later proof closure:

```clojure
(lp/final-answer-gate-error
  {:extensions (atom [ext])}
  3
  [{:code "(answer ...)"}]
  [:ir [:p "Done"]]
  [ext]
  {:previous-iterations
   [[1 {:blocks [{:code "(z/patch ...)"
                  :error {:message "z/patch failed"}}]}]
    [2 {:blocks [{:code "(v/bash verify)"
                  :error nil
                  :journal [{:success? true
                             :form "(v/bash \"./verify.sh --quick\")"}]}]}]]]})
```

Expected: nil.

### `vis run` with zai-coding / glm-5.1

Only run if credentials/quota exist.

Command shape:

```bash
vis run --provider zai-coding --model glm-5.1 --trace --persist \
  "In this turn, intentionally inspect a nonexistent file, then do not claim success; explain the failure as blocked."
```

And action-gate smoke:

```bash
vis run --provider zai-coding --model glm-5.1 --trace --persist \
  "Fix the nonexistent file issue now. If you cannot, say blocked and name the failed proof."
```

Expected:

- model uses tool before final answer for action request
- if tool fails, final answer is blocked/partial, not false done
- transcript shows no answer accepted immediately after open failure obligation

If provider unavailable:

```bash
vis providers status zai-coding
```

Record unavailable reason instead of pretending runtime was checked.

## Critique after spec + prompt review

### What is strong

- Minimal extension-spec delta is correct: one new hook phase, not a new extension surface.
- Hard answer validation belongs in runtime dispatch, not only prompt text.
- Foundation policy is the right owner for failure obligations and action evidence.
- Previous-turn context is already present, which gives short follow-ups a concrete referent.

### What is weak / risky

- Failure closure by generic `./verify.sh --quick` is still too broad. It closes build/lint/test failures but does not prove the requested feature. Keep it for v1, but later prefer obligation-specific proof labels or file/test correlation.
- Same-tool retry closure is useful but can be false-positive if the same tool succeeds on a different target. Later improve by comparing tool + normalized path/form intent.
- The action-request detector can over-trigger on broad verbs like `what` / `explain`. The prompt now allows planning/opinion/design answers, but runtime action gate should key mostly on mutating/action verbs.
- Prompt-only objective binding is not enough. The prompt now says current goal wins, but durable fix needs a first-class `current-objective` value derived before model call and exposed to hooks/tests.
- Stale-trace pruning must preserve failure obligations. Do not prune away unresolved error evidence from the validator context; only compress model-facing prompt projection.
- The answer validator should eventually distinguish `done/fixed` claims from explicit `blocked/partial` answers. Blocking all answers after open failures can trap useful failure reports.

### Prompt optimization made

Core prompt was checked and tightened without exceeding its 90-line budget:

```text
CURRENT OBJECTIVE: <user_turn_request_main_goal> wins; old <journal>
  is evidence, not backlog. Short follow-ups (`do it`, `yes`, `A`) bind
  to <previous_turn_context>; older tasks only if user says so.
```

Also made failure language harder:

```text
Tool failed? read `:op/error :hint`, retry; blocked -> say blocked, not done.
```

This directly targets the reproduced drift: old slash-popup journal should not beat current `do it` request.

### Next optimization worth doing

Implement deterministic objective binding before prompt assembly:

```clojure
{:objective/source :previous-assistant-answer
 :objective/user-request "do it"
 :objective/text "Patch TUI code-block/header spacing..."
 :objective/confidence :high}
```

Then render it as `<current_objective>` and pass it to `:turn.answer/validate` hooks. This gives tests a stable seam instead of relying on prompt wording.

## Current stance on user's items

### 4. Stale trace pruning

Yes. Do deterministic prompt projection pruning, not transcript deletion.

### 5. Active skill dump

User says ignore. Do not prioritize.

### 6. Zero-action fixed claims

Yes. Implement semantic action-request evidence gate.

Not blanket first-iteration tool call. Only action-like requests need evidence. Conceptual questions can answer directly.
