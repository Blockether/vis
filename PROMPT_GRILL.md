# System Prompt Grilling

Analysis of `src/com/blockether/vis/internal/prompt.clj` — the assembled system prompt injected into every turn.

This document is intentionally written as a prompt-design critique for Vis' **RLM** loop, not as generic LLM prompt advice. In Vis, the model is not a chat bot that may optionally call tools. It is the controller of a read/eval/observe loop:

```text
emit Clojure forms → host evaluates forms → host persists journal → next iteration observes journal → repeat until answer
```

The prompt must therefore teach **control flow**, not just available functions.

## RLM Contract We Want

The model should understand these invariants before it sees any tool catalog:

1. **An iteration cannot inspect its own top-level results except inside the same lexical form.**
   - A top-level `(def x (v/rg ...))` plus bare `x` only surfaces the value into `<journal>`.
   - The model reasons over that surfaced value in the **next** iteration.
   - If it needs to compute and answer in the same iteration, the computation must happen inside one form, e.g. `(let [x (v/rg ...)] (answer ...))`.

2. **`(answer ...)` is a commit/stop instruction, not a progress marker.**
   - Once accepted, the turn is over.
   - It must mean: "I have enough observed evidence and have performed required action/verification or have a concrete blocker."
   - It must not mean: "scan complete", "next I will inspect", or "I found files".

3. **The journal is the model's working memory boundary.**
   - Top-level exploration forms are for producing evidence into `<journal>`.
   - The next iteration reads `<journal>`, decides, and either gathers more data, acts, or answers.

4. **RLM progress is stateful.**
   - The prompt should name states and transitions explicitly:
     `PLAN → EXPLORE → OBSERVE → ACT → VERIFY → ANSWER`.
   - The model should know which states allow `(answer ...)` and which forbid it.

5. **Host-observed facts beat model claims.**
   - "I inspected" is not progress unless the journal contains the read/search result.
   - "I fixed" is not progress unless an edit/write event happened.
   - "Verified" is not progress unless a command/test result appears.

## Proposed Identity Opener Critique

Candidate opener:

```text
You are recursive language model called Vis operating in sandboxed Clojure environment via Small Clojure Interpreter (SCI).

You are given all power of Clojure and the core libraries: Clojure.core, clojure.walk, Clojure.string, Clojure.edn and convenient walk for Clojure.walk, s/ or str/ for string and Clojure.core is fully referred.

Think in code, reason in code, produce the code to model the answer. Clojure is data oriented functional programming language where you are operating on data and symbol to construct the end result.

You will be given tasks/questions/queries from the user which you need to ...
```

This is directionally good because it frames Vis as an RLM, not a chat assistant. But it needs tightening.

What is good:

- **Names the runtime identity**: Vis is recursive / iterative, not one-shot chat.
- **Names SCI**: the model knows code is sandboxed Clojure, not arbitrary host Clojure.
- **Pushes code-first reasoning**: good fit for the architecture.
- **Frames Clojure as data-oriented**: useful because the model should build data, transform data, then answer.

Problems:

1. **"all power of Clojure" is false.**
   SCI is sandboxed and does not expose all host power. Saying "all power" invites hallucinated APIs, Java interop assumptions, filesystem assumptions, namespace requires, and unsafe operations.

2. **Library names / aliases are imprecise.**
   Use exact names and aliases. `Clojure.core` should be `clojure.core`; `Clojure.string` should be `clojure.string`; `Clojure.edn` may be wrong in this runtime because current prompt exposes `edn/ -> fast-edn.core`.

3. **"think in code" can cause premature code emission.**
   Good slogan, but it must be bounded by the RLM state machine. Otherwise the model may emit code just to appear active and then terminate with `(answer "done")`.

4. **It does not mention the journal boundary.**
   The most important RLM fact is missing: top-level results become evidence in `<journal>` for the next iteration.

5. **It does not define answer readiness.**
   It must say `(answer ...)` is terminal commit, not progress.

6. **It over-focuses on Clojure identity before control flow.**
   The first 10 lines should teach the loop contract. The language/tool identity comes immediately after.

Better opener:

```text
You are Vis, a recursive language model (RLM) running in a sandboxed
Small Clojure Interpreter (SCI). You control a read/eval/observe loop:

  emit Clojure forms → host evaluates them → results/errors enter <journal>
  → next iteration observes <journal> → repeat until terminal answer

Think in Clojure data. Build small values, transform them, inspect host-
observed results, then act or answer. Prefer explicit data structures over
prose-only reasoning.

SCI is sandboxed Clojure, not an unrestricted JVM. clojure.core is referred.
Common aliases are preloaded: str/ for clojure.string, walk/ for
clojure.walk, s/ for clojure.spec.alpha, edn/ for fast-edn.core. Do not
require these namespaces; call the aliases directly.

`(answer ARG)` is terminal commit. One accepted answer ends the user turn.
Call it only after the task is complete, verified when relevant, or
concretely blocked. Never use it for progress messages like "scanned" or
"next I will inspect".
```

This preserves the user's intent while making it accurate for Vis.

## Var Index: Rendered Prompt vs System Var

Yes, this is worth changing, but not all the way to "system var only".

Current `<var_index>` is rendered into every iteration prompt so the model can see its persisted `(def ...)` bindings without spending another iteration asking for them. That helps continuity, but it has two costs:

1. **Token pressure** — every iteration pays for the full rendered index even when the model does not need it.
2. **Control-flow confusion** — it makes persisted variables look like prompt text, not host-observed state with access semantics.

A pure SYSTEM var is also not ideal:

```clojure
VAR_INDEX
```

If the model only gets the var index as a sandbox value, it must spend an iteration to inspect it before deciding what to do. That is bad for RLM ergonomics: the model needs a small visible working-memory map at iteration start, especially after exploration.

Recommended hybrid:

- Keep a **tiny rendered summary** in the prompt:

  ```text
  <var_index>
  7 defs: provider-clj, dialogs-clj, codex-provider-clj, zai-hits, ...
  Full value available as VAR_INDEX or `(v/vars)`.
  </var_index>
  ```

- Expose the **full structured value** as a SYSTEM var, e.g. `VAR_INDEX`, with machine-readable entries:

  ```clojure
  [{:name 'provider-clj
    :shape [:map {:path [:string 72] :lines [:vector 180 [:string 80]]}]
    :created-iteration 0
    :last-seen-iteration 1
    :source :def}
   ...]
  ```

- Add a tool/helper for paging or filtering full var state:

  ```clojure
  (v/vars)
  (v/var-info 'provider-clj)
  (v/var-value 'provider-clj) ; if needed and safe
  ```

For the exact `answer "scanned"` failure, `<var_index>` was not the root cause. The root cause was missing RLM state gating. But a better var-index design helps because the model should see:

```text
You have these persisted observations from previous iterations: provider-clj, dialogs-clj, zai-hits.
```

without paying to render huge values or confusing the current iteration's new results with already-observed memory.

Best direction:

```text
Visible prompt = compact memory index.
System var/tool = full structured memory.
Journal = recent observed evidence.
Gates = what remains incomplete.
```

So the prompt should eventually have three separate state channels:

1. `<journal>` — recent evidence/results.
2. `<memory_index>` — compact list of durable names/summaries.
3. `VAR_INDEX` / `(v/vars)` — full structured access on demand.

Do not remove visible memory entirely. Compress it and make the full version programmatic.

## 🔴 Critical Issues

### 1. Implicit state machine — the model must guess its own control flow

The iteration model is a state machine but the prompt currently says only:

```text
write code -> get data -> process data -> emit answer
```

That line is directionally correct but too compressed. It does not define:

- which phase the current iteration is in,
- when top-level results become observable,
- when `(answer ...)` is legal,
- what to do after a tool error,
- what to do after an edit,
- what counts as verification.

This is the core failure behind conversation `d5591095-b0b0-4f01-9066-9bdb46c17f41`: the model emitted exploration forms and then immediately called `(answer "scanned")`. The runtime accepted the answer because it was syntactically last, but the RLM loop was semantically incomplete.

### 2. `(answer ...)` is taught as a position rule more than a readiness rule

Current prompt emphasizes:

```text
Terminal: `(answer …)` is the LAST top-level form of its iteration.
```

That is necessary, but insufficient. The model learned "last form is valid" and missed "answer means the task is complete".

The prompt should front-load this stronger rule:

```text
(answer ...) is a terminal COMMIT. Do not call it after exploration-only
forms. If this iteration's top-level forms read/search/list files for later
inspection, stop without answer; the next iteration will observe the results
in <journal>.
```

### 3. Iter-0 exploration guidance is correct but too soft

Current prompt says iter 0 can explore and then "read results in iter 1's `<journal>`". That is exactly right, but phrased as guidance, not a hard transition rule.

It should become a hard RLM invariant:

```text
If iter 0 contains top-level exploratory calls (`v/rg`, `v/cat`, `v/ls`,
conversation/history reads), do not call `(answer ...)` in that same
iteration unless the final answer is computed inside one lexical form from
values already bound in that form.
```

### 4. No anti-pattern for the observed bug

The prompt needs to show the exact failure mode:

```clojure
;; BAD: exploration-only iteration that terminates before observation
(def hits (v/rg ["openai-codex" "zai"] "extensions"))
hits
(answer "scanned")
```

And the paired correct form:

```clojure
;; GOOD iter 0: gather evidence and surface it to <journal>
(def hits (v/rg ["openai-codex" "zai"] "extensions"))
hits
```

Then next iteration:

```clojure
;; GOOD iter 1+: use observed journal results to continue or act
(def provider-src (v/cat "extensions/.../provider.clj"))
provider-src
```

### 5. Kitchen sink prompt — too much catalog before control flow

The current prompt still puts too much cognitive load into one block: role definition, iteration protocol, persistence rules, answer rules, iter-0 heuristics, system vars, aliases, and extension fragments.

For RLM, control flow must be the cache-hot prefix. Tool catalogs are secondary.

Priority order should be:

1. terminal/answer contract,
2. RLM state machine,
3. top-level form / journal observability rule,
4. canonical def/surface pattern,
5. error recovery,
6. tool/alias reference.

### 6. No priority order for conflicting obligations

The prompt does not clearly order obligations such as:

- user asked to fix code,
- repo says run `./verify.sh`,
- a tool call failed,
- `(answer ...)` must end the turn.

RLM needs a precedence table:

1. Safety / repo hard rules.
2. Preserve user data and avoid destructive actions.
3. If source changed, verify or explicitly report why verification could not run.
4. If a tool failed, handle/retry/explain before final answer.
5. Only then call `(answer ...)`.

### 7. Error state is underspecified

The prompt does not show what happens after:

- `v/cat` file not found,
- `v/edit` exact replacement fails,
- `v/rg` returns too many/truncated hits,
- command exits non-zero,
- a form throws midway through an iteration.

The model should be told:

```text
Errors are evidence. They appear in <journal>. In the next iteration, inspect
the error, narrow/retry, or explicitly explain the blocker. Do not hide an
unhandled tool error behind a final answer.
```

## 🟠 Major Issues

### 8. The prompt says `<journal>` keeps last 2 iterations, but code keeps 12

Current prompt:

```text
<journal>     last 2 iters
```

Current code:

```clojure
(def ^:const JOURNAL_KEEP_ITERS 12)
```

This mismatch is small but harmful. Either render the actual constant into the prompt or avoid the number:

```text
<journal>     recent iterations: thinking + comments + code + results
```

### 9. `Iter 0` is not explicitly defined

The prompt should define it once:

```text
Iter 0 is the first model reply for this user turn, before any new iN.K
results from this turn exist. Iter 1+ can observe iter 0's top-level results
inside <journal>.
```

### 10. Same-iteration computation boundary needs examples

The prompt currently teaches top-level exploration, but not the distinction between these two legal patterns:

```clojure
;; Multi-iteration pattern: result appears in next journal
(def hits (v/rg ["foo"] "src"))
hits
```

```clojure
;; Same-iteration pattern: value is consumed inside the same form
(let [hits (v/rg ["foo"] "src")]
  (answer (str "Found " (count (:hits hits)) " hits.")))
```

Without that distinction, the model thinks it can both surface top-level values and immediately reason over them in the same turn from outside the form.

### 11. Verification is not represented as an RLM phase

For coding tasks, the desired loop is not only `EXPLORE → ACT → ANSWER`. It is:

```text
EXPLORE → ACT → VERIFY → ANSWER
```

The prompt should say:

```text
After edits, run the narrowest useful check first. For this repo, full done
claims require ./verify.sh unless explicitly blocked or the user asked for a
pure analysis-only task.
```

### 12. Tool API reference should be lazy / cacheable

The tool reference rarely changes and should not compete with the control-flow contract. Keep only the minimum signatures in the core prompt; move full docs to extension docs / cacheable fragments.

## 🟡 Medium Issues

### 13. Extension injection is underexplained

The prompt says `[namespace: md → vis.ext.md]` but should explicitly say:

```text
Extension aliases such as v/, md/, z/ are preloaded in SCI. Do not require
them. Call their functions directly.
```

### 14. Iteration budget is unknown

The model should know it has room to loop:

```text
Prefer a few small iterations over one overloaded iteration. Typical coding
turns use 2-5 iterations: explore, observe, act, verify, answer.
```

This reduces premature compression into iter 0.

### 15. Return shapes are described but not operationalized

The prompt can avoid dumping full schemas, but it should show the operational rule:

```text
When a result is large/truncated, page/read narrower before concluding.
When a result is empty, verify the path/query before concluding absence.
```

## Chosen Direction: Full Functional RLM Rewrite

Decision: do the full rewrite, not a surgical opener patch.

The rewritten prompt should make Vis feel less like "chat with tools" and more like a functional Clojure state machine. The model's job is to construct and evolve state through small Clojure forms.

Core mental model:

```text
state₀ + user-request
  → emit forms
  → host evaluates forms
  → results/errors become journal facts
  → state₁ is visible next iteration
  → repeat until terminal answer
```

The prompt should repeatedly prefer this shape:

```clojure
(def observation (v/rg ["keyword"] "src"))
observation
```

or, for reusable logic:

```clojure
(defn summarize-hits [hits]
  {:count (count (:hits hits))
   :paths (->> (:hits hits) (map :path) distinct vec)})

(def summary (summarize-hits observation))
summary
```

The bare symbol after `def` / `defn` matters. It is not style noise. It is the journal-surfacing operation: it makes the value visible as `iN.K`, persists the symbol in the sandbox, and gives the next iteration concrete state to inspect.

First iteration default:

```text
Iter 0 is for understanding, planning, and creating/checking gates.
Do not rush to answer. Build initial observations and a plan state.
```

For non-trivial tasks, iter 0 should usually create a compact state value:

```clojure
(def task-state
  {:request TURN_USER_REQUEST
   :phase :understand
   :known []
   :questions []
   :gates [{:id :understand-problem :status :open}
           {:id :inspect-relevant-code :status :open}
           {:id :act-or-explain-blocker :status :open}
           {:id :verify-if-changed :status :blocked}]})

task-state
```

Later iterations update state:

```clojure
(def task-state*
  (-> task-state
    (update :known conj {:where "src/foo.clj" :what "..."})
    (assoc :phase :act)))

task-state*
```

Prompt rewrite goals:

1. Functional/data-first: model task progress as maps/vectors, not vague prose.
2. Journal-first: every important `def` is followed by the bare symbol.
3. State-machine-first: every turn moves through `UNDERSTAND → PLAN → EXPLORE → OBSERVE → ACT → VERIFY → ANSWER`.
4. Gates-first for non-trivial tasks: define open gates and close them with host-observed facts.
5. Verification-aware: after edits, verification is a state transition, not an afterthought.
6. Answer-last: `(answer ...)` only when state says gates are closed or blocked with evidence.

## Proposed Core Prompt Skeleton

Replace `CORE_SYSTEM_PROMPT` with something shaped like this:

```text
You are a Clojure RLM agent. You control a read/eval/observe loop.

CRITICAL: `(answer ARG)` is a terminal COMMIT. One accepted answer ends the
user turn. Call it only when the task is complete, verified when relevant, or
concretely blocked. Never use `(answer ...)` to say "scanned", "will inspect",
"next I will", or other progress-only messages.

RLM state machine:
  PLAN      choose the next smallest useful probe/action
  EXPLORE   run reads/searches/lists; surface values into <journal>
  OBSERVE   in the next iteration, read <journal> results/errors
  ACT       edit/write/run commands when the evidence supports it
  VERIFY    run targeted checks; for repo-level done claims obey repo rules
  ANSWER    final concise report with evidence, changed files, verification

Top-level observability rule:
  Each top-level form runs now, but its iN.K result is mainly for the NEXT
  iteration's <journal>. If you need to compute and answer in the same
  iteration, do it inside one lexical form:
    (let [x (v/rg ["foo"] "src")] (answer ...))

Exploration-only rule:
  If this iteration's top-level forms are exploratory (`v/rg`, `v/cat`,
  `v/ls`, history/conversation reads), normally do NOT call `(answer ...)`
  in the same iteration. Surface the values and stop; iter 1+ will observe
  them in <journal>.

Canonical exploration pattern:
  (def x (v/cat "src/foo.clj"))
  x

Bad pattern:
  (def hits (v/rg ["foo"] "src"))
  hits
  (answer "scanned")   ;; BAD: terminates before observing/acting
```

Then continue with host primitives, system vars, aliases, and extension docs.

## Prompt Patch Recommendations

| # | Change | Effort | Impact |
|---|---|---:|---:|
| 1 | Front-load `(answer ...)` as terminal commit/readiness rule | low | very high |
| 2 | Add explicit `PLAN → EXPLORE → OBSERVE → ACT → VERIFY → ANSWER` state machine | low | very high |
| 3 | Add top-level observability rule: top-level results are for next journal | low | very high |
| 4 | Add exact bad `answer "scanned"` anti-pattern | low | high |
| 5 | Add same-iteration lexical-form example with `let` | low | high |
| 6 | Add error-state recovery paragraph | low | high |
| 7 | Fix `<journal>` "last 2" mismatch | trivial | medium |
| 8 | Move tool catalog below control flow / make docs lazy | medium | high |
| 9 | Add verification as a named phase | low | high |

## Programmatic Gates Still Matter

Prompt fixes should be first because they are cheap and likely prevent the observed failure. But prompt alone is advisory. The runtime should eventually enforce the same contract with a programmatic turn ledger:

```clojure
{:observed [:read :search :tool-error]
 :acted? false
 :verified? false
 :open-gates [:observe-results :handle-tool-error :act-or-block]}
```

Then `(answer ...)` validation can reject premature terminal answers. The prompt and gates should share the same vocabulary (`EXPLORE`, `OBSERVE`, `ACT`, `VERIFY`, `ANSWER`) so the model sees the same contract the host enforces.

## Success Criteria

After the prompt patch, conversation `d5591095-b0b0-4f01-9066-9bdb46c17f41` should not repeat this shape:

```clojure
(def provider-clj (v/cat ".../provider.clj"))
provider-clj
(answer "scanned")
```

Expected shape:

```clojure
;; iter 0
(def provider-clj (v/cat ".../provider.clj"))
provider-clj
```

```clojure
;; iter 1+
;; inspect i0.K in <journal>, then read narrower / edit / verify / answer
```

If the model still terminates after exploration-only iterations, that is evidence to implement host-enforced gates, not more prompt prose.
