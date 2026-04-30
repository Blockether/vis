# System Prompt Grilling

Analysis of  `src/com/blockether/vis/internal/prompt.clj`  — the assembled system prompt injected into every turn.

## 🔴 Critical Issues

### 1. Implicit state machine — the model must guess its own control flow

The iteration model (write → eval → journal → decide → answer) is a  **state machine**  but it's never called that. The model has no explicit states, no transition rules, and  **no error state** . What happens when a form throws? Does the iteration abort? Do partial  `def` s survive? Can the model retry?  *Silence.*  This is the #1 source of confusion-driven repetition loops.

### 2. Kitchen sink prompt — ~6000 tokens of dense instruction

Loading into every turn: role definition, iteration protocol,  `def`  persistence rules, answer constraint, iter-0 guidance,  **full `md/` API** ,  **full `v/` tool API** ,  **full `z/` zipper API** , project guidance, skills list, environment block. Research shows instruction adherence degrades sharply past ~2000–3000 tokens. The tool reference (which rarely changes) should be a cacheable prefix or on-demand doc, not baked into every system prompt.

### 3. Buried lede — the answer constraint is the 4th paragraph

`(answer ...)`  is the  **single most important behavioral constraint** . Every turn  *must*  end with exactly one. But this rule appears mid-prompt, sandwiched between  `def`  persistence and iter-0 heuristics. If the model remembers nothing else, it should remember this. Put it  **first** .

## 🟠 Major Issues

### 4. No priority ordering between conflicting instructions

AGENTS.md says "run  `./verify.sh`  before every commit." System prompt says "answer ends the turn." What if  `verify.sh`  fails? Does the model commit anyway? Answer with the failure? Retry?  *No guidance on priority.*  When instructions conflict, the model guesses. It should never have to guess.

### 5. Positive-only examples — the model has never seen failure

Every example is the happy path. No example shows:

- What to do when 
- `v/cat`
-  returns 
- `:truncated-by :limit`
-  (pagination is described but never demonstrated)
- What to do when 
- `v/edit`
-  fails because 
- `s`
-  isn't unique
- What to do when a 
- `def`
-  throws — can you re-def? Use a different name?
- What to do when the model emits a form that produces no iN.K result

Add 2–3  **anti-patterns** . They teach boundaries faster than positive examples teach capabilities.

### 6. Terminology inconsistency

| Term Used | Also Appears As | Notes |
| --- | --- | --- |
| iteration | `<journal>` | Used interchangeably with angle-bracket refs |
| var_index | `<var_index>` | Underscore vs angle brackets, never explained |
| top-level forms | `(let [...] (answer ...))` | Single example, boundary with 'wrapper forms' implicit |
| namespace alias | `[namespace: md → vis.ext.md]` | Convention introduced without explanation |

Pick one term per concept. Stick to it. The angle-bracket convention  `<journal>`  is introduced without explanation — is it XML? A placeholder? A reference to an external section?

## 🟡 Medium Issues

### 7. Extension injection is unexplained

The prompt says  `[namespace: md → vis.ext.md]`  but never says whether  `(require '[vis.ext.md :as md])`  is needed, harmful, or redundant. Add one line:  *"Namespaces md/, v/, z/ are pre-loaded. Do not require them. Call their fns directly."*

### 8. Iter-0 boundary is undefined

"Iter 0 answer fits when..." — but what  *is*  iter 0? It's never defined as "the first iteration, before seeing any iN.K results." The model must infer this. Define it explicitly.

### 9. Iteration budget is unknown

The model doesn't know if it has 2 iterations or 20. It either over-compresses (missing data) or over-explores (burning tokens). Add:  *"Turns typically get 3–5 iterations. Plan: iter 0–1 for exploration, last iter for the answer."*

### 10. Return shapes described in prose, not shown

`v/cat`  return shape is described inline.  `v/ls`  says "nested tree" with no depth examples.  `v/rg`  says  `{:hits [...] :truncated-by ...}` . The model discovers actual shapes by experiment. Either show concrete examples or point to a reference doc.

## 📊 Signal-to-Noise Metrics

| Metric | Value |
| --- | --- |
| Prose lines | 170 |
| Code lines | 16 |
| Code/prose ratio | 0,09 |
| Distinct tools mentioned | 80 |
| Total bytes | 21049 |
| Estimated tokens | 6014 |
| (answer ...) mentions | 12 |
| MUST mentions | 4 |
| FORBIDDEN mentions | 2 |

## ✅ What's Good

- The canonical 
- `(def x ...) / x`
-  pattern is excellent — teaches persistence + surfacing in one example.
- The 
- `(answer ...)`
-  shapes section (4 variants) is clear and covers real use cases.
- Separating SYSTEM vars (read-only) from tool fns (read-write) is clean.
- Project guidance in 
- `AGENTS.md`
-  (injected separately) keeps the base prompt decoupled from project-specific rules.
- The 
- `v/`
-  tool return shapes being explicit maps (not English prose) is the right call.

## 🎯 Priority Recommendations

| # | Recommendation | Effort | Impact | Detail |
| --- | --- | --- | --- | --- |
| 1 | Front-load the answer constraint | trivial | high | Make it paragraph 1. |
| 2 | Add error recovery section | low | high | 3–4 sentences on what happens on throw, how to retry, how to re-def. |
| 3 | Make the state machine explicit | medium | high | Define EXPLORING → PROCESSING → ANSWERING. State what's allowed in each. |
| 4 | Extract tool API reference to cacheable prefix | medium | high | Move md/, v/, z/ full API to a separate cacheable block. Keep only signatures in the main prompt. |
| 5 | Add 2–3 anti-patterns | low | medium | Show what broken looks like. |
| 6 | Clarify namespace injection | trivial | medium | One sentence: pre-loaded, don't require. |
| 7 | State iteration budget | trivial | medium | One sentence: typical range and planning guidance. |

*The core prompt is solid. The issues are all in the margins: what happens when things go wrong, and what the model can safely ignore. Fix those and you'll see fewer repetition loops and faster convergence.*