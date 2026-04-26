# Critique: Why Vis Loops in Circles

> **Premise (corrected).** Vis is intentionally not Pi. The design contract is
> a **projection-based RLM loop**: messages do **not** accumulate, no
> compaction is ever needed, and the LLM-visible context is rebuilt every
> iteration from a small, bounded re-projection of state. That contract is
> good. **The critique below is entirely within that contract.** Nothing
> proposed here violates it. The bug is not "we don't keep messages around";
> the bug is "the projection we ship is the wrong projection."
>
> Source-of-truth references:
>
> - `packages/vis-core/src/com/blockether/vis/loop/runtime/conversation/environment/query/iteration/core.clj`
> - `packages/vis-core/src/com/blockether/vis/loop/runtime/conversation/environment/query/core.clj`
> - `packages/vis-core/src/com/blockether/vis/loop/runtime/conversation/environment/core.clj`
> - `packages/vis-core/src/com/blockether/vis/loop/core.clj` (`CORE_SYSTEM_PROMPT`)
> - `extensions/editing/src/com/blockether/vis/ext/editing.clj` (the four built-in tools)

---

## 0. The single sentence

The model loops because the projection contains the **state shape** but not
the **work ledger**. It can see *what exists* (var-index), it can see *what
just happened in the last tick* (journal + prior_thinking), but it cannot
see **what it has already tried, what it has already eliminated, and what
it originally planned** — so every iteration it has to re-derive those from
a one-tick-deep echo of itself, and the only way it can persist anything
beyond that one tick is by manually `(def)`-ing it. Forgetting to def =
guaranteed re-execution. That's the loop.

---

## 1. The current projection, reduced to its essence

Trailing user block, rebuilt every iteration:

```
[iter N/M]
<prior_thinking> = previous iter's :thinking only, ≤4000 chars      </prior_thinking>
<journal>        = previous iter's :code blocks + results            </journal>
<var_index>      = every (def) ever, as fake-Clojure with NO values  </var_index>
[system_nudge] …
```

That is the **entire** memory the model has of its own work. Three slots,
all of them either single-tick or value-less. Everything else lives in:

- `*reasoning*`, `*answer*`, `*query*` SYSTEM vars (visible only as ellipses
  in `<var_index>`; recall = `(var-history)` round-trip).
- The SCI sandbox itself (must be queried by emitting code; values not
  inlined anywhere in context).

The trailing block is the projection. **The projection is missing four
critical surfaces.**

---

## 2. Why the loop loops — six structural causes

### 2.1 No attempts ledger

`<journal>` is one iter deep (`format-expression-results` over
`prev-expressions`). If iter 2 ran `(fs/grep-files "FOO" "src")` and got an
empty result, by iter 5 the model has zero record of having tried it.
Reasonably, it tries again.

The repetition-warning nudge fires at threshold **3** — i.e. the model has
to retry the same call **three** times before the loop tells it to stop.
That nudge is a band-aid on a missing data structure. The right fix is for
the loop to maintain a deduped, cumulative `<attempts>` ledger across iters
that lists every distinct `(tool-call args)` and its outcome (`:ok` / empty
/ error / N-results). That ledger is bounded (last K distinct entries, ~50
lines), projection-style, fits the RLM contract. Without it, the model is
*structurally* incapable of knowing what it tried two iterations ago.

Concretely: the call-counts atom (`call-counts-atom`, used only to drive
the nudge) already has the data. We just don't ship it back.

### 2.2 The original plan dies on iter 1

`build-prior-thinking` does `(take-last 1 chain)`. So at iter 5 the
`<prior_thinking>` block contains iter 4's thinking, which contains a
paraphrase of iter 3's thinking, which contains a paraphrase of iter 2's,
… The iter-0 thinking — which is where the model usually decomposes the
goal into a plan — is gone by iter 2.

Empirically this is the dominant cause of "going in circles": the model at
iter 5 has *no record of its own original plan*, only the most recent
tactical thought, so it re-plans from scratch. Re-planning at iter 5 with
`<journal>` showing only iter 4's results produces a different plan than
the one made at iter 0 with the original query in mind, and now the agent
oscillates between two plans neither of which is rooted in the original
decomposition.

Fix that costs nothing within the RLM contract: **two slots, not one.**
`<plan>` (sticky — the model sets it once and the loop carries it
verbatim until the model overwrites it via a tool/special form) plus
`<recent_thought>` (last iter as today). Together they're still a
constant-size projection.

### 2.3 `<var_index>` advertises existence but hides values

Every entry is `(def ^{:v N :s :l :t :map :n 12} foo ...)`. The body is
literal `...`. So when the model wants to use `foo` it has three options:
(a) re-derive `foo`'s value by re-running the code that produced it (the
*re-execution* failure mode), (b) emit `(var-history 'foo)` to fetch the
prior version (a *full LLM round-trip* that buys exactly one value), (c)
emit `foo` bare in `:code` and read the journal next iter (*another
round-trip*).

In all three cases the model pays a turn to look at data that is **already
on the server**, in `expression_state.result`, and could have been inlined
into the projection at near-zero token cost. A trivially better
`<var_index>` entry would be:

```
(def foo {:n 12 :v 3 :preview "{:status :open, :id #uuid \"...\", ...}"})
```

— first ~120 chars of `pr-str`, plus a 1-line schema (key list for maps,
element type for vecs, first/last for seqs). For values larger than that
threshold, fall back to the current ellipsis. This is still a projection;
still bounded; still no message accumulation. But it eliminates the entire
class of "I need to fetch my own var to remember what's in it."

### 2.4 Result pinning is a manual ritual the model is taxed for forgetting

The system prompt commands:

> Referenced by :answer / Mustache template → `(def x val)`.
> Needed >1 iteration ahead → `(def x val)`. Always.

i.e. the model is asked to *predict the future utility* of every
intermediate value and pin it manually. When it predicts wrong (which is
most of the time on novel tasks) the value vanishes after one tick and
the model re-runs the producing code. That re-run is the loop.

A projection-style fix that respects the contract: the loop **auto-pins**
values it has structural reason to expect will matter. Examples already
visible to the loop:

- File paths returned by `read-file` / `list-files` / `grep-files`
  (already metadata of the call). Ledger key: `:files-read`.
- Symbols whose previous version was used in this turn's :answer-mustache.
- Patches applied (`patch-file` targets) — agent will almost certainly
  refer back.
- `(grep ...)` patterns + match counts — auto-deduped attempts.

These don't need to live in the SCI sandbox; they live in a separate
"derived state" view that ships in the trailing block as a *small* table.
This is exactly the shape Pi's `details: { readFiles, modifiedFiles }`
serves; we just compute it ourselves at projection time, and ship a
freshly computed copy each iter (no accumulation needed).

### 2.5 No structured state slots — the loop is amnesiac about its own task

The trailing block has slots for `<prior_thinking>`, `<journal>`,
`<var_index>`. It has **no slot** for any of:

- **Open questions** ("does foo.clj contain a defmacro?")
- **Eliminated paths** ("approach via reflection — rejected because SCI
  has no class lookup")
- **Pending tool results** ("waiting on bigger read of util.clj")
- **Next concrete step** (one bullet)
- **Confidence in current answer** (carried in `:final.confidence` but
  thrown away between iters)

These are the surfaces a search-tree-aware agent needs. Without them, the
*only* place the model can record any of this is in free-form
`:thinking`, which then gets summarized into iter N+1's `:thinking`,
which gets summarized into iter N+2's, … Lossy compression of the very
thing that prevents loops.

Adding four named slots to the projection costs ~10 lines in
`build-iteration-context` and a tiny addition to the iteration spec
(`:plan`, `:open`, `:eliminated`, `:next` — all optional strings, all
sticky across iters unless the model overwrites them). Still bounded.
Still projection. Still RLM.

### 2.6 SYSTEM vars are present but inaccessible

`*query*`, `*reasoning*`, `*answer*` are stored, versioned, indexed — and
**rendered as ellipses** in the only place the model looks. To read them
the model emits `(var-history '*reasoning*)` or just `*query*`, sees the
journal next iter, then proceeds. That's a one-iter round trip to read
data the loop already has in hand.

The right move: inline the *current values* of SYSTEM vars in a tiny
`<system_state>` block at the top of the trailing chunk. They're at most a
few hundred chars each (`*query*` is the user message; `*answer*` is last
turn's final). The cost is rounding error; the savings is one round-trip
per recall.

---

## 3. The system prompt is a lecture, not a state machine

`CORE_SYSTEM_PROMPT` is ~3 KB of imperative narrative. Excerpted with my
critique inline:

### 3.1 The "STEP 1–4 every iteration" framing is what tells the model to forget

```
EVERY ITERATION:
  STEP 1 — READ. …
  STEP 2 — COMPUTE in :code. …
  STEP 3 — PERSIST or DON'T:
  STEP 4 — FINALIZE.
```

This is a 4-step *one-iteration* program. It does not describe the
*multi-iteration* shape of the task. There is no "STEP 0 — the plan you
made at iter 0", no "STEP 2.5 — what you've already tried", no "STEP 3.5 —
update your open-questions list". The prompt actively trains the model to
treat each iteration as a fresh ReAct cycle, which is exactly the
single-tick amnesia we're trying to escape.

### 3.2 Static rules dominate the signal

Hard rule lines like:

- ``Recursion: `letfn`, never `(let [f (fn [] (f))] ...)`.``
- ``Quote lists: `'(1 2 3)`. One complete expr per block.``
- ``Bare string literal = wrong. Prose → :answer.``
- ``Nested `#()` is illegal.``

These are *evergreen tips* — they help maybe 1% of iterations. They are in
the system prompt, which is cacheable, so token cost is cheap. **But they
crowd the attention budget against the dynamic state the model actually
needs.** Models trained on RLHF over-attend to imperatives. This prompt
reads as "thirty things you must do correctly" with the actual current
task attention-suppressed below all of it.

The fix is layering: **separate the evergreen Clojure-style tips into a
collapsible `<style>` section the model can pattern-match once per query**,
and put the active task-state at the *end* of the system prompt where
recency primes attention.

### 3.3 Conflicting guidance about when to `(def)`

From the system prompt:

> One-shot value used only this iter → bare expression in :code, no def.
> Referenced by :answer / Mustache template → `(def x val)`.
> Needed >1 iteration ahead → `(def x val)`. Always.

…then in the same prompt:

> DEF grep results. Results in a var survive; bare results vanish after the journal.

…and then `<var_index>` grows unboundedly as a result, and AGENTS.md
elsewhere worries about var-index bloat. The model is being asked to
**predict utility** with no help, **and** to keep the index small. These
two pressures pull in opposite directions; the model resolves the conflict
by either def-spamming everything (var-index bloats) or by not def-ing
(re-execution loops). There is no middle road exposed to the model.

The right division of labor: **the loop auto-pins (per §2.4); the model
manually pins only "non-derivable insight" (e.g. a specifically-shaped data
structure it built).** The system prompt should *not* tell the model to
def grep results. The loop should auto-record grep attempts in
`<attempts>` and let the var-sandbox stay user-conceptual.

### 3.4 "GROUNDING: :answer MUST come from <journal>, <var_index>, or tool values pulled this turn"

This rule is half-true and half-impossible:

- `<journal>` only contains iter N-1's results.
- `<var_index>` has no values.
- "Tool values pulled this turn" requires the model to re-pull every input
  to the answer in the very iteration it answers, even if it pulled them
  three iters ago.

The model's only compliant path is to re-run every supporting query in
the final iteration, then write the answer. That is **explicitly
prescribed re-execution**. The grounding rule guarantees a wasted final
iteration on most non-trivial tasks.

The fix: replace this rule with **"answer must reference either a
`<plan>` slot, a `(var-history)`-resolvable name, or a result whose ID
appears in `<attempts>` / `<journal>`"**, where attempts and journal hold
*addressable* iteration IDs (`i4.2`, `i7.1`) that survive across the
projection.

### 3.5 The system prompt has nothing about the iteration counter

The model gets `[iter 7/30]` in the trailing block but the system prompt
never tells it what to *do* with that information. When does it
finalize-with-low-confidence vs. ask for more iterations? Not specified.
The result: the model usually finalizes too early (when nudge fires at
N-2) or runs out of budget without ever finalizing (when nudge doesn't
fire because of `BUDGET_WARNING_WINDOW=2` cliffing).

A two-line addition to the system prompt — *"At ≥70% budget consumed,
either finalize or call `(request-more-iterations N)` with concrete
justification: cite `<plan>` items still open."* — would change behavior
materially.

---

## 4. The toolset is too narrow, and every default costs ≥1 extra iteration

Currently registered (per `extensions/editing/src/.../editing.clj`):

| Tool          | Default behavior                              | Ramification                              |
| ------------- | --------------------------------------------- | ----------------------------------------- |
| `read-file`   | 1500-char **preview**                         | Real reads = 2-iter (preview → re-read)   |
| `list-files`  | depth **2**, gitignore-respecting             | Deep tree = 2-iter (probe → re-list)      |
| `grep-files`  | RE2, structured maps                          | Decent — but no `--files-with-matches` mode, no glob filter |
| `patch-file`  | single SEARCH/REPLACE block                   | N-edit refactor = N tool calls = N iters  |

Plus the SCI sandbox (Clojure core, plus `zprint`, `clojure+`,
`charred.api`, `lazytest`, `fast-edn`). No `bash`. No `git`. No web. No
structural code search. No multi-edit.

### 4.1 Every "preview by default" tool is a ≥2-iter tax

`read-file` returns 1500 chars unless the model asks for a slice. Most
files in this repo are bigger. The agent reads, sees the truncation, then
issues a second `read-file` with offset/limit. **Two iterations per real
read.** Combined with the one-iter-deep journal, by the time the file
content is "in mind", three iters have elapsed and the model is already
forgetting why it asked.

Fix: default to "small file = full content; large file = full content
with summarized middle". A 30-line `summarize-large-file` heuristic on
the server costs 0 LLM iters. The current 1500-char universal preview
optimizes for token cost in a *single iter* and pays 2× LLM cost across
the loop. That trade was wrong.

### 4.2 No bash → the agent simulates bash

Every coding-agent task involves running tests, checking `git status`,
running the project, `wc -l`, `find … -mtime`, etc. Without bash the
model fakes these by composing fs/list-files + reduce + manual time
filtering. Each fake is a few SCI calls and one iteration. Pi has
`bash`; Claude.ai has `bash`; Cursor has `bash`. Vis without bash means
the agent's verification step is *itself a multi-iter sub-task*.

The objection "bash is unsafe" is correct in the abstract and irrelevant
in practice: an allowlist (`git status|diff|log|show`, `clojure -M:test`,
`bin/vis`, `wc`, `find`, `head`, `tail`, `cat`, `ls`, `pwd`, `which`)
covers ~90% of needs with bounded blast radius. Run-with-confirmation for
the long tail. **The current 100% denial is what produces the loop.**

### 4.3 No git tools → re-implementation in-loop

To answer "what did I change last commit", the agent must read every
file or … not answer. There is no `git status`/`diff`/`log`/`show`
exposed. Telegram and TUI users routinely ask repo-history questions
that take Pi one bash call and Vis a multi-iter goose-chase.

Fix: ship a `vis-ext-git` extension with `git-status`, `git-diff
[paths]`, `git-log [n]`, `git-show <ref>`. Read-only, safe, immediately
unlocks half of common tasks.

### 4.4 No multi-edit → N patches = N iters

`patch-file` does one SEARCH/REPLACE per call. A two-line refactor
across three files is **three** iterations minimum. Adding
`(fs/patch-files [{:path … :patch …} {:path … :patch …}])` collapses N
related edits to **one** iteration. This single tool would eliminate the
most common "five iters to do one obvious change" loop.

### 4.5 No structural code search

`grep-files` is regex-only. Finding "the function named `query!` defined
across the project" needs three queries (`defn query!`, `def query!`,
maybe `query!\\b`). Each is one iter via the journal-deep
"investigate-then-act" pattern. A `find-symbol` tool backed by clj-kondo
or even a simple `(defn-?[ ]+name)` regex with arity extraction would
collapse this to one call and inline the arglists.

### 4.6 No web fetch

For "what does the latest svar README say about ITERATION_SPEC", the
agent is blind. The model invents an answer. The user catches it. Many
loops are hallucinated documentation; a bounded `web-fetch <url>` (with
domain allowlist + 100KB cap) eliminates a huge share.

### 4.7 `:on-error-fn rescue-*` papers over UX bugs

`read-file`, `list-files`, etc. all carry `rescue-path-args` /
`rescue-grep-args`. The fact that we *need* per-tool rescue functions
means callers (i.e. the LLM) routinely pass wrong shapes. The right fix
is to surface the schemas in tool docs and reject early with a
**schema-shaped error** ("expected `[path offset? limit?]`, got
`[{:path …}]`"). The current rescue-then-coerce silently masks the
mistake, and the model never learns what the right shape is, so it makes
the same mistake on the next call. That's a micro-loop of its own.

### 4.8 Tool errors don't surface schemas

When a tool errors today, the model sees `ERROR: <message>`. It does
**not** see the tool's `:arglists` and `:examples`. Those are sitting on
the symbol metadata and shipped in the system prompt at startup, but by
iter 5 the system prompt is too far from the iter trailing-block to be
salient when an error fires. Errors should *re-attach* the relevant tool
spec inline in the journal. Two extra lines per error; one fewer
"why did this fail" iter.

---

## 5. The fixes that respect the RLM contract

Everything below is bounded, projection-style, no message accumulation.
Listed by ROI.

### 5.1 Replace single `<prior_thinking>` with two sticky+volatile slots

```
<plan>           (sticky; model writes via :plan field in spec)
<recent_thought> (last iter only; today's behavior)
```

Implementation: add `:plan` to `ITERATION_SPEC_BASE` as optional sticky
string; persist on iteration row; `build-iteration-context` carries the
last-written plan forward verbatim until overwritten. Iter 0 the plan
slot is empty; the model writes it; iters 1..N see it; the model can
amend at any iter. Cost: one new field, ~30 lines of code, zero
accumulation.

### 5.2 Add `<attempts>` ledger

Auto-derived from `expression_state` rows for the current query, deduped
by (head-form, args), capped at last 50 distinct entries:

```
<attempts>
i2.1 (fs/grep-files "FOO" "src")        → 0 hits
i3.1 (fs/read-file "src/core.clj")      → 4321 chars, 142 lines
i4.2 (fs/patch-file "src/core.clj" ...) → ok
…
</attempts>
```

The repetition-warning nudge can stay but at threshold **1** (immediate),
because the ledger is now self-evident — and any subsequent identical
call short-circuits to "already tried, see iN.K". This is the single
biggest anti-loop measure available.

### 5.3 Address results across iters (`iN.K` IDs)

`<journal>` already prefixes results with `[1] [2] [3]`. Promote those to
`[iN.K]` form and ship a small `<recent>` window of the last 2 iters'
results, not just last 1. Three lines in `format-expression-results`,
constant projection size, addressable references in `prior_thinking` and
`<attempts>` finally point somewhere real.

### 5.4 Inline values in `<var_index>` for cheap vars

`render-var-form` chooses between:

- `(def foo "preview")` when `pr-str` ≤ 200 chars.
- `(def foo ^{:v 3 :n 12 :keys [:a :b :c]} {:a … :b … :c …})` for maps
  ≤ 8 keys.
- `(def foo ^{:v 3 :n 1234} ...)` only for genuinely large values.

Status keywords already used (`:l :f :sys`) move from `:s` into the
top-level metadata under their full names so a Clojure-trained model
recognizes them: `:scope :live`, `:scope :forgotten`, `:scope :system`.
The pseudo-Clojure becomes real-Clojure-shaped enough that the model's
parser priors help instead of hurt.

### 5.5 Inline current values of SYSTEM vars

A 6-line `<system_state>` block at the top of the trailing chunk:

```
<system_state>
*query*     "Write a function to …"
*answer*    nil                                ; current turn
*reasoning* nil                                ; current turn
</system_state>
```

Eliminates `(var-history '*query*)` and friends. Costs nothing at
turn-start (`*query*` ≤ a few hundred chars), short-circuits to nil for
the in-progress turn.

### 5.6 Strip the system prompt by 60% and restructure as state machine

Remove every Clojure-style imperative ("`iterate` takes ONE-arg fn",
"Quote lists", etc.) into a collapsible `<style_tips>` block at the END.
Keep the agent identity + the iteration shape + the projection schema +
the grounding rule, in that order. Move the grounding rule from "must
come from journal/var_index" to **"every claim in :answer must cite a
slot reference (`<plan>`, `iN.K`, `*var*`) — anything else is
fabrication."** That makes grounding *checkable* at projection time
(literal substring match for slot tokens) and the model knows what
"grounded" actually means.

### 5.7 Tool surface

Concretely, in priority order:

1. **`vis-ext-bash`** — allowlisted shell. `git status|diff|log|show`,
   `clojure -M:test`, `bin/vis`, `wc|head|tail|cat|ls|find|pwd|which`.
   Read-only by default; mutating commands gated by an extension flag.
2. **`fs/patch-files [{:path … :patch …} …]`** — atomic multi-edit. Same
   SEARCH/REPLACE format, vector arg, all-or-nothing. One iter, N edits.
3. **`read-file` default = full content with elided middle for >5KB
   files.** Kill the 1500-char preview; it's a false economy.
4. **`fs/find-symbol <name>`** — clj-kondo-backed (already a dep
   somewhere) or regex fallback. Returns
   `[{:path … :line … :arglists … :doc …}]`.
5. **`web-fetch <url>`** — domain allowlist (`*.anthropic.com`,
   `*.openai.com`, `clojure.org`, `clojuredocs.org`, configured per
   project), 100KB cap.
6. **Tool error responses re-attach `:arglists` + `:examples`** for the
   tool that failed. Five lines in `exception->iter-err` for tool errors.
7. **Strict schema rejection over `rescue-*`** — tools refuse wrong
   shapes early with a structured ex-info that includes the expected
   shape verbatim. Faster failure; concrete teaching.

### 5.8 Loop-level

- **`call-counts-atom` should drive auto-deduplication, not just a
  nudge.** Identical `(tool-call args)` to a previous successful one
  short-circuits to the cached result with `[deduped from iN.K]`. No
  LLM round-trip for repeated calls.
- **`request-more-iterations` should be implicit** when a `<plan>` item
  remains uncompleted at budget exhaustion + the model sets
  `:final.confidence < 0.5`. Two lines in the budget check.
- **Auto-finalize hint** at confidence ≥ 0.8 with all `<plan>` items
  ticked: ship a `[system_nudge] Plan complete; you may finalize this
  iteration.` instead of waiting for budget pressure.

---

## 6. The summary table

| Surface           | Today                                       | What's missing (within RLM contract)                          | Failure mode it produces      |
| ----------------- | ------------------------------------------- | ------------------------------------------------------------- | ----------------------------- |
| `<prior_thinking>`| iter N-1 only, ≤4000 chars                  | sticky `<plan>` slot from iter 0                              | Loses original goal decomposition; re-plans from residue |
| `<journal>`       | iter N-1 only, code+results                 | `<attempts>` deduped ledger across iters; addressable IDs     | Re-runs same query 2-3× before nudge     |
| `<var_index>`     | name + shape tag + ellipsis                 | inline value preview for cheap vars; key-list for maps        | `(var-history)` round-trips for own data |
| SYSTEM vars       | versioned, indexed, hidden                  | inline current values in `<system_state>`                     | Round-trip to read `*query*` etc.        |
| Result pinning    | manual `(def)` or vanish                    | auto-pin file paths, patches, grep targets                    | "Forgot to def" → re-execution loop      |
| Plan / open / next| free-form `:thinking` only                  | dedicated optional spec slots                                 | Loses search-tree state across iters     |
| System prompt     | 3KB lecture; STEP 1-4 every iter            | state-machine framing; collapsible `<style>` block            | Re-derives loop shape every tick         |
| Grounding rule    | "from journal/var_index/tool calls"         | "cite a slot ID — `<plan>`/iN.K/*var*"                        | Forces re-execution in final iter        |
| Tool: read        | 1500-char preview default                   | full + elided-middle default; 1500-preview opt-in             | 2-iter cost per real read                |
| Tool: list        | depth=2 default                             | smart depth (full when small dir, lazy otherwise)             | 2-iter cost per real list                |
| Tool: patch       | one block per call                          | `patch-files` vector form                                     | N-iter cost per N-file refactor          |
| Tool: search      | regex grep                                  | `find-symbol`                                                 | 3-grep triangulation per symbol lookup   |
| Tool: shell       | none                                        | allowlisted `bash`                                            | Verification = sub-task = multi-iter     |
| Tool: git         | none                                        | `git-{status,diff,log,show}`                                  | Hallucinates repo state                  |
| Tool: web         | none                                        | allowlisted `web-fetch`                                       | Hallucinates external docs               |
| Tool errors       | message only                                | re-attach arglists+examples; reject wrong shapes early        | Repeats wrong-shape calls                |
| Loop dedup        | none (nudge at threshold 3)                 | call-counts auto-dedup short-circuit                          | 3 identical calls before nudge fires     |

The shape of the redesign is: **the projection should be a richer ledger
of work, not a richer ledger of state.** The current design is
state-heavy (var-index inflates) and work-light (no attempts, no plan,
no eliminated). Inverting that ratio is what stops the looping — and it
stops it without violating one byte of the RLM contract.
