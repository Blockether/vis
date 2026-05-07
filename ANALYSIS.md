# Analysis of Vis conversation `207e223f-e8b1-4692-b284-c28a3ea1bbbb`

Compared against my fix notes in `STEPS.md` for task conversation `90f8f895-454d-42aa-b70d-291f9630177c`.

## 0. Executive verdict

The other agent did **not finish the fix**. It was interrupted after 7 iterations with:

- no patch applied;
- no regression tests added;
- no final answer;
- open unresolved intent/gate state;
- high token/cost burn: `234342` input tokens, `10373` output tokens, `$1.3919`;
- repeated malformed/duplicated fenced code blocks causing duplicated execution inside iterations.

It did some useful early work:

- loaded the `diagnose` skill;
- discovered relevant files;
- formed hypotheses;
- eventually called `chat/resume-conversation` for the supplied conversation.

But the core failure was feedback-loop failure. It inspected many files but did not build the small deterministic pass/fail renderer seam that exposed the bug:

```clojure
(chat/resume-conversation "90f8f895-454d-42aa-b70d-291f9630177c")
(render/format-answer-with-thinking ...)
(count/search rows for "SHELL bash" and "SEARCH any")
```

That seam was the difference between fixing and wandering.

---

## 1. State of the other agent in the conversation

### Runtime state

Conversation metadata from transcript:

- conversation id: `207e223f-e8b1-4692-b284-c28a3ea1bbbb`
- title: `Fix TUI duplicate entries`
- channel: `:tui`
- provider/model: `openai-codex` / `gpt-5.5`
- turn status: `:interrupted`
- iterations: `7`
- failures recorded: `0`
- answer: empty string

### What it actually did

Iteration summary:

1. Created title, classification, intent, plan, gate. Loaded diagnose skill with `(v/load-skill "diagnose")`. Ran broad `v/rg` searches.
2. Discovered nREPL port using `v/bash`. Read `chat.clj`, searched tests/docs. Emitted **two identical fenced blocks**, so the same probes ran twice.
3. Wrote hypotheses and attempted runtime reproduction through `chat/load-conversation`. This function does **not exist**. Also used brittle shell parsing for nREPL port and got `NO_NREPL_PORT`.
4. Read more code: `chat.clj`, `render.clj`, `footer.clj`, `screen.clj`.
5. Inspected render labels/header/state and again tried nonexistent `chat/load-conversation`, now getting `No such var: chat/load-conversation`.
6. Inspected tests/state/header and called `z/locators`, but still no patch. Again duplicated fenced blocks.
7. Finally used the correct public function `chat/resume-conversation`, but dumped huge output through shell/nREPL; output was truncated. It still did not call the render formatter to reproduce visible duplicate rows. Conversation was interrupted here.

### What it did not do

It did not:

- use `com.blockether.vis.ext.foundation.transcript/transcript`, despite `AGENTS.md` explicitly saying to start there for conversations;
- use `v/inspect` / `v/report`, despite the prompt advertising them;
- render the resumed conversation with `render/format-answer-with-thinking`;
- count duplicate rendered rows;
- inspect `extensions/common/vis-voice-parakeet`, where the recording duration race actually lived;
- patch code;
- add regression tests;
- run `./verify.sh --quick`;
- resolve the intent/gate.

### Biggest concrete mistakes

#### Mistake A: wrong reproduction seam

It kept inspecting DB/history projection (`chat.clj`) but the user-reported symptom was a **rendered TUI row duplication**. The right seam was the formatter:

- `chat/resume-conversation` rebuilds the trace;
- `render/format-answer-with-thinking` creates the visible rows;
- duplicate rows can be asserted with simple string counts.

The other agent stopped at trace/history data and never completed the render step.

#### Mistake B: wrong root-cause ranking

Its top hypothesis was:

> TUI rebuild-history emits both top-level expression summaries and nested tool result rows.

Actual cause from my run:

- `render.clj` emitted an explicit `tool-detail-badge` row for every tool;
- large shell/search result bodies also emitted their own collapse/summary row;
- for shell this produced `SHELL bash` + `▸ SHELL bash ... hidden`;
- for search this produced vague `SEARCH any` + the real search result body.

So the true root was render summary composition, not persistence/history duplication.

#### Mistake C: failed runtime command, then repeated around it

It tried:

```clojure
(chat/load-conversation ...)
```

But `chat.clj` exposes `resume-conversation`, not `load-conversation`. This error appeared in the transcript:

```text
No such var: chat/load-conversation
```

A stronger agent would immediately ask the namespace for public vars or use `rg "defn .*conversation"` and switch. It eventually did switch in iteration 7, but too late.

#### Mistake D: huge opaque output instead of bounded diagnostic values

When it finally used `chat/resume-conversation`, it printed a giant trace through `clj-nrepl-eval`, producing `235560` stdout chars and truncation. That made the result hard to reason over.

Better shape:

```clojure
(let [m (-> (chat/resume-conversation cid) :history second)
      text (render/format-answer-with-thinking ...)
      lines (str/split-lines text)]
  {:shell-bash (count (re-seq #"SHELL bash" text))
   :search-any (count (re-seq #"SEARCH any" text))
   :sample (filter interesting? lines)})
```

Small, pass/fail, deterministic.

#### Mistake E: recording-duration search missed the owning extension

It searched mostly under:

```text
extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui
```

But the first-open timer bug lived in:

```text
extensions/common/vis-voice-parakeet/src/com/blockether/vis/ext/voice_parakeet/core.clj
```

Actual issue:

- ticker future started before shared `state` contained `:recorder`;
- ticker loop checked `(:recorder @state)`;
- on first launch, ticker could run immediately, observe nil, exit, and duration stayed `00:00`.

The other agent focused on TUI header/footer/state and missed the source of recording status updates.

#### Mistake F: multiple fenced blocks executed duplicate work

Several raw LLM responses contained duplicate `clojure` fenced blocks. `svar/ask-code!` concatenated them, and Vis evaluated both. This doubled some probes and wasted context.

This is not only model error. It is also a host/interface weakness: if the protocol says exactly one fenced block, the host should reject or repair multiple blocks before evaluation, especially identical blocks.

---

## 2. Comparison between the two real runs

### My run from `STEPS.md`

What happened:

1. Loaded diagnose skill in the harness.
2. Used nREPL runtime state.
3. Resumed the supplied conversation with `chat/resume-conversation`.
4. Rendered it through `render/format-answer-with-thinking`.
5. Counted and printed actual duplicate rows.
6. Patched `render.clj`, `chat.clj`, `progress.clj`, and `voice_parakeet/core.clj`.
7. Added render/progress/voice regression tests.
8. Re-rendered original conversation and confirmed:
   - `SEARCH any` count became `0`;
   - duplicate standalone shell badge rows disappeared.
9. Ran quick verification and targeted tests.

Weakness in my run:

- Full `./verify.sh` was started but not completed; it was aborted during the test step.
- I initially used some direct text edits because the Pi harness did not expose project `z/patch` directly; for Clojure this is less ideal than native zipper editing.

But the key difference: I got a deterministic repro early.

### Other agent run

What happened:

1. Loaded diagnose skill but did not operationalize it.
2. Created intent/plan/gate overhead before reproducing.
3. Read many files.
4. Formed hypotheses.
5. Used nonexistent function `chat/load-conversation`.
6. Repeated duplicate fenced code blocks.
7. Eventually used `resume-conversation`, but printed huge truncated raw data.
8. Was interrupted before patch/test/verify.

### Output comparison

| Dimension | Other Vis agent | My Pi run |
|---|---|---|
| Reproduced user symptom | Not fully. It inspected data but did not render visible TUI rows. | Yes. Rendered supplied conversation and counted `SHELL bash` / `SEARCH any`. |
| Time-to-first real pass/fail signal | Never reached clean signal. | Early: renderer row counts. |
| Root cause | Drifted toward `chat.clj` history projection and TUI status/header. | `render.clj` badge/collapse duplication + voice ticker race. |
| Skill behavior | Loaded diagnose but ignored core loop. | Loaded diagnose and followed repro-first. |
| Tooling | Used `v/bash` + `clj-nrepl-eval` with brittle quoting/port parsing. | Used harness nREPL directly and small Clojure evals. |
| Code editing | None before interrupt. | Patched code and tests. |
| Verification | None. | Quick + targeted tests passed; full started but aborted. |
| Token efficiency | Very poor: 234k input tokens without fix. | Much lower effective loop; direct tool use. |

### Context comparison

The other agent had richer in-Vis context:

- `z/` structural tools;
- `v/inspect` / `v/report`;
- `TURN_ACCESSIBLE_SKILLS`;
- active extensions list;
- AGENTS instructions embedded;
- intent/gate machinery.

But richer context did not help because the prompt diluted priority. It had too many competing obligations:

- create intent/plan/gate;
- manage proof refs;
- obey one-fence protocol;
- inspect extensions;
- use z tools;
- load skills manually;
- use nREPL via shell;
- answer only after proof.

The model spent iterations satisfying ceremony rather than building the feedback loop.

---

## 3. Comparison of Pi system prompt vs Vis system prompt

### Pi strengths in this task

Pi harness instructions were operational and external:

- explicit developer tools: `read`, `bash`, `edit`, `write`;
- direct nREPL instruction from `AGENTS.md`;
- strong project rule: reproduce first;
- skill file was loaded by me as agent using normal tool read;
- no in-band intent/proof ceremony required before debugging;
- no code-fence emission protocol. Tool calls are separate from prose.

This reduces failure modes. There is no chance that a Markdown fence gets executed twice because Pi tool calls are not Markdown code blocks.

### Vis strengths

Vis has important capabilities Pi did not expose directly:

- structural Clojure editing (`z/patch`, locators, lsp-ish tools);
- in-app provenance and transcript APIs;
- intent/gate proof model;
- sandboxed repeatable observation loop;
- active skills as runtime values.

Those are good primitives. The failure is orchestration.

### Vis prompt problems

#### Problem 1: too much prompt, not enough priority

The captured Vis system prompt was ~42k chars before journal/tool context. It embeds:

- symbolic frame;
- state machine;
- intent/gate protocol;
- ref contract;
- many examples;
- AGENTS.md;
- skills index;
- extension docs.

The critical rule was present:

> For conversations: start with `com.blockether.vis.ext.foundation.transcript/transcript`.

But the model ignored it. That means the rule is not salient enough relative to the surrounding machinery.

#### Problem 2: examples violate the output-protocol vibe

The prompt says:

> reply with exactly one executable ```clojure fenced block only; never emit multiple fenced blocks or nested fences.

But the prompt itself contains many fenced examples. The model then emitted duplicate fenced blocks and even malformed fence glue in iteration 7.

Even if technically examples are in system text, they prime the wrong shape. For an RLM whose output parser executes code, examples should not use triple backtick fences inside the instruction text. Use XML-ish tags or indented pseudo-blocks instead.

#### Problem 3: intent/gate ceremony happens before reproduction

The prompt pushes INTENT before EXPLORE. For a bug report this caused iteration 1 to spend many blocks on:

- `conversation-title`
- `v/issue-intent!`
- `v/issue-plan!`
- `v/issue-gate!`
- `v/intents`

This is not wrong in general, but for debugging it competes with the most important first action: reproduce the bug. The system should allow a lightweight diagnostic intent or defer gate construction until after a repro exists.

#### Problem 4: no hard routing from user task to skill body

The prompt lists skills and says `(v/load-skill "name")`. It relies on the model to decide. The model did call it, but the body only becomes active context on the next iteration and is not enforced. Loading is not enough.

For bug reports, diagnose should be host-selected and injected as high-priority active context before first iteration, or at least before first EXPLORE.

#### Problem 5: Clojure runtime checks are awkward inside Vis

AGENTS says:

> Bash = discovery/process launch. Clojure behavior checks = nREPL.

In Pi, that was easy: direct `clj-nrepl-eval` calls through the harness.

In Vis, the model has to shell out through `v/bash`, then quote Clojure inside shell inside Clojure inside Markdown. That produced:

- bad port parsing;
- huge escaped commands;
- nonexistent var calls;
- output truncation.

Vis needs a first-class `v/nrepl-eval` or `v/repl-eval` tool with bounded printed output and automatic port discovery.

#### Problem 6: weak guard against repeated duplicate executable code

The loop has repeat-preflight across iterations, but not enough intra-iteration protection against duplicate fenced blocks. When `ask-code!` returned two identical fenced blocks, both got executed in one iteration.

Host should reject or deduplicate repeated top-level forms caused by multiple returned fences.

---

## 4. Skill loading: what actually happened and why it still failed

Important correction: Vis **did load** the diagnose skill.

Transcript block:

```clojure
(def diagnose-skill (v/load-skill "diagnose"))
```

Returned body from:

```text
/Users/fierycod/vis/.agents/skills/diagnose/SKILL.md
```

So the issue is not "Vis cannot load diagnose". The issue is:

1. **Manual activation**: the model has to choose to call `(v/load-skill "diagnose")`.
2. **Delayed effect**: loaded skill appears in `<active_skills>` on a later iteration.
3. **No behavioral enforcement**: after loading, nothing forces the model to build a feedback loop before further broad inspection.
4. **Skill body competes with huge context**: active skill is inserted alongside journal, var index, AGENTS, extension docs, and intent machinery.
5. **No progress check**: after several iterations without reproduction, host did not nudge "you loaded diagnose; where is the pass/fail loop?".

### Better skill behavior

For bug/fix/debug/perf requests, host should auto-activate diagnose before iteration 1.

Concrete design:

```clojure
(defn auto-skills-for-request [request]
  (cond-> []
    (re-find #"(?i)\b(bug|fix|broken|throw|fail|regression|debug|diagnose)\b" request)
    (conj "diagnose")))
```

Then inject:

```xml
<active_skills auto="true" priority="critical">
  ...full diagnose body...
</active_skills>
```

And add a progress nudge if no repro exists by iteration 2:

```text
You loaded diagnose but have not produced a concrete pass/fail reproduction. Stop broad reads. Build the smallest runnable repro now.
```

---

## 5. Proposed system prompt changes

### 5.1 Add task-specific first moves

Add a compact, high-priority block before the generic state machine:

```text
BUG/FIX OVERRIDE:
If TURN_USER_REQUEST reports broken/failing/duplicate/wrong behavior:
1. Do not edit in iteration 1.
2. Build a concrete reproduction through the real integration path.
3. If a conversation UUID is supplied, inspect it with v/inspect or transcript first.
4. For TUI rendering bugs, render through the pure TUI renderer seam before reading broad code.
5. Only after a failing signal exists may you patch.
No repro -> no diagnosis -> no fix.
```

### 5.2 Add UUID recipe

Because users often provide conversation ids:

```text
CONVERSATION UUID RECIPE:
If the user provides a conversation UUID:
- First call `(v/inspect "<uuid>")` or `(v/report "<uuid>")`.
- For TUI display bugs, derive a small renderer probe from the inspected trace.
- Never start by grepping for generic terms if the UUID can reproduce the issue.
```

### 5.3 Add TUI renderer recipe

```text
TUI RENDER BUG RECIPE:
Use pure render/state seams before launching the TUI:
- resume conversation via channel API or inspect transcript;
- call `render/format-answer-with-thinking-data` / `progress->lines-data`;
- assert on rows and `line-meta`;
- add/modify render tests.
```

### 5.4 Remove fenced examples from the system prompt

Replace triple-backtick examples with XML or quoted forms:

Bad inside system prompt:

````markdown
```clojure
(def x 1)
```
````

Better:

```xml
<example-clojure>
(def x 1)
</example-clojure>
```

Or:

```text
Example form: (def x 1)
```

The model output protocol itself uses fences, so the instruction prompt should avoid normalizing multiple internal fences.

### 5.5 Defer heavy intent/gate ceremony until after repro

Change state order for bug work:

```text
For bug/fix work: UNDERSTAND -> REPRO -> INTENT -> EXPLORE/ACT/VERIFY.
A lightweight intent may be created early, but plan/gate construction should not delay first reproduction.
```

Alternative invasive change: host creates a default hidden diagnostic intent automatically and lets the model focus on repro.

### 5.6 Add a hard "do not repeat failed symbol" rule

```text
If a REPL/tool call says `No such var` or `Unable to resolve symbol`, immediately inspect public symbols or grep definitions. Do not retry the same symbol name.
```

### 5.7 Add bounded-output rule

```text
Never print an entire conversation trace through shell. Return bounded derived diagnostics: counts, selected rows, paths, first/last samples.
```

---

## 6. Proposed extension/tooling changes

### 6.1 Add `v/nrepl-eval` or `v/repl-eval`

Current bad pattern:

```clojure
(v/bash "clj-nrepl-eval -p 7888 '(...huge quoted clojure...)'")
```

Proposed tool:

```clojure
(v/nrepl-eval {:port :discover
               :expr "(do (require '[...]) ...)"
               :timeout-ms 30000
               :max-output-chars 20000})
```

Benefits:

- no shell quoting hell;
- automatic port discovery;
- structured `{:value :stdout :stderr :error}`;
- bounded output;
- can reject giant print results and suggest summarizing.

### 6.2 Add TUI render diagnostic helper

Expose a public helper, maybe in TUI extension:

```clojure
(tui/diagnose-conversation-render cid opts)
;; => {:rows [...]
;;     :counts {:shell-bash 6 :search-any 0}
;;     :interesting-rows [...]}
```

Or foundation tool:

```clojure
(v/tui-render-report conversation-id)
```

This would directly encode the seam that fixed this bug.

### 6.3 Add public-vars inspection tool

The other agent called nonexistent `chat/load-conversation`. Add a simple tool:

```clojure
(z/publics 'com.blockether.vis.ext.channel-tui.chat)
;; => [{:name resume-conversation :arglists ...} ...]
```

Or improve `z/symbols` docs to make this first-class.

### 6.4 Add intra-iteration duplicate fence guard

In `svar/ask-code!` handling or `code-entries-preflight`:

- if more than one fenced block is returned, reject before eval;
- if multiple blocks are byte-identical, dedupe or reject;
- if raw response contains malformed fence glue like ``````clojure`, reject and ask for one clean block;
- record diagnostic with raw response preview.

Current raw-fence leak guard exists, but duplicate valid fenced blocks still become duplicate execution.

### 6.5 Add active skill progress nudges

If `diagnose` active and no concrete repro artifact exists after N iterations, inject:

```text
DIAGNOSE SKILL VIOLATION: no reproducible pass/fail signal yet. Stop reading broad code. Build a runnable repro now.
```

This can be detected heuristically by absence of:

- test command;
- render output count;
- failed assertion;
- explicit `:repro` var;
- transcript/render helper result.

### 6.6 Add `v/inspect` examples to foundation docs and prompt

The system prompt advertises `v/inspect cid? -> data`, but not strongly enough. Add examples near the UUID recipe:

```clojure
(def observed (v/inspect "90f8f895-454d-42aa-b70d-291f9630177c"))
(v/preview observed {:turns [[:iterations {:from 0 :to 3} ...]]})
```

### 6.7 Add voice-status contract docs

Document ownership:

- voice extension owns recording lifecycle and status text;
- TUI header displays `:status/set` events;
- ticker must start only after recorder state is visible;
- status events should carry either full text or structured `:started-at-ms` so TUI can synthesize duration.

Potential invasive improvement:

```clojure
{:op :status/set
 :id :voice/parakeet
 :text "Recording"
 :started-at-ms t
 :kind :recording}
```

Then TUI header can render elapsed from `now-ms`, independent of extension ticker. That removes a whole class of timer-stuck bugs.

---

## 7. Proposed strategy changes for Vis agents

### 7.1 Standard bug loop policy

For bug tasks:

1. Reproduce via real path.
2. Reduce to smallest seam.
3. Add failing test if seam exists.
4. Patch.
5. Run targeted test.
6. Re-run original repro.
7. Run quick verify.

No exceptions unless the user explicitly asks for analysis only.

### 7.2 Two-iteration deadline for repro

If no concrete reproduction by end of iteration 2:

- stop broad searching;
- output a diagnostic self-correction form;
- choose one seam and create a pass/fail probe.

### 7.3 Prefer data-shaped probes over transcript dumps

Bad:

```clojure
(v/preview whole-conversation-history)
```

Good:

```clojure
(def repro
  {:rows interesting-rows
   :counts counts
   :failed? (pos? (:search-any counts))})
repro
```

### 7.4 Use structural tools only after cause is localized

`z/locators` and `z/patch` are excellent once the edit target is known. But they do not replace reproduction. The other agent used structural tools as late exploratory inspection and never reached patch.

Recommended order:

```text
runtime repro -> source localization -> z/locators -> z/patch -> reload/test
```

### 7.5 Treat tool errors as branch points, not noise

`NO_NREPL_PORT` and `No such var: chat/load-conversation` were decisive evidence. The next step should have been symbol discovery or alternate API, not more broad inspection.

---

## 8. Concrete critique of the other agent as a structural Clojure agent

It had the right native Clojure tooling available:

- `z/locators`
- `z/symbols`
- `z/patch`
- `z/diagnostics`
- `v/inspect`
- `v/report`

But it did not use them in the order that matters.

Structural editing is useful only after you know what expression to change. Here the decisive expression was:

```clojure
tool-detail-badge
```

and the decisive branch was the rendering of `badge-entry` in `format-iteration-entry-entries`.

The other agent saw `tool-detail-badge`, but did not run a render test fixture that would prove changing it fixes the UI. It stayed in inspection mode.

A structural Clojure agent should have done:

1. `v/inspect` or `chat/resume-conversation`.
2. `render/format-answer-with-thinking-data` on that trace.
3. `z/locators` for `tool-detail-badge` and `badge-entry` region.
4. `z/patch` minimal edits.
5. render test.
6. re-render supplied conversation.

It did only parts of steps 1 and 3.

---

## 9. Invasive architectural recommendations

### 9.1 Separate "agent protocol" from "Clojure examples"

Use a non-Markdown transport for model code outputs if possible. If not possible, system prompt examples must never use the same code-fence syntax expected from the model.

### 9.2 Host-enforced workflow modes

Instead of relying only on prose:

```clojure
{:mode :diagnose
 :required-next [:repro]
 :forbidden-next [:patch :answer]}
```

The host can enforce:

- no patch before a repro marker;
- no final answer before verification marker;
- no broad read after repeated failures without a changed hypothesis.

### 9.3 First-class repro artifacts

Add a `v/repro!` helper:

```clojure
(v/repro! {:name "duplicate TUI shell/search rows"
           :command-or-fn "render conversation 90f8..."
           :observed {:shell-bash 12 :search-any 10}
           :expected {:shell-bash 6 :search-any 0}})
```

Then the host can know whether diagnose is being followed.

### 9.4 Public app diagnostic APIs for common seams

Create helpers for common bug classes:

- `vis/tui-render-conversation`;
- `vis/provider-diagnostics`;
- `vis/voice-status-snapshot`;
- `vis/conversation-render-counts`;
- `vis/test-namespace-for`.

Missing diagnostic views caused the other agent to shell out and drown in raw data.

### 9.5 Tool output budget should prefer computed summaries

If `v/bash` output exceeds cap, return structured warning that says:

```text
Output truncated. Do not retry with a bigger cap. Rerun with a summarizing expression.
```

The other agent used `max-output-chars 80000` and still got truncation. Bigger cap was not the solution.

---

## 10. Proposed docs changes

### Foundation extension docs

Add a "conversation debugging" section:

```clojure
(def data (v/inspect "<conversation-id>"))
(v/report "<conversation-id>")
```

And explicitly say:

- supplied UUID beats grep;
- use bounded summaries;
- use transcript/render helpers before storage poking.

### TUI extension docs

Add "pure render test seam" section:

```clojure
(render/format-answer-with-thinking-data answer trace width settings nil false opts)
(render/progress->lines-data progress width settings extra)
```

Explain when to test:

- duplicate rows;
- labels;
- collapse summaries;
- metadata/click rows;
- progress spinner/duration.

### Clojure extension docs

Add "from locator to patch" examples for private fns:

```clojure
(def locs (z/locators path {:source-contains "defn- tool-detail-badge" :limit 5}))
(z/patch [{:path path :search (:locator (first locs)) :replace new-form}])
```

Also add public-var discovery examples to avoid nonexistent `chat/load-conversation` calls.

### Skills docs

Clarify:

- `v/load-skill` loads full body but does not itself enforce the workflow;
- after loading a skill, the next iteration must explicitly obey its phase checklist;
- bug reports should auto-load diagnose if host supports it.

---

## 11. Bottom line

The other agent failed because it had tools but lacked a tight feedback loop. It treated the task as broad structural code archaeology instead of a reproducible UI-rendering bug.

Real fix path was short:

1. resume supplied conversation;
2. render it through TUI pure renderer;
3. count duplicate rows;
4. patch render summary policy;
5. fix voice ticker race in voice extension;
6. add regression tests;
7. re-render original conversation.

Vis can become much better than Pi for this class of work, but only if the host/prompt/tooling force this order:

```text
repro artifact first -> structural edit second -> proof third
```

Right now Vis has excellent structural editing primitives, but the prompt and tool surfaces let the model spend seven expensive iterations before reaching a clean repro.

---

## 12. Addendum: z/locators, prompt clutter, extension metadata, nucleus, and bash-vs-structure critique

This addendum responds to the follow-up question: what did the `z/locators` calls actually do, does the extension metadata mess in responses make sense, does the nucleus help, and which Vis code should change.

### 12.1 What the other agent's `z/locators` did

The other agent called:

```clojure
:z/locators "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj"
{:source-contains "defn- tool-detail-badge" :limit 5}

:z/locators "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj"
{:source-contains "tool-result-detail" :limit 10}
```

That was directionally correct: use structural Clojure locators before editing Clojure. But in practice it happened too late and did not lead to a patch.

#### What `z/locators` is good at

`z/locators` returns rows shaped like:

```clojure
{:tag     :list
 :value   '(defn- tool-detail-badge ...)
 :locator "(defn- tool-detail-badge ...)"
 :source  "(defn- tool-detail-badge\n  [detail] ...)"
 :span    [[line col] [line col]]
 :path    ".../render.clj"
 :index   1234}
```

Those rows are good when the agent already knows the function/form to patch. The ideal workflow was:

```clojure
(def loc (-> (z/locators render-path {:source-contains "defn- tool-detail-badge" :limit 1})
             :result first))
(z/patch [(assoc loc :replace new-tool-detail-badge-source)])
```

or for the badge-entry region:

```clojure
(z/locators render-path {:source-contains "badge-entry (when tool-badge" :limit 5})
```

#### What went wrong in this run

The locators were used as another **inspection dump**, not as an edit bridge.

Worse, one transcript result showed locator rendering like:

```clojure
{:tag :list,
 :value (defn- rebuild-history ... #:vis{:ref :depth-exceeded} ...),
 :locator "(defn- rebuild-history ...)", ...}
```

That is a problem for model cognition:

- `:value` is too large and gets depth-collapsed with `#:vis{:ref :depth-exceeded}`.
- `:locator` and `:source` duplicate each other.
- If rendered through generic preview, the row becomes a large EDN blob instead of a focused patch instruction.
- The model sees structural noise but not an obvious next action.

For the other agent, `z/locators` did not create momentum. It became one more thing in the journal.

### 12.2 Should locator rows include all fields?

For tool payload storage: yes, keep rich locator rows.

For model-facing rendering: no. The default rendering should be task-oriented and patch-oriented.

Current code in `extensions/languages/clojure/src/com/blockether/vis/ext/lang_clojure/patch.clj`:

```clojure
(defn- locator-row
  [zloc]
  (when (z/sexpr-able? zloc)
    (try
      (let [v (z/sexpr zloc)]
        {:tag     (z/tag zloc)
         :value   v
         :locator (pr-str v)
         :source  (z/string zloc)
         :span    (z/position-span zloc)})
      (catch Throwable _ nil))))
```

Problem: `:value` can be huge. It is useful internally, but harmful in journal/TUI previews.

Proposed change: keep `:value` in payload if needed, but add a compact display row and prefer `:source`/`:locator` only.

```clojure
(def ^:private locator-source-preview-chars 1200)

(defn- locator-row
  [zloc]
  (when (z/sexpr-able? zloc)
    (try
      (let [v      (z/sexpr zloc)
            source (z/string zloc)]
        {:tag            (z/tag zloc)
         ;; Keep full sexpr only for programmatic use. Rendering should not show it by default.
         :value          v
         :locator        (pr-str v)
         :source         source
         :source-preview (if (> (count source) locator-source-preview-chars)
                           (str (subs source 0 locator-source-preview-chars)
                                " …<+" (- (count source) locator-source-preview-chars) " chars>")
                           source)
         :span           (z/position-span zloc)})
      (catch Throwable _ nil))))
```

Current renderer:

```clojure
(defn- render-locators-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (tool-error-text tool-result)
    (let [rows (:result tool-result)
          shown (take 40 rows)
          body (->> shown
                 (map (fn [{:keys [tag locator source span]}]
                        (str (pr-str span) " " tag " " locator " <- " source)))
                 (str/join "\n"))]
      (md/join
        (md/p "Found" (count rows) "zipper locator(s).")
        (when (seq shown)
          (md/code-block "text" (preview-text body)))))))
```

Problems:

- Up to 40 rows is too much by default.
- It prints both `locator` and `source` on one line; for large forms this is unreadable.
- It does not show the immediate next action (`z/patch`).
- It does not distinguish exact one result vs many results.

Proposed renderer:

```clojure
(defn- render-locators-result
  [{:keys [tool-result]}]
  (if-not (:success? tool-result)
    (tool-error-text tool-result)
    (let [rows      (vec (:result tool-result))
          shown     (take 8 rows)
          one?      (= 1 (count rows))
          row-lines (->> shown
                      (map-indexed
                        (fn [i {:keys [path tag span source-preview source locator]}]
                          (str (inc i) ". " path " " (pr-str span) " " tag "\n"
                               (or source-preview source locator)))))
          patch-hint (when one?
                       (str "Patch hint:\n"
                            "(z/patch [(assoc loc :replace <new-source>)])"))]
      (md/join
        (md/p "Found" (count rows) "zipper locator(s)."
              (when (:truncated? (:provenance tool-result)) " Narrow filters before patching."))
        (when (seq row-lines)
          (md/code-block "text" (str/join "\n\n" row-lines)))
        (when patch-hint
          (md/code-block "clojure" patch-hint))))))
```

Even better: return an edit-ready compact result when `:limit 1` and exactly one match:

```clojure
{:result [{:path p :search {:span span :source source :locator locator}}]}
```

Then docs can say:

```clojure
(def loc (first (:result (z/locators path {:source-contains "tool-detail-badge" :limit 1}))))
(z/patch [{:path (:path loc) :search loc :replace new-source}])
```

### 12.3 `z/locators` vs `z/symbols` vs xref strategy

The other agent used `z/locators` as a search tool. That is okay but not ideal.

Better structural strategy for this exact task:

1. Runtime repro says duplicate rows come from `tool-detail-badge` and `badge-entry`.
2. Use `z/locator-for-symbol` for function-level edit:

```clojure
(z/locator-for-symbol "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj"
                      'tool-detail-badge)
```

3. Use `z/locators` with `:source-contains` for local let binding:

```clojure
(z/locators render-path {:source-contains "badge-entry (when tool-badge" :limit 3})
```

4. Patch.

The missing part is that Vis did not teach this as a compact decision tree. The prompt currently says many available z tools, but does not say:

```text
For private function replacement: z/locator-for-symbol first.
For local binding/body region: z/locators with source-contains.
For call graph uncertainty: z/who-calls or z/calls-who.
Do not call z/locators before a runtime repro names a target.
```

Add that.

---

## 13. Extension metadata clutter: does it make sense?

Short answer:

- In persistent provenance: yes.
- In every model-facing result/journal/TUI row: no.

The transcript shows tool results like this in ordinary previews:

```clojure
:provenance {:op :v/preview,
             :started-at-ms ...,
             :finished-at-ms ...,
             :duration-ms 0,
             :tool {:sym preview, :call "v/preview", :alias v},
             :extension {:license "Apache-2.0",
                         :author "Blockether",
                         :alias v,
                         :kind "foundation",
                         :version "0.7.0",
                         :doc "Foundation `v/`: inspect/report, ...",
                         :registry-id v,
                         :namespace com.blockether.vis.ext.foundation.core,
                         :owner "vis"},
             :source {:paths [...], :mtime-max ..., :hash-sha256 "..."}}
```

This is too much for model-facing work. It bloats:

- `<journal>`;
- TUI display;
- transcript previews;
- model attention.

Most of it is irrelevant while debugging a TUI row bug. The model needs:

```clojure
{:op :v/preview :status :done :duration-ms 0}
```

Maybe nested tool identity:

```clojure
{:tool {:sym 'preview :call "v/preview"}}
```

It does not need extension license, author, owner, source hash, full doc string, and source paths on every tool result.

### 13.1 Current code that injects metadata into every tool result

Current code in `src/com/blockether/vis/internal/extension.clj`:

```clojure
(defn- enrich-tool-result-provenance
  [ext sym-entry result]
  (if (tool-result? result)
    (let [ext-prov (extension-provenance ext)]
      (merge-provenance
        result
        {:tool      (cond-> {:sym  (:ext.symbol/sym sym-entry)
                             :call (tool-call-name ext (:ext.symbol/sym sym-entry))}
                      (get-in ext [:ext/ns-alias :alias])
                      (assoc :alias (get-in ext [:ext/ns-alias :alias])))
         :extension (dissoc ext-prov :source-paths :source-mtime-max :source-hash-sha256)
         :source    {:paths       (:source-paths ext-prov)
                     :mtime-max   (:source-mtime-max ext-prov)
                     :hash-sha256 (:source-hash-sha256 ext-prov)}}))
    result))
```

This is fine for DB-level provenance, but not for display. The missing abstraction is **provenance projection by surface**.

### 13.2 Proposed provenance projection

Add in `extension.clj`:

```clojure
(defn compact-provenance
  "Model/TUI-facing provenance projection. Keeps identity and lifecycle,
   drops package metadata and source hashes. Full provenance remains stored."
  [provenance]
  (when (map? provenance)
    (cond-> (select-keys provenance
              [:op :op-class :presentation-kind :color-role
               :target :status :duration-ms :started-at-ms :finished-at-ms
               :ref :iteration :form-position :form-count
               :spec :paths :hit-count :truncated-by
               :command :cwd])
      (:tool provenance)
      (assoc :tool (select-keys (:tool provenance) [:sym :call :alias])))))
```

Then use it in journal/TUI renderers, not necessarily in persistence.

### 13.3 Current default tool rendering can leak too much

Current `render-value` in `src/com/blockether/vis/internal/extension.clj`:

```clojure
(defn render-value
  "Default renderer for plain-value symbols. Returns string representation
   of the tool result. Attached automatically by `vis/symbol` when no
   explicit `:render-fn` is provided."
  [{:keys [tool-result]}]
  (if (map? tool-result)
    (if-let [result (:result tool-result)]
      (pr-str result)
      (pr-str tool-result))
    (if (string? tool-result)
      tool-result
      (pr-str tool-result))))
```

This is mostly okay when `:result` exists. But when `:result` is nil or falsey, it can print the whole tool envelope. Also false and nil results are legitimate values.

Proposed safer default:

```clojure
(defn render-value
  "Default renderer for tool results. Render payload only; never dump full
   extension/source provenance by default."
  [{:keys [surface tool-result]}]
  (let [payload? (and (map? tool-result) (contains? tool-result :result))
        value    (if payload? (:result tool-result) tool-result)]
    (cond
      (string? value) value
      :else (pr-str value))))
```

This uses `contains?`, not truthiness.

### 13.4 `TURN_ACTIVE_EXTENSIONS` is too rich

Current `extensions-snapshot` doc says the model gets:

```clojure
:version :author :owner :license :registry-id
:source-paths :source-mtime-max :source-hash-sha256
:symbols :docs
```

Current implementation:

```clojure
(defn extensions-snapshot
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [provenance (extension/extension-provenance ext)
                  registry-id (:registry-id provenance)
                  doc-names   (try (if registry-id
                                     (extension/extension-doc-names registry-id)
                                     [])
                                (catch Throwable _ []))]
              (assoc provenance
                :symbols (mapv :ext.symbol/sym (:ext/symbols ext))
                :docs    (vec doc-names)))))))
```

For a human/debugging agent, `:source-hash-sha256`, `:source-mtime-max`, author/license, and source paths on every turn are mostly noise. Keep them in `(v/extensions)` or a detailed tool, not in always-bound `TURN_ACTIVE_EXTENSIONS`.

Proposed compact version:

```clojure
(defn extensions-snapshot
  "Compact model-facing extension snapshot. Detailed source/provenance is
   available through `(v/extensions)` / `(v/extension-docs ...)`."
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [prov        (extension/extension-provenance ext)
                  registry-id (:registry-id prov)
                  doc-names   (try (if registry-id
                                     (extension/extension-doc-names registry-id)
                                     [])
                                (catch Throwable _ []))]
              (cond-> {:namespace (:namespace prov)
                       :alias     (:alias prov)
                       :kind      (:kind prov)
                       :doc       (:doc prov)
                       :registry-id registry-id
                       :symbols   (mapv :ext.symbol/sym (:ext/symbols ext))
                       :docs      (vec doc-names)}
                (nil? (:alias prov)) (dissoc :alias)
                (nil? (:kind prov))  (dissoc :kind)
                (nil? (:doc prov))   (dissoc :doc)))))))
```

If source freshness matters, expose:

```clojure
(v/extensions {:detail :source})
```

or:

```clojure
(v/extension-provenance 'v)
```

### 13.5 Active skills are wrapped in `<extensions>` — confusing

Current code in `src/com/blockether/vis/internal/prompt.clj`:

```clojure
(defn- active-skills-block
  [environment]
  (when-let [skills (some-> (:active-skills-atom environment) deref vals seq)]
    (str "<extensions>\n"
      "<active_skills count=\"" (count skills) "\">\n"
      (str/join "\n\n" (map format-active-skill (sort-by :name skills)))
      "\n</active_skills>\n"
      "</extensions>")))
```

This is semantically wrong. Skills are not extensions. Wrapping them in `<extensions>` makes prompt sections ambiguous and can encourage the model to conflate extension metadata with active skill bodies.

Proposed change:

```clojure
(defn- active-skills-block
  [environment]
  (when-let [skills (some-> (:active-skills-atom environment) deref vals seq)]
    (str "<active_skills count=\"" (count skills) "\">\n"
      (str/join "\n\n" (map format-active-skill (sort-by :name skills)))
      "\n</active_skills>")))
```

Also add an attribute for auto/manual activation:

```clojure
<active_skills count="1" activation="auto">
```

### 13.6 Preview/rendering should suppress envelope by default

Current foundation preview code uses `payload-map`:

```clojure
(defn- payload-map
  [value]
  (if (and (map? value)
        (or (extension/tool-result? value) (contains? value :provenance))
        (some value [:result :stdout :stderr :error]))
    (select-keys value [:result :stdout :stderr :error])
    value))
```

Problem: `(some value [:result :stdout :stderr :error])` tests truthy values. It fails when `:result` is `nil`, `false`, `0`, or empty string. Then full envelope can leak.

Proposed change:

```clojure
(defn- contains-any?
  [m ks]
  (boolean (some #(contains? m %) ks)))

(defn- payload-map
  [value]
  (if (and (map? value)
        (or (extension/tool-result? value) (contains? value :provenance))
        (contains-any? value [:result :stdout :stderr :error]))
    (select-keys value [:result :stdout :stderr :error])
    value))
```

Same principle: do not let provenance envelope dominate display.

---

## 14. Does the nucleus make sense?

The nucleus has strong ideas:

```text
λ operate(x). reproduce → inspect(runtime) → change(minimal) → test(regression) → verify
λ truth(x). runtime > source > docs > assumption
λ fix(bug). reproduce(minimal) → trace(cause) → fix(structural) → regression_test | ¬repro → ¬diagnosis
```

Those are exactly right. The failure is not the ideals. The failure is enforcement and signal-to-noise.

### 14.1 What works

Good parts:

- `runtime > source > docs > assumption` is correct.
- `¬repro → ¬diagnosis` is correct.
- `edit -> reread -> reload -> verify` is correct.
- Headless-safe terminal rule is correct.
- Clojure zipper edit preference is correct.

### 14.2 What does not work

#### Too symbolic for failure recovery

The lambda/nucleus framing is compact for humans who already know it, but weak for a model under long context pressure. In the failed run, the model had the rule but ignored it.

The prompt needs operational tripwires, not only principles.

Bad as only rule:

```text
λ fix(bug). reproduce(minimal) → trace(cause) → fix(structural)
```

Better with enforceable checklist:

```text
For bug tasks, by end of iteration 2 you must have one of:
- failing test;
- rendered UI row count showing wrong output;
- command output showing exact failure;
- explicit statement that no repro is possible and why.
If none exists, stop broad inspection and build repro now.
```

#### Nucleus competes with intent/proof ceremony

The nucleus says reproduce first, but the state matrix says INTENT before EXPLORE for evidence work. In practice the model did intent/plan/gate first. That contradicts the debugging nucleus.

Fix: bug mode should reorder:

```text
UNDERSTAND -> REPRO -> INTENT -> EXPLORE -> ACT -> VERIFY
```

or create intent automatically outside the model.

#### "Caveman terse" did not happen

The prompt includes caveman style in AGENTS, but the model generated large verbose maps and repeated code. Style rules are being drowned by machinery.

Fix: add a token-budget nudge tied to observed behavior:

```text
Your last iteration emitted duplicate code blocks / huge output. Next iteration: max 3 top-level forms, each returning bounded data.
```

#### "Human ⊗ Vis ⊗ Workspace ⊗ REPL" is accurate but not actionable

It is poetic, but the agent needed:

```text
Use `v/inspect` for UUIDs.
Use render pure seam for TUI row bugs.
Use `v/nrepl-eval`, not shell quoting.
```

Keep the nucleus as a banner, but move actionable recipes above it or immediately after it.

### 14.3 Proposed nucleus rewrite

Current symbolic nucleus can stay, but add a concrete control plane:

```text
BUG MODE HARD RULES:
- Supplied UUID => inspect it before grep.
- UI/TUI symptom => render pure seam before source archaeology.
- No patch before repro artifact.
- No broad search after two failed probes; change seam.
- Every tool error changes the next action; do not retry same call.
```

Then keep:

```text
λ fix(bug). repro_artifact → trace(cause) → structural_patch → regression → original_repro_passes
```

---

## 15. Bash vs structural editing and thinking

### 15.1 Bash was overused for Clojure behavior checks

The failed agent used `v/bash` to run `clj-nrepl-eval` commands with nested quoting. That created multiple avoidable errors:

- brittle nREPL port parsing;
- `NO_NREPL_PORT` due bad awk pattern;
- `No such var: chat/load-conversation` buried in stderr;
- giant truncated stdout;
- shell escaping inside Clojure inside Markdown.

The project guidance says:

```text
Bash = discovery/process launch. Clojure behavior checks = nREPL.
```

But Vis only gives the model `v/bash`, not a first-class nREPL tool. So the model had to violate the spirit of the rule.

### 15.2 Structural editing was available but not reached

The agent called `z/locators`, but never `z/patch`. That is not a tooling failure alone. It is a strategy failure: it did not know what to patch because it did not reproduce at the renderer seam.

Correct sequence:

```text
runtime repro -> one render predicate fails -> z locator for exact function -> z patch -> test
```

Incorrect sequence from failed run:

```text
broad grep -> read many files -> hypotheses -> bad nREPL shell -> more reads -> z locators -> no patch
```

### 15.3 Bash should be blocked or nudged for Clojure nREPL evals

`v/bash` already blocks shell-driven Clojure/EDN source edits:

```clojure
(throw (ex-info "Refusing v/bash shell edit of Clojure/EDN source; use z/patch after z/locators or z/symbols."
         {:type :ext.foundation.editing/bash-clojure-source-edit-blocked
          :reason :use-z-patch
          :command-preview ...}))
```

Add similar warning for nested `clj-nrepl-eval`:

```clojure
(when (and (str/includes? command "clj-nrepl-eval")
        (> (count command) 240))
  (throw (ex-info "Use v/nrepl-eval for Clojure runtime checks; v/bash is only for process boundaries."
           {:type :ext.foundation.editing/bash-nrepl-eval-discouraged
            :reason :use-v-nrepl-eval
            :command-preview (subs command 0 (min 240 (count command)))})))
```

This requires adding `v/nrepl-eval` first.

### 15.4 Add `v/nrepl-eval` code sketch

New namespace/tool sketch:

```clojure
(defn nrepl-eval-tool
  [{:keys [expr port timeout-ms max-output-chars]
    :or {port :discover timeout-ms 30000 max-output-chars 20000}}]
  (let [port* (if (= port :discover)
                (discover-current-project-nrepl-port!)
                port)
        result (nrepl-eval port* expr {:timeout-ms timeout-ms})]
    (-> result
      (update :out bounded max-output-chars)
      (update :err bounded max-output-chars)
      (assoc :port port*))))
```

Expose as:

```clojure
(vis/symbol 'nrepl-eval nrepl-eval-tool
  {:doc "Evaluate Clojure in the project nREPL. Use this for runtime Clojure checks instead of shell-quoting clj-nrepl-eval."
   :arglists '([{:keys [expr port timeout-ms max-output-chars]}])
   :examples ["(v/nrepl-eval {:expr \"(require '[com.blockether.vis.core :as vis] :reload)\"})"]
   :render-fn render-nrepl-eval})
```

---

## 16. More Vis code snippets that should change

### 16.1 Duplicate fenced block guard

Current `code-entries-preflight` parses whatever `svar/ask-code!` concatenates:

```clojure
[forms parse-error] (if raw-fence-error
                      [nil (ex-info raw-fence-error ...)]
                      (split-top-level-forms raw-code))
```

But if `ask-code!` returns two identical fenced blocks concatenated as code, both execute. Add metadata from `ask-result :blocks` into preflight.

In `run-iteration`, current call:

```clojure
{:keys [code-entries answer-preflight-error repeat-preflight-error
        code-hash]}
(code-entries-preflight iteration-position raw-code (some-> repeat-atom deref))
```

Proposed:

```clojure
{:keys [code-entries answer-preflight-error repeat-preflight-error
        duplicate-block-preflight-error code-hash]}
(code-entries-preflight iteration-position raw-code
                        (some-> repeat-atom deref)
                        {:blocks (:blocks ask-result)})
```

Add helper:

```clojure
(defn- duplicate-fenced-block-error
  [blocks]
  (let [sources (mapv (comp str/trim :source) (or blocks []))]
    (when (and (< 1 (count sources))
            (< (count (distinct sources)) (count sources)))
      (str "Provider returned duplicate executable fenced blocks. "
           "Aborting before evaluation; emit exactly one clojure fence."))))
```

And reject before eval:

```clojure
(let [duplicate-error (duplicate-fenced-block-error (:blocks opts))]
  (cond
    duplicate-error
    {:code-entries [{:expr "(vis/preflight-error :duplicate-fenced-blocks)"
                     :preflight-error duplicate-error}]
     :duplicate-block-preflight-error duplicate-error}
    ...))
```

### 16.2 Repro-required nudge

Add state to environment:

```clojure
:diagnostic-repro-atom (atom nil)
```

Add a helper in prompt or loop:

```clojure
(defn- bug-request?
  [s]
  (boolean (re-find #"(?i)\b(fix|bug|broken|failing|duplicate|wrong|stuck|debug|diagnose|regression)\b"
             (str s))))

(defn- repro-nudge
  [{:keys [iteration current-user-request diagnostic-repro?]}]
  (when (and (bug-request? current-user-request)
          (>= (long iteration) 2)
          (not diagnostic-repro?))
    {:importance :critical
     :text "Bug-mode violation: no concrete reproduction/pass-fail signal has been recorded. Stop broad inspection; build a runnable repro now."}))
```

More invasive but better: add `(v/repro! ...)` tool that sets `diagnostic-repro-atom`.

### 16.3 Auto-load diagnose skill

Current skill loading is manual. Add in turn setup after `active-skills-atom` exists:

```clojure
(defn- auto-skill-names
  [request]
  (cond-> []
    (re-find #"(?i)\b(fix|bug|broken|failing|duplicate|wrong|stuck|debug|diagnose|regression)\b" (str request))
    (conj "diagnose")))
```

Then:

```clojure
(doseq [skill-name (auto-skill-names (:user-request opts))]
  (when-let [skill ((requiring-resolve 'com.blockether.vis.ext.foundation.environment.skills/lookup) skill-name)]
    (swap! active-skills-atom assoc (:name skill) (assoc skill :auto? true))))
```

This makes diagnose active from iteration 1 and removes reliance on the model choosing it.

### 16.4 Stop embedding active skill body inside `<extensions>`

Change current:

```clojure
(str "<extensions>\n"
  "<active_skills count=\"" (count skills) "\">\n"
  ...
  "</active_skills>\n"
  "</extensions>")
```

to:

```clojure
(str "<active_skills count=\"" (count skills) "\">\n"
  ...
  "\n</active_skills>")
```

### 16.5 Make `v/bash` truncation push summarization

Current `bash-tool-result` returns `:stdout-truncated?`, but nothing tells the model not to increase the cap.

Add warning:

```clojure
(cond-> result
  (:stdout-truncated? result)
  (update :warnings conj {:type :output-truncated
                          :message "Output truncated. Do not raise max-output-chars; rerun with a summarizing command/expression."})
  (:stderr-truncated? result)
  (update :warnings conj {:type :error-output-truncated
                          :message "stderr truncated. Narrow the command or summarize."}))
```

### 16.6 Add conversation render helper

New TUI diagnostic helper sketch:

```clojure
(defn diagnose-rendered-conversation
  [conversation-id opts]
  (let [conv    (chat/resume-conversation conversation-id)
        message (->> (:history conv) (filter #(= :assistant (:role %))) last)
        payload (render/format-answer-with-thinking-data
                  (:text message) (:trace message)
                  (long (or (:bubble-width opts) 100))
                  (or (:settings opts) {:show-thinking true :show-iterations true})
                  (:confidence message)
                  (= :cancelled (:status message))
                  {:conversation-id (:id conv)
                   :conversation-turn-id (:conversation-turn-id message)})
        text    (:text payload)]
    {:conversation-id (:id conv)
     :line-count (count (:lines payload))
     :counts {:shell-bash (count (re-seq #"SHELL bash" text))
              :search-any (count (re-seq #"SEARCH any" text))
              :searched   (count (re-seq #"Searched" text))}
     :interesting-lines (->> (:lines payload)
                          (map-indexed vector)
                          (filter (fn [[_ line]]
                                    (or (str/includes? line "SHELL")
                                        (str/includes? line "SEARCH")
                                        (str/includes? line "search any"))))
                          vec)}))
```

Expose through a dev/test namespace or extension symbol for agents. This would have collapsed seven wandering iterations into one probe.

---

## 17. Should all extension metadata be in responses?

No. Split metadata into three tiers:

### Tier 1: model-facing default

Only what changes decisions:

```clojure
{:op :v/rg
 :status :done
 :duration-ms 57
 :spec {:any ["alpha" "beta"] :paths ["src"]}
 :hit-count 3
 :truncated-by :end-of-results}
```

### Tier 2: debug-on-demand

When user/model asks for provenance:

```clojure
(v/provenance-event ref)
(v/extensions {:detail :source})
```

### Tier 3: persistence/audit

Full source hashes, source paths, license, owner, extension docs. Keep in DB/logs.

Current Vis mixes all three too often. That hurts agents.

---

## 18. Strategy recommendations after this comparison

1. **Make repro a first-class artifact**, not just a prompt instruction.
2. **Auto-load diagnose** for bug/fix tasks.
3. **Add `v/nrepl-eval`** so Clojure checks stop going through shell quoting.
4. **Keep rich provenance in DB, compact it in journal/TUI.**
5. **Make z locator rendering patch-oriented**, not EDN-dump-oriented.
6. **Reject duplicate fenced blocks before eval.**
7. **Move poetic nucleus below hard operational recipes**, or keep it but enforce it with nudges.
8. **For UUID-supplied tasks, block broad grep until `v/inspect` or transcript has been called.**
9. **For TUI bugs, provide a public pure render diagnostic helper.**
10. **Treat tool errors as mandatory branch points.** Repeating a nonexistent var should be considered non-progress and nudged/rejected.

