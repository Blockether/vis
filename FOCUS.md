# Focus — Context Contract

This is the current high-priority implementation focus. It groups prompt/context, previews, vars, tools, skills, reasoning memory, and audit/attestation display into one task. It is the canonical detail plan; `TASKS.md` links here instead of duplicating the same checklist.

## Goal

Make every byte that lands in model context explicit, tagged, bounded where lossy, and retrievable when full fidelity is needed.

No hidden dialog replay. No silent truncation of protected user/env/system material. No full giant payloads in journals. No proof from previews.

## Context surfaces

Provider messages should be shaped as XML-tagged blocks.

```text
system role:
  <system_prompt>
    core Vis rules + caller system instructions only
  </system_prompt>

  <environment-info>
    live runtime/workspace facts from extension environment-info hooks
  </environment-info>

  <extensions>
    extension-owned prompt fragments
    foundation-owned <skills> catalog lives here already:
      <skill name="..." source="..."><activation_trigger>...</activation_trigger></skill>
  </extensions>

  <specific_provider_model_prompt provider="..." model="...">
    provider/model-specific addendum only
  </specific_provider_model_prompt>

user role:
  <user_turn_request_main_goal>
    exact current user request only
  </user_turn_request_main_goal>

per-iteration user role:
  <extensions>
    <active_skills>
      <skill name="..." source="...">
        full prompt/body returned by v/load-skill
      </skill>
    </active_skills>
  </extensions>

  <journal>
    recent observed events/results, preview-only, canonical refs
  </journal>

  <var_index>
    hot executable symbol directory, preview/shape only
  </var_index>

  <reasoning>
    optional previous/current reasoning summaries, if surfaced
  </reasoning>

  <audit>
    optional audit/attestation summary, if relevant
  </audit>

  <system_nudges>
    <system_nudge importance="low|normal|high|critical">
      context/title/strategy nudge
    </system_nudge>
  </system_nudges>
```

Tag rule: if Vis injects structured context, it uses an XML-ish tag. Free prose belongs inside a named tag, not floating between blocks.

## What goes where

| Surface | Lossy? | Full data? | Purpose |
|---|---:|---:|---|
| `<system_prompt>` | no | yes | Stable behavior/protocol. Keep lean. Do not put live data here. |
| `<environment-info>` | no silent cut | yes | Live cwd/git/repo/provider/runtime facts. If too large, fail loud. |
| `<extensions>` | bounded by extension prompt design | no | Tool availability and extension protocol. Foundation `<skills>` catalog is here. |
| `<skills>` | yes, catalog trigger | no | XML skill entries with `name` + `source` attrs and `<activation_trigger>` only. Already exists. Do not duplicate elsewhere. |
| `<specific_provider_model_prompt>` | no silent cut | yes | Provider/model quirks only. |
| `<user_turn_request_main_goal>` | no | yes | Exact current human request. No prior dialog. |
| `<extensions><active_skills>` | no silent cut | yes | XML skill entries with `name` + `source` attrs and full loaded prompt/body after `(v/load-skill ...)`. Dynamic extension context; if too large, fail loud. |
| `<journal>` | yes | no | Recent observed event display: refs + previews. Includes block-authored intermediate `;;` comments; excludes LLM-only iteration `:thinking`. |
| `<var_index>` | yes | no | Live var names/shapes/previews. Lets model know what full values it can use. |
| SCI vars | no | yes | Full live values. Model should use vars directly instead of rereading. |
| DB / future ledger | no | yes | Durable truth, replay, audit, full event payloads. |

## Preview contract

Preview means lossy prompt/UI summary.

Preview is not:

- full value;
- durable truth;
- proof;
- storage boundary;
- reason to reread automatically.

Preview must include enough retrieval coordinates:

- var name;
- canonical ref;
- tool op;
- file path/range;
- result shape/size;
- omission/truncation marker when lossy.

Full data stays in one of:

- SCI var value;
- tool-result `:result`;
- persisted iteration/block row;
- future `provenance_event` payload;
- file system;
- explicit read/inspect/audit function.

## Vars and `<var_index>`

`<var_index>` is a hot executable symbol directory, not a value dump.

It should show:

```clojure
;; v=3 scope=live n=812
(def file-lines {:n 812 :head ["..." "..." "..."]})

;; v=1 scope=live
(def read-result {:tool-result true :ok? true :op :v/cat :result-shape ...})
```

The model can then use full values directly:

```clojure
(count file-lines)
(nth file-lines 41)
(str/join "\n" file-lines)
```

No duplicate reread unless:

- file changed;
- exact persisted bytes matter;
- external writer/race may have happened;
- user explicitly asks for reread/full file.

Model may read full files when it needs full files. The rule is not “never read full files.” The rule is: full read returns/binds full value; prompt surfaces only preview.

## File/tool naming

Use names that state effect scope.

| Name | Meaning |
|---|---|
| `read` / `cat` | inspect file content; may be full or paged depending helper/options. |
| `write` | create/replace exact target content or explicit line range. Caller owns whole replacement for that scope. |
| `patch` | targeted change against expected existing text/range/diff. Fails if target does not match. |
| `update` | function-based mutation of current file text. Good for programmatic transforms; result should include computed diff. |
| `delete` | remove path explicitly. |
| `bash` / `shell` | run command; journal preview stdout/stderr only; full output in result/DB. |

Current names may remain for compatibility, but the presentation/op vocabulary should distinguish:

```text
file.read
file.write
file.patch
file.update
file.delete
shell.run
```

Patch vs write rule:

```text
patch = preserve most existing file, prove target matched;
write = replace/create explicit content scope.
```

## Skills

We already have skills.

Do not create a second skill catalog.

- `<skills>` = activation catalog, owned by foundation extension prompt under `<extensions>`; each entry is `<skill name="..." source="..."><activation_trigger>...</activation_trigger></skill>`.
- `(v/load-skill "name")` = activation/read full skill.
- `<extensions><active_skills>` = full loaded skill prompts/bodies in next iteration; each entry is `<skill name="..." source="...">FULL_PROMPT</skill>`.

`<active_skills>` is nested under `<extensions>` because skills are extension-owned dynamic context. It carries source metadata, but not duplicate description/path wrappers; the skill tag body is the loaded prompt/body. It should not be silently trimmed. If loaded skills exceed budget, Vis should fail loud and tell which skill bodies caused overflow.

## Reasoning memory

Previous reasoning should not be dialog history.

Storage:

- each iteration stores model reasoning/thinking in the iteration row;
- latest previous reasoning is available through `ITERATION_PREVIOUS_REASONING` when bound;
- `<journal>` must not render LLM-only iteration `:thinking` / `ITERATION_PREVIOUS_REASONING` previews;
- `<journal>` may render block-authored intermediate `;;` / `#_(...)` comments because those are part of the emitted block evidence;
- future ledger can store reasoning as event metadata if useful, but reasoning is not proof.

Rules:

- reasoning summaries may guide next probes;
- reasoning is not evidence;
- proof/audit must cite observed refs/events, not hidden thoughts.

## Evidence, bundles, attestation, audit

These should be first-class functions/data, not giant permanent system-prompt prose.

System prompt should contain only the minimal protocol and names. Full audit data loads through stable functions.

Target always-available function surface:

```clojure
(v/events opts)          ;; provenance_event query
(v/event ref-or-id)      ;; one event/full payload
(v/evidence-bundles opts)
(v/evidence-bundle id)
(v/attestations opts)
(v/attestation id)
(v/audit opts)           ;; structured whole-system check
(v/audit-markdown opts)  ;; concise Markdown/presentation summary
```

Context blocks, when needed, must be tagged:

```xml
<audit>
  <summary status="ok" gates="3/3" attestations="4" violations="0" />
</audit>
```

Full audit/evidence data should not be dumped every turn. The model calls functions when needed.

Preview is never proof. Proof path is:

```text
observed event -> evidence bundle -> attestation -> audit
```

## Context budget policy

Protected/pinned, never silently cut:

- `<system_prompt>`
- `<environment-info>`
- `<specific_provider_model_prompt>`
- `<user_turn_request_main_goal>`
- `<extensions><active_skills>` once explicitly loaded

Bounded previews:

- `<journal>` token budget, capped at 50% of model context and reduced by protected/pinned context plus `<var_index>`, newest evidence kept;
- `<var_index>` token budget, newest hot vars kept;
- `<skills>` catalog activation-trigger budget, omitted skills still loadable;
- tool stdout/stderr/file previews.

If protected blocks exceed context:

```text
fail loud before provider call with token counts and responsible blocks.
```

Do not solve protected overflow by silently trimming environment info, user request, provider prompt, or active skill bodies.

## Implementation plan

This checklist is the single detailed plan for the context contract. `TASKS.md` should only reference it from the Pareto table and Recommended execution plan.

### A. Prompt shape

- [x] No prior dialog replay.
- [x] `<system_prompt>` wrapper.
- [x] `<environment-info>` extension hook, no char cap.
- [x] `<extensions>` wrapper for extension prompt fragments.
- [x] `<specific_provider_model_prompt>` wrapper.
- [x] `<user_turn_request_main_goal>` wrapper.
- [x] `<extensions><active_skills>` for loaded skill bodies.
- [x] Wrap system nudges in `<system_nudges>` with spec-checked `<system_nudge importance="low|normal|high|critical">` entries.
- [x] Keep LLM-only previous reasoning out of `<journal>`; expose latest prior reasoning only as `ITERATION_PREVIOUS_REASONING` unless a future explicit `<reasoning>` surface is designed.

### B. Preview/full contract

- [x] `<journal>` token-budgeted, newest lines kept.
- [x] `<journal>` renders block-authored intermediate comments but not LLM-only iteration `:thinking` / `ITERATION_PREVIOUS_REASONING`.
- [x] `<var_index>` token-budgeted, newest entries kept.
- [x] File read/write/update journal renderers preview only.
- [x] Define shared preview rendering contract for tool calls.
- [x] Require every `:ext.symbol/render-fn` to honor `:surface :journal` as preview-only.
- [x] Add tests that large tool/file payloads do not appear in `<journal>` or `<var_index>` in full. (`large-payloads-stay-preview-only-test`, `iteration-previous-reasoning-stays-out-of-journal-test` in `prompt-test`.)
- [x] Add explicit full retrieval helpers by ref/result id if current inspect APIs are too implicit.

### B'. Trivial vs coding context floor (CTX1)

- [x] Trivial / no-tool turn yields a nil per-iteration trailer (no `<journal>`, no `<var_index>`, no `<active_skills>`, no `<system_nudges>`).
- [x] Coding / proof turn surfaces canonical provenance refs, intent / gate / tool / error refs, audit-callable APIs, active-skill bodies, and `<var_index>` entries through the same XML-tagged surfaces.
- [x] Tests pin the trivial-vs-coding contract end-to-end
      (`context-floor-trivial-vs-coding-test` in `prompt-test`,
      `iteration-context-floor-test` in `loop-test`).
- [x] Auto-skill activation is gated by user-request keywords; trivial chat ("hi", "hello there") loads zero skills (`auto-skill-activation-test`).
- [x] Bench worktree no longer depends on `.agents/skills/` filesystem state for the auto-skill activation test (uses `with-redefs` over the foundation skill lookup).
- [x] Public `prompt/model-facing-context-stats` reports per-surface
      bytes + tokens for any turn shape so autoresearch / judge
      runners and diagnostic tooling can attribute prompt size to
      `<system_prompt>`, `<user_turn_request_main_goal>`, or the
      per-iteration trailer without reaching into private helpers.
      Trivial / no-tool turns return `:iteration-trailer-empty? true`
      with 0 bytes / 0 tokens for the trailer; coding turns return a
      bounded trailer carrying provenance / intent / gate / audit /
      tool evidence.

#### Exact token/context impact (measured against `assemble-system-prompt` + `assemble-initial-messages` + `build-iteration-context`)

Numbers below are bytes + tokens of fully-rendered prompt slices,
captured by the public `prompt/model-facing-context-stats` helper
against the unmodified Vis prompt assembler. Tokens are computed
through `prompt/count-tokens`, which uses `svar-router/count-tokens`
for known model ids and falls back to chars/4 for unknown models
(rough English prose / Clojure code rule of thumb on Anthropic Opus).

| Slice | Trivial turn | Coding turn |
|---|---:|---:|
| `<system_prompt>` (no caller addendum, no extensions active) | 21,021 B / 5,255 tok | 21,021 B / 5,255 tok |
| `<user_turn_request_main_goal>` wrapper | 63 B / 15 tok (`"hi"`) | 104 B / 26 tok (coding goal) |
| Per-iteration trailer (`<journal>` + `<var_index>` + `<active_skills>` + `<system_nudges>`) | **0 B / 0 tok** (nil) | 1,325 B / 331 tok |
| **Total model-facing context (this assembler only)** | **21,084 B / 5,270 tok** | **22,450 B / 5,612 tok** |

Iteration-trailer delta: **+1,325 B / +331 tokens** from the coding
turn carrying intent / gate refs, tool evidence, an active skill
body and the journal canonical-ref index. The trivial trailer is
empty by construction; the system prompt is identical across
shapes, so provider prompt caching latches onto it.

Real Vis runs add the foundation extension's `<environment-info>`
+ `<extensions>` + `<skills>` catalog (~6–8 KB) on top of the
base system prompt. Those slices are protected per the
"Context budget policy" table and stay constant across trivial
and coding turns; they do NOT amplify the trivial-vs-coding
delta.

These numbers are pinned by `model-facing-context-stats-test` in
`prompt-test`, which prints the per-surface bytes/tokens on every
test run so autoresearch / judge logs capture the exact impact.

### C. Full data retrieval

- [ ] Document model policy: bind full read once, reuse var; reread only on change/exact-byte/race/user request.
- [ ] Make full file/tool data discoverable from preview coordinates.
- [ ] Add/refine `v/event`, `v/result`, or equivalent full-result lookup when ledger lands.

### D. Evidence/attestation/audit

- [ ] Keep audit/attestation protocol minimal in `<system_prompt>`.
- [ ] Implement function-first audit surface: `v/events`, `v/evidence-*`, `v/attestation*`, `v/audit`, `v/audit-markdown`.
- [ ] Render only concise tagged `<audit>` summaries in context when relevant.
- [ ] Store proof-critical truth in immutable event/bundle/attestation tables, not previews.
- [ ] Ensure every audit/presentation output uses XML tags or typed presentation blocks before rendering.

## Links

- Main backlog: `TASKS.md`
- Recommended execution plan: `TASKS.md` → Sprint 0 — Context contract (`FOCUS.md`)
- Relevant clusters: Presentation/render contract, Extension contract v2, Ref D attestation ledger
