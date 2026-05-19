# XML → `ctx` Replacement Audit

## Goal

Remove XML-ish prompt-control protocol from Vis. Model-facing state should be Clojure data (`ctx`) plus plain Clojure-comment sections, not ad-hoc markup wrappers.

`ctx` is real model-visible sandbox data. Host binds it before each model call:

```clojure
(env/bind-and-bump! environment 'ctx current-ctx-map)
```

Target model-facing shape:

```clojure
{:session {:id ...
                :title ...
                :turn-id ...
                :iteration {:id ... :position ...}
                :hints [{:id ... :importance ... :text ... :satisfy-with ...}]}
 :llm-provider {:selected ...
                :actual ...
                :routing ...
                :error ...}
 :project {:root ...
           :host {...}
           :git {...}
           :languages {...}
           :monorepo {...}
           :repositories {...}
           :guidance {:source :agents-md :path "AGENTS.md" :content "..."}
           :warnings [...]}
 :extensions [...]
 :defs {...}}
```

Model reads:

```clojure
(:session ctx)
(get-in ctx [:session :iteration])
(get-in ctx [:session :hints])
(:llm-provider ctx)
(:project ctx)
(:extensions ctx)
(:defs ctx)
```

Current user request is deliberately **not** duplicated in `ctx`; it lives in the current user-role provider message.

## Principle

Use XML/HTML only for final human rendering formats where needed. Do **not** use XML-ish tags as prompt-control protocol.

Prompt-control data should be:

- EDN in `ctx` for dynamic runtime state.
- Comment-section text (`;; -- NAME --`) for static prompt prose.
- Structured DB/metadata for transcript classification, not tag sniffing.

## Current state

Hints already moved from legacy hint wrappers into EDN under session state:

```clojure
(get-in ctx [:session :hints])
;; => [{:id :vis.foundation/session-title
;;      :importance :high
;;      :text "..."
;;      :satisfy-with '(satisfy-hint! :vis.foundation/session-title)}]
```

Remaining work is to persist explicit transcript message kinds and keep regression tests around retired wrappers/fake `ctx.*` labels.

## Audit table

| Area | Files | Current issue | Model-facing? | Problem | Replacement |
|---|---|---:|---:|---|---|
| Current user wrapper | `src/com/blockether/vis/internal/prompt.clj`, transcript tests | Legacy `current_user_message` wrapper in old fixtures/fallback detection | yes in stored old prompts | Duplicates user role; teaches tag protocol | User-role message with `;; -- CURRENT-USER-MESSAGE --`; ctx omits request |
| Current turn context | `extensions/common/vis-foundation/.../transcript.clj`, transcript tests | Legacy `current_turn_context` wrapper in old fixtures/fallback detection | yes in stored old prompts | Duplicates `ctx`; transcript parser sniffs text | Per-iteration trailer ends with `;; ctx =` EDN; later persist message kind |
| Provider error nudges | `src/com/blockether/vis/internal/loop.clj` | Provider failure text wrapped as markup | yes | Model may copy markup; no structured state | `;; llm-provider-error =` EDN trailer comment plus `(:llm-provider ctx) {:error ...}` |
| Extension prompt docs | `src/com/blockether/vis/internal/prompt.clj`, foundation core docs | Stale names from retired XML blocks | docs mostly | Docs can reintroduce wrong protocol | Comment-section wording: `SYSTEM-PROMPT`, `TURN-SYSTEM-CONTEXT`, `EXTENSIONS` |
| Environment/project ctx | foundation environment docs/tests | Runtime/project-guidance/scan-warning data previously rode prompt labels | yes | Prompt labels fake ctx and drift from real state | `:ext/ctx` contributes runtime facts, guidance content, and warnings under `(:project ctx)` |
| Manifest scan warnings | `src/com/blockether/vis/internal/manifest.clj`, `main.clj` | Stale scan-warning wrapper comments | no direct output | Misstates current surface | Say “foundation scan warning section” or `(:project ctx) :warnings` |
| Transcript markdown | foundation transcript renderer | Human Markdown disclosure markup | no | Human rendering only | Keep, clearly mark not prompt-control |
| IR HTML renderers | `src/com/blockether/vis/internal/render.clj` | Final-answer HTML/Telegram tags | no | Output renderer only | Keep if escaped |

## Detailed findings

### 1. Current user request

Old fixtures still contain a legacy current-user wrapper. Live prompt assembly now uses comment headers:

```clojure
{:role "user"
 :content ";; -- CURRENT-USER-MESSAGE --\n..."}
```

Recommended path:

- Do not wrap current user message in XML-ish syntax.
- Keep provider role = `user` as main semantic marker.
- Do not put request into `ctx`; user text already lives in the user-role provider message.
- Transcript classifier should prefer persisted role/index/kind, with legacy fallback only for old rows.

New classifier intent:

```clojure
(cond
  (= role "system") "stable system prompt"
  (and (= role "user") first-user?) "current user message"
  (and (= role "user") (str/includes? content ";; ctx =")) "per-iteration trailer"
  (= role "assistant") "assistant optional replay")
```

### 2. Current turn / iteration context

Old fixtures carried separate turn metadata in a wrapper. That is now duplicate state. The trailer should carry prior REPL observations plus fresh ctx:

```clojure
;; iter 1
(def x (+ 2 2))
;; => 4

;; ctx =
{:session {:id ...
                :turn-id ...
                :iteration {:id ... :position 2}
                :hints [...]}
 :llm-provider {...}
 :project {...}
 :extensions [...]
 :defs {...}}
```

Recommended path:

- Delete live emission of legacy turn-context wrappers if any remains.
- Detect `;; ctx =` for new transcripts.
- Keep legacy wrapper detection only as fallback for historical transcript rendering.
- Later persist explicit message kind:

```clojure
:llm-message/kind :vis.message/per-iteration-trailer
```

### 3. Provider failure feedback

Current live path still sends provider errors as markup. Replace it with EDN-ish data.

Trailer form:

```clojure
;; llm-provider-error =
{:phase :llm-provider/generate
 :type :llm-provider/incomplete-output
 :message "Provider stopped the response as incomplete because output budget was exhausted."
 :hint "Use compact recovery: one small probe if essential; avoid dumping large maps, file contents, diffs, or repeated diagnostics."}
```

Next-iteration ctx form when recovery needs structured state:

```clojure
{:llm-provider {:error {:phase :llm-provider/generate
                        :type :llm-provider/incomplete-output
                        :message "Provider stopped ..."
                        :hint "Use compact recovery."}}}
```

Recommendation: use both. Trailer preserves historical evidence; `ctx` gives current structured provider state.

### 4. Extension/system prompt sections

Runtime section headers are comment sections, not XML:

```text
;; -- SYSTEM-PROMPT --
;; -- TURN-SYSTEM-CONTEXT --
;; -- EXTENSIONS --
```

Cleanup:

- Replace stale XML-ish docstrings with “comment section” wording.
- Rename “block” docs to “section” where they describe prompt-control text.
- Do not change runtime unless tests prove old tags are emitted.

### 5. Project/environment state

Do not fake ctx in comments. If text says `ctx`, it should be real data in `ctx`.

Recommended split:

```clojure
(:project ctx)
;; => {:root ...
;;     :host {...}
;;     :git {...}
;;     :languages {...}
;;     :monorepo {...}
;;     :repositories {...}
;;     :guidance {:source :agents-md :path "AGENTS.md" :content "..."}
;;     :warnings [...]}
```

- Runtime facts, project guidance, and warnings: `(:project ctx)`.
- AGENTS.md / CLAUDE.md content is `(:project ctx) :guidance :content`, not prompt-control prose.
- Warning summaries relevant every iteration: `(:project ctx) :warnings`.

### 6. Human rendering is allowed

Markdown/HTML in final renderers is not prompt-control protocol. Keep human-facing Markdown disclosure and HTML/Telegram output tags if escaped and isolated from provider prompts.

## Cross-validation against source

| Idea | Source check | Verdict | Next action |
|---|---|---:|---|
| `ctx` top domain is `:session` | `ctx/build` accepts `:session`; core prompt documents `(:session ctx)`; loop passes `:session current-session` into `vctx/build`. | ✅ implemented | Keep. |
| `:iteration` and `:hints` live under `:session` | `ctx/build` nests both; prompt tells model to read `(get-in ctx [:session :hints])`. | ✅ implemented | Keep tests around top-level absence. |
| Current user request absent from ctx | `ctx/build` strips `:user-request`; prompt says use `CURRENT-USER-MESSAGE`. | ✅ implemented | Keep. |
| `:llm-provider` is domain for selected/routing/error | `ctx/build` accepts `:llm-provider`; loop now passes selected provider/model, routing request, and previous provider error into `vctx/build`. | ✅ implemented | Add `:actual` after provider call if/when a later ctx needs last successful routing. |
| `:project` owns runtime/project facts/warnings | Foundation contributes `{:project {:root :host :git :languages :monorepo :repositories :guidance :warnings}}` through `:ext/ctx`; loop merges active extension ctx into `vctx/build`; fake prompt labels are gone. | ✅ implemented | Keep prompt free of runtime/project data labels. |
| Provider errors become EDN-ish | `loop.clj` emits `;; llm-provider-error =` EDN feedback and threads `[:llm-provider :error]` into next ctx. | ✅ implemented | Keep tests around absence of provider-error markup. |
| Transcript no longer depends on wrappers | Transcript renderer classifies new comment-section messages; fixtures use `CURRENT-USER-MESSAGE` and `;; ctx =`. | ✅ implemented | Persist explicit message kind later to avoid sniffing. |
| Human Markdown/HTML renderers stay out of prompt protocol | Final renderers are separate from provider prompt assembly. | ✅ acceptable | Do not include in prompt-control cleanup. |

## Proposed migration order

### Phase 1 — docs/comments truth pass

Low risk.

- Replace stale XML-ish wording with comment-section or ctx-domain wording.
- Replace fake `ctx.project-guidance` / `ctx.scan-warnings` labels unless backed by real `(:project ctx)` data.

### Phase 2 — transcript classifier

New path is tag-free and fixtures use comment sections:

```clojure
(str/includes? content ";; ctx =")
```

Later improvement: persist message kind and stop classifying by content strings.

### Phase 3 — provider error EDN

Provider-error feedback now uses:

```clojure
;; llm-provider-error =
{:type :llm-provider/incomplete-output
 :message "..."}
```

and threads `[:llm-provider :error]` into next-iteration ctx where recovery needs structured state.

### Phase 4 — remove remaining prompt-control wrappers

Search target should find no live model-facing prompt-control wrappers:

```bash
rg -n "current_user_message|current_turn_context|llm-provider-error|system_prompt|turn_system_context|scan-warnings|project-guidance" src extensions test XML_CTX_REPLACEMENT_AUDIT.md
```

Any match must be classified at review time as either non-prompt human rendering or a migration target to remove. Do not keep an `Allowed leftovers` bucket in this audit.

## Acceptance criteria

- No model-facing prompt-control XML in live prompt paths.
- Dynamic per-iteration facts live in `ctx` under domain keys: `:session`, `:llm-provider`, `:project`.
- Static extension prose lives in comment sections.
- Human markdown/HTML rendering remains allowed.
- Tests assert absence of retired wrappers in live prompt/trailer paths.

## Next concrete edits

1. Add tests for provider-error EDN feedback and next-iteration `[:llm-provider :error]`.
2. Persist explicit provider-message kind to remove transcript content sniffing.
3. Clean remaining stale scan-warning/project-guidance comments in non-prompt docs.
