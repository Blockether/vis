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
           :guidance {:present? true :source :agents-md :path "AGENTS.md"}
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

Remaining work is to finish removing legacy wrappers, fake `ctx.*` comment labels, and provider-error markup from prompt-control paths.

## Audit table

| Area | Files | Current issue | Model-facing? | Problem | Replacement |
|---|---|---:|---:|---|---|
| Current user wrapper | `src/com/blockether/vis/internal/prompt.clj`, transcript tests | Legacy `current_user_message` wrapper in old fixtures/fallback detection | yes in stored old prompts | Duplicates user role; teaches tag protocol | User-role message with `;; -- CURRENT-USER-MESSAGE --`; ctx omits request |
| Current turn context | `extensions/common/vis-foundation/.../transcript.clj`, transcript tests | Legacy `current_turn_context` wrapper in old fixtures/fallback detection | yes in stored old prompts | Duplicates `ctx`; transcript parser sniffs text | Per-iteration trailer ends with `;; ctx =` EDN; later persist message kind |
| Provider error nudges | `src/com/blockether/vis/internal/loop.clj` | Provider failure text wrapped as markup | yes | Model may copy markup; no structured state | `;; llm-provider-error =` EDN trailer comment plus `(:llm-provider ctx) {:error ...}` |
| Extension prompt docs | `src/com/blockether/vis/internal/prompt.clj`, foundation core docs | Stale names from retired XML blocks | docs mostly | Docs can reintroduce wrong protocol | Comment-section wording: `SYSTEM-PROMPT`, `TURN-SYSTEM-CONTEXT`, `EXTENSIONS` |
| Environment prompt docs | foundation environment docs/tests | Stale environment/project-guidance/scan-warning wrapper language; fake `ctx.*` labels | maybe | Docs lie; fake ctx labels imply data exists when not bound | Long prose = comment sections; small dynamic facts = `(:project ctx)` |
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
;;     :guidance {:present? true :source :agents-md :path "AGENTS.md"}
;;     :warnings [...]}
```

- Small dynamic facts and warnings: `(:project ctx)`.
- Long AGENTS.md / CLAUDE.md prose: static prompt comment section.
- Warning summaries relevant every iteration: `(:project ctx) :warnings`.

### 6. Human rendering is allowed

Markdown/HTML in final renderers is not prompt-control protocol. Keep human-facing Markdown disclosure and HTML/Telegram output tags if escaped and isolated from provider prompts.

## Proposed migration order

### Phase 1 — docs/comments truth pass

Low risk.

- Replace stale XML-ish wording with comment-section or ctx-domain wording.
- Replace fake `ctx.project-guidance` / `ctx.scan-warnings` labels unless backed by real `(:project ctx)` data.

### Phase 2 — transcript classifier fallback

Keep historical transcript support, but make new path tag-free:

```clojure
(or (str/includes? content ";; ctx =")
    (legacy-current-turn-wrapper? content))
```

Then update fixtures to use comment sections, with one clearly named legacy fallback test.

### Phase 3 — provider error tags

Replace provider-error markup with:

```clojure
;; llm-provider-error =
{:type :llm-provider/incomplete-output
 :message "..."}
```

and add `[:llm-provider :error]` to next-iteration ctx where recovery needs structured state.

### Phase 4 — remove remaining prompt-control wrappers

Search target should find only legacy fallback tests/docs and human renderers:

```bash
rg -n "current_user_message|current_turn_context|llm-provider-error|system_prompt|turn_system_context|scan-warnings|project-guidance" src extensions test XML_CTX_REPLACEMENT_AUDIT.md
```

Allowed leftovers:

- legacy transcript fallback tests, explicitly named `legacy`.
- human Markdown/HTML rendering code.
- migration docs naming retired legacy identifiers without showing wrapper syntax.

## Acceptance criteria

- No model-facing prompt-control XML except legacy transcript fallback.
- Dynamic per-iteration facts live in `ctx` under domain keys: `:session`, `:llm-provider`, `:project`.
- Static extension prose lives in comment sections.
- Human markdown/HTML rendering remains allowed.
- Tests assert absence of retired wrappers in live prompt/trailer paths.

## Next concrete edits

1. Replace provider-error markup in `loop.clj` with EDN comment text.
2. Thread `:llm-provider {:error ...}` into `ctx/build` for recovery iterations.
3. Update transcript fixtures away from legacy current-user/current-turn wrappers.
4. Clean stale environment/project-guidance/scan-warning docstrings and fake ctx labels.
