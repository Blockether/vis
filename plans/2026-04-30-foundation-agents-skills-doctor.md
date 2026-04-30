# Plan — vis-foundation: project guidance, skills, extension reload, doctor protocol

**Date**: 2026-04-30
**Author**: grilled into shape across 22 design questions
**Status**: Approved, ready for implementation
**Scope**: Single mega-PR landing the read side, the doctor protocol, and F1-lite extension hot-reload together

---

## 0. Summary

Adds three coupled capabilities to vis-foundation, plus one new core extension-contract field:

1. **AGENTS.md / skills awareness** — vis reads project guidance and skills catalogs from disk and inlines them into the system prompt every iteration. The model is *steered* by the user's project rules, not just informed they exist.
2. **Doctor protocol** — every extension can declare `:ext/doctor-checks`. `vis doctor` aggregates every check across every loaded extension, prints level-aware diagnostic output (error / warn / info), exits with 0 / 1 / 2 by max level. Foundation contributes the first batch of checks (system, agents-md, skills, scan-warnings).
3. **Extension hot-reload (F1-lite)** — `(vis/reload-extensions!)` re-discovers extensions, surgically applies `:added` / `:removed` / `:reloaded` against the in-memory registry and every cached env, with continue-on-error per-extension failure isolation. Source markers (paths, max-mtime, sha256) are persisted into `iteration.metadata`'s existing JSON column for v2 change-detection to read.

No new DB schema. No new migrations. No new top-level CLI commands beyond `vis doctor` (which already exists, just relocates from host to foundation).

---

## 1. Locked decisions (Q1 – Q22)

| # | Decision |
|---|---|
| Q1 | **Steer**: AGENTS.md content inlined into system prompt; not just metadata. |
| Q2 | AGENTS.md sources: repo-root only. Skills: `<repo>/.agents/skills/` + `~/.agents/skills/`. `.pi/skills/` ignored. |
| Q3 | `<project-guidance>`, `<skills>`, `<scan-warnings>` rendered inside foundation's `:ext/prompt`. XML-wrapped. No host-prompt change. |
| Q4 | Strict precedence: repo `AGENTS.md` → repo `CLAUDE.md` (with `source="repo:claude-md-fallback"`). No user-global merge for instructions. |
| Q5 | 16 KB byte-truncate, fixed constant, no config knob. **Applies to whichever file is loaded — AGENTS.md OR the CLAUDE.md fallback** (your repo's CLAUDE.md is 47 KB and would truncate). Marker: `[TRUNCATED — N more bytes. Read full content via (vis/main-agent-instructions).]` |
| Q6 | All read fns return MAPS with `:found?` flag. Never nil. Skill-name collision: repo wins **silently** (no warning). |
| Q7 | Skills index: alphabetical by `:name`, full descriptions verbatim, `:source` rendered inline (`name [repo]: ...`), 8 KB total cap with `+ N more skills` marker. |
| Q8 | Two new files: `environment/agents.clj` + `environment/skills.clj`. Each holds its own private `defonce` cache atom — **NOT plugged into `compute-snapshot`** (they aren't cwd-derived). Cache invalidated only by `(vis/reload-skills!)` / `(vis/reload-instructions!)`, plus `(vis/refresh!)` cascades into both for symmetry (see caveats). |
| Q9 | YAML lib: **`org.yamlstar/yamlstar "0.1.3"`** — pure Clojure, GraalVM-friendly, by Ingy döt Net. Caveat: very new (0.1.3, ~36 Clojars downloads). Acceptable trade. |
| Q10 | Malformed = parse error / missing `name` / missing `description`. Drop with trace. Surfaces: startup banner + `<scan-warnings>` prompt block + `(vis/scan-warnings)` fn + Telemere `:warn` log. Unknown extra YAML fields pass through under `:extra`. |
| Q11 | Explicit reload fns. No auto-rescan per iteration. |
| Q12 | Three reload fns: `(vis/reload-skills!)`, `(vis/reload-instructions!)`, `(vis/reload-extensions!)`. |
| Q13 | F1 diff-and-patch (surgical add/remove/reload, NOT nuke-and-rebuild). |
| Q14 | F1-**lite** in v1 — every still-present extension is `:reloaded` (no change detection). v2 (separate PR) reads source markers from `iteration.metadata` to skip unchanged. |
| Q15 | Persistence via existing `iteration.metadata` JSON column + `log` table. **No new schema, no migration.** Markers cached in a sidecar atom at register-time. **Written into `iteration.metadata.extensions[]` only on the FIRST iteration of each query** (`position = 0`); subsequent iterations omit them — cuts ~99 % redundant writes vs every-iteration. v2 read query becomes `WHERE position = 0 ORDER BY created_at DESC LIMIT 1`. Reload events go to `log` via Telemere. |
| Q16 | `reload-extensions!` failure handling: **continue-on-error**, per-ext `:errors` accumulated, structured summary returned. No transactional rollback. |
| Q17 | Surfaces: SCI sandbox (always) + CLI (only `vis doctor`). Reload-via-CLI walked back — see Q20-redux. |
| Q18 | Single `vis doctor` command. No doctor subcommands. |
| Q19 | `:ext/doctor-checks` extension contract field. Map-wrapping shape. Diagnostic message map. Exit codes 0 / 1 / 2 by max level. **Output ordering**: extensions in topo-sorted-install order (already deterministic via `topo-sort-extensions`); within each extension, checks in declaration order; messages in fn-return order. **Output format**: see §10. **Activation contract**: checks run for EVERY registered extension regardless of `:ext/activation-fn` — check fns must defensively handle missing env keys. |
| Q20-redux | **Doctor-only CLI**. Inspection content surfaces as doctor `:info` messages. Reload fns are SCI-only — CLI invocation would spawn a fresh JVM and produce an empty diff, so the shim would be misleading. |
| Q21 | **Golden-file tests** for prompt blocks (`<project-guidance>`, `<skills>`, `<scan-warnings>`); **unit assertions** for data-fn behavior. |
| Q22 | **Single mega-PR**. Internal phases for review structure (read → doctor → reload), but one merge. |

### Caveats locked alongside

- **`defonce` contract**: extensions holding mutable state across reload MUST use `defonce`. Foundation's `environment/core.clj` cache atom migrated from `def` → `defonce`. Any other `def`-of-atom in the codebase audited. Documented in `docs/src/extensions/spec.md`.
- **Per-env `:lock` during reload**: `reload-extensions!` acquires each cached env's existing `:lock` before re-installing extension symbols. Reload waits for in-flight iterations; new queries kicked off after see new state.
- **Side-effect cleanup on `:removed`**: when an extension is removed, `deregister-extension!` walks its `:ext/cli` / `:ext/channels` / `:ext/providers` / `:ext/persistance` and calls the matching deregister fn for each. Provider-deregister wired up if not already.
- **Reload deadlock prevention**: `reload-extensions!` MUST detect when the calling thread already holds the env's `:lock` (i.e., the call is *inside* a query for that env) and **defer that env's reseat** to the next `send!`. Reseating an SCI sandbox while it is interpreting calls *from* that sandbox is racy at best and AbstractMethodError-territory at worst. Other envs reseat normally, with a per-env lock-acquisition `:reload/timeout-ms` (default 30 000) — envs whose locks aren't acquired in time are recorded as `:env-reseat-skipped {:reason :busy :env-id ...}` in the return map, and rebind on next access.
- **`(vis/refresh!)` cascades**: in addition to invalidating the cwd-keyed snapshot cache, `(vis/refresh!)` now ALSO calls `reload-instructions!` and `reload-skills!`. Otherwise users editing `AGENTS.md` and calling `refresh!` (the existing muscle-memory) would be confused why nothing changed. The new explicit reload fns remain available for granular control.
- **yamlstar fallback path**: yamlstar is at v0.1.3 (~36 Clojars downloads) and pre-1.0. If it fails the GraalVM verify gate, hits a real bug during Phase 1, or chokes on a folded-scalar edge case, fall back to `clj-commons/clj-yaml` without revisiting Q9. Plan B detail in §9.

---

## 2. New core extension contract

`com.blockether.vis.internal.extension/extension` validation grows one optional field:

```clojure
:ext/doctor-checks      ;; vec, optional
[{:check/id          :namespace/keyword     ;; required, unique within extension
  :check/name        "Human-readable label" ;; required
  :check/description "What this check verifies." ;; required
  :check/run-fn      (fn [environment]      ;; required
                       ;; Returns vec of diagnostic messages.
                       ;; Empty vec = check ran cleanly with nothing to report.
                       [{:level       :info | :warn | :error  ;; required
                         :message     "Human-readable summary"  ;; required
                         :remediation "Optional fix instruction" ;; optional, recommended for :warn / :error
                         :id          :namespaced/keyword       ;; optional, for dedup
                         :data        {...arbitrary...}}])}]    ;; optional, structured
```

The aggregator (`internal/doctor/run-doctor-checks`) auto-injects `:ext` (the extension's `:ext/namespace`) and `:check-id` (the check's `:check/id`) into every emitted message before grouping/printing.

---

## 3. New SCI sandbox surface (foundation)

```clojure
;; --- Read fns (always return maps, never nil) ---
(vis/main-agent-instructions)
;; => {:found? true  :source :repo | :repo:claude-md-fallback
;;     :path "..." :bytes 7408 :content "..."}
;;    or {:found? false}

(vis/skills)
;; => [{:name "diagnose" :description "..." :path "..." :source :repo
;;      :extra {:disable-model-invocation true}}  ;; :extra holds non-required YAML fields
;;     ...]
;;    sorted alphabetical by :name; [] when empty

(vis/skill "name")
;; => {:found? true  :name "..." :description "..." :path "..."
;;     :source :repo | :user-global :body "..." :extra {...}}
;;    or {:found? false :name "..."}

(vis/scan-warnings)
;; => [{:source :skill-frontmatter | :agents-md | :claude-md-fallback
;;      :path "..." :reason "..." :exception "..."} ...]
;;    [] when clean

;; --- Reload fns (return diff summaries; mutate in-memory state) ---
(vis/reload-skills!)
;; => {:scanned 5 :loaded 4 :dropped 1 :warnings [...]}

(vis/reload-instructions!)
;; => {:found? true :source ... :path ... :bytes ... :changed? true}
;;    :changed? compares against the previous loaded content

(vis/reload-extensions!)
;; => {:added                [com.foo.ext]
;;     :removed              [com.bar.ext]
;;     :reloaded             [...]
;;     :errors               [{:ns ... :phase :require | :register | :deregister | :side-effects | :env-reseat
;;                            :reason "..." :stack-trace "..."} ...]
;;     :envs-reseated        3                ;; envs successfully reseated this call
;;     :env-reseat-deferred  [#uuid "..."]    ;; the calling env (when invoked mid-query) — picks up new state on next send!
;;     :env-reseat-skipped   [{:env-id #uuid "..." :reason :busy :waited-ms 30000}]
;;     :duration-ms          847
;;     :blocked-ms           127}
```

---

## 4. New CLI surface

```
$ bin/vis doctor          # Runs all extension :ext/doctor-checks. Exit code 0/1/2.
```

That's it. The existing `vis doctor` body in `internal/main.clj` becomes a thin wrapper that calls the aggregator. The OS/Java/DB/conversations content moves into a `system-health-check` entry on `vis-foundation`'s `:ext/doctor-checks`.

**No** `vis agent ...`. **No** `vis main-agent-instructions`. **No** `vis reload-extensions` (would be a no-op shim). **No** `vis skills`. **No** `vis scan-warnings` (use `vis doctor`'s exit code).

---

## 5. File-level changes

### New files

| Path | Role |
|---|---|
| `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/environment/agents.clj` | AGENTS.md / CLAUDE.md fallback discovery, content read, `:found?`-shaped return |
| `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/environment/skills.clj` | Skills directory scan, yamlstar frontmatter parse, catalog + body lookup, malformed tracking |
| `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/doctor.clj` | Foundation's `:ext/doctor-checks` definitions (system, agents-md, skills, scan-warnings) |
| `src/com/blockether/vis/internal/doctor.clj` | Cross-extension `run-doctor-checks` aggregator + `doctor-exit-code` + TTY formatter |
| `extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/environment/agents_test.clj` | Unit + golden-file tests |
| `extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/environment/skills_test.clj` | Unit + golden-file tests; covers folded-scalar `description: >` (caveman case) and `disable-model-invocation: true` (grill-with-docs case) |
| `extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/doctor_test.clj` | Foundation's checks under unit test |
| `test/com/blockether/vis/internal/doctor_test.clj` | Aggregator + exit-code tests with synthetic extensions |

### Modified files

| Path | Change |
|---|---|
| `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/core.clj` | Wire new symbols into `:ext/symbols`; declare `:ext/doctor-checks`; register `vis doctor` via `register-cmd!` (foundation owns it now) |
| `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/environment/core.clj` | `def cache` → `defonce`. Agents + skills NOT plugged into `compute-snapshot` — they live in their own files with private `defonce` caches (Q8). `(vis/refresh!)` extended to cascade into `reload-instructions!` + `reload-skills!`. |
| `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/environment/render.clj` | Render `<project-guidance>`, `<skills>`, `<scan-warnings>` blocks. Conditional on data presence. Extend FN_INDEX with new fns. |
| `extensions/common/vis-foundation/test/com/blockether/vis/ext/foundation/environment/render_test.clj` | Golden-file fixtures for the new blocks |
| `extensions/common/vis-foundation/deps.edn` | Add `org.yamlstar/yamlstar {:mvn/version "0.1.3"}` |
| `src/com/blockether/vis/internal/extension.clj` | Validate `:ext/doctor-checks`. Source-marker resolution lives in a new sibling namespace (see §5.5); markers stored in a sidecar atom keyed by ns-sym, NOT mutated into the extension map (keeps spec validation contract clean). Beef up `deregister-extension!` to reverse all `:ext/cli` / `:ext/channels` / `:ext/providers` / `:ext/persistance` side effects. Keep idempotency. |
| `src/com/blockether/vis/internal/loop.clj` | `iteration-metadata` includes per-ext source markers. Add `reload-extensions!` orchestrator: scan via `manifest/rediscover!`, diff registry, per-ext `(require :reload)` + side-effects, per-cached-env lock + surgical install/drop, continue-on-error accumulation. |
| `src/com/blockether/vis/internal/main.clj` | **Delete** lines ~742–781 (`cli-doctor!` body) and ~860–863 (top-level `doctor` registration). Foundation owns `vis doctor` now. |
| `src/com/blockether/vis/core.clj` | Export new public symbols: `run-doctor-checks`, `doctor-exit-code`, `reload-extensions!`, `resolve-source-markers`. |
| `docs/src/extensions/spec.md` | Document `:ext/doctor-checks`. Document `defonce` contract for extension state. |
| `docs/src/extensions/common/vis-foundation.md` | Document new SCI surface (read + reload fns), prompt block contributions. |
| `docs/src/architecture/diagnostics.md` (new) | Doctor protocol overview, message shape, aggregation semantics, exit codes. |
| `docs/src/SUMMARY.md` | Link the new diagnostics page. |

### 5.5 Source marker resolution (new helper)

A new file `src/com/blockether/vis/internal/extension/source_markers.clj` owns the messy bit. Two source-resolution paths depending on classpath origin:

- **File-classpath sources** (local-dev `:paths`): `(.getResource cl ns-path)` → `file:` URL → `java.io.File` → `(.lastModified)` for mtime, SHA-256 over file content for hash.
- **Jar-classpath sources** (packaged extensions): `(.getResource cl ns-path)` → `jar:file:` URL → `JarFile` + `JarEntry` → entry mtime (which is the jar build time, identical for every entry from that jar — acceptable since jars don't change at runtime), SHA-256 over `JarEntry`'s `InputStream` for hash.
- **Mixed extensions** (multiple ns spread across file-paths AND jars): max-mtime across all entries, SHA-256 over concatenated content sorted by entry name.

Returned shape per extension:
```clojure
{:source-paths      ["..."]              ;; file paths or jar:URLs, sorted
 :source-mtime-max  1714403520000          ;; epoch-ms
 :source-hash-sha256 "abc..."             ;; hex digest
 :manifest-id       "vis-foundation"}     ;; manifest dir name (passed in from the scan)
```

Storage:
```clojure
;; in internal/extension.clj
(defonce extension-source-markers (atom {})) ;; {ns-sym -> markers-map}
```

Populated by a new hook in `register-extension!`: after the extension validates and registers, compute markers via the helper and store them in the sidecar atom keyed by `:ext/namespace`. Cleared on `deregister-extension!`. Read by the iteration-metadata writer (first iteration only — see Q15).

Test file: `test/com/blockether/vis/internal/extension/source_markers_test.clj`. Cases: file-only, jar-only, mixed, missing-file (graceful nil), unreadable-jar (graceful nil with Telemere :warn).

---

## 6. Test plan

Per AGENTS.md mandatory rule: every new namespace gets a test file.

### Golden-file fixtures (under `test/.../foundation/environment/golden/`)

- `project-guidance-repo.txt` — verbatim `<project-guidance source="repo">` block for a fixture AGENTS.md
- `project-guidance-claude-fallback.txt` — `<project-guidance source="repo:claude-md-fallback">` variant
- `project-guidance-truncated.txt` — 16 KB ceiling rendering with marker
- `skills-block-clean.txt` — alphabetical, source-tagged, full descriptions
- `skills-block-budget-truncated.txt` — `+ N more skills` marker
- `skills-block-empty.txt` — no skills found case
- `scan-warnings-block.txt` — multi-error rendering

### Unit assertions

- `agents.clj`: AGENTS.md found / CLAUDE.md fallback / neither / oversized → truncate-with-marker / unreadable → goes to `:scan-warnings`
- `skills.clj`: parse single-line + folded-scalar (`>`) descriptions; `disable-model-invocation` lands in `:extra`; missing `name` → drop+warn; missing `description` → drop+warn; YAML parse error → drop+warn; collision repo-vs-user-global → repo wins, no warning
- `doctor.clj` (foundation): each check returns expected message shapes for every input scenario
- `internal/doctor.clj`: aggregator auto-injects `:ext` and `:check-id`; check fn throwing → captured as `:error` with check-id; exit-code 0/1/2 mapping; deterministic ordering
- `loop.clj`: `iteration-metadata` includes source markers; `reload-extensions!` happy path + per-phase failure isolation; per-env `:lock` blocking; `:added`/`:removed`/`:reloaded` semantics
- `extension.clj`: `:ext/doctor-checks` validation rejects bad shapes; `:ext/doctor-checks` of `nil` and `[]` both accepted as no-checks; `deregister-extension!` reverses side effects across all four registries
- **Reload safety**: `reload-extensions!` invoked from inside an active iteration defers the calling env's reseat (regression test for the deadlock S1 fix); reload of OTHER envs proceeds; per-env lock-acquisition timeout records `:env-reseat-skipped` after waiting `:reload/timeout-ms`
- **Boundary cases**: AGENTS.md exactly 16384 bytes (no truncate) vs 16385 bytes (truncate marker fires); empty SKILL.md frontmatter (`---\n---\n`) → drop+warn; folded-scalar `description: >` (caveman fixture) parses to flat string; `disable-model-invocation: true` (grill-with-docs fixture) lands in `:extra` and is preserved through `(vis/skill ...)` return
- **Doctor edge cases**: `vis doctor` with zero registered extensions exits 0 with empty output; an extension declaring `:ext/doctor-checks []` contributes nothing; circular `:ext/requires` during reload fails gracefully (one `:errors` entry, other extensions unaffected)
- **First-iteration markers**: `iteration.metadata.extensions[]` includes source markers when `position = 0`; omits them for `position > 0`

---

## 7. Implementation order (single PR, internal phases for self-review)

The PR is one merge commit. These phases are review structure and rebase-able commits during development:

### Phase 1 — Read side (no behavior changes elsewhere)
1. Add yamlstar dep.
2. `environment/agents.clj` + tests.
3. `environment/skills.clj` + tests.
4. `def cache` → `defonce` in `environment/core.clj`.
5. `environment/render.clj` adds `<project-guidance>`, `<skills>`, `<scan-warnings>` blocks (still no doctor) + golden-file tests.
6. Foundation core wires new symbols into `:ext/symbols`.

**Verify gate**: `./verify.sh` green. Functional outcome: prompt now includes new blocks; `(vis/main-agent-instructions)` etc. callable from SCI; reload fns NOT YET (defer to phase 3).

### Phase 2 — Doctor protocol
1. `internal/doctor.clj` aggregator + formatter + exit-code.
2. `internal/extension.clj` validates `:ext/doctor-checks`.
3. `core.clj` exports.
4. `vis-foundation/doctor.clj` defines system/agents-md/skills/scan-warnings checks.
5. Foundation core declares `:ext/doctor-checks`; registers `vis doctor` via `register-cmd!`.
6. `internal/main.clj` deletes old `cli-doctor!` and top-level `doctor` registration; replaces with thin shim that calls aggregator.
7. Tests for aggregator + each foundation check.

**Verify gate**: `./verify.sh` green. `bin/vis doctor` produces level-aware output. Exit code reflects max level.

### Phase 3 — Hot-reload (F1-lite)
1. `internal/extension.clj` adds `resolve-source-markers`. Beefs up `deregister-extension!`.
2. `internal/loop.clj` extends `iteration-metadata` with source markers; adds `reload-extensions!` orchestrator.
3. `core.clj` exports.
4. `vis-foundation` exposes `(vis/reload-skills!)` + `(vis/reload-instructions!)` + `(vis/reload-extensions!)` SCI symbols.
5. Tests covering happy path, partial failure, per-env lock blocking, side-effect cleanup, `iteration.metadata` write content.

**Verify gate**: `./verify.sh` green. Manual smoke: edit AGENTS.md / SKILL.md mid-session, call reload fn, observe `<project-guidance>` / `<skills>` block update on next iteration.

---

## 8. `verify.sh` expectations

All eight gates green on the final commit:

1. **Format** — `cljfmt fix extensions/ src/` before commit.
2. **Lint** — `clj-kondo` clean. May need `:lint-as` hints for `:check/...` keywords if linter complains about unknown kw namespaces.
3. **GraalVM safety** — yamlstar advertises GraalVM-native compatibility. If the `*warn-on-reflection*` count rises, investigate before considering `--update-baseline`. Default expectation: same baseline holds.
4. **Tests** — `clojure -M:test` passes; every new namespace has a test file.
5. **Docs build** — `cd docs && mdbook build` clean after the new diagnostics page + SUMMARY.md update.
6. **Smoke** — `bin/vis` help banner unchanged in shape.
7. **Git hygiene** — no whitespace / conflict markers.
8. **Secret scan** — clean.

---

## 9. Out of scope (deferred to follow-up PRs)

- **v2 change-detection for `reload-extensions!`** — read `iteration.metadata` for prior source markers, classify still-present extensions as `:changed` vs `:unchanged`, skip reload work for `:unchanged`. Schema is already correct for this; v2 is purely read-side logic.
- **TUI slash commands** (`/reload-skills`, `/skills`, etc.) — requires building slash-command infrastructure in `vis-channel-tui`. Separate design exercise.
- **`vis doctor --quiet`** flag (suppress `:info` lines).
- **`vis doctor --strict`** flag (treat warnings as errors for exit code).
- **`vis doctor --json`** machine-readable output for CI.
- **Honoring `disable-model-invocation: true`** in skill frontmatter — the field is preserved under `:extra` in v1; whether it should hide the skill from the prompt index is a future product decision.
- **Skill body size limit** — currently `(vis/skill "name")` returns the full body regardless of size. May need a cap analogous to the AGENTS.md 16 KB limit.
- **Sub-LLM-call / forked-env / futures** — was the third item in the original conversation; punted as a separate feature track entirely.
- **yamlstar → clj-yaml fallback** (Plan B): if yamlstar bites during Phase 1 (verify-gate failures, real-world bugs, or unimplemented YAML features beyond what's in the test fixtures), swap the dep for `clj-commons/clj-yaml` without revisiting Q9. The frontmatter usage is small (`name`, `description`, plus pass-through under `:extra`); the only YAML-1.2 feature actually exercised in your repo is folded scalars (`description: >`), which clj-yaml's snakeyaml-engine handles fine. Worst case if even that's too much: hand-roll a 30-line frontmatter parser for the three named fields and treat unknown fields as opaque blobs.

---

## 10. Doctor output format spec

Locked rendering rules for `vis doctor` output. Authored here so check authors and frontend builders share a contract.

### Color & icon

- **ANSI colors when stdout is a TTY** (detected via `(some? (System/console))`); plain text otherwise. CI logs see uncolored output by default; interactive terminals see colors.
- **Icons**: UTF-8 by default — `ℹ` for info, `⚠` for warn, `✗` for error. No `--ascii` flag in v1; users on terminals without UTF-8 see the raw bytes (acceptable since modern terminals all support it; revisit if reports come in).
- **Color mapping**: `:info` → no color, `:warn` → yellow, `:error` → red. `:remediation` rendered in dim/grey indented under its parent message.

### Layout

Grouped by extension, in topo-sorted-install order (already deterministic via `topo-sort-extensions`). Within each extension, checks in declaration order; within each check, messages in fn-return order. **Levels NOT re-sorted within a section** — preserves cause-and-effect narrative (info "AGENTS.md found" → error "skill X malformed" reads as a story, not a histogram).

Sample shape:

```
vis doctor

  com.blockether.vis.ext.foundation.core
  ──────────────────────────────────────
  ℹ system-health: macOS 14.5 (aarch64), Java 21.0.2, Clojure 1.12.0
  ℹ system-health: 412 MB / 4096 MB used
  ℹ system-health: DB at ~/.local/share/vis/vis.db
  ℹ agents-md-presence: AGENTS.md loaded from /Users/.../vis/AGENTS.md (7.4 KB, source: repo)
  ℹ skills-frontmatter: 5 skills loaded successfully (4 repo, 1 user-global)

  com.blockether.vis.ext.channel.tui
  ──────────────────────────────────
  ℹ tui-availability: lanterna available, terminal 198x52

  com.blockether.vis.ext.provider.zai
  ───────────────────────────────────
  ✗ zai-credentials: API key missing
      → Set ZAI_API_KEY env var or run `bin/vis auth zai`.

Summary: 1 error, 0 warnings, 6 info
```

### Exit code

- `0` — only `:info` messages emitted (or none at all).
- `1` — at least one `:warn`, no `:error`.
- `2` — at least one `:error`.

Deferred to v2: `--quiet` (suppress info), `--strict` (warn → exit 1 too), `--json` (machine-readable).

### Empty case

Zero registered extensions or all extensions declaring `:ext/doctor-checks []`:

```
vis doctor

No diagnostic checks registered.
```

Exit code 0.

### Section: startup hint (one-line)

When any non-doctor `bin/vis ...` command starts and at least one doctor check returns `:warn` or `:error`, print a single line to `original-stdout` BEFORE the command runs its body:

```
⚠ vis: 2 issues detected — run `bin/vis doctor` for details.
```

Keeps users aware of broken state without forcing the full doctor output on every invocation. Skipped when the command IS `vis doctor` (avoid double output). Implemented in `internal/main.clj`'s dispatcher, calling the same `run-doctor-checks` aggregator + counting levels.

---

## 11. Provenance

This plan was extracted from a 22-question grilling session per the `grill-me` skill, then critically reviewed once before lock. Each decision is traceable to a numbered Q in section 1; the rationale lives in the conversation history, not duplicated here.

The plan is the contract; implementation deviations require revisiting the relevant Q.
