# PLAN — make `/reload` the only act that admits Python extension bytes

*Authorization is a human act, not a file: `/reload` (or the gateway's own start) is the only moment new extension bytes may begin to run.*

## Context

### The state before

- **Freshness is decided in exactly one place, and it always says yes.**
  `load-python-extensions!` (`src/com/blockether/vis/internal/python_extensions.clj:1559`) scans the
  extension dirs, builds a fingerprint of `[canonical-path, (extension/sha256-hex (slurp f))]` per
  file (`:1580-1583`) and compares it to `@last-fingerprint` (`:1585`). Same bytes → cheap no-op.
  One byte different → every file is re-executed through `load-file!` (`:1615`) and re-registered.
  There is no other freshness decision: a tool call never re-reads its own source.
- **Measured, this session, against a live GraalPy extension in a temp dir:** load a
  `probe_version()` returning `V1`, call it → `V1`; edit the file to `V2` and call the tool again
  with nothing in between → still `V1`; a plain `load-python-extensions!` (no `/reload`) →
  `{:loaded 1 :failed 0 :changed? true}`; call again → `V2`. The call site is not the risk. The
  loader is.
- **Three loader callers, and the third is not a human action.** `main.clj:4427` inside
  `discover-all!` (`:4415-4429`, run at process startup from `:4864`); `python_extensions.clj:1698`
  inside `reload-slash` (`:1690`), which calls `reload-python-extensions!` (`:1662-1667`) — the
  FORCE door that `reset!`s `last-fingerprint` to nil and re-executes even unchanged bytes; and
  `loop.clj:11470` inside `create-environment` (`:11466-11472`). That third one runs on an env cache
  miss via `open-env!`/`ensure-env!` (`loop.clj:12117`, call site `:12095`), on `recycle-env!`
  (`:12134`, invoked at `:12423`/`:12442` on the per-context turn cap and on a policy-epoch change),
  and for the `sub_loop` CHILD env (`loop.clj:10513`) — which the model builds on demand, **inside
  its own turn**.
- **A second door executes extension-dir Python with the SAME host trust and no fingerprint at
  all — and the model can knock on it directly.** `python_test_runner.clj:163-206` `run-test-file!`
  builds `pyx/build-context` (`:182`), calls `pyx/bind-host!` (`:184`), evaluates the bootstrap
  (`:186`) and then `exec`s the file's own source (`:192`). Its dirs come from
  `test-python-extensions!` (`:208-234`, defaulting to `default-extension-dirs` at `:228`), whose
  caller is the Python language pack: `extensions/languages/vis-language-python/src/com/blockether/vis/ext/language_python/core.clj:210`
  (`graalpy-test`), fed by `resolve-test-paths` (`:176-198`) — **FILES or directories named by the
  tool call**. So `run_tests("python", paths=["…"])` runs any `test_*.py` / `*_test.py` the model
  just wrote, in a trusted context, with no loader, no fingerprint and no `/reload`. The namespace
  docstring says this out loud (`:3-4`, "each in its own TRUSTED GraalPy context (same trust level
  as the extension it covers)"), which is the right sentence for an extension author's own tests and
  the wrong one for bytes nobody admitted. `test-python-extensions!` is public API too
  (`core.clj:629`).
- **Extension code is trusted by construction.** `python_extensions.clj:259` `build-context` grants
  `allowCreateProcess true` and `IOAccess/ALL`; `bind-host!` (`:283`) binds `__vis_host_shell__` to
  `foundation.shell/trusted-extension-shell` (`foundation/shell.clj:2421`) unconditionally at
  `:316-327` — not gated by the session's shell toggle, not the jailed path — beside
  `__vis_host_jailed_shell__` and `__vis_host_jailed_shell_session__` (`:328-349`). Plain
  `subprocess` also runs there, because this context is not `env_python/create-python-context` and
  carries no POSIX refusal shim (measured in session `15db52fb-9a44-45db-96e7-13b423eff274` turn 56,
  pinned by `python-extension-process-boundary-test`, commit `ed1161431`).
- **A load EXECUTES the file's top level**, so no symbol call is needed: cross-validated in the same
  session (turn 57) — an extension whose top level wrote a file created it during
  `load-python-extensions!`, `{:loaded 1}`, nothing ever invoked. A gate at a call site or in the
  symbol registry is therefore not a gate.
- **Both loader doors are PUBLIC API.** `core.clj:623-624` re-exports `load-python-extensions!` and
  `reload-python-extensions!`, so the phases must gate the Vars, not the three call sites.
- **The extension dirs are ordinary writable directories the model already reaches:**
  `~/.vis/extensions` and `<cwd>/.vis/extensions` (`python_extensions.clj:1407-1410`), writable by
  `write`, `patch`, `python_execution`'s `open()` and `shell`.
- **The static checker is a linter, not a gate:** `internal/extension_check.clj` judges an extension
  without running it (`:127` reaches the same dirs), and nothing consults it before `load-file!`.
- **No file watcher exists in the tree** (`WatchService`/`FileWatcher`: 0 hits) — the other half of
  "one loader decides freshness".

### The root problem

Nothing between "bytes on disk" and "bytes executing with `trusted-extension-shell`" distinguishes
*a human asked for this* from *the model wrote this 200 ms ago*. Freshness is not the hole;
**admission** is, and today admission happens silently on paths no human touched:

1. write `.vis/extensions/x.py` → call `sub_loop(…)` → the child env calls `create-environment` →
   the loader sees a new fingerprint → the file's top level executes → `vis.shell(…)` is live;
2. write `.vis/extensions/test_x.py` → call `run_tests("python", paths=[…])` → `run-test-file!`
   execs it in a trusted context — the loader is not even involved.

`/reload` never happened in either path, the shell toggle was never consulted, and no human ever saw
the file.

### What we solve, and what we do not

**The rule this plan enforces, stated once:** the only acts that may admit new or changed extension
bytes into a trusted context are **`/reload`** and **the gateway process starting**. Every other
path — child env, env recycle, idle rebuild, policy-epoch change, `run_tests`, doctor, a library
consumer of the public Vars — may keep running what was already admitted and may REPORT what is
pending, and may not adopt.

We do NOT solve:

- **Extension trust itself.** An admitted extension keeps `trusted-extension-shell`; `/reload` IS the
  trust decision. Narrowing what a trusted extension may do is a different plan.
- **Writing into the extension dirs.** The model may still put a file there; it will simply not run
  until a human reloads.
- **Classpath/JVM extensions** (`internal/manifest.clj`) — installed from the classpath the operator
  built, not from a directory the model can write.
- **A human who reloads without looking.** `/reload` is a deliberate act; this plan makes it the only
  one and makes it say what it admitted.

### Alternatives considered

- **A persisted approval ledger (`~/.vis/extension-trust.edn`) plus `vis-agent extension trust`.**
  The previous shape of this plan, **rejected on the user's instruction**: authorization is `/reload`,
  nothing else. It also fails on its own terms — a ledger is a file, and every seam that could
  protect it refuses: `~/.vis` is granted READ-WRITE engine-level and unconditionally
  (`config_spec.clj:869-881` → `:963` → `:989`), the sandbox filesystem's `confine!`
  (`sandbox_fs.clj:113`) names Vis's own `~/.vis` tree as an ALWAYS-allowed extra root, and the
  `:fs/access` gate is EXTENSION vocabulary on purpose (`editing/core.clj:656-660`,
  `extension.clj:2317-2324`), so protecting the ledger with a hook would rest the admission gate on
  the layer it admits. An approval that lives in a file the attacker can write is not an approval.
  Making the act the authorization deletes the artifact and the problem with it.
- **Deny the model's write tools inside `~/.vis/extensions/**` and `<project>/.vis/extensions/**`.**
  Defends a directory instead of an execution: `shell` when the toggle is on, `python_execution`'s
  `open()`, `git checkout` and a tarball extraction all still land bytes there.
- **Drop `<cwd>/.vis/extensions` from the default dirs** (proposed in session `15db52fb`, turn 57).
  Only moves the target — `~` is writable too — and costs the legitimate feature of a repo shipping
  its own extension.
- **Make `vis.shell` follow the caller's jail instead of the extension's trust.** Narrows the prize,
  not the hole: unadmitted bytes would still run arbitrary Python in the host context (host
  callbacks, op hooks, network, state store), and the same context reaches `subprocess` directly, so
  the shell binding is not even the last door.
- **Use `extension_check.clj` as the admission gate.** It judges the code's SHAPE; a hostile
  extension is trivially well-formed. Provenance is not a lint.
- **Sign extension files with a keypair.** A second trust root, key management and rotation, to
  answer a question `/reload` already answers.
- **Provenance by mtime or by "which tool wrote it".** mtime is set by the same write that planted
  the file, and tool provenance evaporates the moment bytes arrive over `shell` or `git`.
- **Prompting per pending file with a diff at `/reload`.** Not rejected on merit — it is a
  refinement of an act that is already the authorization, so it is out of scope here and can be
  proposed on its own once Phase 1 has made `/reload` the only door.

## Phase 1 — Adopt a changed fingerprint only at `/reload` and at startup

**Rationale.** Without it, escalation path (1) works today on every machine: three model steps, no
human, full trusted shell — and every 25-turn recycle re-reads whatever the bytes now say. This is
the phase that closes it.

**Data.** None. `:on-change` is an argument shape shared by two namespaces; nothing is persisted,
sent over the wire or mirrored in another language — which is the point of dropping the ledger.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/python_extensions.clj` — `load-python-extensions!` takes
  `:on-change`, `:adopt` or `:refuse`, and **`:refuse` is the default**, because both loader Vars are
  public API (`core.clj:623-624`) and a caller that names neither must get the safe one. Under
  `:refuse` a new or changed file is NOT executed and is reported as pending; already-loaded entries
  stay live.
- `src/com/blockether/vis/internal/python_extensions.clj` — under `:refuse` the fingerprint is taken
  over the **admitted** set, not the scan. Today a single changed hash tears down and re-executes
  EVERY file (`:1585`, `:1610-1627`), so a never-advancing `last-fingerprint` plus one pending file
  would re-run every admitted extension's top level on every child env, forever.
- `src/com/blockether/vis/internal/python_extensions.clj` — a pending file is reported through the
  EXISTING failure-row shape (`{:file :error}`, `:1634`), naming the file and saying that `/reload`
  admits it, so `vis-agent doctor` and the `/reload` footer print it with no new surface.
  Build-then-swap (`:1600-1609`) already keeps a previously admitted file live when a later load
  refuses.
- `src/com/blockether/vis/internal/python_extensions.clj` — `reload-python-extensions!`
  (`:1662-1667`) passes `:adopt` and stays the FORCE door: it nils `last-fingerprint`, so `:refuse`
  there would mean "a human asked and nothing happened". Its docstring states which of the two Vars a
  caller wants and that `/reload` is the authorization.
- `src/com/blockether/vis/internal/main.clj:4427` — startup (`discover-all!`) passes `:adopt`: a
  gateway start is the second human act. `src/com/blockether/vis/internal/loop.clj:11470` —
  `create-environment` passes `:refuse` explicitly, so the child env, `ensure-env!`, `recycle-env!`
  and the policy-epoch rebuild all report instead of adopting.
- `test/com/blockether/vis/internal/python_extensions_test.clj` — **the escalation reproduction**:
  write an extension whose top level records a marker and whose symbol calls `vis.shell`, run the
  loader the way `create-environment` does (`:on-change :refuse`), assert the marker was never
  written and no symbol registered and a pending row names the file; then
  `reload-python-extensions!` and assert it loads and registers. Also: an admitted file edited
  afterwards is not re-executed by a `:refuse` load, and the still-admitted old version keeps
  serving.
- `test/com/blockether/vis/internal/python_extensions_test.clj` — the existing fixture loads
  (`:55, :193, :376, :770, :778, :801, :826, :2081`) call the loader directly and stay `:adopt` by
  naming it; `:88/:110/:829` are `{:dirs []}` teardowns and `:1754` is a `with-redefs` stub, all
  untouched. There is no test-only bypass flag: naming `:adopt` at a direct call IS the human act
  the Var models.
- `test/com/blockether/vis/internal/loop_test.clj` — the env-build path calls the loader with
  `:refuse`.

**Unknowns.** Is every gateway process start a human act? A start the user commands is; a daemon
auto-started while resolving a client request is not. Planned answer: Phase 1 ships startup as
`:adopt` per the stated rule, and the auto-start path is measured immediately after — if the daemon
can be started by a tool call, that one call site takes `:refuse` and the rule becomes "the gateway
start a human commanded".

## Phase 2 — Stop `run_tests` from executing unadmitted bytes with host trust

**Rationale.** Without it Phase 1 closes one door and leaves the wider one open: escalation path (2)
needs no loader at all, and `run_tests("python", paths=[…])` is a tool the model calls freely. A test
file is bytes nobody admitted, by definition — it is not even scanned by the loader
(`python_extensions.clj:1428`).

**Data.** None. Nothing about the runner's result shape changes; only the context it builds.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/python_test_runner.clj:163-206` — `run-test-file!` builds a
  context WITHOUT host trust: no `__vis_host_shell__` binding, and the `vis` module's shell entry
  refuses with a message naming `/reload` and `vis-agent extension test`. The pytest shim,
  `sys.path` sugar and per-test record contract are unchanged, so an author's tests keep passing.
- `src/com/blockether/vis/internal/python_test_runner.clj:1-18` — the namespace docstring stops
  promising "the same trust level as the extension it covers" and states the new contract: a test
  runs beside the extension's code, not inside its trust.
- `extensions/languages/vis-language-python/src/com/blockether/vis/ext/language_python/core.clj:200-210`
  — `graalpy-test`'s docstring records that model-supplied `paths` never buy host trust.
- `test/com/blockether/vis/internal/python_test_runner_test.clj` — a test file calling `vis.shell`
  reports one failing test with the refusal message instead of spawning a process; a test file
  importing its extension and exercising pure Python still passes; a test file whose top level tries
  `subprocess` is refused the same way.

**Unknowns.** Does any shipped extension's own test suite depend on `vis.shell` inside a test? To be
answered by running the repo's own extension tests under the new context before the phase lands; if
one does, it moves to `vis.jailed_shell` in the same commit rather than the contract bending back.

## Phase 3 — Make `/reload` say what it just admitted

**Rationale.** After Phases 1 and 2, `/reload` is the authorization — and an authorization that
prints nothing teaches the user that reloading is free. Without this phase the pending state is
visible only as a failure row, and "which files did my reload just start running" has no answer.

**Data.** None. The `/reload` footer and `vis-agent extension list` are rendered text, not a
persisted or wire-crossing shape.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/python_extensions.clj:1690` — `reload-slash` reports, per file,
  what the reload did: admitted (new), re-admitted (changed), unchanged, removed — name and path,
  counts first so the line stays short in a narrow terminal.
- `src/com/blockether/vis/internal/main.clj` — the existing `extension list` (`cli-extensions!`,
  `:4296`, under the `extension` command at `:3846`) gains an admitted/pending column, and
  `vis-agent doctor` lists pending files with the sentence that `/reload` admits them. **Reporting
  only: no `trust`/`approve` subcommand exists, because a CLI approval would be a second
  authorization door and the rule has exactly one.**
- `test/com/blockether/vis/internal/python_extensions_test.clj` — a reload over one new, one changed,
  one unchanged and one removed file reports each exactly once; `extension list` marks a pending file
  pending; `extension check` still works on a pending file, since checking before reloading is the
  workflow this leaves the user.

**Unknowns.** None.

## State of the plan

**DONE**

Phases 1, 2 and 3 landed as ONE commit — *Admit Python extension bytes only at `/reload` or at
startup*. They are interlocked: Phase 1's refusal is only a real boundary once Phase 2 closes the
test-runner door, and Phase 3 is the sentence a refused file needs in order to be actionable.
Verified with `run_tests` on `com.blockether.vis.internal.python-extensions-test` (80/80),
`docs-test`, `self-docs-test`, `doctor-test`, `extension-check-test`, `private-deployment-hygiene-test`
(51/51) and `main-test`/`extension-test`; `lint_code` clean on every changed namespace.

What the code does differently from the acceptance criteria above, and why:

- **`:refuse` executes nothing at all**, so `last-fingerprint` is simply not touched on a refused
  scan — the planned "fingerprint over the admitted set" is unnecessary. The property it existed to
  protect is pinned directly: an admitted extension's top level does not re-run on a `:refuse` load
  that has a pending file beside it.
- **No `loop_test.clj` test.** Building a real environment needs a full session; the guarantee that
  covers `create-environment` is the DEFAULT — `a caller that names no :on-change gets the safe one`
  — so the loop call site could lose its explicit `:refuse` and still refuse.
- **The Phase 2 tests live in `python_extensions_test.clj`**, beside the runner's other tests
  (`untrusted-test-context-test`), rather than in a new `python_test_runner_test.clj`.
- **The refusal is a Python `PermissionError`, not a host throw.** A host exception is not a
  `BaseException`, so the pytest shim's `except Exception` would not catch it and ONE refusing call
  would abort the whole file instead of failing its own test.
- **`extension list` gained no pending column.** The CLI is a fresh process, and a fresh process
  adopts at startup, so its pending set is always empty — a column that can only ever read "admitted"
  is a lie about the running daemon. `vis-agent doctor` carries the pending WARNING instead, and
  `extension check` never loads a file, so it already works on a pending one.
- **The orphaned `/test` slash was wired back** (`python-test-runner/test-slash` was registered
  nowhere), because `cli-and-slash-wiring-test` was red on `main` for exactly that reason and this
  work is what made the test runner's trust a contract worth reaching from the TUI.

TODO: nothing. The Unknown in Phase 1 stands as recorded — whether a daemon auto-started while
resolving a client request counts as the human act that `:adopt` at startup assumes. Measuring that
auto-start path is the next piece of work, and it is a change to ONE call site if the answer is no.
