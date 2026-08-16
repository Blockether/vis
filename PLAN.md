# PLAN — Split the Vis extension contract from the engine and ship its Python half on PyPI

*The contract is a document both languages read; the engine is only its first implementation.*

## Context

**State before.**

- The whole Python extension surface — `vis.extension`, `vis.ask`, `vis.shell`, `vis.state`,
  `vis.provider`, the HITL field builders — is a 658-line Python program held in a Clojure-loaded
  string. `resources/vis-python/extension_bootstrap.py:3` opens `_vis_body = """`, `:662` closes it,
  `:664-681` build a `ModuleType("vis")`, inject a 13-entry `_host` dict of `__vis_host_*` polyglot
  globals and register `sys.modules["vis"]`. Nothing inside that string is importable, testable,
  installable or lintable: ruff sees one assignment, and `import vis` works in exactly one place on
  earth — inside a Vis extension context.
- The 13 host callbacks are hand-written three times: the binder
  `src/com/blockether/vis/internal/python_extensions.clj:332-462`, the static list
  `host-member-names` at `:484-493`, and the `_host` dict at `extension_bootstrap.py:665-679`.
  `doc("human-input")` can only state the drift rule as prose — "A new host callback is registered
  in `host-member-names` or the static checker breaks with a `NameError`".
- The extension VOCABULARY is 37 `s/def`s (`:ext/name` … `:ext/engine`, `:shim/name` …
  `:shim/bindings`) inside the 3637-line implementation namespace
  `src/com/blockether/vis/internal/extension.clj`. Nothing an author can depend on is smaller than
  the engine.
- 9 of the 16 `extensions/*/*/deps.edn` depend on `com.blockether/vis {:local/root "../../.."}` —
  the AGGREGATE, which root `deps.edn:129-151` defines as all 16 extensions. So every extension
  transitively drags every other one: `extension -> aggregate -> vis-foundation-voice ->
  com.github.k2-fsa.sherpa-onnx` (`extensions/common/vis-foundation-voice/deps.edn:15`). That is why
  all 16 extension `deps.edn` repeat `:mvn/repos {"jitpack" …}` (commit `0403732e6`) and why the
  13-job classpath matrix downloads an ONNX runtime to check a provider extension.
- The precedent for a cross-language contract in this repo is a test that READS the other language:
  `human_input_cross_channel_test.clj` reads the companion's TypeScript and fails when a field type,
  a bound or a choice glyph drifts from `internal/human_input/spec.clj`.

**Root problem.** Vis has a contract, but it exists only as implementation. Every mirror of it — the
Clojure binder, the Python bootstrap, the static checker, the companion's TypeScript, an extension
author's editor — is a hand copy kept honest by a reviewer's memory, and the only artifact anyone
can depend on is the entire distribution, ONNX included.

**What we solve.** One data document per contract seam, read by the engine at runtime and by every
mirror in test; the Python half becomes a real package that installs from PyPI and runs with no Vis
process at all, answering each host op with a declared outside behavior — a terminal prompt where
Vis would have raised a HITL dialog, a local file where Vis would have used the session store, an
explicit refusal where Vis would have jailed a process.

**What we do not solve.** No new schema library (`clojure.spec.alpha` only, `doc("human-input")`).
No compatibility shim for the string bootstrap — it is deleted, not deprecated. Not the companion's
TypeScript mirror, which already has its own drift test. Not a Python re-implementation of the
engine: outside Vis the package is an authoring and testing surface, never a second agent runtime.

**Alternatives considered.**

- *Generate the Python from the Clojure specs at build time.* Lost: a generated file cannot be
  edited by an extension author reading a traceback, and the generator becomes a fourth mirror.
- *Publish the package by copying `_vis_body` into a `pyproject.toml` project.* Lost: two sources of
  the same module, and the copy silently rots exactly like the three `__vis_host_*` lists do today.
- *Keep the aggregate and give extensions a `:provided` alias that excludes voice.* Lost: it hides
  the cycle instead of removing it, and every new extension has to remember the alias.
- *Name the PyPI distribution `vis`.* Lost: taken on PyPI ("Simple Visualization Tools"). The
  distribution is `vis-agent`, matching the `bin/vis-agent` CLI, and it provides the `vis` import
  name every existing extension already writes (`resources/examples/python-extensions/todo.py:13`).

## Phase 1 — Declare the Python host contract as data

**Rationale.** Without it the 13 host callbacks stay hand-copied in three places with a prose rule
guarding them, and neither the package nor its outside-the-sandbox behavior has anything to be
generated from or checked against.

**Data.**

```clojure
(s/def :op/name (s/and non-blank-string? #(re-matches #"[a-z][a-z0-9_]*" %)))
(s/def :op/global (s/and non-blank-string? #(re-matches #"__vis_host_[a-z0-9_]+__" %)))
(s/def :op/arity (s/int-in 1 4))
(s/def :op/summary non-blank-string?)
(s/def :op/outside #{:outside/local :outside/prompt :outside/refuse})
(s/def :op/refusal non-blank-string?)
(s/def :contract/op
  (s/and (s/keys :req [:op/name :op/global :op/arity :op/summary :op/outside]
                 :opt [:op/refusal])
         #(= (contains? % :op/refusal) (= :outside/refuse (:op/outside %)))))
(s/def :contract/ops (s/coll-of :contract/op :kind vector? :distinct true :min-count 1))
(s/def :contract/version pos-int?)
(s/def :contract/python-host (s/keys :req [:contract/version :contract/ops]))
```

**Acceptance criteria.**

- `resources/vis-contract/python-host.edn` — the 13 ops, each with its global, arity, summary and
  outside behavior.
- `src/com/blockether/vis/internal/python_contract.clj` — reads and validates the document; exposes
  `ops`, `op-names`, `host-globals`.
- `src/com/blockether/vis/internal/python_extensions.clj` — `host-member-names` derives from the
  document instead of listing names.
- `test/com/blockether/vis/internal/python_contract_test.clj` — the document conforms; its globals
  are exactly what the binder binds; its op names are exactly the `_host` keys read out of
  `extension_bootstrap.py`.

**Unknowns.** None.

## Phase 2 — Turn the `vis` module into a real Python package

**Rationale.** While the module is a string, it cannot be linted, unit-tested, type-hinted, read in
an editor or installed — and Phase 3 has no file to add a fallback host to.

**Data.** None. The module body moves file-to-file; no persisted, wire or mirrored shape changes.

**Acceptance criteria.**

- `packages/vis-agent/src/vis/__init__.py` — the former `_vis_body`, verbatim in behavior, with
  `_host` taken from the injected module dict when present.
- `resources/vis-python/extension_bootstrap.py` — shrinks to the injector: build the module, seed
  `_host` from the contract's globals, exec the packaged source, register `sys.modules`.
- `deps.edn`, `build.clj` — `packages/vis-agent/src` on `:paths`, the package in
  `-H:IncludeResources`.
- `test/com/blockether/vis/internal/python_extensions_test.clj` stays green; `ruff check` covers the
  new package.

**Unknowns.** Does GraalPy's `compile()` of the packaged source keep the ~100 ms context cost the
existing test measures?

## Phase 3 — Give the package a host for outside the sandbox

**Rationale.** Installed from PyPI with no Vis process, `import vis` currently cannot even be
imported; an author cannot unit-test a single tool function, and `vis.ask` has nowhere to ask.

**Data.** None. Phase 1's `:op/outside` already carries the policy; this phase implements it.

**Acceptance criteria.**

- `packages/vis-agent/src/vis/_outside.py` — one implementation per op: `state_*` on a JSON file
  under `.vis/`, `log`/`notify` on stderr, `request_input`/`check_input` as terminal prompts
  (`input()`, `getpass` for secrets, the same field vocabulary), `reveal_secret`/`forget_secret` on
  the in-process table those prompts fill, `declare_env` from `os.environ`, `shell` as a local
  subprocess, `jailed_shell*` refusing by name.
- `packages/vis-agent/tests/` — pytest over every op with no host bound, plus a registration test
  that loads `resources/examples/python-extensions/todo.py` outside Vis.
- `test/com/blockether/vis/internal/python_contract_test.clj` — every contract op has an outside
  implementation.

**Unknowns.** Should `shell` outside be a real subprocess or a refusal? Current answer: real, since
the jail is what Vis adds and `jailed_shell` is the one that refuses.

## Phase 4 — Publish `vis-agent` to PyPI

**Rationale.** Until it is installable, "write a Vis extension" still means "clone the repo".

**Data.** None. Packaging metadata only.

**Acceptance criteria.**

- `packages/vis-agent/pyproject.toml`, `README.md`, `LICENSE` — distribution `vis-agent`, import
  package `vis`, version stamped from `VIS_VERSION`.
- `scripts/` — the version stamp, mirroring how the companion's mirrors are stamped.
- A release step in `doc("release-vis")`'s sequence; publishing runs only on an explicit request.
- A test that the stamped version equals `VIS_VERSION`.

**Unknowns.** Which PyPI account and trusted-publisher identity ships it?

## Phase 5 — Extract `com.blockether/vis-contract` and repoint the extensions

**Rationale.** Until an extension can depend on something smaller than the distribution, a provider
extension resolves an ONNX runtime, the classpath matrix pays for it 13 times, and all 16
`deps.edn` carry a JitPack repo they have no use for.

**Data.** None. The 37 `s/def`s move namespace; no key is added, renamed or removed.

**Acceptance criteria.**

- `contract/deps.edn`, `contract/src/com/blockether/vis/contract/*.clj` — the `:ext/*` and `:shim/*`
  vocabulary and the registry, with no engine dependency.
- `src/com/blockether/vis/internal/extension.clj` — requires the contract instead of declaring it.
- The 9 extension `deps.edn` that name the aggregate name `com.blockether/vis-contract` instead;
  the `:mvn/repos` JitPack repeat is deleted wherever the dependency is gone.
- `.github/workflows/ci.yml` — the classpath matrix still resolves every directory.
- The existing extension registration tests stay green.

**Unknowns.** `vis-channel-tui` requires 8 engine namespaces beyond the vocabulary — does it depend
on the engine artifact, or does the contract grow a channel section?

## State of the plan

**ACCEPTED.** Phases 1-3 landed together in `feat(python): ship the extension API as the vis-agent
package`; a phase could not be split from the next one without leaving the module in two places.

- Phase 1 — DONE. `resources/vis-contract/python-host.edn` declares all 13 host ops;
  `internal.python-contract` reads, validates and serves them; `python_contract_test` reads a LIVE
  extension context's `vis._host` back and fails on any drift.
- Phase 2 — DONE. The module is `packages/vis-agent/src/vis/__init__.py` — on `:paths`, in the
  native image, and the file the engine execs; `resources/vis-python/extension_bootstrap.py` is
  the injector alone.
- Phase 3 — DONE. `packages/vis-agent/src/vis/_outside.py` answers every op by its `:op/outside`
  verdict; 25 pytest cases prove it, the last of them running `resources/examples/python-extensions/todo.py`
  in a bare interpreter. The `shell` op vocabulary is contract data too, so both hosts dispatch
  from one list; 33 pytest cases.
- Phase 4 — REQUIRES WORK. `scripts/version.mjs` mirrors `VIS_VERSION` into `pyproject.toml` and CI
  runs the package suite on the interpreter floor the package advertises; nothing uploads yet, and
  `vis-agent` is still unclaimed on PyPI.
- Phase 5 — TODO.

Remaining, in order:

1. Phase 4 — settle the PyPI trusted-publisher identity, add the build/upload step to
   `doc("release-vis")`, and claim the name. Publishing itself waits for an explicit request.
2. Phase 5 — extract `com.blockether/vis-contract` and repoint the 9 extension `deps.edn` that name
   the aggregate, so a provider extension stops resolving an ONNX runtime.
