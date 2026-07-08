# TODO — attachments feature

Tracking what's left after the attachment rail work (sink-at-source capture →
`session_attachment` persistence → vision-gated replay → introspection read-back).

## Done (for context)
- V4 `session_attachment` — one table, both rails (tool = iteration-owned,
  user = turn-owned), `source` column, `(iteration, tool_call_id, position)`
  grain for tools; self-describing handle ids kill the read-back fallback.
- Capture: dependency-free `*attachment-sink*`, scoped per `python_execution`
  block, conveyed across `gather` virtual threads (regression test locked).
- Generic `vis_attach` / `vis_attach_bytes` + filesystem outbox tap
  (confined-FS `newByteChannel`) — any artifact type, not just matplotlib.
- Replay gated on `:vision`; non-vision gets a text placeholder.
- `session_fold` now collapses vision replay too (collapse wins over
  provenance) — image lives exactly as long as its iteration's text.
- Web history restores artifacts from DB bytes (survives restart); `vis-image`
  fence stripped from DB-restored bodies; comment profanity removed.
- Session-level introspection: `db-list-session-attachments` rolls up the whole
  session (user + tool) in one join over the state chain; the `vis_attachments()`
  reader now uses it instead of a hand-rolled per-turn N+1 walk.

## Open

### P0 — Real vision-model round-trip verification
Everything so far is **unit-level only** (`conversation-suffix` shapes + fold
gating). Never proven against a live model that (a) a generated plot actually
reaches a vision model as an image, and (b) after `session_fold` the
`image_url` block is truly gone from the outbound request.
- Option A (fastest): manual — `plt.show()` → `session_fold` that iteration →
  continue, confirm behaviour/cost on the live endpoint.
- Option B: integration test that dumps the real outbound request (or hits a
  live vision endpoint) and asserts no `image_url` block post-fold.

### P1 — Triage pre-existing red test
`provider-stream-rewind-retry` fails on main (ArityException in
`run-iteration`) — confirmed pre-existing / unrelated to attachments, but it's
red. Triage separately.

### P2 — External storage (S3) is a stub
`storage_uri` column + nil-`bytes` reader guards + exactly-one CHECK are in
place, but **no producer** writes external payloads and **no resolver** turns
`storage_uri` → bytes on read-back/replay. Wire the resolver + a producer path
when S3 (or any external store) lands.

### P2 — Python read-back ergonomics
`vis_read_attachment(id)` + listing exist. Confirm the reuse story (list →
pick id → read bytes back into a later block) is ergonomic enough; add
examples/tests if a real reuse flow surfaces.

### P3 — Perf: byte reload on live replay
Live (non-folded) image iterations reload attachment bytes from DB every turn
for vision replay. Fine for now; consider a per-turn cache if cost bites at
scale.

---

# TODO — Python extensions / shims / test runner

Gaps left after the requests shim, pytest shim, package-aware scan, `sys.path`
sugar, `/test` + `vis ext test`, and the runner extraction into
`python_test_runner.clj`. Everything below is verified against on-disk source
(t21), not warm-REPL state.

## Done (for context)
- `requests` shim (`shim_requests.clj`) — urllib-backed, full verb surface,
  `Session`, `Response`, multipart `files=`, cookie jar, redirects,
  `iter_content`/`iter_lines`, Basic `auth=`. Pure stdlib, zero `:shim/bindings`.
- `pytest` shim (`shim_pytest.clj`) — assert introspection, `raises`/`warns`/
  `approx`/`skip`/`xfail`/`skipif`, `@fixture` (function/module/session scope,
  yield-teardown, autouse, recursive inject), `parametrize`, `monkeypatch`/
  `capsys` builtin fixtures, `pytest.main()`.
- Package-aware scan: `my_ext/extension.py` = one package extension; `test_*.py`
  / `*_test.py` excluded from the extension scan; `sys.path` sugar so
  `import mypkg` works from `extension.py`.
- Test runner: `discover` → per-file fresh trusted ctx → structural per-test
  outcomes (`frequencies` over `:outcome`, NOT regex-scraped) → `/test` slash +
  `vis ext test` CLI (returns exit-code, no `System/exit` in the runner).
- Docs: `resources/vis-docs/python-extensions.md` (packages, testing, batteries,
  trust section).

## Open

### P0 — Trust gate (RCE) — the one real hole
Every `.py` under `.vis/extensions/` — extension **and** `test_*.py` — runs in
`build-context` with `IOAccess/ALL` + native access. `git clone` a repo with
`.vis/extensions/foo.py` and Vis executes attacker code with full host access,
no prompt. `vis ext test` made this worse: it now scans + runs `test_*.py` too,
so the auto-run surface grew. Docs only *warn* (`python-extensions.md:218`);
nothing enforces. Need one of: per-project trust prompt ("trust this project's
extensions?", remembered like VS Code), a manifest allowlist, or an explicit
opt-in flag before any project-local `.py` is loaded/tested.

### P1 — No test selection / filtering
Runner is whole-suite only. No `vis ext test <file>` / `-k <pattern>` / single
extension, no run-one-test. Painful once an author has more than a handful of
files. Add a selector to `test-python-extensions!` + CLI/slash arg.

### P1 — `tmp_path` / `capfd` unavailable in the trusted test ctx
The shim disables `tmp_path`/`capfd` because the **model sandbox** has no FS
(`shim_pytest.clj:473`). But extension tests run in a **trusted** context that
*does* have a real filesystem — so an author testing FS-touching code can't use
the most common fixture. Consider FS-backed builtin fixtures on the trusted
runner path only (a real temp dir, cleaned per test).

### P2 — Runner is serial
One fresh trusted context per test file, run sequentially (~30 ms/file warm).
Fine now, linear at scale — `pmap`/bounded pool over `discover-tests` pairs
would cut wall-time for large suites. Contexts are independent, so it's safe.

### P2 — `requests` shim edges
`verify=`/`cert=` are accepted but not honored (always default TLS); no real
`stream=True` lazy body (urllib buffers the whole response); no proxies /
adapters / `Retry`. Fine for scripting — fill or document on demand.

### P2 — `pytest` shim edges
No `conftest.py`, no plugins/pluggy, no CLI. Documented as a subset, but worth a
`--collect-only`-style listing and clearer per-test node IDs in the report.

### P3 — Discovery / preamble cost per run
Each run re-pulls the pytest preamble and builds a fresh trusted context per
test file. Acceptable (shared warm engine), but a cached preamble + optional
context reuse would trim cold runs if suites grow.
