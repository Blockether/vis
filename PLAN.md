# PLAN — remove the native filesystem tools, move the work into `python_execution`

Status: **in progress.** Phase 0 (delete ACP), the `vis_attach_bytes` merge, P0 (the
protected-paths predicate at `confine!`) and Phase 1 (the five filesystem verbs) are
DONE; Phase 2 and Phase 3 are still proposal.

## The finding that drives it

Every native tool we advertise is a schema the model reads before it acts, and each one
is an invitation to reach for a verb instead of writing code. Measured behaviour: **the
larger the native surface, the less `python_execution` gets used.** The model spends turns
choosing between `ls`, `file_exists` and `copy` when three lines of Python would have
answered the question and composed the answer in one pass.

The goal is therefore stated as a goal, not as a trade to be re-litigated per tool:
**remove as many native tools as possible; Python is the general instrument.** A tool
survives only if it is a JAIL or a CAPABILITY that Python cannot have, never because it is
convenient.

The same rule applies inside the sandbox: one verb per job. `vis_attach_bytes` was the
second spelling of `vis_attach`, so it is gone — `vis_attach(source, filename=None, …)`
takes a confined PATH, in-memory BYTES, or a matplotlib figure.

## P0 — the protected-paths predicate at `confine!`

The Python sandbox does not get the default filesystem. It gets
`sandbox-fs/confined-filesystem`, a GraalPy Truffle `FileSystem` that canonicalizes and
confines **every** path-touching operation before delegating: `newByteChannel`
(`file-read`/`file-write` by open options, `:219-241`), `createDirectory` (`:243`),
`delete` (`:244`), `copy` (`:245`), `move` (`:246`), `createLink` /
`createSymbolicLink` (`:247-249`), `setAttribute` (`:251`), plus every read/metadata
method. `..` traversal is defeated by `normalize`, symlink escapes by resolving through
the nearest existing ancestor's real path, and an empty root set denies everything
(fail-closed, `:5-25`). A refusal is
`[vis:sandbox_denied] operation=… reason=outside_approved_filesystem_roots`.

So `shutil.move`, `os.rename`, `open(…, "w")` and `pathlib.Path.unlink` are already gated
at the layer below Python — they *cannot* be patched away from inside the guest
(`env_python.clj:1629`). Removing the native verbs does not weaken confinement.

**What is genuinely not enforced there** is the finer, per-extension
`:ext/protected-paths` registry (`extension.clj:2266-2282`), which the native verbs consult
via `path-protected-before-fn` (`editing/core.clj:1072`). That is one predicate, and it has
exactly one natural home: the `c` confine closure in `confined-filesystem`
(`sandbox_fs.clj:202-203`), which every operation above already funnels through, with the
operation name already in hand.

**P0 (DONE):** `internal/protected_paths.clj` is now the ONE resolver — rule matching,
first-match-wins per extension and most-restrictive-wins across them — shared by
`editing/core.clj` and by `confine!`, which takes a `protected-fn` predicate
(`protected-paths/deny-fn`, wired in `loop.clj` from the live env + workspace root and
threaded through `env_python/create-python-context`). A protected path now refuses
`open(..., "w")`, `shutil.move` and `Path.unlink` with
`[vis:sandbox_denied] operation=… reason=path_protected hint=<the owner's own hint>`, and
a throwing registry fails CLOSED.

## Phase 1 — remove `copy`, `move`, `delete`, `create_directory`, `file_exists` (DONE)

Five schemas became zero. The Python replacements are exact and already in the sandbox
(`os`, `shutil`, `pathlib`, `glob`), and they run under the SAME confined filesystem —
`sandbox-fs/confine!` gates every path-touching operation, including the per-extension
`:ext/protected-paths` rules landed in P0. Removing the verbs weakened nothing.

Deleted from `src/com/blockether/vis/internal/foundation/editing/core.clj`: the five
`*-tool` fns and their symbols, the `*-safe` filesystem primitives (`create-dirs-safe`,
`copy-safe`, `move-safe`, `delete-if-exists-safe`, `exists-safe?`), the batch envelope
(`path-list`, `paths-arg-paths`, `paths-success`, `need-targets`, `first-two-arg-paths`)
and the renderers (`render-paths-result`, `render-copy-result`, `render-move-result`,
`paths-body`, `fs-plural`) — 396 lines. `src/com/blockether/vis/internal/prompt.clj`
now routes tree CHANGES to Python in one line; `src/com/blockether/vis/internal/loop.clj`
lost the `file_exists` mention in its concurrency-gate comment. Tests updated in
`test/com/blockether/vis/internal/foundation/editing/core_test.clj`,
`test/com/blockether/vis/internal/loop_test.clj`,
`test/com/blockether/vis/internal/prompt_test.clj`,
`test/com/blockether/vis/internal/env_python_test.clj` and
`test/com/blockether/vis/internal/env_python_form_eval_test.clj`;
`resources/examples/python-extensions/protected_paths.py` and
`resources/vis-docs/extending.md` no longer hook op names that do not exist.

One behaviour genuinely went away and is recorded here: the read-only ANCESTOR bypass
(`safe-read-ancestor-match?`) needed a composite WRITE target to be interesting, and
`delete(["."])` was the only one. After the removal every composite target is a read, and
writes into a protected path are refused per path at `confine!`.

## Phase 2 — remove `ls`, ship it as a sandbox helper (AGREED)

Decided: `ls` goes the same way. Its schema disappears; its capability does not.

What `os.listdir` lacks and the helper keeps: gitignore-aware filtering, batching over
several paths, `depth` nesting, dirs-first ordering, hidden-gating. That is ~30 lines the
model otherwise rewrites slightly wrong each time, so it ships as a **prebound sandbox
helper** callable from `python_execution`, documented in one prompt line like the other
prebound globals — no tool stanza, no JSON schema, composable with the surrounding code.

`cat` / `grep` / `struct_*` are the same fork applied later and separately; out of scope
for this plan, but the direction is the same one.

## Phase 3 — the shell family: one gated implementation, a Python handle, a readable log

The reported failure, in the reporter's own terms: a long command hits the 120 s
timeout, the model then polls `shell_logs` again and again, never manages to see the
WHOLE log, and spins. Three separate defects hide under that one symptom, and the fix
order below is the order in which they bite.

Measured, from `src/com/blockether/vis/internal/foundation/shell.clj`:

- `shell_run`'s sync timeout is `:521` — default **120 s**, floor 1, cap 600. On timeout
  the partial streams come back, but the command keeps no handle: there is nothing left
  to attach to, so the only recovery is to run it AGAIN.
- A background shell's ring buffer holds **2000 lines** (`:99-100`); older lines are
  dropped and only COUNTED (`dropped_lines`). `shell_logs` returns a TAIL of at most
  2000 (`n`, default 200). So for any build noisier than 2000 lines the head is already
  gone before the first poll, and no sequence of `shell_logs` calls can recover it.
- Each read additionally clips head+tail per stream and drops the MIDDLE (`:659`), so
  even one snapshot is not the log — it is a summary of the log.

That is why polling spins: **the tail is not a cursor.** Two consecutive reads overlap,
`dropped_lines` says data is gone without saying what, and there is no way to say
"give me everything AFTER what I already read".

### 3a — the log becomes a FILE with an OFFSET cursor (the actual bug)

Every background shell already owns a pty pump; it writes to a session-owned log file
under `~/.vis/` and the ring buffer becomes a convenience view over it, not the storage.
`logs` then takes an **offset** and returns `{lines, next_offset, eof, dropped: 0}` —
the same contract `cat` already has for files. A bounded polling loop becomes:

    off = 0
    while sh.is_running() or not eof:
        chunk = sh.logs(offset=off); off = chunk["next_offset"]
        if "BUILD FAILED" in chunk["text"]: break

No overlap, no silent drop, and a completed run's log is still there afterwards —
readable with `cat`, greppable with `grep`, and diffable across two runs. The middle-drop
clip stays, but only as a DISPLAY cap on one read, never as the storage.

### 3b — a timeout leaves a HANDLE, never a dead end

`shell_run` at its deadline stops returning "here is what I had". It returns the partial
result PLUS the id of a still-live background shell holding the same process, so the
model resumes reading instead of re-running a 10-minute build. This is what actually
ends the spin: a timeout stops being a lost turn.

### 3c — four handle tools collapse into one Python object

`shell_logs` / `shell_type` / `shell_stop` are pure operations on a handle, and the
documented usage is already a Python loop. `shell_background` returns a handle bound in
the sandbox: `sh.logs(offset=...)`, `sh.type(text)`, `sh.stop()`, `sh.is_running()`,
`sh.wait(seconds)` — and `sh.wait` is the bounded loop written ONCE, in one place, so no
one hand-rolls it wrong again. Four tools become one; nothing leaves the process jail,
because the object's methods call the same gated implementation the tools called.

`shell_run` stays a native tool: it is not `subprocess`. It is `bash -lc` under the OS
process jail with a bounded timeout, per-command exit rows and the `shell` feature
toggle — and the sandbox already routes `subprocess`, `os.system` and `os.popen` THROUGH
it, which is exactly the shape this plan wants everywhere.

`git` merges into `shell_run` last, or not at all: every call is expressible as
`shell_run([["git", ...]])`; the open question is only what the client loses in rendering
and classification. Decide it after 3a-3c ship.

Net: **two tools plus a handle object**, down from six, and a log that can be read whole.

## Order of work

1. ~~Phase 0 — delete ACP.~~ DONE (`8e6e9e413`).
2. ~~Collapse `vis_attach_bytes` into `vis_attach`.~~ DONE.
3. ~~P0 — the `:ext/protected-paths` predicate at the `confine!` seam.~~ DONE.
4. ~~Phase 1 removals + test updates.~~ DONE.
5. Phase 2 — remove `ls`, land the sandbox walk helper and its prompt line.
6. Phase 3 — 3a the offset-cursor log file, 3b the timeout handle, 3c the handle
   object; then the `git` question.

Each step ships as one commit with its own tests, lint-clean and formatted, per `AGENTS.md`.
