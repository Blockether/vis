# PLAN — remove the native filesystem tools, move the work into `python_execution`

Status: **in progress.** Phase 0 (delete ACP), the `vis_attach_bytes` merge and P0 (the
protected-paths predicate at `confine!`) are DONE; everything below is still proposal.

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

## Phase 1 — remove `copy`, `move`, `delete`, `create_directory`, `file_exists`

Python replacements are exact and already in the sandbox (`os`, `pathlib`, `shutil`,
`glob` are auto-imported or importable). Five schemas become zero, and the system prompt
loses a paragraph rather than gaining one.

Touch list (repo-wide grep):

- `src/com/blockether/vis/internal/foundation/editing/core.clj` — the five registrations
  and their finish-call renderers (`:5329`, `:5407`, `:6565-6609`, `:7954-8052`).
- `src/com/blockether/vis/internal/prompt.clj:237` — the "native tools" paragraph.
- `src/com/blockether/vis/internal/loop.clj:796,3973` — observation tagging, call synthesis.
- Tests: `editing/core_test.clj:96,101,111-121,2388-2429,6217-6231`,
  `loop_test.clj:3836,3896-3909,3973`, `prompt_test.clj:94`,
  `env_python_form_eval_test.clj:1023-1059`, `env_python_test.clj:114`.

Delete the paths; do not deprecate them. No aliases, no compatibility shims.

## Phase 2 — remove `ls`, ship it as a sandbox helper (AGREED)

Decided: `ls` goes the same way. Its schema disappears; its capability does not.

What `os.listdir` lacks and the helper keeps: gitignore-aware filtering, batching over
several paths, `depth` nesting, dirs-first ordering, hidden-gating. That is ~30 lines the
model otherwise rewrites slightly wrong each time, so it ships as a **prebound sandbox
helper** callable from `python_execution`, documented in one prompt line like the other
prebound globals — no tool stanza, no JSON schema, composable with the surrounding code.

`cat` / `grep` / `struct_*` are the same fork applied later and separately; out of scope
for this plan, but the direction is the same one.

## Phase 3 — the shell family (OPEN, not settled)

Current surface: `shell_run`, `shell_background`, `shell_logs`, `shell_type`, `shell_stop`
(`internal/foundation/shell.clj`), plus `git`. Six tools. Under the stated goal the target
is the smallest number that keeps the process jail.

- **`shell_run` — keep for now.** It is not `subprocess`: it is `bash -lc` under the OS
  process jail with a bounded timeout, retained/truncated streams, per-command exit rows,
  and the `shell` feature toggle. The sandbox already routes `subprocess`, `os.system` and
  `os.popen` *through* it — one gated implementation, reachable from either surface, which
  is exactly the shape this plan wants everywhere.
- **`shell_logs` / `shell_type` / `shell_stop` — strongest removal candidates.** All three
  are pure handle operations on a live PTY, and the documented usage is already a Python
  polling loop. Target shape: `shell_background` returns a handle OBJECT bound in the
  sandbox with `.logs(n)`, `.type(text)`, `.stop()` — four tools become one, the bounded
  loop becomes idiomatic, nothing leaves the jail.
- **`git` — merge candidate.** Every call is expressible as `shell_run([["git", …]])`.
  Open: what the client loses in rendering/classification.

Net target: **two tools plus a handle object**, down from six. Confirm the handle shape
before committing to it.

## Order of work

1. ~~Phase 0 — delete ACP.~~ DONE (`8e6e9e413`).
2. ~~Collapse `vis_attach_bytes` into `vis_attach`.~~ DONE.
3. ~~P0 — the `:ext/protected-paths` predicate at the `confine!` seam.~~ DONE.
4. Phase 1 removals + test updates.
5. Phase 2 — remove `ls`, land the sandbox walk helper and its prompt line.
6. Phase 3 — the `shell_background` handle object; then the `git` question.

Each step ships as one commit with its own tests, lint-clean and formatted, per `AGENTS.md`.
