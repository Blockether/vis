# PLAN — remove the native filesystem tools, move the work into `python_execution`

Status: **proposal, not started.** Nothing in this file has been implemented.

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

`CHANGES.md` argued one-verb-one-schema against a merged `fs(opts)` mega-tool. That
argument still holds — a mega-tool is worse than five verbs — and it says nothing about
deleting the family outright. This plan is the other axis.

## P0 is NOT a blocker — the sandbox already confines the filesystem

Corrected after reading `src/com/blockether/vis/internal/sandbox_fs.clj`.

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
(`env_python.clj:1629`). The previous draft's claim that removing the verbs would let
`shutil.move` "bypass protection silently" was wrong about roots and is withdrawn.

**What is genuinely not enforced there** is the finer, per-extension
`:ext/protected-paths` registry (`extension.clj:2266-2282`), which the native verbs consult
via `path-protected-before-fn` (`editing/core.clj:1072`). That is one predicate, and it has
exactly one natural home: the `c` confine closure in `confined-filesystem`
(`sandbox_fs.clj:202-203`), which every operation above already funnels through, with the
operation name already in hand.

**P0, restated and de-scoped:** teach `confine!` the protected-paths predicate so a
protected path refuses with its own reason and the owning extension's API hint, alongside
the existing root check. One call site, one function, one test. It is a small task in the
same commit series — not a gate in front of the plan.

## Phase 1 — remove `copy`, `move`, `delete`, `create_directory`, `file_exists`

Python replacements are exact and already in the sandbox (`os`, `pathlib`, `shutil`,
`glob` are auto-imported or importable). Five schemas become zero, and the system prompt
loses a paragraph rather than gaining one.

Touch list (repo-wide grep):

- `src/com/blockether/vis/internal/foundation/editing/core.clj` — the five registrations
  and their finish-call renderers (`:5329`, `:5407`, `:6565-6609`, `:7954-8052`).
- `src/com/blockether/vis/internal/prompt.clj:237` — the "native tools" paragraph.
- `src/com/blockether/vis/internal/foundation/acp.clj` — deleted outright (Phase 0).
- `src/com/blockether/vis/internal/loop.clj:796,3973` — observation tagging, call synthesis.
- Tests: `editing/core_test.clj:96,101,111-121,2388-2429,6217-6231`,
  `acp_test.clj:609-610`, `loop_test.clj:3836,3896-3909,3973`, `prompt_test.clj:94`,
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

## Phase 0 — DELETE ACP

Decided: ACP goes. Not "classified by effect kind", not kept behind a toggle — removed.
It was the only thing that made the removals look expensive: tool cards, permission
prompts and result rendering are keyed on tool NAME (`foundation/acp.clj:430-446`), so
moving work into `python_execution` was said to cost a legible transcript. With ACP gone
that cost does not exist and nothing in Phase 1-3 owes it a design.

What goes:

- `src/com/blockether/vis/internal/foundation/acp.clj` — the whole namespace: the
  `"acp"` extension registration (`:1402`), the stdio `serve!` transport (`:1154+`),
  the `POST /v1/acp` gateway route contribution (`:1229-1358`), the `/acp` slash command
  (`:1364-1395`), the `around-hook` permission/mirror op-hook (`:1109-1143`), the
  session-connection registry (`:513`), `tool-kind`, `event->update`, `prompt->text`.
- `test/com/blockether/vis/internal/foundation/acp_test.clj` — deleted with it.
- `src/com/blockether/vis/internal/extension.clj:3684` — drop it from the builtin
  extension namespace list.
- `src/com/blockether/vis/internal/foundation/mcp/core.clj:230,1152` — the
  session-scoped MCP server path exists for ACP `mcpServers`; check whether any other
  client attaches one, and delete the path if not.
- The `vis acp` CLI entry point and any docs/README mention.

No deprecation, no alias, no compatibility shim (`AGENTS.md`: remove obsolete paths).
This lands FIRST, as its own commit, because it deletes the only stated objection to
everything after it.


## Order of work

1. Phase 0 — delete ACP: namespace, tests, extension registration, gateway route, slash
   command, CLI entry. Suite green without it.
2. P0 — at the `confine!` seam: the `:ext/protected-paths` predicate, with a red-then-green
   test that a Python `shutil.move` onto a protected path refuses. No effect channel is
   needed any more; nothing consumes it.
3. Phase 1 removals + test updates.
4. Phase 2 — remove `ls`, land the sandbox walk helper and its prompt line.
5. Phase 3 — the `shell_background` handle object; then the `git` question.

Each step ships as one commit with its own tests, lint-clean and formatted, per `AGENTS.md`.
