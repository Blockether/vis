# PLAN — shrink the native tool surface to jails, and make the shell log readable

*Python is the general instrument; a native tool survives only if it is a jail.*

## Context

### The state before

- Every native tool is a schema the model reads before it acts, and each one is an
  invitation to reach for a verb instead of writing code. `copy`, `move`, `delete`,
  `create_directory` and `file_exists` were 396 lines of
  `src/com/blockether/vis/internal/foundation/editing/core.clj` restating what `os`,
  `shutil` and `pathlib` already do inside the sandbox.
- The Python sandbox does not get the real filesystem. It gets
  `sandbox-fs/confined-filesystem` (`src/com/blockether/vis/internal/sandbox_fs.clj:179`),
  a GraalPy Truffle `FileSystem` where EVERY path-touching operation funnels through one
  `confine!` closure (`:110-154`) before it delegates: `newByteChannel`
  (`file-read`/`file-write` by open options, `:219-241`), `createDirectory` (`:243`),
  `delete` (`:244`), `copy` (`:245`), `move` (`:246`), `createLink`/`createSymbolicLink`
  (`:247-249`), `setAttribute` (`:251`), plus every read/metadata method. `..` is defeated
  by `normalize`, symlink escapes by resolving through the nearest existing ancestor's real
  path, and an empty root set denies everything (`:5-25`). It is constructed in exactly one
  place, `env_python.clj:1817`, so it gates guest traffic and nothing else.
- Extension-declared path rules (`{:glob :access :hint}`) were consulted only by the native
  file verbs, so `delete("secrets/x")` was refused while `Path("secrets/x").unlink()` went
  straight through. P0 (`b11aa1706`) fixed the seam by threading `protected-paths/deny-fn`
  into `confine!` (`sandbox_fs.clj:114-153`) — but it bought that with a whole second
  extensibility mechanism: `internal/protected_paths.clj` (171 lines), `:ext/protected-paths`
  plus `::protected-path` and the access vocabulary (`extension.clj:532-543`),
  `active-protected-globs` (`:2263-2284`), the `s/keys` entry (`:1292`), and `deny-fn`
  plumbing through `loop.clj:11279-11290` and `env_python`.
- Extensions ALREADY have a general mechanism for exactly this: `register-op-hook!`
  (`extension.clj:2348`) keys hooks by any op keyword, `:around` hands the hook `next` and
  lets it refuse to call it (`:2446-2470`), and Python reaches it today —
  `vis.hook(ops=[…], phase='before')` mints the op keyword from the author's own string
  (`python_extensions.clj:916-918`) and `vis.block(reason)` is the veto (`guard-adapter`,
  `:656-678`). What hooks lacked was not power, it was a SEAM: they fire at the tool
  chokepoint, so a path guard could only scan a tool's string arguments — which is what
  `resources/examples/python-extensions/protected_paths.py` does and why it cannot see
  `open(p, "w")`, `shutil.move` or `Path.unlink`.
- The shell family is six tools and a log nobody can read whole. Measured in
  `src/com/blockether/vis/internal/foundation/shell.clj`: sync timeout default **120 s**
  (`:521`) with NO handle left behind on expiry, so recovery means re-running the command; a
  background shell's ring buffer holds **2000 lines** (`:99-100`) and older lines are dropped
  and merely COUNTED (`dropped_lines`), with `shell_logs` returning a TAIL (`n`, default
  200); and each read additionally clips head+tail per stream and drops the MIDDLE (`:659`).

### The root problem

Two forces, one plan.

1. **A convenience tool costs more than it saves.** The larger the native surface, the less
   `python_execution` gets used: the model spends turns choosing between `ls`, `file_exists`
   and `copy` when three lines of Python would have answered the question AND composed the
   answer in one pass. A tool is worth a schema only when it is a JAIL (a process, a confined
   filesystem) or a capability Python cannot have.
2. **The tail is not a cursor.** Two consecutive `shell_logs` reads overlap, `dropped_lines`
   says data is gone without saying what, and there is no way to ask for "everything AFTER
   what I already read". That is why a timed-out build turns into a polling spin that never
   sees the whole log — the reported symptom.

### What we solve, and what we do not

We solve: the filesystem verbs, the second guard mechanism, `ls`, and the whole shell family
(cursor, timeout handle, one implementation under `shell_run`/`subprocess`/`git`).

We do NOT touch `cat`, `grep` or `struct_*` in this plan. They are the same fork applied
later and separately — anchors, structural parsing and gitignore-aware search are a different
argument, and mixing them in would stall the shell fix that actually bit the user.

### Alternatives considered

- **Keep the filesystem verbs and merely route them through the same guard.** Lost: it keeps
  five schemas alive to duplicate `shutil`, and the guard was never the reason they existed.
- **Unify `protected_paths.clj` and `extension.clj` into one vocabulary (the previous P1).**
  Lost: it still ends with two extensibility mechanisms, and the merge needed a generic
  `active-declared-rows` collector, a Python spelling of the key that does not exist, and a
  cross-language mirror test to keep the two spellings honest. A gate hook expresses the same
  refusal with the mechanism extensions already have and Python already speaks.
- **Add `:ext/protected-paths` to `vis.extension(...)` for Python.** Lost for the same
  reason: a fourth declarative key, mirrored in `extension_bootstrap.py`, pinned by a drift
  test, to say what one hook already says.
- **Make the ring buffer bigger (say 200k lines).** Lost: it moves the cliff instead of
  removing it, and still cannot answer "after what I read". A file plus an offset has no
  cliff and is greppable after the run.
- **Keep `shell_logs`/`shell_type`/`shell_stop` as tools and only add the cursor.** Lost: the
  documented usage of all three IS a Python loop; leaving them native keeps three schemas for
  operations on one object, and keeps `sh.wait` hand-rolled at every call site.
- **Delete `shell_run` too and expose only `subprocess`.** Lost: the process gate is a real
  jail (feature toggle, cwd confinement, timeout policy, exit-as-data) and one native entry
  point is what makes `subprocess` gateable at all.
- **Keep `shell_background` as its own tool beside `shell_run`.** Lost: they differ only in
  how long the caller waits, so `wait=0` says it with an argument instead of a schema — and
  two tools mean two lifecycles, two result shapes and two places for the log contract to
  drift.
- **Store the log in the session database.** Lost: the access pattern is append-and-read-by-
  offset, which sqlite serves by rewriting a blob per flush and substringing it per read. The
  DB gets the INDEX row (session, id, command, `::log-path`, exit) so a log is findable in a
  later turn; the bytes stay a file `cat` and `grep` already know how to read.

## Phase 1 — Delete the five filesystem verbs

**Rationale.** Without it the plan is a memo: five schemas keep advertising a verb for work
Python does better, and the guard question stays theoretical because the verbs are the only
thing consulting the rules.

**Data.** None. Schemas and their result maps are removed; nothing new is encoded, persisted
or mirrored.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/foundation/editing/core.clj` — the five `*-tool` fns and
  symbols, the `*-safe` primitives (`create-dirs-safe`, `copy-safe`, `move-safe`,
  `delete-if-exists-safe`, `exists-safe?`), the batch envelope (`path-list`,
  `paths-arg-paths`, `paths-success`, `need-targets`, `first-two-arg-paths`) and the
  renderers (`render-paths-result`, `render-copy-result`, `render-move-result`, `paths-body`,
  `fs-plural`) deleted — 396 lines.
- `src/com/blockether/vis/internal/prompt.clj` — tree CHANGES route to Python in one line.
- `src/com/blockether/vis/internal/loop.clj` — the `file_exists` mention leaves the
  concurrency-gate comment.
- Tests: `editing/core_test`, `loop_test`, `prompt_test`, `env_python_test`,
  `env_python_form_eval_test` updated; `editing/core_test` pins that path protection is now
  proven at `confine!` (`internal.sandbox-fs-test`), not at a verb.

**Unknowns.** None left — the one behaviour that genuinely went away is recorded: the
read-only ANCESTOR bypass (`safe-read-ancestor-match?`) needed a composite WRITE target to be
interesting, and `delete(["."])` was the only one.

## Phase 2 — Replace protected paths with one `:fs/access` GATE hook

**Rationale.** Without it the engine carries two extensibility mechanisms for one job, and
the older one is the weaker: it is Clojure-only, it needs a spec, a resolver, a precedence
rule and an aggregation, and its shipped example guards TOOL NAMES rather than paths. Delete
the vocabulary, keep the seam P0 proved.

**Data.** None. The phase DELETES a declarative key and calls a mechanism that already
exists; nothing new is persisted, put on the wire, or mirrored in another language. The hook's
argument is two strings and is described where it is used, below.

**How an extension uses it, exactly.** The gate is asked from `confine!`
(`sandbox_fs.clj:110-154`), which is the Truffle `FileSystem` under the guest interpreter — so
it fires for ORDINARY Python, not for a list of sandbox verbs: `open(p, "w")` and
`Path(p).write_text` (`newByteChannel`, `:219-241`), `os.mkdir` (`:243`), `Path.unlink` /
`os.remove` / `shutil.rmtree` (`:244`), `shutil.copy` (`:245`), `shutil.move` / `os.rename`
(`:246`), `os.symlink` (`:247-249`), `os.chmod` (`:251`), and every read/metadata call. There
is no way for guest code to reach a file except through it. The native writers `write`,
`patch` and `struct_patch` ask the SAME op, so one rule covers both surfaces. A guard is:

```python
@vis.hook(ops=["fs_access"])
def no_secrets(ctx):
    if ctx["operation"] != "file-read" and fnmatch(ctx["path"], "**/secrets/*"):
        return vis.block("secrets/ is read-only; ask the owner before writing there")
```

`ctx` is `{"operation": <one of file-read file-write create-directory delete copy move
create-link set-attribute metadata>, "path": <absolute, already real-pathed>}`; returning
`None` allows, `vis.block(reason)` refuses with that sentence, and Clojure extensions register
the identical op through `register-op-hook!`.

Three rules the mechanism itself must carry, because the guard author cannot: gate hooks are
asked in registration order and ANY block wins, with no `next` handed to anyone (no extension
can swallow another's refusal — the property `most-restrictive-wins` bought, without a
precedence rule); a hook that THROWS denies (fail CLOSED, deliberately inverting
`guard-adapter:660`, which logs and runs the op — right for a convenience guard, wrong for a
boundary, and fail-open stays the default for every non-gate op); and a path under
`extra-roots` (the engine outbox, `/tmp`/`$TMPDIR`, `~/.vis`) is NOT gated, because those are
engine-owned surfaces lent to the guest, not user data.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/protected_paths.clj` — deleted (171 lines).
- `src/com/blockether/vis/internal/extension.clj` — `:ext/protected-paths`,
  `::protected-path`, `::protected-paths-result`, the access vocabulary (`:532-543`),
  `active-protected-globs` (`:2263-2284`) and the `s/keys` entry (`:1292`) removed;
  `gate-op?` and `run-gate-hooks` added beside `run-op-around`, with the same "no entry ⇒ no
  call" short-circuit so an unhooked engine pays nothing.
- `src/com/blockether/vis/internal/sandbox_fs.clj` — `confine!` loses `protected-fn` and asks
  the `:fs/access` gate after the root check and only for non-`extra-roots` paths; a
  thread-local in-gate flag skips the gate for operations a hook itself performs, so a guard
  may read a file without recursing; the refusal keeps
  `[vis:sandbox_denied] operation=… reason=path_protected hint=<the hook's reason>`.
- `src/com/blockether/vis/internal/foundation/editing/core.clj` — `write`/`patch`/
  `struct_patch` ask the SAME gate before touching a file, so a rule is never "Python only";
  the `pp` require and `path-protected-before-fn` go.
- `src/com/blockether/vis/internal/loop.clj`, `internal/env_python.clj` — `deny-fn`
  construction and `protected-fn` threading through `create-python-context` and
  `fork-context!` removed.
- `src/com/blockether/vis/internal/python_extensions.clj` — `guard-adapter` fails CLOSED for
  gate ops; no new builder key (`vis.hook(ops=['fs_access'])` already mints `:fs/access`).
- `resources/examples/python-extensions/protected_paths.py`,
  `resources/vis-docs/extending.md` — rewritten around `fs_access`: the example declares its
  own globs in Python, and the bootstrap docstring gains the one sentence that a gate hook
  returns `vis.block(reason)` or `None` and that raising denies.
- Tests: `sandbox_fs_test` keeps the P0 regression restated as a hook (a registered
  `:fs/access` hook denies `Path.unlink`; a THROWING hook denies too; an `extra-roots` write
  is not gated); a new test proves two hooks compose as a veto chain and that the re-entrancy
  skip lets a guard read a file; `python_extensions` test proves a Python `fs_access` guard
  blocks a sandbox write with its own sentence while `cat` of the same file still works.

**Unknowns.** Which TOOL-side writers have to ask the gate themselves? The question exists
because `confine!` is not universal. `confined-filesystem` is built in exactly one place,
`env_python.clj:1817`, and installed only as the GUEST interpreter's `FileSystem`; native
Clojure never passes through it — `editing/core.clj` writes with a plain `spit` (`:3811`,
`:4060`) straight onto the JVM filesystem. So a rule an extension declares would bind every
`open(p, "w")` in Python and NOTHING in Clojure unless each writer is made to ask. Three
groups, and only the middle one is actually open:

1. **Model-driven writers** — `write`, `patch`, `struct_patch`. They MUST ask; it is already
   in the acceptance criteria above, because otherwise "protected" only ever meant
   "protected from Python", and the model would route around the rule by picking a tool.
2. **Engine writers acting on the guest's behalf** — the outbox tap (`sandbox_fs.clj:242-244`,
   which turns a guest write under `$VIS_OUTBOX` or a temp root into a
   `session_iteration_attachment` row) and `attach`. Undecided: the guest chose the bytes but
   the engine chose the destination, and that destination is under `extra-roots`, which this
   phase already declares ungated. Current lean is NOT gated — the rule is about the user's
   tree, not about engine surfaces lent to the guest.
3. **Engine bookkeeping** — session persistence, the event journal, anything under `~/.vis`.
   Never gated. A hook that can refuse the engine's own writes can brick a live session.

The answer decides exactly one thing: whether `run-gate-hooks` is called from one place more
than `confine!` and the three editors.

## Phase 3 — Remove `ls`, ship the walk as a sandbox helper

**Rationale.** Without it the cheapest tool to replace keeps its schema: `ls` is a directory
listing, and the model reaches for it instead of composing a listing into the answer it was
already computing. But `os.listdir` is not the same thing, so deleting the schema without
shipping the capability would make every listing slightly wrong.

**Data.** None. The listing rows are the ones the `ls` tool already returns; the phase moves
who computes them, and the rows never leave the guest's own Python.

**Acceptance criteria.**

- The `ls` tool schema and handler are deleted; the prompt loses its stanza and gains one
  prebound-global line, like the other sandbox helpers.
- The helper keeps what `os.listdir` lacks: gitignore-aware filtering, batching over several
  paths, `depth` nesting, dirs-first-then-alphabetical ordering, hidden gating — ~30 lines
  the model otherwise rewrites slightly wrong each time.
- It runs under the confined filesystem, so Phase 2's gate governs it with no extra wiring.
- Test: the helper's rows equal the tool's former rows for a fixture tree (ordering, hidden
  gating, gitignore, `depth`), proving nothing was lost with the schema.

**Unknowns.** Is a gitignore-aware walk fast enough in guest Python on a large tree, or does
the helper need to call the host's existing ignore matcher? Measure on this repo before
choosing.

## Phase 4 — Give the background log a FILE and an OFFSET cursor

**Rationale.** This is the reported bug. Without it the head of any log noisier than 2000
lines is gone before the first poll, `dropped_lines` names nothing, and no sequence of reads
recovers the whole output — the polling spin.

**Data.** The chunk is about BYTES and nothing else.

```clojure
(s/def ::id           (s/and string? seq))
(s/def ::offset       (s/int-in 0 Long/MAX_VALUE))   ; BYTE offset into the log file
(s/def ::next-offset  ::offset)                      ; feed this back to continue
(s/def ::text         string?)                       ; exactly the bytes between the two
(s/def ::is-eof       boolean?)                       ; nothing more WRITTEN yet — not "done"
(s/def ::is-truncated boolean?)                       ; a DISPLAY cap on THIS read only
(s/def ::log-chunk
  (s/and (s/keys :req-un [::id ::offset ::next-offset ::text ::is-eof]
                 :opt-un [::is-truncated])
         #(>= (:next-offset %) (:offset %))))
```

Three keys the earlier draft carried are gone, and each one was the same fact twice.
`::log-lines` was `::text` split on newlines — one payload in two spellings, and the reader
that trusts the wrong one loses a partial last line. `::status` and `::exit` describe the
PROCESS, not the read: they belong to `::handle` (Phase 6), they are identical on every chunk
of the same shell, and stamping them per read is how a caller ends up believing a stale copy.
`::log-path` is per SHELL for the same reason, so it too lives on the handle. What is left is
the `cat` contract for files, key for key: give an offset, get the bytes and the next offset.

There is no `dropped_lines` key, and its absence is the contract: the FILE is the storage and
the ring is only a view. `is-eof` therefore means "you have read everything written so far",
never "the command finished" — the handle answers that.

The log is PERSISTENT and it belongs to the SESSION, not to the process or the turn: it
outlives the command's exit, a restart, and the turn that started it.

**Bytes on disk, index in the DB.** The log does not go into the session database as content.
It is an append-only stream read by byte offset; sqlite would turn every pump flush into a
blob rewrite and every cursor read into a substring over that blob, which is the wrong shape
for the one access pattern there is. What DOES go into the DB (`internal/persistance.clj`,
the same store that owns `session_iteration_attachment`) is the ROW that makes a log
findable: session, shell `::id`, the command, start/end, exit, and `::log-path`. So the model
can ask "what did that build print" in a later turn without holding a handle, and the bytes
stay a normal file that `cat` and `grep` read like any other.

**Acceptance criteria.**

- `src/com/blockether/vis/internal/foundation/shell.clj` — the pty pump writes to a
  session-owned log file under that session's directory in `~/.vis/`; the 2000-line ring
  becomes a convenience VIEW over it, never the storage; the middle-drop clip (`:659`)
  survives only as a display cap on one read.
- The log read takes `offset` and returns `::log-chunk`.
- `internal/persistance.clj` — one indexed row per shell carries `::log-path` and the command,
  so a log is reachable by session without the handle. No log BYTES in the DB.
- Persistence is proven, not incidental: a log written before a restart is still readable
  after it, at the same offsets.
- Test: a command emitting 10k lines is read back WHOLE by feeding `next-offset` in a loop,
  with no overlap and no gap; a second test greps the log file after the shell exits; a third
  reopens it from the DB row after the owning shell is gone.

**Unknowns.** What is the retention rule — deleted with the session record, or capped by
age/size under `~/.vis`? A build log is the kind of file that is large and boring the day
after.

## Phase 5 — Every run IS a handle; a timeout is just a short wait

**Rationale.** Without it Phase 4 fixes reading and the reported spin still happens: at the
120 s deadline `shell_run` returns partial streams and no way to attach, so the only recovery
is re-running a ten-minute build.

The deeper point is that "sync run" and "background shell" were never two things. Both spawn a
process and pump it into a log; they differ only in HOW LONG THE CALLER WAITS. Once every run
returns an `::id` — not only a timed-out one — the timeout stops being a failure mode and
becomes the ordinary case of a wait that expired, and Phase 6 can delete `shell_background`
instead of teaching it a second lifecycle.

**Data.**

```clojure
(s/def ::cwd          (s/and string? seq))
(s/def ::command      (s/and string? seq))
(s/def ::stdout       string?)
(s/def ::stderr       string?)
(s/def ::exit         (s/nilable int?))              ; nil only while still running
(s/def ::duration-ms  nat-int?)
(s/def ::is-timed-out boolean?)                      ; the WAIT expired, not the process
(s/def ::command-result
  (s/keys :req-un [::command ::stdout ::stderr ::exit ::duration-ms ::is-timed-out]
          :opt-un [::args ::note]))
(s/def ::commands (s/coll-of ::command-result :min-count 1))
;; ::id is ALWAYS here — that is the whole phase. A finished run is a handle whose log
;; happens to be complete; a timed-out run is the same map with ::exit nil.
(s/def ::run-result
  (s/keys :req-un [::id ::cwd ::commands ::exit ::duration-ms ::is-timed-out]
          :opt-un [::program]))
```

Making `::id` unconditional is what removes the old conditional predicate ("timed out implies
an id"): a rule the caller has to remember becomes a key that is simply always there.

**Acceptance criteria.**

- `foundation/shell.clj` — every run registers a shell before it waits, so the result carries
  `::id` whether it finished or not; on deadline the partial result is returned and the
  process is NOT killed.
- The prompt line for `shell_run` says the result is always a handle and a timeout just means
  the wait expired, so the model resumes instead of re-running.
- Test: a command that outstays a 1 s wait returns `:is-timed-out true` with an `:id`, and
  reading that id from offset 0 yields the output the timed-out call never saw; a command that
  finishes inside the wait carries an `:id` too, and reading it returns the same bytes.

**Unknowns.** Does a run promoted past its wait inherit the caller's stdin, and is a shell
still running at the end of a turn reaped, reported, or left alone?

## Phase 6 — ONE shell tool: `wait` replaces `shell_background`, a handle replaces the rest

**Rationale.** Without it the family still costs six schemas for operations on one object,
`subprocess` and `shell_run` remain two paths that merely agree, and `git` keeps a schema for
argv normalization that belongs in the shared gate.

**Do we still need `shell_background`?** No — Phase 5 already removed the reason for it. There
is one primitive: spawn, pump into the log, hand back a handle. `wait` is the only knob:

```python
r  = shell_run(["npm test"])                      # wait for the default deadline
sh = shell_run(["npm run dev"], wait=0)           # do not wait — what "background" meant
sh = shell_run(["npm run build"], wait=5).handle  # waited 5 s, still going, keep reading
```

All three return the same `::run-result`, all three carry an `::id`, and the handle around
that id is the same object in every case. `wait=0` is not a special mode: it is the wait that
expires immediately, so it takes the ordinary timed-out path Phase 5 already built and tests.
The `id="dev"` naming that `shell_background` offered survives as an argument, because a
re-issued named shell must still return the LIVE one rather than a second copy.

**Data.**

```clojure
(s/def ::program      (s/and string? seq))            ; "git", "npm"
(s/def ::argv         (s/coll-of ::program :kind vector? :min-count 1))
(s/def ::lines        (s/coll-of ::command :min-count 1))
(s/def ::argvs        (s/coll-of ::argv :min-count 1))
(s/def ::wait-ms      (s/int-in 0 600001))            ; 0 = do not wait; the old background
;; `shell_run`, `subprocess` and `git(...)`/`wrap_with_shell` all build THIS.
(s/def ::request  (s/keys :req-un [(or ::lines ::argvs)]
                          :opt-un [::cwd ::wait-ms ::program ::id]))
(s/def ::pid      (s/nilable pos-int?))
(s/def ::status   #{:starting :running :exited :stopped})
(s/def ::handle   (s/keys :req-un [::id ::cwd ::commands ::status ::log-path]
                          :opt-un [::pid ::exit ::program ::note]))
```

Interactions the specs pin: the gate runs against `::request`, so the three spellings cannot
diverge; a `wrap_with_shell` product differs only by `::program` on the request and the
result, which is all the client needs to keep rendering a git batch as a git batch; and
`::status`/`::exit`/`::log-path` live HERE, on the handle, which is why Phase 4's chunk does
not repeat them.

**Acceptance criteria.**

- `shell_background`, `shell_logs`, `shell_type` and `shell_stop` schemas are deleted.
  `shell_run` gains `wait`, and its result exposes a sandbox HANDLE object — `sh.logs(offset=…)`
  returning a `::log-chunk`, `sh.type(text)`, `sh.stop()`, `sh.is_running()`,
  `sh.wait(seconds)`. `sh.wait` is the bounded poll loop written ONCE, in the engine.
- `resources/vis-shims/posix.py` — `subprocess`/`os.system`/`os.popen` call the same gated
  implementation directly instead of the `shell_run` tool binding (`posix.py:125`), so the
  feature toggle, cwd, wait policy, exit rows and log file are one code path. A command
  refused for `shell_run` is refused for `subprocess`, in the same words.
- `src/com/blockether/vis/internal/foundation/git_tool.clj` — deleted as a TOOL; argv
  normalization, refusal of a shell-quoted blob, the ordered serial batch, exit-as-data and
  verbose-add staged-path parsing (`:110`, `:154`) move into the shared implementation and
  are exposed as `wrap_with_shell(program, **defaults)`, with `git` prebound:
  `git(["status","--short"], ["log","-1","--format=%H"])`.
- Net native surface for the whole family: **`shell_run` only** — one process jail — plus the
  handle object and `wrap_with_shell` in the sandbox. Six schemas become one.
- Tests: one gate refuses identically through all three spellings; `wait=0` returns before the
  process exits and its handle then reads the whole log; re-issuing a named shell returns the
  live one; a git batch through the factory returns the rows the tool returned, `program`
  included; `sh.wait` returns on exit and on expiry without spinning.

**Unknowns.** Does any client rendering key off the `git` TOOL name rather than `::program`,
and does the companion transcript need a migration for that?

## State of the plan

**ACCEPTED.**

Done:

- Delete ACP — `8e6e9e413`.
- Collapse `vis_attach_bytes` into `vis_attach` — `1afa4df70`. Shipped, but NOT part of this
  plan's argument; the attachment surface is being reworked elsewhere.
- P0, the protected-paths predicate at the `confine!` seam — `b11aa1706`. Superseded by
  Phase 2, which keeps the seam and deletes the vocabulary; it stays on the record because it
  is what proved `confine!` is where a filesystem boundary belongs.
- Phase 1, the five filesystem verbs — `87c1562ea`.
- Phase 2, the `:fs/access` gate — `49d5a182e`. `protected_paths.clj`, `:ext/protected-paths`
  and the access vocabulary are gone; `confine!` and the native editors ask one gate op, the
  Bridge re-declares its path sandbox as its own hook, and a Python extension guards the
  filesystem with `vis.op_hook(["fs_access"], …)`.
- Phase 3, `ls` as a sandbox helper — `d3db4c514`. The tool schema, its renderers and its
  symbol are gone; `list-directories` is public, asks the `:fs/access` gate itself and keeps
  the fff-backed walk, and `resources/vis-shims/ls.py` binds it as `ls(paths, depth,
  is_hidden)`. The measurement decided it: a gitignore-aware walk in guest Python costs
  295 ms for this repo at `depth` 3 against 15.6 ms for the host walk, so the helper calls
  the host.
- Project rename (`struct_rename`) deleted — `71f00d8c9`. Requested during the plan and not
  part of its argument; it removed the one editing symbol whose targets were discovered rather
  than passed, so the surface the later phases reason about is smaller.
- Phase 4, the offset-cursor shell log — `dccadb987`. `internal/shell_log.clj` owns the file
  (`~/.vis/logs/shell/<session>/<id>.log`), the `::log-chunk` answer and the sidecar index row;
  the pump tees the PTY through it, `shell_logs` takes `offset`/`limit` and returns
  `text`/`offset`/`next_offset`/`is_eof`/`is_truncated`, and the line ring plus `dropped_lines`
  are gone. Retention was the phase's Unknown: the log dies with the session record, deleted
  beside `db-delete-session-tree!`, and the index row rides the existing `extension_aggregate`
  rail (`ON DELETE CASCADE`), so there is no migration.

TODO, in order:

1. Phase 5 — every run carries an `::id`, so a timeout is a wait that expired.
2. Phase 6 — `wait` replaces `shell_background`, the handle object replaces `shell_logs` /
   `shell_type` / `shell_stop`, one shell under `subprocess`/`shell_run`, `git` as
   `wrap_with_shell`.

Each step ships as one commit with its own tests, lint-clean and formatted, per `AGENTS.md`.
