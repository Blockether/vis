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
(cursor, timeout handle, one implementation under `shell_run`/`subprocess`).

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
`subprocess` and `shell_run` remain two paths that merely agree, and argv normalization keeps a
schema of its own instead of living in the shared gate.

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
(s/def ::program      (s/and string? seq))            ; "npm", "cargo"
(s/def ::argv         (s/coll-of ::program :kind vector? :min-count 1))
(s/def ::lines        (s/coll-of ::command :min-count 1))
(s/def ::argvs        (s/coll-of ::argv :min-count 1))
(s/def ::wait-ms      (s/int-in 0 600001))            ; 0 = do not wait; the old background
;; `shell_run`, `subprocess` and `wrap_with_shell` all build THIS.
(s/def ::request  (s/keys :req-un [(or ::lines ::argvs)]
                          :opt-un [::cwd ::wait-ms ::program ::id]))
(s/def ::pid      (s/nilable pos-int?))
(s/def ::status   #{:starting :running :exited :stopped})
(s/def ::handle   (s/keys :req-un [::id ::cwd ::commands ::status ::log-path]
                          :opt-un [::pid ::exit ::program ::note]))
```

Interactions the specs pin: the gate runs against `::request`, so the three spellings cannot
diverge; a `wrap_with_shell` product differs only by `::program` on the request and the
result, which is all the client needs to keep rendering a wrapped batch as that program's batch; and
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
- `src/com/blockether/vis/internal/foundation/git_tool.clj` — deleted outright. There is no
  `git` tool and no prebound `git` binding: a Git command is an ordinary `shell_run` argv, so
  argv normalization, refusal of a shell-quoted blob, exit-as-data and the process jail are the
  shell implementation's, once.
- Net native surface for the whole family: **`shell_run` only** — one process jail — plus the
  handle object and `wrap_with_shell` in the sandbox. Six schemas become one.
- Tests: one gate refuses identically through all three spellings; `wait=0` returns before the
  process exits and its handle then reads the whole log; re-issuing a named shell returns the
  live one; a wrapped batch through the factory returns the rows the runner returned, `program`
  included; `sh.wait` returns on exit and on expiry without spinning.

**Unknowns.** Does any client rendering key off a TOOL name rather than `::program`,
and does the companion transcript need a migration for that?

## Phase 8 — The result IS the handle

**Rationale.** Without it the request is one shape but its continuation is three: `shell` answers
with an `id`, and driving that process means re-typing the id into `shell_logs` / `shell_type` /
`shell_stop` — three more names in the sandbox surface, three more schemas to read, and a poll loop
every caller rewrites by hand (and gets wrong by sleeping blindly).

**Data.** None. Nothing crosses a boundary: the wire result keys are unchanged, and the handle is a
sandbox-side TYPE over the same dict.

**Acceptance criteria.**
- `resources/vis-python/async_runtime.py` — `__VisShell__(__VisResult__)`: a shell result is a dict
  WITH `logs()`, `wait()`, `type()`, `stop()`; `__vis_pyify__` types it by the engine-stamped `op`.
- `src/com/blockether/vis/internal/foundation/shell.clj` — the three verbs become PRIVATE transport
  (`_shell-logs` / `_shell-type` / `_shell-stop`, underscore = filtered out of `apropos`), and every
  note/description points at the handle.
- `resources/vis-shims/posix.py`, `internal/prompt.clj`, `internal/loop.clj` — followers.
- Proof: `shell-test` "answers with a HANDLE whose own methods drive the process" starts a shell,
  `sh.type("ready")`, `sh.wait(30)` and asserts the text, the `exited` status and that `shell_logs`
  is NOT a global.

**Unknowns.** None.

## Phase 9 — ONE result shape for the whole shell family

**Rationale.** Without it the tools are one call shape with five answer shapes: a `run` answered
`stdout`, a `logs` read answered `text`, `send`/`stop` carried their own stage-scoped subsets, and
an argv run built a map of its own with an extra `args` and no `status`/`note`/`*_omitted_chars`. A caller
therefore had to know WHICH stage produced the map before reading it, and reading the wrong name is a
`KeyError` — the exact failure the total-shape rule exists to prevent.

**Data.** None. The shell result never leaves the process as a persisted or mirrored contract: it is
the tool's own return value, rendered and handed to Python in the same run.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/shell.clj` — `result-core`, `stage-keys` and
  `command-result-base` collapse into one `shell-result-base`; `logs` puts its window under `stdout`;
  `send` keeps `keys`/`sent` and drops its `text` echo; an argv run echoes a QUOTED bash line.
- `src/com/blockether/vis/internal/foundation/git_tool.clj` — gone; nothing returns a
  git-specific result shape.
- `resources/vis-python/async_runtime.py`, `resources/vis-shims/posix.py` — `sh.wait()` and
  `Popen.communicate()` accumulate `stdout`.
- Test: `shell-one-shape-test` asserts run / wait-0 / logs / send / stop answer the SAME key set.

**Unknowns.** None.

## Phase 10 — A handle is OWNED, NAMED and honestly identified

**Rationale.** Without it a handle is a bare id and everything that drives one after the spawn
trusts it: two ids that sanitize alike (`a/b`, `a_b`) shared ONE log file, so the second `open!`
truncated the first shell's output and both handles reported the other's bytes; re-issuing a live id
with a different command or `cwd` answered SUCCESS for a process that never ran what was asked;
`logs`/`send`/`stop`/re-issue reached a process by id with no ownership check, so a jailed extension
could read, type at or kill a trusted shell in the same session; `wait: -5` clamped to a one-second
wait that called a real command timed out and `wait: 0.4` rounded into the background start; and the
handle OBJECT existed only in the model's sandbox, leaving extensions to hand-author the `op`
grammar Phase 8 deleted.

**Data.** None. Nothing crosses a boundary: the origin stamp lives in the in-process registry map and
the log file name is a local path.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/shell_log.clj` — `safe-name` is INJECTIVE; a segment that is not
  the id verbatim carries a digest of the raw id.
- `src/com/blockether/vis/internal/foundation/shell.clj` — `->pos-long` refuses negative, fractional
  and non-finite numbers; `env-origin`/`authorize-origin!` stamp every entry and gate
  `logs`/`send`/`stop`/re-issue; `reissue-live-entry` makes identity (id, command, cwd) and sets
  `already_running`.
- `resources/vis-python/extension_bootstrap.py` — `vis.Shell` gives an extension the SAME
  `.logs()/.type()/.stop()/.wait()` handle the sandbox gets.
- Test: `shell-handle-integrity-test` covers the four host defects; `python-extensions-test` drives
  an extension handle through all four verbs.

**Unknowns.** None.

## Phase 11 — Every run is a BACKGROUND run: delete `wait` from the request

**Rationale.** Without it a number on the request still selects a MODE. `wait: 0` meant
"background", `wait: 0.4` rounded into it and `wait: -5` clamped into a fake timeout (Phase 10 had
to refuse both at the boundary — a refusal that only exists because the knob does). Phase 8 already
killed the argument for it: `(await shell({"command": …})).wait(300)` is the SAME round trip inside
one `python_execution` block, so `wait` buys nothing the handle does not give and costs a second
meaning for one call. The log a run leaves behind is the FEATURE, not the reason to keep two modes:
"what did that build print" stays answerable by id for as long as the session lives.

**Data.** None. The request loses a property and the result keeps `shell-result-base` verbatim;
nothing crosses a persisted or wire boundary that did not already.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/shell.clj` — the `run` op always spawns under a PTY
  and answers immediately (`timed_out` false, `exit` nil); the schema is `{command, id?, cwd?}`;
  `shell-run-call` becomes the INTERNAL `run-blocking`; `logs` normalizes the PTY's CRLF so text has
  one spelling of a newline.
- `src/com/blockether/vis/internal/loop.clj` — the `!cmd` bang path calls `run-blocking` directly
  instead of passing a request flag.
- `resources/vis-shims/posix.py` — `subprocess.run` is a spawn plus `Popen.communicate`, so the shim
  has ONE waiting idiom.
- `resources/vis-docs/context-and-prompts.md` — the bang section documents the handle, not the
  removed `op: "wait"`/`until` grammar.
- Test: `no-wait-knob-test` proves a run returns at once with no exit, that `wait` is not in the
  schema, and that `sh.wait` is what fills `exit`/`stdout`; `run-is-a-handle-test` proves the log
  outlives the process.

**Unknowns.** None.

## Phase 12 — A failing shell must be READABLE: status, pid and a bounded wait

**Rationale.** Without it the evidence about a failure is missing exactly where a failure is
normal. A spawn answered `status` nil, `pid` nil and `is_eof` true, so the one stage that knows a
child was just started could not say it was running, could not name the process, and claimed its log
was already complete; a re-attach still reported `timed_out` true, a leftover of the deleted `wait`
knob. Worse, `sh.wait(secs)` checked its deadline ONLY at EOF, so a command that never stops
printing (`yes`, a chatty build) always had more bytes available and the "bounded" poll loop ran
until the sandbox watchdog killed the block.

**Data.** None. No key is added or removed from `shell-result-base`; three producers stop dropping
keys the shape already declares.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/shell.clj` — `run-of-background` keeps the background
  start's own map (`pid`, `status`, `uptime_ms`) instead of rebuilding it; `live-run-result` answers
  from `bg-core` with `timed_out` false; a synchronous run carries `pid` and `status`
  (`exited` / `running`); `bg-core` reports `is_eof` false while the child lives.
- `resources/vis-python/async_runtime.py`, `resources/vis-python/extension_bootstrap.py` — the poll
  loop checks its deadline on EVERY iteration, so `sh.wait(1)` is bounded whatever the child prints.
- Test: `shell-failure-visibility-test` proves exit 127 with "command not found" in the log, a
  non-zero exit with its stderr, a spawn that says `running` with a pid, `stop` killing the
  grandchild and keeping the log, and no `vis-shell-*` / `vis-pty-*` thread outliving the command;
  a Python case pins that `sh.wait(1)` returns from an endless printer.

**Unknowns.** None.

## Phase 13 — ONE wait: the poll loop moves into the host

**Rationale.** Without it the bounded wait is written three times — `__VisShell__.wait` in
`resources/vis-python/async_runtime.py`, `vis.Shell.wait` in
`resources/vis-python/extension_bootstrap.py` and the test's own `wait*` — free to disagree about
the deadline, the cursor and what "done" means, and `resources/vis-shims/posix.py` kept a fourth
loop inside `Popen.wait`/`communicate`. They already disagreed once: Phase 12 had to fix the same
unbounded-deadline defect in two files. A Python-side accumulator was also unbounded — a runaway
printer measured ~1 MB/s, so a 600 s wait was a heap problem rather than a slow answer.

**Data.** None. `shell-result-base` is unchanged; `wait` is a `stage` value the shape already
admits, exactly as `logs` / `send` / `stop` are.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/shell.clj` — `shell-wait-impl` is the ONE bounded
  loop (deadline on every iteration, drain-to-quiet after the child exits, `capped-capture`
  head+tail accumulation with `stdout_omitted_chars`), dispatch op `wait`, private transport
  symbol `_shell-wait`; `capped-capture` grows an `:append!` so a pumped Reader and a wait share
  one bounded buffer; `clamp-timeout-secs` names the option the caller spelled.
- `resources/vis-python/async_runtime.py`, `resources/vis-python/extension_bootstrap.py`,
  `resources/vis-shims/posix.py` — every Python wait is one call to the host op; no `time.sleep`
  poll loop survives in any of them.
- Test: `shell-one-wait-test` proves the loop is single (no Python file keeps one), that memory is
  bounded on an endless printer, that a retired shell still answers from its log and a cursor is
  never replayed, that an unknown id / another origin / a negative or fractional duration are
  refused by name, that a stop ends the wait with its bytes intact, that a burst leaves no
  `vis-shell-*` / `vis-pty-*` thread, and that a cancelled turn unwinds the wait at once with the
  child still running.

**Unknowns.** None.

## Phase 14 — The handle REPORTS: status, clock, log path and live cost

**Rationale.** Without it the handle can read a process's bytes and kill it but cannot SAY what it
is doing. "Has it finished?" costs a `sh.logs()` call — a log read that moves a cursor to answer a
question about the process, not about the bytes — and "where are the bytes on this machine",
"when did it start", "how long has it been running" and "what is it costing in CPU and RAM" have
no answer at all: `shell-log/log-file` is deterministic but never surfaced, `:started-at` and
`:exited-at` live only inside `bg-procs`, and nothing samples the process tree.

**Data.** Yes — `shell-result-base`
(`src/com/blockether/vis/internal/foundation/shell.clj`) is the result map that crosses to the
Python sandbox, the extension boundary and the card renderers, and it gains six keys:

```clojure
(s/def :shell/started_at (s/nilable nat-int?))    ;; epoch ms, stamped at spawn
(s/def :shell/finished_at (s/nilable nat-int?))   ;; epoch ms; nil IS "still running"
(s/def :shell/log_path (s/nilable string?))       ;; absolute path of the log file
(s/def :shell/cpu_ms (s/nilable nat-int?))        ;; CPU consumed by the whole tree
(s/def :shell/cpu_percent (s/nilable (s/and number? #(<= 0 %))))
(s/def :shell/rss_bytes (s/nilable nat-int?))     ;; resident memory of the whole tree
```

Every key is nilable and present on EVERY stage, so the one-shape rule holds: the three cost keys
are nil exactly when there is no live process to sample, which is a measurement's honest answer
and not a missing key.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/shell.clj` — the six keys on `shell-result-base`;
  `tree-handles` / `cpu-time->ms` / `tree-ps-usage` / `process-usage` sample the process TREE;
  `bg-core`, `retired-log-core` and the sync run fill the clock, the log path and the cost;
  `shell-status-impl` + dispatch op `status` + private transport symbol `_shell-status`; the
  registry entry carries `:log-path`.
- `resources/vis-python/async_runtime.py`, `resources/vis-python/extension_bootstrap.py` —
  `sh.status()` on both handle classes, one call to the host op.
- `resources/vis-docs/context-and-prompts.md`, `src/com/blockether/vis/internal/prompt.clj` — the
  handle's verbs name `sh.status()`.
- Test: `shell-status-test` proves a running shell reports `running` / no `exit` / no
  `finished_at` / its real `log_path` on disk / a positive `rss_bytes`, that a finished one
  reports `exit` and a stopped clock with the cost keys nil, that a retired entry still answers,
  and that an unknown id and a foreign trust origin are refused by name; `shell-one-shape-test`
  pins `status` into the identical key set and stage sequence; the sandbox and extension handles
  are each driven through `sh.status()` end to end.

**Unknowns.** None.

## Phase 15 — ONE door to a process: `subprocess` never spawns

**Rationale.** Without it there are two spawn doors and the model picks whichever it remembers.
`resources/vis-shims/posix.py` was a 313-line bridge that re-implemented `run`, `Popen`,
`communicate`, `check_output`, `os.system` and `os.popen` on top of the shell transports — a second
copy of the id, cursor, timeout and stop contracts that Phases 9-14 spent five commits making
single. It also lied by omission with the toggle off: the message said only that `subprocess` could
not run, which reads as "the other door might work". The prompt block said the same thing —
`subprocess` "routes through the active `shell` tool" — teaching the shape we no longer want.

**Data.** None. No key on any boundary changes; a Python module the sandbox synthesises is not a
wire contract.

**Acceptance criteria.**
- `resources/vis-shims/posix.py` — a refusal, not a bridge: every spawning entry point of
  `subprocess`, plus `os.system` / `os.popen`, raises ONE message resolved at CALL time. Shell on:
  name the `shell` tool and its invocation. Shell off: say the toggle disabled the tool AND
  `subprocess`. The exception types stay real classes so an `except` line cannot mask the refusal.
- `src/com/blockether/vis/internal/env_python.clj` — `install-posix-refusal-shim!` eval's it
  eagerly; `resources/vis-python/posix_lazy_init.py` and the `__vis_load_posix__` callback are
  deleted, because the lazy `meta_path` finder existed only to defer a ~95ms bridge that is gone.
- `src/com/blockether/vis/internal/prompt.clj` — both branches of the sandbox-shims block state the
  ban; `src/com/blockether/vis/internal/runtime_settings.clj` drops `subprocess` from the
  shell-timeout scan, since a call that raises at once buys no budget.
- Test: `posix-refusal-shim-test` proves the refusal with `shell` BOUND (naming the tool) and with
  it absent (naming both), across `run`/`call`/`check_call`/`check_output`/`getoutput`/
  `getstatusoutput`/`Popen`/`os.system`/`os.popen`, with the handler line still importable and no
  shim internals in the live-vars baseline; `shell-one-wait-test` pins that the shim keeps no wait.

**Unknowns.** None.

## Phase 16 — The process surface is said ONCE

**Rationale.** Without it the same fact is worded four times and the copies drift. The ban lived in
`prompt.clj`'s sandbox-shims block, again in `resources/vis-shims/posix.py` (`_USE_SHELL` /
`_NO_SHELL`) and a third time in `resources/vis-python/async_runtime.py:606` ("The shell tools are
not enabled…"), each with its own sentence — so the rule the model READS in the prompt and the rule
it HITS at the call site were free to disagree, and Phase 15 had to edit three files to change one
fact.

**Data.** None. Nothing crosses a boundary: the sentences are engine prose seeded into one sandbox
global, never persisted and never sent over the wire.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/env_python.clj` — `PROCESS_SURFACE` is the ONE wording, composed
  rather than concatenated ad hoc: `ban` (the rule), `use` (the invocation, call sites only), `off`
  (the toggle state, naming BOTH doors). `install-process-surface!` seeds it as the
  `__vis_process_surface__` sandbox global and then installs the refusal shim.
- `resources/vis-shims/posix.py` and `resources/vis-python/async_runtime.py` — carry no wording of
  their own; both raise out of `__vis_process_surface__`.
- `src/com/blockether/vis/internal/prompt.clj` — the block emits `ban` or `off` verbatim; it never
  emits `use`, because invocation grammar belongs to the `shell` symbol's own docs.
- Test: `posix-refusal-shim-test/process-surface-is-said-once-test` proves the prompt block, the
  `subprocess` refusal (both toggle states) and an undriveable handle return the SAME host strings,
  and that neither `.py` file contains a literal copy.

**Unknowns.** None.

## Phase 17 — A wait ANSWERS: no idle tail, and the ticker says what it waits for

**Rationale.** Without it a wait looks stuck twice over. `shell-wait-impl` polled on a flat 50 ms
sleep, so a command that had ALREADY exited still cost its caller ~120-154 ms of measured wait
(the sleep, paid once before the exit was seen and once more to confirm the log had drained) —
a 200 ms `echo` answered in ~340 ms. And while any wait ran, the TUI ticker read
`Vis is running: _shell-wait tt`: a private transport name the caller never typed and an opaque
id, naming neither the command nor the budget. Together they made an HONEST wait — session
`2cd3c95b`, a genuinely 50.7 s `grep -rln`, reproduced at 50.7 s — read as a wait hung on nothing.

**Data.** None. `phrase` rides the existing in-process activity map and the gateway's live-activity
payload beside `op`/`label`; it is UI prose, never persisted and never replayed as a contract.

**Acceptance criteria.**
- `src/com/blockether/vis/internal/foundation/shell.clj` — `wait-idle-poll-ms` is a ladder
  (2 ms while a finish is plausible, 10 ms, then 50 ms once it clearly is not) driven by an idle
  counter in the wait loop, and `wait-drain-poll-ms` (5 ms) charges the shortest sleep to a
  command that is already done. `shell-ticker` builds one phrase from the id, the budget and the
  live script; `:ext.symbol/ticker-fn` is declared on `shell-{wait,status,logs,type,stop}-symbol`.
- `src/com/blockether/vis/internal/extension.clj` — `:ext.symbol/ticker-fn` is a symbol option with
  its spec and its doc line; `tool-start-phrase` calls it and `tool-start-event` carries the result.
- `src/com/blockether/vis/internal/progress.clj` and
  `src/com/blockether/vis/internal/gateway/state.clj` — `:tool/phrase` on the iteration, `phrase` on
  the live-activity payload, so both channels read one sentence instead of assembling their own.
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj` and
  `apps/vis-companion/src/screens/SessionScreen.tsx` — print it verbatim after `Vis is `; a tool
  with no ticker keeps the `op` + `label` default.
- Test: `shell-test/shell-wait-answers-test` pins the poll ladder, that a finished command's wait
  returns within 100 ms of its own `finished_at`, and the phrase for every shell transport;
  `render-test` pins that `:tool/phrase` replaces `_shell-wait tt` in the bubble.

**Unknowns.** None.

## State of the plan

**DONE.**

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
- Phase 4, the offset-cursor shell log — `e98cc607e`. `internal/shell_log.clj` owns the file
  (`~/.vis/logs/shell/<session>/<id>.log`), the `::log-chunk` answer and the sidecar index row;
  the pump tees the PTY through it, `shell_logs` takes `offset`/`limit` and returns
  `text`/`offset`/`next_offset`/`is_eof`/`is_truncated`, and the line ring plus `dropped_lines`
  are gone. Retention was the phase's Unknown: the log dies with the session record, deleted
  beside `db-delete-session-tree!`, and the index row rides the existing `extension_aggregate`
  rail (`ON DELETE CASCADE`), so there is no migration.

- Phase 5, every run IS a handle — `451a644a2`. A run claims its id and its log file BEFORE it
  waits, so the result carries `id` whether it finished or not; a wait that expires no longer
  kills the child but ADOPTS it as an ordinary background handle, and nothing after it in the
  ordered batch is started. `capped-capture` replaced `read-capped` so a stream can be
  snapshotted while it is still filling, and `shell_logs` answers for a finished run from the
  file plus its sidecar row once the registry entry is gone. The phase's Unknowns are answered:
  an adopted run has NO writable stdin (it was spawned on pipes, not a PTY, so `send` refuses it
  by name), and it is LEFT ALONE at the end of a turn — listed as a session resource, running
  until it exits or `resource_stop`.

- Phase 6, `wait` is the only knob — `634c0476c`, renamed in `25089f16f`. `shell_background` is deleted: `shell` takes
  `wait` (0 does not wait at all, and takes the adopted-run path Phase 5 built), so "run" and
  "background" are one request with one result shape and one id. `shell` is now the ONLY
  native shell tool; `shell_logs` / `shell_type` / `shell_stop` stay as sandbox Python symbols on
  the handle's id, so the model's schema surface is one tool instead of five. `subprocess` in
  `resources/vis-shims/posix.py` spawns through `shell` with `wait` 0 and reads the log
  cursor to EOF. With one verb left, `shell_run` is NAMED `shell`: the `_run` suffix only ever
  distinguished it from `shell_background`, so it now names a distinction that does not exist.

- Phase 7, ONE call is ONE command — `32d1b9ccf`. `commands` is deleted from the tool: `shell`
  takes `command` (one `bash -lc` line; `&&` chains, independent work is separate calls) and
  takes one argv or one line and answers with a FLAT result — `r["stdout"]`, `r["exit"]`,
  never `r["commands"][i]` — which is the same shape a `wait` 0 start already returned, so a
  call has ONE result shape whatever it waits for. `foundation/serial_batch.clj` and its test are
  deleted with the batch they existed for: with one command there is no budget to divide, no
  `started: false` entry to explain, and the shared-deadline defect fixed in `cdcfe21e8` cannot
  recur. `vis.shell` / `vis.jailed_shell` / `vis.jailed_shell_session`, `posix.py`'s
  `subprocess`, the `!` bang path and the synthesized Python call all move to the same key.

- Phase 8, the result IS the handle — `c57822453`. `shell` answers with `__VisShell__`, a dict
  that carries `sh.logs(offset=…)`, `sh.wait(secs)`, `sh.type(text)` and `sh.stop()`, so a process is
  driven on the object the call already returned. `shell_logs` / `shell_type` / `shell_stop` are gone
  from the model's surface: they survive only as the private `_shell_*` transport the handle calls,
  underscore-prefixed so `apropos` never lists them. `sh.wait(secs)` is the bounded poll loop written
  ONCE in the engine — read, keep reading while bytes remain, sleep only at EOF while the process
  lives, stop at the deadline — which is what makes "never sleep blindly" a rule with a tool behind it.

- Phase 9, ONE result shape for the whole shell family — `0b698b3be`. `shell-result-base` is the
  single key set every stage answers with; `stage` names the producer and is the only thing
  that varies. Output has one name (`stdout`) whether the call waited for it or came back for it
  later, an argv run no longer carries an `args` field beside `command`, and a key a stage has nothing to
  say about is nil / false / 0 rather than absent.
- Phase 10, a handle is OWNED, NAMED and honestly identified — `bcf56c59f`. Distinct ids never share a log file, a re-issue that names different work is refused instead
  of silently succeeding, a handle cannot be driven across a trust origin, a nonsense `wait` is
  refused rather than rounded, and `vis.Shell` gives extensions the sandbox's handle object.

- Phase 11, every run is a background run — `f2b7ce9e8`. `wait` is gone from the request: `shell`
  takes `{command, id?, cwd?}`, always spawns under a PTY and returns the handle NOW, and
  `sh.wait(secs)` is the only wait there is. A run's log is kept by id for the session — retention
  is the product, not a leak — and `logs` normalizes the PTY's CRLF so a line reads the same
  whether the caller waited for it or came back for it.

- Phase 12, a failing shell is readable — `f17636374`. A spawn now says `running` with its `pid` and
  an honest `is_eof`, a re-attach no longer claims a wait expired, a synchronous run carries
  `status`, and the handle's poll loop is bounded on every iteration rather than only at EOF. Proven
  by measurement, not by reading: exit 127 with the shell's own complaint in the log, `stop` killing
  a backgrounded grandchild, and the thread count returning to its baseline after every run.
- Phase 13, ONE wait — `0fcb2b524`. The bounded poll loop is host code (`shell-wait-impl`, transport
  `_shell_wait`); the sandbox handle, an extension's handle, `subprocess.Popen` and the tests all
  call it, and a wait now bounds MEMORY as well as time (a runaway printer measured ~1 MB/s).
- Phase 14, the handle REPORTS — `c6ecf7b18`. `sh.status()` (transport `_shell_status`) answers
  `status`/`exit`, `started_at`/`finished_at`/`uptime_ms`, `log_path` and the live
  `cpu_ms`/`cpu_percent`/`rss_bytes` of the process TREE, without reading a byte or moving a
  cursor.
- Phase 15, ONE door to a process — `fa6251ee9`. `subprocess`, `os.system` and `os.popen` never spawn:
  they raise, naming the `shell` tool when it is on and naming BOTH as disabled when the toggle is
  off. The 313-line delegation bridge and its lazy loader are deleted.
- Phase 16, the process surface is said ONCE — `cf7b47c58`. `env-python/PROCESS_SURFACE` (`ban` /
  `use` / `off`) is the only wording; the prompt block, the `subprocess` refusal and an undriveable
  handle all read it, the second through the `__vis_process_surface__` sandbox global.
- Phase 17, a wait ANSWERS — `bd3774d8b`. The idle poll is a 2/10/50 ms ladder with a 5 ms drain, so a
  finished command's wait returns 9-17 ms after its exit instead of 120-154 ms; a tool declares its
  own ticker sentence (`:ext.symbol/ticker-fn`), so the bubble reads
  `Vis is waiting for tt (up to 60s): npm test` instead of `Vis is running: _shell-wait tt`.

TODO, in order: nothing. The plan is DONE.

Each step ships as one commit with its own tests, lint-clean and formatted, per `AGENTS.md`.
