# CHANGES — un-unify the mega-tools (`shell`, `fs`)

Status: **decided, not yet implemented**. This file is the working agreement; it is the
commit plan. Decisions from review are folded in — the split is FULL, both sides.

## 1. Why we are reverting the unification

The unification of `shell_run` / `shell_background` / `shell_logs` / `shell_wait` /
`shell_send` / `shell_stop` into ONE `shell(opts)` — and of `copy` / `move` / `delete` /
`create_dirs` / `file_exists` into ONE `fs(opts)` — was sold as a token saving. Measured
against how the model actually behaves it is a loss:

- **The schema stopped being a contract and became prose.** `shell`'s JSON Schema cannot
  say "`until` is required" — it is required only for `wait`. It cannot say "`commands` is
  required" — only for `run`/`background`. So every precondition moved into the description
  and into runtime `ex-info` throws (`shell-dispatch`, `shell.clj:1715-1856`, ~140 lines of
  `reject-commands` / `need-id` / `need-text` policing). The model discovers the contract by
  failing a call.
- **One description carries six jobs.** `shell.clj:2032-2051` is a ~1.5 KB doc covering six
  unrelated verbs; every one of them is paid for on every request whether or not a background
  process exists in the session. Focused tools cost *more* schema bytes at rest but far fewer
  wasted turns, and the tokens saved were never the expensive tokens.
- **Implicit mode is a trap.** `op` defaults to `run`, or to `background` when `id` is
  present (`shell.clj:1749`). A `logs` call that forgets `op` starts a process.
- **The result shape is a union.** One `stage`-discriminated map where "absent fields mean
  not applicable" (`shell.clj:530-585`). The model indexes `stdout` on a `wait` result and
  gets nil.
- **`fs` is the same disease with a needless twist:** `is_overwrite` is *required* on
  `delete`, `create_dirs` and `exists`, where it is documented as ignored
  (`editing/core.clj:8232`), and `copy`/`move` take their two paths as an *ordered array*.

Verdict: unify *implementations*, never *contracts*. The runtime already proves this is
cheap — see §4.

## 2. Principles for the new surface

1. **One tool = one verb = one satisfiable schema.** Every property is either required for
   that tool or genuinely optional. No `op` discriminator on a model-facing tool.
2. **`required` does the work `ex-info` does today.** If a check can be expressed in JSON
   Schema, it must be, and the corresponding runtime throw becomes unreachable defence, not
   the primary UX.
3. **One result shape per tool.** No `stage` union. A tool's `:result` line describes exactly
   the keys that tool returns.
4. **The implementation does NOT fork.** `shell-run-impl`, `shell-bg-impl`, `shell-logs-impl`
   (`:1300`), `shell-wait-impl` (`:1388`), `shell-send-impl` (`:1602`), `shell-stop-impl`
   (`:1672`) already exist as separate functions. Splitting the surface is a registration
   change, not a rewrite. Same for `fs`: `copy-symbol`, `move-symbol`, `delete-symbol`,
   `create-dirs-symbol`, `file-exists-symbol` already exist.
5. **No compatibility layer.** Per repo policy: `shell` and `fs` are *removed*, not deprecated.
   No alias, no shim, no `op` passthrough.
6. **Split by verb, not by parameter.** Nothing merges. Ten verbs, ten tools.
7. **No tool ever blocks on the model's behalf.** A tool call is one bounded action that
   returns now. Waiting, polling, retrying and "until it says ready" are CONTROL FLOW, and
   control flow belongs in `python_execution`, where the model can see the loop it wrote,
   decide when to give up, and act on what it read. A `until`/`timeout_secs` argument buries
   that loop inside a native tool where nothing can be inspected or adjusted.

## 3. Target tool surface

### 3.1 Shell — five tools, no waiting, no timeouts

| tool | verb | required | optional |
|---|---|---|---|
| `shell_run` | bounded foreground batch, returns output | `commands` | `cwd` |
| `shell_background` | spawn a PTY under an id, returns immediately | `commands` | `id`, `cwd` |
| `shell_logs` | snapshot a background's retained output, returns immediately | `id` | `n` |
| `shell_type` | type into a background's stdin (keystrokes, answers, `y`) | `id`, `text` | `is_enter` |
| `shell_stop` | kill the tree, drop the retained resource | `id` | — |

**There is no `shell_wait`, and `shell_logs` never blocks.** `until` is deleted, not moved:
`shell-wait-impl` (`:1388`) exists only to poll `shell-logs-impl`'s ring buffer against a regex
on a clock, and that is a `while` loop the model should write itself:

```python
await shell_background(["npm run dev"], id="dev")
for _ in range(60):
    r = await shell_logs("dev")
    if any("Local:" in l for l in r["lines"]):
        break
    await sleep(1)
```

That loop is better than `until=` in every way that matters: the model chooses the predicate
(a regex, a JSON parse, two conditions, a health request), sees every line it rejected, can
give up early on an error line instead of burning the whole budget, and can decide what to do
next from what it read. `until=` could do none of that — it returned "no match" and the model
re-ran it.

**There is no `timeout_secs` on any tool.** It was three different concepts wearing one name:
a kill deadline on `shell_run`, an observational give-up on `wait`, and dead weight elsewhere.
The give-up is now the loop's own `range`. The kill deadline stays as an INTERNAL default
bound on `shell_run` (the existing 120s) — a safety net so a foreground call cannot hang the
turn, not a knob the model tunes. A command that legitimately runs longer is not a bigger
number; it is `shell_background` plus the loop above. If that internal bound fires, the result
says so (`timed_out`, `note`) and names `shell_background` as the answer.

`shell_send` is renamed **`shell_type`**: what it does is type at an interactive program, and
"send" reads like sending a signal or a request.

Notes that survive the split:

- `shell_background`'s `id` stays optional — it is derived from the program name
  (`bg-derived-id`, `shell.clj:909-918`) and re-issuing the same script returns the live
  shell's own id. That idempotency is a real feature and does not change.
- Live ids stay in `session["resources"]`, so no tool is needed to list them.
- `shell_logs` keeps `dropped_lines`: a polling loop MUST be able to tell that the ring buffer
  overran between two reads.

**Attach: there is no attach op today, and we are not inventing one here.** Nothing in
`shell.clj` adopts a process this session did not spawn. If we want `shell_attach` (adopt an
already-running PID or a `tmux`/`screen` target so `shell_logs` and `shell_type` work against
it) that is a *new capability* with its own reader thread and lifecycle, and it ships in its
own commit after this split lands — not smuggled in as a rename. See Q1.

### 3.2 Filesystem — five tools, bare names

| tool | required | optional |
|---|---|---|
| `copy` | `src`, `dest` | `is_overwrite` |
| `move` | `src`, `dest` | `is_overwrite` |
| `delete` | `paths` | — |
| `create_directory` | `paths` | — |
| `file_exists` | `paths` | — |

- Bare names, no `fs_` prefix: these are already the Python symbol names today
  (`editing/core.clj:7847+`), the model composes them in `python_execution`, and
  `await copy("a", "b")` is the call site we want.
- `create_directory` and `file_exists` (not `create_dirs`/`exists`): `exists` alone is far too
  generic a top-level name, and `file_exists` says what it answers about. Both still take a
  LIST — one call checks or creates many — so the plural of the argument carries the plurality
  and the tool name stays readable.
- `src`/`dest` become **named scalars**. The `[source, destination]` positional-array
  convention (`editing/core.clj:8229`) is exactly the ordering the model gets wrong, and the
  schema cannot express "exactly two, in this order".
- `is_overwrite` is required nowhere and offered only where it means something.
- `delete` keeps destructive-intent gating via `:before-fn`; the confirmation requirement is
  unchanged.

## 4. What actually changes in the code

### 4.1 `src/com/blockether/vis/internal/foundation/shell.clj`

- **Delete** the model-facing `shell` var (`:2030-2054`) and `shell-symbol` (`:2690`).
- **Add** five thin `defn`s, each taking one options map and calling the existing `*-impl`
  with `env` — no behavioural code moves. `shell-logs` forwards straight to
  `shell-logs-impl`; nothing forks.
- **Replace** `shell-symbol` with five `vis/symbol` registrations; `shell-symbols` (`:2741`)
  becomes that vector. Each keeps `:native-tool? true`, `:inject-env? true`, `:tag :mutation`,
  and its own `(shell-on-error :shell_run)` etc., so the failure envelope and the cancellation
  split are unchanged.
- **Delete** `shell-wait-impl` (`:1388`) and its `wait` branch of `shell-dispatch`: with no
  model-facing `until` and no extension caller for it, a blocking poller in host code has no
  remaining consumer. Verify with an occurrence pass before removing.
- **Renderers already split:** `render-shell-run-result` (`:2291`) and
  `render-shell-bg-result` (`:2345`) are separate functions today; each symbol takes its own
  `:render-finish-call-fn` instead of the stage-dispatching wrapper.
- **`shell-dispatch` (`:1715-1856`) survives as an internal, unexported helper** for
  `trusted-extension-shell` (`:1873`), `jailed-shell` (`:1972`) and `session-jailed-shell`
  (`:1979`). Those take a user-authored options map from a Python extension, so they
  genuinely need a discriminator, and none of §1 applies to a non-model-facing contract.
  The ~140 lines of `need-id`/`reject-commands` policing stay exactly there, and only there.
- The `shell` **toggle** (`:2802-2807`) keeps its id `shell` and now gates all five symbols at
  once. `vis.yml` `toggles: {shell: false}` behaves exactly as before.

### 4.2 `src/com/blockether/vis/internal/foundation/editing/core.clj`

Much cheaper than shell — the split already exists and is switched off:

- `create-dirs-symbol` (`:7847`), `copy-symbol` (`:7865`), `move-symbol`, `delete-symbol`,
  `file-exists-symbol` are all registered with `:native-tool? false`. They are Python-callable
  today and simply invisible to the model.
- **The change is flipping `:native-tool? true` on those five, renaming two
  (`create_dirs` → `create_directory`, `exists` → `file_exists`), re-shaping `copy`/`move` to
  named `src`/`dest`, giving each a `:result` line, and deleting `fs-tool` / `fs-symbol`
  (`:8099-8236`) plus `render-fs-result` and the `fs-before-fn` union.**
- `available-editing-symbols` (`:8238`) drops `fs-symbol` and keeps the five.

### 4.3 Prose, prompts and tool descriptions — the full inventory

This is the half of the commit that actually decides whether the split works. The tools are
renamed in one file each; the *sentences* that teach the model how to use them are spread over
nine production files and eight test files. Every site below was grepped, not guessed.

**First correction to this document:** the earlier draft claimed `AGENTS.md` and
`resources/vis-docs/**` mention `shell` op names in four places. They do **not** — grepping
`shell`/`fs(` across `AGENTS.md`, `README.md` and `resources/vis-docs/**` returns three hits and
all three are unrelated (`Shell` the React component, "non-login shells", a tab-bar comment).
The repo's human-facing docs never named the ops. **No doc file changes in this commit.** The
agent-facing prose lives entirely in `prompt.clj`, `loop.clj` and the tool `:description`s —
which is exactly why the merge went unnoticed: nothing a human reads regressed.

#### 4.3.1 `internal/prompt.clj` — `CORE_SYSTEM_PROMPT`

Three lines, and only one of them is a rename.

- **`:224` (§2 Execution surfaces) — DELETE, do not rewrite.**
  Today: `"- For background shells use \`shell\` op \`wait\` with its REQUIRED \`until\` regex; \`logs\` snapshots."`
  There is no `wait` and no `until` after this commit, so the line has no successor spelling.
  It is replaced by a line that teaches the *shape* rather than the op, because the whole point
  of §3.1 is that waiting is the model's own control flow:
  `"- Long-running work: \`shell_background\`, then poll \`shell_logs\` inside \`python_execution\`; no tool waits for you."`
  It stays in §2 (Execution surfaces), not §3 (Inspect) — it is a routing rule about where a
  loop belongs, which is exactly §2's subject.
- **`:237` (§3 Inspect) — rename in place, and name ALL FIVE.**
  Today: `"  \`fs\` moves/copies/deletes; keep \`shell\` for running programs.\n"`
  Becomes: `"  \`copy\`/\`move\`/\`delete\`/\`create_directory\`/\`file_exists\` change and probe it; keep \`shell_run\` for running programs.\n"`
  **Correction to the previous draft of this document:** it proposed listing only the three
  destructive verbs and calling the other two "a catalogue the schemas already hold". That is
  wrong, and it is the same mistake that produced the mega-tools. A tool the routing line does not
  name is a tool the model does not know it may reach for, and the two unnamed ones are exactly the
  two with a shell reflex behind them: an unnamed `create_directory` becomes `mkdir -p` through
  `shell_run`, an unnamed `file_exists` becomes `test -f`. The rule is "do not shell out for
  filesystem work", so it has to enumerate the whole filesystem surface or it silently exempts
  whatever it omitted. Five names cost ~6 tokens; one `mkdir -p` in a shell costs a whole call
  plus its output.

  **Depth goes to the editing prompt, not to core.** Core states the routing rule and the five
  names; the *behaviour* of each verb — that `delete` is destructive and needs explicit intent,
  that a missing target is a no-op rather than an error, that `create_directory` makes parents,
  that `file_exists` never reads content, that `copy`/`move` take `src`/`dest` as NAMED scalars
  because the positional pair is what got swapped, that `is_overwrite` exists only on those two —
  lives in the editing-domain prose and in the five `:description`s (§4.3.3). That is the division
  the whole document argues for: core routes, the tool describes itself.
- **`:246` (§3, BATCH) — one word.** `"\`shell\`/\`git\` \`commands\`"` → `"\`shell_run\`/\`git\` \`commands\`"`.
  The batching advice is unaffected: `shell_run` keeps the plural `commands` array, so the
  "one call, never one per file" rule survives verbatim. This is the proof the split does not
  cost tokens where it mattered — the batching lever was never the `op` key.

Net effect on the system prompt: **±0 lines, ~4 tokens.** The claimed saving of the merge was
never in the prompt; it was in the schema block, and there it was negative (one `op` enum plus
six conditional-precondition sentences costs more than five small schemas).

#### 4.3.2 `internal/loop.clj` — the `python_execution` tool description

- **`:4682-4684` — the inversion.** The current text is a *prohibition*:
  `"Never \`time.sleep\`/poll for background shells — use \`shell\` op \`wait\` (a REQUIRED \`until\` regex ends it line), via \`gather\` when parallel."`
  After this commit polling **is** the sanctioned mechanism, so the prohibition cannot merely be
  renamed — inverted advice that keeps its old shape reads as a trap. Replacement:
  `"Waiting is control flow you write here: \`shell_background\`, then \`shell_logs\` in a bounded loop that breaks on what you actually read (an error line, a parsed port), never a fixed sleep."`
  Two things are preserved deliberately: the **bounded** loop (the old `until` at least could not
  spin forever, and the replacement must say so) and the fact that the loop reads *content*, which
  is the capability `until`'s regex never had.
  The `via \`gather\` when parallel` clause is dropped: `gather` is for *independent* calls, and
  two poll loops on two shells are better written as one loop over two ids.
- **`:4686` — one word.** `"leaked descriptors stop the process spawning \`shell\`/\`git\` children"` →
  `\`shell_run\`/\`git\``. Same for the identical sentence rendered into the sandbox-shims blurb.
- **`:9224` — a comment that becomes false.** `;; ONE shell tool for both kinds: the background one differs by its \`op\``
  is the `!`/`!&` bang-sugar path. `!` now resolves `shell_run` and `!&` resolves
  `shell_background`; the comment is rewritten to name the two, and `:9152`/`:9191`/`:9232`/`:9240`
  lose the definite article ("the shell tool" → the specific one each resolves).

#### 4.3.3 Tool `:description` / `:result` strings — the real token budget

- **`shell.clj:2705` — deleted and rewritten five times.** The current opener,
  `"Run bounded commands or manage background shells; ONE options map, never a positional string or array. Long or interactive: \`background\`, then \`wait\` with its REQUIRED \`until\` regex; \`logs\` snapshots; \`send\` writes \`text\`; \`stop\` kills."`,
  is a **table of contents for a dispatcher** — five of its six clauses are irrelevant to any
  given call, and the model pays for all of them on every call. Each new tool gets one sentence
  about itself and one cross-reference at most (`shell_run` → "long or interactive work goes to
  `shell_background`"). `shell_logs`, `shell_type`, `shell_stop` name no sibling at all: they are
  only reachable with an `id` you already got from `shell_background`.
  The `"ONE options map, never a positional string or array"` warning **disappears** rather than
  being copied five times — it exists because a six-op dispatcher had to reject arbitrary shapes;
  a schema with two named properties rejects them structurally.

  **What replaces it in `shell_run`'s opener is the Python pairing.** The sentence the model needs
  on every shell call is not "pass a map", it is *where the shell is driven from*: shells are used
  WITH `python_execution`, always. `shell_run` returns a result object, not a rendered string —
  `r["commands"][0]["stdout"]` — so the value of a shell call is realized by Python that filters,
  parses and decides; and every loop over a background shell (`shell_background` → `shell_logs`)
  is Python by construction now that nothing blocks on the model's behalf (§2 principle 7).
  So `shell_run`'s description ends with the pairing ("drive it from `python_execution`: batch the
  `commands`, read `r[\"commands\"][i][\"stdout\"]`, print only what you need"), and
  `shell_background`'s ends with the loop ("then poll `shell_logs` in `python_execution`").
  `shell_logs`/`shell_type`/`shell_stop` say nothing about it — they are already only reachable
  from an `id` a Python block is holding. This is the one cross-reference worth its tokens on
  every call, and it is the exact thing the merged description spent its budget *not* saying.
- **`shell.clj:363-364` — a live error message.** The descriptor-exhaustion text says
  `"or many live background shells (\`shell\` op \`stop\`)"` → `` (`shell_stop`) ``.
- **`shell.clj:1431` — a live error message** interpolating `shell({"op": "background"})` into
  the timeout advice → `shell_background(...)`. This one matters more than it looks: it is the
  string a model reads at the exact moment it is stuck, so it must name a tool that exists.
- **`shell.clj:1674, 1708` — comments arguing FOR the merge** ("the four shell tools became one:
  stopping was only reachable through…"). These are the merge's own rationale. They are deleted,
  not edited; the replacement comment records the reverse finding (one schema with six
  mutually-exclusive shapes reads as six schemas the model must first disambiguate).
- **`editing/core.clj:8210-8222`** — the `fs` description and its `op` enum die with `fs-symbol`.
  Five replacement descriptions, one line each; `is_overwrite` is documented only on the two
  tools that honour it, which retires the "required but ignored" wart named in §1.
- **`editing/core.clj:6534`** — the renderer docstring `"create_dirs → \`{:summary}\` only…"`
  follows the rename to `create_directory`.

#### 4.3.4 Docstrings that describe the topology (not model-facing, still wrong if unedited)

- **`rewind.clj:8`** lists the mutating tools `(\`write\`/\`patch\`/\`struct_patch\`/\`fs\`/\`format_code\`/\`struct_rename\`)` →
  `fs` becomes `move`/`copy`/`delete`/`create_directory`. **`rewind.clj:865`**: `";; Only \`fs\` can destroy a whole subtree (delete/move), so only \`fs\` pays"` →
  `";; Only \`delete\`/\`move\` can destroy a whole subtree, so only those two pay"`. This one is
  load-bearing prose: it explains why exactly one tool takes a snapshot cost, and after the split
  the *set* is what must be kept in sync with the checkpointing predicate.
- **`env_python.clj:1126-1133`** — the posix-compat docstring says subprocess/`os.system` delegate
  to "the ONE vis shell tool (`shell`)" and raises an 'enable the shell tool' message when its
  toggle is off. Renamed to `shell_run`; the docstring's own claim of *soft string-level coupling
  to the tool NAME only* is precisely why this is a one-word change and not a refactor.
- **`resources/vis-shims/posix.py:12, 50, 122`** — the shim resolves the tool from `globals()` at
  call time, so the **name string changes** (`shell` → `shell_run`) alongside the three comments.
  This is the one prose site that is also behaviour: get it wrong and `subprocess.run` raises
  "enable the shell tool" on a machine where the tool is enabled.
- **`git_tool.clj:16, 138`** — "inherits the shell tool's working directory", "through the SHELL
  tool's own runner". These refer to `foundation.shell`'s internal `run-argv`, which is unchanged;
  reword to name the **namespace**, not a tool, so the sentence stops tracking a tool name at all.
- **`gateway/state.clj:4576, 4606`** and **`channel_tui/state.clj:5767`** — resource-teardown
  comments, the last of which spells `` (`shell` op "background", … ) ``. Renamed.

#### 4.3.5 Tests that assert the prose (these are the RED tests of §8)

The prose is pinned, which is the good news: rewriting it without updating these fails loudly.

- **`prompt_test.clj:107`** asserts the system prompt contains `` "`shell` op `wait`" `` and
  `` "REQUIRED `until` regex" ``. Both assertions **invert**: the new test asserts the prompt
  contains neither string anywhere, and does contain `shell_background`/`shell_logs`.
  **`:148-149`** asserts core keeps "only the routing rule (background shells → `shell` op
  `wait`), never the tool-local prohibition" — the *principle* survives (core states routing, not
  tool-local rules), only its example changes.
- **`loop_test.clj:3752`** asserts the `python_execution` description contains
  `` "`time.sleep`/poll for background shells" `` and `` "`shell` op `wait`" `` → inverted the same way.
  **`:2749`** asserts the prompt mentions `` `fs` `` → becomes an assertion that all five names
  (`copy`, `move`, `delete`, `create_directory`, `file_exists`) appear in the filesystem routing
  line, so an omitted verb fails the suite instead of quietly re-opening the `mkdir -p` path.
  **`:3897`** pins call-synthesis rendering `create_dirs("d")` → `create_directory("d")`.
- **`editing/core_test.clj:99`** (`fs` is in the advertised names), **`:6174-6307`** (the merged
  tool's action-discriminated result shapes), **`:6535`** (`fs` carries no `is_missing_ok` twin):
  the first two are deleted with the tool; the third is **kept and re-pointed at `delete`**,
  because its subject is the no-twin rule, not the merge.
- **`shell_test.clj:1161`** — `"advertises exactly one native shell tool covering the lifecycle"` is
  the merge's headline assertion, inverted to *five*, each with its own schema and no `op` key.
  **`:443`** — the TOTAL contract that every op returns the same envelope becomes: every one of
  the five returns its own documented keys (§7), which is a stronger statement, not a weaker one.
- **`env_python_test.clj:109`** — `fs` also answers to the alias `fs_tool`. Both names go; per
  repo policy no alias is left behind.
- **`posix_compat_shim_test.clj:6, 51, 118`** and **`sandbox_shim_contract_test.clj:149`** — the
  shim's routing and its 'enable the shell tool' message, re-pointed at `shell_run`.
- **`bang_integration_test.clj:5, 8`** — `!`/`!&` run "the shell tool directly"; now two tools.

#### 4.3.6 What is deliberately NOT rewritten

- `CHANGELOG.md:1451, 1578` ("Reduce shell tool surface") and
  `apps/vis-companion/CHANGELOG.md:16` — history is history. The new entry says what it reverses;
  it does not edit the old line.
- `resources/vis-docs/context-and-prompts.md:140` — "tints the leading `!`/`!&` marker in the
  shell tool color". This is about a TUI colour keyed on the tool's *category*, not its name, and
  stays true whichever tool the bang resolves. Left alone. If the split gave each of the five its
  own colour the sentence would need work — it does not; they share one category.
- Nothing in `AGENTS.md`, `README.md`, or the rest of `resources/vis-docs/**` (verified empty).


## 5. How it is called from Python

Each tool is bound bare in the sandbox, exactly as `shell` is today. Positional ergonomics
come from `:call {:pos [...]}`.

```python
r = await shell_run(["git status", "npm ci"], cwd="apps/vis-companion")
r["commands"][0]["stdout"]

await shell_background(["npm run dev"], id="dev")
await shell_logs("dev", n=50)
await shell_type("dev", "y")
await shell_stop("dev")

# waiting is a loop the model writes and can reason about
while True:
    r = await shell_logs("dev")
    if any("Local:" in l for l in r["lines"]):
        break
    if any("EADDRINUSE" in l for l in r["lines"]):
        raise RuntimeError("port taken")   # <- what `until=` could never do
    await sleep(1)

await copy("a.txt", "b.txt", is_overwrite=True)
await move("a.txt", "sub/a.txt")
await create_directory("build/out")
await file_exists("build/out")
await delete("build/out")
```

`:call` shapes to register:

| tool | `:call` |
|---|---|
| `shell_run` / `shell_background` | `{:pos ["commands"]}` |
| `shell_logs` / `shell_stop` | `{:pos ["id"]}` |
| `shell_type` | `{:pos ["id" "text"]}` |
| `copy` / `move` | `{:pos ["src" "dest"]}` |
| `delete` / `create_directory` / `file_exists` | `{:pos ["paths"]}` |

## 6. Schemas (proposed, verbatim shape)

```clojure
;; shell_run
{:type "object"
 :properties {"commands" {:type "array" :items {:type "string" :minLength 1} :minItems 1
                          :description "`bash -lc` lines, run strictly in order."}
              "cwd" {:type "string" :description "Dir under an allowed root; workspace root default."}}
 :required ["commands"]
 :additionalProperties false}

;; shell_background
{:type "object"
 :properties {"commands" {:type "array" :items {:type "string" :minLength 1} :minItems 1}
              "id" {:type "string" :minLength 1
                    :description "Handle; derived from the program name when omitted. Re-issuing the same script returns the live shell."}
              "cwd" {:type "string"}}
 :required ["commands"]
 :additionalProperties false}

;; shell_logs
{:type "object"
 :properties {"id" {:type "string" :minLength 1 :description "Background handle; live ids are in session resources."}
              "n" {:type "integer" :minimum 1 :maximum 2000
                   :description "Tail lines returned; default 200."}}
 :required ["id"]
 :additionalProperties false}

;; shell_type
{:type "object"
 :properties {"id" {:type "string" :minLength 1}
              "text" {:type "string" :description "Keystrokes written to stdin."}
              "is_enter" {:type "boolean" :description "Append a newline; default true."}}
 :required ["id" "text"]
 :additionalProperties false}

;; shell_stop
{:type "object"
 :properties {"id" {:type "string" :minLength 1}}
 :required ["id"]
 :additionalProperties false}

;; copy / move
{:type "object"
 :properties {"src" {:type "string" :minLength 1}
              "dest" {:type "string" :minLength 1}
              "is_overwrite" {:type "boolean" :description "Replace an existing destination; default false."}}
 :required ["src" "dest"]
 :additionalProperties false}

;; delete / create_directory / file_exists
{:type "object"
 :properties {"paths" {:type "array" :items {:type "string" :minLength 1} :minItems 1}}
 :required ["paths"]
 :additionalProperties false}
```

Every one of them is small enough to read in full — which is the point.

## 7. Result contracts, one per tool

Today `:result` on `shell-symbol` (`:2695`) describes a union. Split it:

- `shell_run` → `{cwd, commands:[{command, stdout, stderr, exit, duration_ms, timed_out, note}], exit, duration_ms, timed_out}`. No `id`, no `lines`, no `stage`.
- `shell_background` → `{id, cwd, commands, pid, status, note}`, plus `is_already_running` when an idempotent re-issue hit a live shell.
- `shell_logs` → `{id, lines, dropped_lines, status, exit}`. Returns immediately, always the
  same keys; `dropped_lines` is how a polling loop detects it missed output between reads.
- `shell_type` → `{id, text, is_enter, status}`.
- `shell_stop` → `{id, status, exit, note}`.
- `copy` / `move` → `{src, dest}`.
- `delete` / `create_directory` / `file_exists` → `{paths: [{path, is_deleted | is_created | is_exists}]}`.

`stage` disappears as a wire key: the tool name already says it.

## 8. Landing plan

One commit, per repo policy — production code and tests together.

1. RED first: a surface test asserting the ten tool names exist, that no schema anywhere
   carries `until` or `timeout_secs`, that `copy`'s schema requires `src`/`dest`, and that no
   registered model-facing symbol named `shell` or `fs` remains. Watch it fail.
2. Split `shell.clj` registrations; keep `*-impl` and the now-internal `shell-dispatch`
   untouched.
3. Flip the five editing symbols native, rename two, re-shape `copy`/`move`; delete
   `fs-tool`/`fs-symbol`.
4. Rewrite prompts, `AGENTS.md`, `resources/vis-docs/**`, and the `shell.clj:1431` message.
5. `run_tests` on the smallest namespaces: foundation shell, editing core, surface-contract,
   plus the TUI render tests that name `shell` cards.
6. `lint_code` + `format_code` clean, then commit and push.

## 9. Remaining open questions

- **Q1 `shell_attach`.** Do we want to adopt a process this session did not spawn (bare PID,
  or a `tmux`/`screen` target) so `shell_logs`/`shell_type`/`stop` work against it? Nothing in
  `shell.clj` does this today. Recommendation: **not in this commit** — it needs its own reader
  thread, its own resource lifecycle, and its own "who owns the kill" answer.
- **Q2 Replay of old sessions.** The DB still carries `:vis/tool-name "shell"` and `"fs"` on
  finished calls. Confirm the TUI op cards and the companion transcript render an unknown tool
  name generically before the symbols disappear.
- **Q3 Toggle granularity.** One `shell` toggle for all five (rec), or per-tool toggles? And do
  the five filesystem tools need a toggle at all — they have none today.
