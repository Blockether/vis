# Context management

A long session fills the context window of the model with old file reads and tool results. Vis
returns only selected tool output, and it can replace completed work with a summary. Your session
history stays saved.

## When to use

- **A long session spends tokens on old file reads and tool output.** Ask Vis to
  [fold the settled work](#folding-settled-work) into a summary before the next phase.
- **Another session already investigated your problem.**
  [Reuse its findings](#reuse-another-session-s-findings).
- **You need a helper that Vis wrote earlier, with changes.** Ask Vis to
  [find and refine it](#reuse-and-refine-session-helpers).
- **You want to know why Vis prints only part of a result, or how it edits files.** Read
  [One tool, many functions](#one-tool-many-functions) and
  [Read and edit files](#read-and-edit-files).
- **Your own program must show the context budget, the usage, or the cost of a session.** Use the
  [Context management API](context-management-api.md).

## Folding settled work

When a phase of research is complete, ask for a summary:

> Summarize what we learned and what remains open, then continue with the fix.

Vis calls that summary a **fold**. In the active context of the model, a fold replaces completed
steps with conclusions, open questions, relevant files, and test state. The steps stay saved. With
session introspection on, you can inspect the raw transcript. A call to `read_session()` alone
does not put folded steps back into the conversation.

Fold when a completed phase makes room for substantial new work. Vis gives the model a `hint` when
the context budget calls for a fold. A fold after every small tool call can reduce the reuse of the
prompt cache.

## Reuse another session's findings

Ask Vis to [consult the other session](council.md#ask-vis-to-consult-another-session). Vis brings
back the relevant findings, not the transcript, and checks them against the current code. A
consultation can cost model charges, and it does not guarantee a lower cost.

## Reuse and refine session helpers

Ask Vis to adapt a helper that it already wrote:

> Find the helper that summarized those rows, update it for the new columns,
> and check its callers.

A one-line docstring makes a helper easier to find with `defs()`. `doc(name)` returns the whole
docstring. Helpers do not appear in `apropos`.

After each block, Vis saves your helpers and variables. A new sandbox restores them, for example
after an idle timeout or a gateway restart.
[Sandbox state after a restart](python-sandbox.md#sandbox-state-after-a-restart) gives the limits.

If you define a name again, Vis replaces its saved copy. `del name` removes a helper or a variable
and frees its memory. Python does not update references. Before you redefine or remove a name,
check its aliases, captured defaults, and callers.

On your request, a useful helper can become an [extension](extending.md), after a review of its
dependencies, preconditions, and tests.

## How Vis keeps the context small

### One tool, many functions

`python_execution` gives the model search, reads, edits, and tests as Python. Only printed output
returns to the model. Other results stay in variables, and related work shares one call. Before
each block, Vis builds `session` again with the latest turn, workspace roots, budget, and
extension context.

### Discovery instead of catalogs

The prompt does not carry every function signature. `apropos(pattern)` matches a regular
expression against the public symbol names, in manifest order and without regard to case. Guides
and skills also match by their title, opening, headings, and `When to use` problems. `doc(name)`
reads a contract, a guide, or a skill.

### Read and edit files

The agent reads and writes files with plain Python. It prints only the lines that it needs, with
line numbers. To edit, it replaces exact text, and it first checks that the text occurs one time.
`grep` finds the lines and shows numbered rows under a `path` header.

## Reference: folds and context budget

### Fold keys

`fold_session(key, gist)` replaces settled steps with your summary. A string key selects them:

| Key | Steps |
|---|---|
| `"t2"` | One turn |
| `"t2/i4-i5"` | A range of iterations |
| `"-t3/i9"` | Everything through an iteration |
| `"-t3"` | Every turn through a turn |
| `"t2/i5-"` | The settled steps since an iteration |

Commas join separate ranges. You cannot fold the live step. Without `gist`, the steps leave the
active context and no summary replaces them. The history stays saved. A fold has no inline undo.

A turn key, or a range that covers a whole turn, also removes the request and the answer of that
turn. The turn must be before the current turn. A chat turn without tool calls folds the same way.

Iteration numbers start again in each turn. `fold_session` refuses a range that ends at an
iteration that an earlier turn does not have. For example, it refuses `"-t1/i16"` when turn 1 has
two iterations. The refusal names the key for every earlier turn.

### Context budget

`session["utilization"]` reports these values:

| Key | Value |
|---|---|
| `latest_measured_input_tokens` | The latest request input that the provider measured, context included. It can include a rejected overflow request. It is not a live count, output usage, or a turn total |
| `auto_compress_above` | The soft budget |
| `model_input_limit` | The hard input limit for each request |
| `hint` | A request to fold. It is absent below 75% of the soft budget |

From 90% of the soft budget, the `hint` asks for a fold before the next answer. Above 100%, it
requires a fold while the pressure remains. After the first turn, it gives the exact key that
folds every earlier turn that is not folded yet, so your earlier gists stay. A fold that covers an
earlier fold replaces its gist.

| Model | Soft budget |
|---|---|
| Claude Opus, Claude Fable, and Claude Sonnet | 250k tokens |
| GPT | 230k tokens |
| Other models, or an unknown input limit | 200k tokens |

The soft budget is never more than 90% of a known input limit. For example, a 204k input limit
gives a GPT model a budget of 183,600 tokens.

### Fold measurement

A fold receipt estimates the removed text locally. The next provider response measures the input
change for the whole request, without an extra call. That change includes new tool output, the
summary, and other prompt changes, so it differs from the savings of the fold.

Request health records `fold_measurement` with `status: "measured"`, `before_input_tokens`,
`after_input_tokens`, and `net_reduction_tokens`. A positive `net_reduction_tokens` means less
input, and a negative value means growth. All folds between two requests share one measurement and
its `fold_count`.

The status is `pending` until the response arrives. It is `unavailable`, with a `reason`, when
usage is missing, the turn changes, or the provider or the model is unknown or changed.

The detailed metrics are in the session diagnostics, not in every model request.

## Reference: session helpers

These calls look up a helper named `summarize_rows`:

```python
print(defs(pattern="summar|count"))
print(defs("summarize_rows"))
print(defs("summarize_rows", details=True))
```

- `defs()` lists up to 20 helpers in alphabetical order, each with a call hint and a docstring
  gist. A call hint has at most 120 characters and no annotations. It shows the type of a default
  (`=<int>`), not its value. Your variables follow, with type and size but
  never values. A row marks a restored helper and a value that is not saved.
- Match counts appear only when a pattern or a page hides rows.
- `defs(pattern="summar|count", limit=10, offset=0)` matches a case-sensitive regex against names,
  first-line docstring gists, and variable types. `limit` is from 1 to 100, and `offset` is zero or
  more. For more results, increase the offset or narrow the pattern.
- `defs("summarize_rows")` returns the unchanged source. For a variable, it returns the listing
  row. Check the source for secrets before you share it.
- `defs("summarize_rows", details=True)` returns the origin, the SHA-256 of the source, and up to
  20 global or captured names from the source, with types and presence. These hints do not prove
  dependencies or liveness. Vis does not analyze default and decorator expressions. The digest
  identifies the source, not argument defaults, captured values, or mutable state.

## See also

- To read context, usage, and cache health from your own program, read
  [Context management API](context-management-api.md).
- For the interpreter that runs the programs of the model, read [Python sandbox](python-sandbox.md).
- To turn a repeated helper into a tool, read [Extending Vis](extending.md).
- For instructions that Vis loads only on demand, read [Skills](skills.md).
