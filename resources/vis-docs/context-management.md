# Context management

Long sessions can fill the model's context window with old file reads and tool
results. Vis returns only selected tool output and can compress completed work
into a summary without deleting your session history.

## When to use

- **A long session spends tokens on old file reads and tool output.** Ask Vis to
  [fold the settled work](#folding-settled-work) into a summary before the next
  phase.
- **Another session already investigated your problem.** [Reuse its
  findings](#reuse-another-session-s-findings) instead of repeating the research.
- **You need a helper that Vis wrote earlier, with changes.** Ask Vis to [find and
  refine it](#reuse-and-refine-session-helpers) instead of writing a new one.
- **You want to know why Vis prints only part of a result, or edits files by line
  address.** See [One tool, many functions](#one-tool-many-functions) and
  [Addresses, not copies](#addresses-not-copies).
- **Your own program must show the context budget, the usage or the cost of a session.** Use
  [Context management API](context-management-api.md).

## Folding settled work

When research is done, you can say:

> Summarize what we learned and what remains open, then continue with the fix.

Vis calls a summary a **fold**. It replaces completed steps in the model's
active context with conclusions, open questions, relevant files and test state.
The originals stay saved, but the summary is what the model sees next. With
session introspection enabled, you can inspect the raw transcript. A call to
`read_session()` alone will not put folded steps back into the conversation.

Fold when a completed phase makes room for substantial new work. Vis provides
a `hint` when the context budget calls for it. Folding after every small tool
call can undermine prompt-cache reuse.

## Reuse another session's findings

Ask Vis to [consult another session](council.md#ask-vis-to-consult-another-session)
if it has already investigated your problem. Vis brings back relevant findings
and checks them against the current code, rather than copying a transcript.
Consultation may incur model charges. It does not guarantee lower cost.

## Example editing workflow

Ask Vis to find a function, change it and test it. It can keep file reads short,
reuse intermediate results and fold the investigation before editing. A useful
session helper can later become an [extension](extending.md) if you request it,
after reviewing its dependencies, preconditions and tests.

## One tool, many functions

`python_execution` provides search, reads, edits and tests through Python.
Only printed output returns to the model. Other results can stay in variables,
and related work can share a call. Vis rebuilds `session` before each block
with the latest turn, workspace roots, budget and extension context.

## Discovery instead of catalogs

The prompt does not carry every function signature. `apropos(pattern)` matches a
regular expression, ignoring case, against public symbol names in manifest order.
Guides and skills also match by their title, opening, headings and `When to use`
problems. `doc(name)` reads a contract, guide or skill when needed.

## Reuse and refine session helpers

You can ask Vis to adapt a helper it already wrote:

> Find the helper that summarized those rows, update it for the new columns,
> and check its callers.

A one-line docstring makes a helper easier to find with `defs()`. `doc(name)` returns the whole
docstring. Helpers do not appear in `apropos`.

After each block, Vis saves your helpers and variables. A new sandbox restores them, for example
after an idle timeout or a gateway restart. [Sandbox state after a
restart](python-sandbox.md#sandbox-state-after-a-restart) gives the limits.

If you define a name again, Vis replaces its saved copy. `del name` removes a helper or variable
and frees its memory. Before you redefine or remove a name, check aliases, captured defaults and
callers. Python references do not update automatically.

## Addresses, not copies

`grep` and `cat` label file lines with `line:hash` addresses. `patch` uses those
addresses, with all edits for a file in one call. Stale addresses or syntax
errors reject the batch. Use `Path.read_text()` for data you will not edit.

## Reference: folds and context budget

`fold_session(key, gist)` replaces settled steps with your summary. A string key selects one of these:

- A turn: `"t2"`.
- A range: `"t2/i4-i5"`.
- Everything through an iteration: `"-t3/i9"`.
- Settled steps since an iteration: `"t2/i5-"`.

Commas join disjoint ranges. You cannot fold the live step. Without `gist`, the steps leave active
context and no summary replaces them. History is still saved. There is no inline undo.

A turn key, or a range that covers a whole turn, also removes the request and answer of that turn.
This applies only to turns before the current turn.

`session["utilization"]` reports these values:

- `latest_measured_input_tokens` is the latest request **input** that the provider measured,
  including context. It can include a rejected overflow request. It is not a live count, output
  usage or turn total.
- `auto_compress_above` is the soft budget.
- `model_input_limit` is the hard input limit for each request.
- `hint` is conditional. It starts at 75% of the soft budget and becomes urgent at 90%. Above
  100%, it requires a fold while the pressure remains.

The soft budget depends on the model: 250k tokens for Claude Opus models, 230k for GPT models and
200k for other models. It is never more than 90% of a known input limit, so smaller windows get a
lower budget. For example, a 204k input limit gives a GPT model a budget of 183,600 tokens. When
the input limit is unknown, the soft budget is 200k.

A fold receipt estimates the removed text locally. The next provider response measures the input
change for the **whole request**, not only the savings of the fold. This needs no extra call. New
tool output, the summary and other prompt changes also count.

Request health records `fold_measurement` with `status: "measured"`, `before_input_tokens`,
`after_input_tokens` and `net_reduction_tokens`. A positive `net_reduction_tokens` means less
input, and a negative value means growth. All folds between two requests share one measurement and
its `fold_count`.

The status is `pending` until the response arrives. It is `unavailable`, with a `reason`, in these
cases:

- Usage is missing.
- The provider or model is unknown or changed.
- The turn changes.

Detailed metrics are in session diagnostics, not in every model request.

## Reference: session helpers

### Helper lookup reference

For a helper named `summarize_rows`:

```python
print(defs(pattern="summar|count"))
print(defs("summarize_rows"))
print(defs("summarize_rows", details=True))
```

- `defs()` lists up to 20 helpers alphabetically, with origin, source length
  and a docstring gist. Call hints are at most 120 characters, omit annotations
  and show only a default's type (`=<int>`), not its value. Hints are not source.
  Your variables follow, with type, size and save status. `defs()` never shows values.
- `defs(pattern="summar|count", limit=10, offset=0)` matches names, first-line docstring gists
  and variable types with a case-sensitive regex. `limit` is 1–100, and `offset` is nonnegative.
  For more results, increase the offset or narrow the pattern.
- `defs("summarize_rows")` returns unchanged source. For a variable, it returns
  the listing row. Check source for secrets before sharing.
- `defs("summarize_rows", details=True)` returns origin, source SHA-256
  and up to 20 source-derived global or captured names with types and presence.
  These hints do not prove dependencies or liveness. Default and decorator
  expressions are not analyzed. The digest identifies source, not argument
  defaults, captured values or mutable state.

## See also

- [Context management API](context-management-api.md) — context, usage and cache health from your
  own program.
- [Python sandbox](python-sandbox.md) — the interpreter the model's programs run in.
- [Extending Vis](extending.md) — turning a recurring helper into a tool.
- [Skills](skills.md) — instructions loaded when needed rather than included in every request.
