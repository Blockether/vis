# How Vis manages context

Long sessions can fill the model's working space with old file reads and tool
results. Vis returns only selected tool output and can summarize completed work
without deleting your session history.

When research is done, you can say:

> Summarize what we learned and what remains open, then continue with the fix.

## Folding settled work

Vis calls a summary a **fold**. It replaces completed steps in the model's
active context with conclusions, open questions, relevant files and test state.
The originals stay saved, but the summary is what the model sees next. With
session introspection enabled, you can inspect the raw transcript; a call to
`read_session()` alone will not put folded steps back into the conversation.

Fold when a completed phase makes room for substantial new work. Vis provides
a `hint` when the context budget calls for it. Folding after every small tool
call can undermine prompt-cache reuse.

## Reuse another session's findings

Ask Vis to [consult another session](council.md#ask-vis-to-consult-another-session)
if it has already investigated your problem. Vis brings back relevant findings
and checks them against the current code, rather than copying a transcript.
Consultation may incur model charges; it does not guarantee lower cost.

## Example editing workflow

Ask Vis to find a function, change it and test it. It can keep file reads short,
reuse intermediate results and fold the investigation before editing. A useful
session helper can later become an [extension](extending.md) if you request it,
after reviewing its dependencies, preconditions and tests.

## One tool, many functions

`python_execution` provides search, reads, edits and tests through Python.
Only printed output returns to the model; other results can stay in variables,
and related work can share a call. Vis rebuilds `session` before each block
with the latest turn, workspace roots, budget and extension context.

## Discovery instead of catalogs

The prompt does not carry every function signature. `apropos(pattern)` searches
public symbol names by regular expression in manifest order; `doc(name)` reads
a contract, guide or skill when needed.

## Reuse and refine session helpers

You can ask Vis to adapt a helper it already wrote:

> Find the helper that summarized those rows, update it for the new columns,
> and check its callers.

A one-line docstring makes it easier to find with `defs()`; `doc(name)` returns
the whole docstring. Helpers do not appear in `apropos`. Vis saves definitions
after each block on a best-effort basis and can restore them after a gateway
restart. Redefining a name replaces its saved source; `del obsolete_name`
removes it. Check aliases, captured defaults and callers first: Python
references do not update automatically, and restored resources may not be live.

## Addresses, not copies

`grep` and `cat` label file lines with `line:hash` addresses. `patch` uses those
addresses, with all edits for a file in one call; stale addresses or syntax
errors reject the batch. Use `Path.read_text()` for data you will not edit.

## Reference: folds and context budget

`fold_session(key, gist)` replaces settled steps with your summary. A string
key selects a turn (`"t2"`), range (`"t2/i4-i5"`), everything through an
iteration (`"-t3/i9"`), or settled steps since one (`"t2/i5-"`). Commas join
disjoint ranges. The live step cannot be folded. Without `gist`, the steps
leave active context without a replacement summary; history is still saved.
There is no inline undo.

In `session["utilization"]`, `latest_measured_input_tokens` is the latest
provider-reported request **input**, including context. It can include a
rejected overflow request; it is not a live count, output usage or turn total.
`auto_compress_above` is the soft budget (normally 200k tokens, lower for
smaller windows); `model_input_limit` is the hard per-request input limit. A
conditional `hint` starts at 75% of the soft budget, becomes urgent at 90% and
requires folding above 100% while pressure remains.

A fold receipt estimates removed text locally. The next provider response
measures the input change for the **whole request**, without an extra call—not
just the fold's savings. Request health records `fold_measurement` with
`status: "measured"`, `before_input_tokens`, `after_input_tokens` and
`net_reduction_tokens` (positive means less input; negative means growth).
New tool output, the summary and other prompt changes also count. Folds between
two requests share one measurement and its `fold_count`, not one each. The
status is `pending` until the response, or `unavailable` with a `reason` if
usage is missing, provider/model is unknown or changed, or the turn changes.
Detailed metrics live in session diagnostics, not in every model request.

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
- `defs(pattern="summar|count", limit=10, offset=0)` matches names and first-line
  docstring gists with a case-sensitive regex. `limit` is 1–100; `offset` is
  nonnegative. Narrow the pattern or increase the offset for more results.
- `defs("summarize_rows")` returns unchanged source; check it for secrets before
  sharing. `defs("summarize_rows", details=True)` returns origin, source SHA-256
  and up to 20 source-derived global or captured names with types and presence.
  These hints do not prove dependencies or liveness. Default and decorator
  expressions are not analyzed; the digest identifies source, not argument
  defaults, captured values or mutable state.

## See also

- [Python sandbox](python-sandbox.md) — the interpreter the model's programs run in.
- [Extending Vis](extending.md) — turning a recurring helper into a tool.
- [Skills](skills.md) — instructions loaded when needed rather than included in every request.
