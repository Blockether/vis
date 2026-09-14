# How Vis manages context

The model can read only a limited amount of information at once. That working
context includes your instructions, conversation history, file contents and tool
results. Vis keeps it smaller by filtering tool output, reusing earlier findings
and summarizing completed work. Your full session history remains saved.

## One tool, many functions

The model has a single tool, `python_execution`. Every capability (searching,
reading, editing, running tests, shells) is a Python function inside it, and
only what a block prints returns to the model. That has three consequences:

- Multiple operations can run in one tool call.
- Intermediate results can remain in Python variables instead of being printed.
- The model can reuse helpers it defined earlier. `defs()` shows a bounded index;
  helper definitions persist across turns and can be restored after a gateway restart.

Before each block, Vis rebuilds the `session` dict with turn counters, workspace
roots, utilization figures and extension-provided context.

## Discovery instead of catalogs

The prompt does not list every function signature. The model finds functions
with `apropos(pattern)`, which filters public symbol names using a regular
expression, and reads their documentation with `doc(name)`. Documentation
pages and skills are available the same way. Search matches names only and
returns results in manifest order.

## Reuse and refine session helpers

If a session has accumulated useful Python helpers, ask Vis to reuse them rather
than start another version:

> Find the helper we used to summarize these rows, adapt it to the new columns,
> and check that its existing callers still work.

Vis can search the helper index, read the existing source and redefine the same
name. A one-line docstring makes a helper easier to find: its first line appears
in the index, and `doc(name)` returns the full docstring. Session helpers are not
included in `apropos`.

After each block, Vis saves the current definitions. Redefining a name replaces
its saved source and `del obsolete_name` removes it, so both changes survive a
gateway restart. The save is best effort: when it fails, the previous copy stays.

At a phase boundary, you can ask Vis to review the helpers it owns and remove
obsolete names. Deletion is explicit, not based on age. Python's usual reference
rules still apply: redefining a name does not update aliases or functions saved
in default arguments. Deleting it with `del obsolete_name` can break callers that
look up that global name; it does not remove other references to the function.
Check those references before cleanup.

### Helper lookup reference

For an existing helper named `summarize_rows`, you can inspect it with:

```python
print(defs(pattern="summar|count"))
print(defs("summarize_rows"))
print(defs("summarize_rows", details=True))
```

- `defs()` lists up to 20 helpers in alphabetical order, with origin, source
  length and a short docstring gist. Call hints are capped at 120 characters;
  annotations are omitted and defaults show only their type, such as `=<int>`,
  never their value. These hints are not source code.
- `defs(pattern="summar|count", limit=10, offset=0)` searches names and first-line
  docstring gists with a case-sensitive regular expression. `limit` must be an
  integer from 1 to 100; `offset` must be a nonnegative integer. Increase the
  offset to read another page, or narrow the pattern.
- `defs("summarize_rows")` returns that helper's source, unchanged. Review it for
  secrets before sharing it.
- `defs("summarize_rows", details=True)` returns metadata instead of source:
  origin, source SHA-256 and up to 20 source-derived global or captured names
  with types and presence. These are advisory hints, not a complete dependency
  graph or a check that a browser session, file or other resource is still usable.
  Liveness is unknown. Default and decorator expressions are not analyzed. The
  digest identifies source, not argument defaults, captured values or mutable state.

Restoring a definition does not prove its dependencies are ready. Recheck its
preconditions after a restart; do not assume live handles have been restored.

If the same capability proves useful across sessions, ask Vis to propose it in
Improve before turning it into an extension. A useful proposal includes concrete
uses, a helper name and source fingerprint, sanitized source or evidence,
required globals and preconditions, and verification tied to that source version.
A proposal does not create an extension or authorize promotion; creating one
requires a separate request and the [extension contract](extending.md).

## Reuse another session's findings

If you have already investigated a problem in another session, ask Vis to
[consult that session](council.md#ask-vis-to-consult-another-session) rather than
start from scratch. The other agent can answer from its saved context, so the
current conversation needs only the relevant findings, not the whole transcript.
Vis still needs to check that those findings apply to the current code.

A consultation can avoid repeated research, but waking a session may make new
model calls. Reusing findings does not guarantee a prompt-cache hit or lower
cost; it is most useful when the other session knows something relevant.

## Addresses, not copies

When the agent reads code with `grep` or `cat`, each line comes with a `line:hash`
address. An edit can refer to those addresses instead of repeating the old text.
All edits for one file go in a single `patch` call. A stale address or syntax
error rejects the whole batch, rather than applying part of the change.

For data that will not be edited, the agent can also use `Path.read_text()`.

## Folding settled work

As a conversation grows, the agent can replace completed steps in its working
context with a summary. Vis calls this **folding**. The summary keeps the
conclusions, open questions, relevant files and the state of edits and tests.
Folding does not delete the original steps from the database. With the
`introspection` toggle enabled, the agent can retrieve them using `read_session()`;
without it, the model has only the summary.

The API is `fold_session(key, gist)`, where `gist` is that summary and `key`
selects the history: `"t2"` for a whole turn, `"t2/i4-i5"` for a range, or
`"-t3/i9"` for everything through an iteration. Comma-separated keys select
multiple ranges. The current iteration cannot be folded.

The model monitors `session["utilization"]`. `last_request_tokens` is compared
with `auto_compress_above`, normally 200k tokens and reduced for smaller input
windows. A `hint` begins at 75% of that budget, becomes more urgent at 90%, and
requires folding above 100%. It remains while usage is high. `saturation` and
`headroom_tokens` are measured against the model's hard input limit.

A fold receipt estimates removal with the local tokenizer. The next provider
response supplies the actual post-fold input count, without an extra model call.
`session["utilization"]["fold_measurement"]` then reports `status: "measured"`,
`before_input_tokens`, `after_input_tokens` and their signed difference,
`net_reduction_tokens`. A positive difference means less input; a negative one
means growth. This is the net change of the whole request, including the gist,
new tool traffic and other prompt changes, not isolated fold savings or money saved.

All `fold_session` calls between those two requests share one measurement and a
`fold_count`; the reduction is not credited to each fold separately. Until the response arrives,
the status is `pending`. Missing input usage, an unknown or changed provider/model,
or a turn change makes the measurement `unavailable`, with a `reason`, rather
than reusing an older count. The latest result is also stored in request health
as `fold_measurement` for session introspection.

## Example editing workflow

For a code change, the agent can locate a function with `grep`, read the relevant
lines with `cat`, apply a `patch` and run the affected tests. It does not need to
load whole files or repeat the old code in the edit. Reusable helpers can be
reviewed for an Improve proposal before you request an [extension](extending.md).

## See also

- [Python sandbox](python-sandbox.md) — the interpreter the model's programs run in.
- [Extending Vis](extending.md) — turning a recurring helper into a tool.
- [Skills](skills.md) — instructions loaded when needed rather than included in every request.
