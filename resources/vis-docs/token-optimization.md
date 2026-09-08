# How Vis manages context

Files, tool output and history consume the model's input tokens. Vis reduces
repeated input by storing data in the runtime, returning selected results and
replacing earlier steps with summaries.

## One tool, many functions

The model has a single tool, `python_execution`. Every capability (searching,
reading, editing, running tests, shells) is a Python function inside it, and
only what a block prints returns to the model. That has three consequences:

- Multiple operations can run in one tool call.
- Intermediate results can remain in Python variables instead of being printed.
- The model can reuse helpers it defined earlier. `defs()` lists them; function
  definitions persist across turns and gateway restarts.

Before each block, Vis rebuilds the `session` dict with turn counters, workspace
roots, utilization figures and extension-provided context.

## Discovery instead of catalogs

The prompt does not list every function signature. The model finds functions
with `apropos(pattern)`, which filters public symbol names using a regular
expression, and reads their documentation with `doc(name)`. Documentation
pages and skills are available the same way. Search matches names only and
returns results in manifest order.

## Addresses, not copies

`grep` and `cat` return text with a `line:hash` address for each line. Pass those
addresses to `patch` to identify the lines to change without repeating their
old text. Include all edits for one file in a single `patch` call. The write is
atomic; a stale address or syntax error rejects the whole batch.

`Path.read_text()` is available for reading data that will not be edited.

## Folding settled work

`fold_session(key, gist)` removes earlier steps from future model requests
without deleting them from the database. A key selects turns or iterations:
`"t2"` selects a whole turn, `"t2/i4-i5"` a range, and `"-t3/i9"` everything
through an iteration. Comma-separated keys select multiple ranges. The
current iteration cannot be folded.

The gist replaces the selected content in model requests. Include conclusions,
open questions, exact paths and symbols, and the state of edits and tests.
With the `introspection` toggle enabled, `read_session()` can retrieve folded
content. Without it, the model has only the gist.

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

Locate code with `grep`, read the relevant region with `cat`, apply a `patch`,
and run the affected tests. This avoids returning whole files and repeating
old text in edits. Reusable helpers can become extensions; see
[Extending Vis](extending.md).

## See also

- [Python sandbox](python-sandbox.md) — the interpreter the model's programs run in.
- [Extending Vis](extending.md) — turning a recurring helper into a tool.
- [Skills](skills.md) — instructions loaded when needed rather than included in every request.
