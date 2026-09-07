# How Vis manages context

Every token the model reads costs money and attention, and a long session can
fill a context window with files, tool output and history that no longer
matter. Vis is designed around one rule: data stays addressable in the runtime,
and the model receives only the slice it needs. This page explains the
mechanisms behind that rule so you can predict what a session costs and why it
behaves the way it does.

## One tool, many functions

The model has a single tool, `python_execution`. Every capability (searching,
reading, editing, running tests, shells) is a Python function inside it, and
only what a block prints returns to the model. That has three consequences:

- A batch of fifty operations costs one tool call, not fifty.
- Intermediate results live in Python variables and never enter the context.
- The model writes small programs, reusing helpers it defined earlier. `defs()`
  lists them, and they survive turns and even a gateway restart.

The `session` dict the model sees is rebuilt before every block from the
engine's snapshot: turn counters, workspace roots, utilization figures and the
slices extensions contribute.

## Discovery instead of catalogs

The prompt does not carry every signature. The model finds capabilities with
`apropos(pattern)`, a regular-expression filter over public symbol names, and
reads one whole contract with `doc(name)`. Documentation pages and skills are
reachable the same way (`doc("gateway")`, `doc("release-checklist")`). Search
never reads document bodies or scores results; it is a literal filter in
manifest order.

## Addresses, not copies

`grep` and `cat` return anchored text: each line is `line:hash│ text`. Those
anchors are the coordinates `patch` edits by, so the search that finds a line
also addresses it, and the read that shows a region is the read the edit
spends. A `patch` call carries every edit for one file in one atomic write,
re-parses the file afterwards and refuses a syntax-breaking batch whole.

Because the edit names an address rather than restating text, the model never
pays to quote back what it replaces. `Path.read_text()` remains available for
files that are only consumed.

## Folding settled work

`fold_session(key, gist)` removes settled steps from future model calls without
deleting anything from the database. A key names turns or iterations:
`"t2"` a whole turn, `"t2/i4-i5"` a range, `"-t3/i9"` everything through a
step, and several keys separated by commas make one fold. The live iteration is
the only thing that cannot be folded.

The gist is what survives on the wire, so a good one is a checkpoint rather
than a transcript: conclusions, open questions, exact paths and symbols, the
state of edits and tests. Folded content remains readable through
`read_session()` when the `introspection` toggle is on; with it off, the gist
is all that remains.

The model watches `session["utilization"]`: `last_request_tokens` against
`auto_compress_above` (200k by default) is the operating pressure, and a
`hint` appears for a few turns once the ceiling is crossed. `saturation` and
`headroom_tokens` are measured against the model's hard input limit instead.

## What this buys

A typical edit costs: one `grep` for the location, one `cat` of the region,
one `patch`, one test run. No whole files, no repeated catalogs, no
intermediate values in the transcript. When a helper recurs across turns it
becomes an extension, so a step that took five calls takes one; see
[Extending Vis](extending.md).

## See also

- [Python sandbox](python-sandbox.md) — the interpreter the model's programs run in.
- [Extending Vis](extending.md) — turning a recurring helper into a tool.
- [Skills](skills.md) — instructions pulled on demand instead of pushed into every request.
