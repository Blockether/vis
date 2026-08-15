# Token optimization

Vis follows one rule: **do not pay tokens for data that can stay addressable.** Files, large tool results, and history remain in the runtime until needed. The model receives only the smallest useful slice.

## Discover before guessing

The live runtime is the source of truth, and it answers exactly two questions. `apropos(text)` **searches** every document the session can reach — each function's whole contract, every Vis documentation page, every skill's whole `SKILL.md`, every MCP tool's description — and `doc(target)` **retrieves** one of them whole:

```python
apropos("skeleton")      # full text: the word need not be in any name
apropos("wire contract") # ask in words: terms are ORed and ranked by relevance
doc("patch")             # one function's whole contract
doc("gateway")           # a Vis documentation page, by slug
doc("spel")              # a skill, whole — reading it is the whole of using it
doc()                    # the curated index: the verbs a session starts from
```

Rank is relevance, not a filter: BM25 over the handle, the first line and the body, with terms ORed and priced by how rare they are — so a whole question ranks the document that covers most of it, a query that IS a handle wins that handle, and a mistyped name is spell-corrected. `doc` states the raw-result shape for bare sandbox verbs too (`doc("resource_stop")`), which nothing else describes.

Each hit answers a ROW, not the document: `{kind, gist, at, hit}` — what it IS (`tool` · `shim` · `page` · `skill` · `mcp` · `local`), a bounded excerpt built from the document's opening, the region your terms landed in and a fragment from deeper down, the LINE that region starts on, and the terms that matched (a correction shows as `pathc→patch`). The body is never in a search result — `doc(name)` answers one whole, and `at` says where to start reading it.

Use them before inventing a name or a call shape. They read the live registry and the live document corpus, so an extension appears the moment it binds and a copied catalog cannot go stale.

## Read structure before bytes

For supported source, `struct_index` returns imports and a tree-sitter definition skeleton before any body is read:

```text
core.clj · clojure · 524 lines

imports (2):
  clojure.string :as str  @6

definitions (29):
  constants:
    code-languages           @68..78
  fn:
    private path-extension   [^String path]  @41..52
    detect-language          [^String path]  @54..66
```

Each row carries plain 1-based `line`/`end_line` numbers and the matching `anchor`/`end_anchor`, so a definition is already a `patch` argument. Read that one definition with `struct_nodes` at its `line` instead of paging the file. In Python, the structured `definitions` and `imports` values can stay bound while the model prints only the rows it needs.

For unsupported text, generated files, or one already-known region, `cat(path, start, end)` returns that window as `line:hash│ text` — one anchored line per source line, so the read that shows you the region also ADDRESSES it. A negative endpoint counts from the end (`cat(path, -50)` is the tail 50 lines, `cat(path, -50, -30)` the window between them). `Path(path).read_text()` is for a file you only consume, never for one you are about to edit.

## Edit structure, not surrounding text

Two editors, two coordinates. Edit by NAME with `struct_patch` for supported code: it re-parses the file and refuses syntax-breaking writes. Edit by ADDRESS with `patch` — prose, config, comments, docstrings, and every unsupported language — spending an anchor `cat` or `grep` already produced.

A definition from `struct_index` can be edited by name:

```python
await struct_patch({
    "path": "src/core.clj",
    "op": "replace",
    "target": "add",
    "code": "(defn add [a b c] (+ a b c))",
})
```

When a definition is too coarse, enter it at a `struct_index` line, navigate with `struct_nodes` — which hands back each node's verbatim `source` PLUS its zipper cursor `at` — then pass that `at` to `struct_patch`. `nodes` is always a list, so many cursors (across many files) ride one call:

```python
nodes = await struct_nodes({
    "path": "src/core.clj",
    "nodes": [{"line": 42, "nav": [{"find": "(+ a b)"}]}],
})
node = nodes["results"][0]   # node["source"] is the code, node["at"] the cursor
await struct_patch({
    "path": "src/core.clj",
    "op": "replace_node",
    "at": node["at"],
    "code": "(* a b)",
})
```

The same editor supports named-definition moves, docs, nested child insertion, and unique sub-expression replacement. For a project-wide rename, first `grep` the identifier, then pass its candidate file paths to `struct_index({"paths": [...], "include_occurrences": True})` to inspect declarations and occurrence blast radius, then rename every one of them in ONE `struct_patch` batch — top-level keys are the shared defaults for each entry: `struct_patch({"op": "rename", "target": "handle_click", "code": "handle_tap", "edits": [{"path": path} for path in paths]})`.

For prose, unsupported code, or one known region, spend the anchor instead of restating the text you are replacing:

```python
print(patch(path, [{"from": "4439:a80", "replace": '     :result "One row/edit: `path`, `op`, `changed`."'}]))
patched src/…/editing/core.clj  1 edit  5182 → 5182 lines  parse: clean
  1  4439..4439  → 1 line  4439:b16
```

`patch(path, edits)` carries EVERY edit for that file in ONE atomic write — `[{"from": a, "to": b, "replace": text}]`, `to` defaults to `from`, `replace: ""` deletes — and the answer is one FRESH anchor per edit, so a follow-up edit needs no second `cat`. The edits may be listed in any order (they all resolve against the one read) and two edits over the same line are refused. Only a new file or a genuine wholesale replacement is a `python_execution` write (`Path.write_text`) — the same filesystem gate applies.

## Keep intermediate data in Python

Every capability is a Python function, so one operation and a batch of fifty cost the same one call: raw results stay in Python vars and only explicit `print()` output reaches context. Run independent calls concurrently with `await gather(...)`; keep dependent chains sequential.

Printing is the whole cost model, and it cuts both ways. An unprinted value costs no context — and it is also gone once the block ends, because nothing stores a result the next block could re-read. Print the slice the answer needs, then keep working from the variable while the block is still running.

```python
index, todos = await gather(
    struct_index({"paths": paths}),
    grep({"query": "TODO", "paths": paths}),
)
hits = [d for r in index["results"] for d in r["definitions"]]
print(len(hits), todos.splitlines()[0])   # grep answers TEXT; line 1 is its summary
```

This turns many reads plus a reduction into one visible result instead of one transcript entry per intermediate value.

## Write a program, not a transcript

The sandbox keeps state between blocks, so treat it as one program the session is building rather than a run of disposable snippets.

Bind the roots once and derive every path from them. The workspace root is already in `session`, so no block ever has to retype an absolute path:

```python
root = Path(session["workspace"]["root"])
src, tests = root / "src", root / "test"
```

Write a small helper the first time a shape repeats, then CALL it from every later block — a function defined in an earlier block is still bound, and redefining it is a paste the context pays for twice:

```python
def hits(needles, *paths, ctx=0):
    """Anchored `line:hash│ text` rows only, without the per-path headers."""
    text = grep({"query": needles, "paths": list(paths), "context": ctx})
    return [l for l in text.splitlines() if "│ " in l]
```

A definition lives as long as the **session**, and now longer than the **process**: a helper written in turn 2 is still callable in turn 9, and a gateway restart re-creates it in the fresh sandbox from a snapshot the host writes after every block (module aliases, scalar constants and the definitions themselves — never a previous block's side effects). `defs()` is the whole inventory; `defs(name)` hands the source back so a helper is refined instead of re-typed:

```python
print(defs())            # 2 definitions in this sandbox
                         #   hits(needles, *paths, ctx=0)  <prog:7>  4 lines
                         #   deploy_ok(env)                <prog:9> (restored)  6 lines
print(defs("hits"))      # its source — edit THAT, never re-paste from memory
```

What does **not** persist is anything written into `session`. That map is host-owned and rebuilt from the engine snapshot before every block, so `session["helpers"] = …` succeeds and is gone by the next one — a silent loss, not an error. Keep state in ordinary names — and give a helper its OWN name: a top-level `def cat(...)` or `class grep:` named after a bound tool is refused where it is written, because that definition could only ever shadow the tool inside its own block and would never be persisted or restored.

When the same helper survives across turns — a deploy check, a fixture loader, a project-specific guard — it has outgrown the sandbox. Propose a **Python extension**: one file in `.vis/extensions/*.py` registers a named tool for every future session in that project, `vis-agent extension check` validates it, and `doc("extending")` is the whole recipe — including the durable `state` an extension owns by NAME, which is the only storage that survives `/reload` and a restart. Propose it; write it when the user asks.

## Fold settled steps

`fold_session(key, gist)` removes **settled wire steps** from future model calls; it does not delete database history. Settled means every completed prior turn AND the current turn's already-finished iterations. At the start of a new turn, understand the new request first, then fold earlier work that no longer needs raw detail:

```python
fold_session("t2/i4-i5", "HTTP timeout fixed in src/vis/net/http.clj:52; regression test passes")
```

The only step the runtime refuses to fold is the **live iteration you are emitting right now** (and any future step) — it is not settled yet. Every completed iteration is foldable, including finished iterations of the current turn: trim the current turn up to the last settled iteration with `"-tN/iK"`. A blocked attempt names only the live scope, so drop it and keep the settled ones. Keep active reproduction output, reads, edits, failures, and verification live until they settle.

A useful gist records the durable finding, rationale or consequence, and a workspace-relative `path:line`, symbol, or test. Omit the gist when the folded steps contain no reusable information. Re-read any preserved line number before editing, because a write moves the lines under it.

The key is a **string**: `"t2/i5"` one step, `"t2"` a whole turn, `"t2/i1-i56"` (or `"t2/i1..i56"`) a range, `"-t2/i56"` everything through it, `"t2/i5-"` everything since it — comma-separate several, and a list of key strings works too. A token that is not a step key, a second range, or a key matching no settled step is refused by name rather than folding nothing. A broader newer fold supersedes every fully covered narrower breadcrumb; equal scopes keep the newer gist. Partial overlaps remain separate.

### Folding changes rendering, not storage

A folded step is not re-readable inline, and there is no destructive `unfold` command:

- Current conversation: `s = await read_session()`, select `s["transcript"]["turns"]` by numeric `position`, then filter `['iterations'][...]['blocks']` for the raw code/results.
- Another conversation: `await list_sessions(search="…")` — the same ranked search the TUI and the companion app run — to find its id, then `await read_session(id)` and filter the same path. `get_session(id)` answers ONE descriptor row when the id is all you need.

This recovers evidence without restoring it to the model wire. Filter in `python_execution`; never dump a full transcript back into context.

`read_session`, `get_session` and `list_sessions` are bound only while the `introspection` toggle is ON (default OFF — enable it in `vis.yml` under `toggles:` or from the settings dialog). Compact token/tool/provider diagnostics live at `read_session()["usage"]`. With introspection OFF a folded step is not recoverable at all — the gist is what survives, so write one that carries the finding.

### The budget stays visible

The fold breadcrumb shows the reclaimed scope and a token estimate. Its `~N% of budget` measures that reclaim against the **operating ceiling** — `auto_compress_above` (the 200k soft compaction guardrail), or the live handled context (`last_request_tokens`) once a bigger task has grown past it — not the hard per-call max, so a working fold never reads as noise. `session["utilization"]["now"]` reports total saved context and the scopes still represented on the wire.

That operating ceiling is the number to act on. `saturation` and `headroom_tokens` are priced against `model_input_limit`, the HARD per-call ceiling: on a 1M-window model, 150k of handled context reads as `saturation 15%` with 850k headroom while `over-budget-hint` is already saying `FOLD SOON`. The live pressure is `last_request_tokens` against `auto_compress_above`.

When handled context climbs above the ceiling, `session["utilization"]["hint"]` adds one throttled nudge to fold settled turns. It fires for at most three turns from the crossing, then goes quiet; dropping back under the ceiling re-arms a fresh window for the next crossing.

## The net effect

The efficient path is:

1. Discover capabilities with `apropos`, then read the one contract with `doc`.
2. Locate relevant files and symbols with `grep` — it answers one anchored TEXT block, never a map, so a hit is already a `patch` argument.
3. Map supported code with `struct_index`, then read only the needed body with `struct_nodes`; `cat(path, start, end)` for anything else you are about to edit.
4. Edit by NAME with `struct_patch`, by ADDRESS with `patch`; fall back to a `python_execution` write only for a new file or a wholesale replacement.
5. Keep batch intermediates in `python_execution`.
6. Fold completed prior-turn noise while preserving durable evidence.

Vis spends context on decisions and proof, not repeated catalogs, whole files, intermediate results, or dead history.
