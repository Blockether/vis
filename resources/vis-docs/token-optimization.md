# Token optimization

Vis follows one rule: **do not pay tokens for data that can stay addressable.** Files, large tool results, and history remain in the runtime until needed. The model receives only the smallest useful slice.

## Discover before guessing

The live runtime is the source of truth, and it answers exactly two questions. `apropos(text)` **searches** every document the session can reach — each function's whole contract, every Vis documentation page, every skill's whole `SKILL.md`, every MCP tool's description — and `doc(target)` **retrieves** one of them whole:

```python
apropos("skeleton")      # full text: the word need not be in any name
apropos("wire contract") # terms are ANDed, hits come back ranked
doc("struct_patch")      # one function's whole contract
doc("gateway")           # a Vis documentation page, by slug
doc("spel")              # a skill, whole — reading it does not activate it
doc()                    # the curated index: the verbs a session starts from
```

Rank is the answer to "where did it match": an exact name outranks a name substring, which outranks the first line, which outranks the body. `doc` states the raw-result shape for bare sandbox verbs too (`doc("resource_stop")`), which nothing else describes.

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

Each row carries plain 1-based `line`/`end_line` numbers. Read that one definition with `struct_nodes` at its `line` instead of paging the file. In Python, the structured `definitions` and `imports` values can stay bound while the model prints only the rows it needs.

For unsupported text, generated files, or one already-known region, read the bytes in `python_execution` (`Path(path).read_text()`) and print only the slice you need.

## Edit structure, not surrounding text

Prefer `struct_patch` for supported code. It re-parses the file and refuses syntax-breaking writes.

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

The same editor supports named-definition moves, docs, nested child insertion, and unique sub-expression replacement. For a project-wide rename, first `grep` the identifier, then pass its candidate file paths to `struct_index({"paths": [...], "include_occurrences": true})` to inspect declarations and occurrence blast radius before calling `struct_patch({"paths": ["."], "op": "rename", "target": "handle_click", "code": "handle_tap"}).

For prose, unsupported code, a new file, or a wholesale replacement, write from `python_execution` (`Path.write_text`) — the same filesystem gate applies.

## Keep intermediate data in Python

Every capability is a Python function, so one operation and a batch of fifty cost the same one call: raw results stay in Python vars and only explicit `print()` output reaches context. Run independent calls concurrently with `await gather(...)`; keep dependent chains sequential.

Printing is the whole cost model, and it cuts both ways. An unprinted value costs no context — and it is also gone once the block ends, because nothing stores a result the next block could re-read. Print the slice the answer needs, then keep working from the variable while the block is still running.

```python
rows = await gather(*(struct_index({"path": path}) for path in paths))
hits = [row for row in rows if "TODO" in json.dumps(row)]
print(hits[:3])
```

This turns many reads plus a reduction into one visible result instead of one transcript entry per intermediate value.

## Fold settled steps

`session_fold` removes **settled wire steps** from future model calls; it does not delete database history. Settled means every completed prior turn AND the current turn's already-finished iterations. At the start of a new turn, understand the new request first, then fold earlier work that no longer needs raw detail:

```python
session_fold(
    ["t2/i4", "t2/i5"],
    "HTTP timeout fixed in src/vis/net/http.clj:52; regression test passes",
)
```

The only step the runtime refuses to fold is the **live iteration you are emitting right now** (and any future step) — it is not settled yet. Every completed iteration is foldable, including finished iterations of the current turn: trim the current turn up to the last settled iteration with `{"through": "tN/iK"}`. A blocked attempt names only the live scope, so drop it and keep the settled ones. Keep active reproduction output, reads, edits, failures, and verification live until they settle.

A useful gist records the durable finding, rationale or consequence, and a workspace-relative `path:line`, symbol, or test. Omit the gist when the folded steps contain no reusable information. Re-read any preserved line number before editing, because a write moves the lines under it.

Targets may be step ids, whole prior turns, or `through` / `from`+`to` / `since` ranges. A broader newer fold supersedes every fully covered narrower breadcrumb; equal scopes keep the newer gist. Partial overlaps remain separate.

### Folding changes rendering, not storage

A folded step is not re-readable inline, and there is no destructive `unfold` command:

- Current conversation: `s = await session_state()`, select `s["transcript"]["turns"]` by numeric `position`, then filter `['iterations'][...]['blocks']` for the raw code/results.
- Another conversation: `await sessions()` to find its id, then `await session_state(id)` and filter the same path.

This recovers evidence without restoring it to the model wire. Filter in `python_execution`; never dump a full transcript back into context.

`session_state` and `sessions` are bound only while the `introspection` toggle is ON (default OFF — enable it in `vis.yml` under `toggles:` or from the settings dialog). Compact token/tool/provider diagnostics live at `session_state()["usage"]`. With introspection OFF a folded step is not recoverable at all — the gist is what survives, so write one that carries the finding.

### The budget stays visible

The fold breadcrumb shows the reclaimed scope and a token estimate. Its `~N% of budget` measures that reclaim against the **operating ceiling** — `auto_compress_above` (the ~144k soft compaction guardrail), or the live handled context (`last_request_tokens`) once a bigger task has grown past it — not the 1M hard per-call max, so a working fold never reads as noise. `session["utilization"]["now"]` reports total saved context and the scopes still represented on the wire.

When handled context climbs above the ceiling, `session["utilization"]["hint"]` adds one throttled nudge to fold settled turns. It fires for at most three turns from the crossing, then goes quiet; dropping back under the ceiling re-arms a fresh window for the next crossing.

## The net effect

The efficient path is:

1. Discover capabilities with `apropos`, then read the one contract with `doc`.
2. Locate relevant files and symbols with `grep`.
3. Map supported code with `struct_index`, then read only the needed body with `struct_nodes`.
4. Edit with `struct_patch`; fall back to a `python_execution` write only when structure is unavailable.
5. Keep batch intermediates in `python_execution`.
6. Fold completed prior-turn noise while preserving durable evidence.

Vis spends context on decisions and proof, not repeated catalogs, whole files, intermediate results, or dead history.
