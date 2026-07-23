# Token optimization

Vis follows one rule: **do not pay tokens for data that can stay addressable.** Files, large tool results, and history remain in the runtime until needed. The model receives only the smallest useful slice.

## Discover before guessing

The live runtime is the source of truth. `apropos` finds capabilities by name; `doc` returns one exact contract:

```python
apropos("struct")
doc("struct_patch")
```

Use them before inventing a tool name or call shape. They read the same live registry that generates native tool descriptions, so extensions appear immediately and copied catalogs cannot go stale.

## Read structure before bytes

For supported source, `struct_index` returns imports and a tree-sitter definition skeleton before any body is read:

```text
core.clj · clojure · 524 lines

imports (2):
  clojure.string :as str  @6:1a2

definitions (29):
  constants:
    code-languages           @68:3de..78:8d3
  fn:
    private path-extension   [^String path]  @41:81b..52:39c
    detect-language          [^String path]  @54:a53..66:58d
```

Each row has fresh `<line>:<hash>` start/end anchors. Read one definition with `cat` using that span instead of paging the file. In Python, the structured `definitions` and `imports` values can stay bound while the model prints only the rows it needs.

Use `cat` directly for unsupported text, generated files, or one already-known region. Every write invalidates old anchors.

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

When a definition is too coarse, enter it with a `struct_index` anchor, navigate with `struct_node`, then pass the returned `path` to `struct_patch` as `at`:

```python
node = await struct_node("src/core.clj", {
    "anchor": "42:abc",
    "nav": [{"find": "(+ a b)"}],
})
await struct_patch({
    "path": "src/core.clj",
    "op": "replace_node",
    "at": node["path"],
    "code": "(* a b)",
})
```

The same editor supports named-definition moves, docs, nested child insertion, and unique sub-expression replacement. For a project-wide rename, inspect `struct_occurrences` first, then use `struct_rename`.

Use anchored `patch` for prose or unsupported code. Use `write` only to create a file or intentionally replace a clean whole file.

## Keep intermediate data in Python

Use a native call for one operation. Use `python_execution` for batches, filters, or transforms: raw results stay in Python vars and only explicit `print()` output reaches context. Run independent calls concurrently with `await gather(...)`; keep dependent chains sequential.

```python
rows = await gather(*(cat(path) for path in paths))
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

The only step the runtime refuses to fold is the **live iteration you are emitting right now** (and any future step) — it is not settled yet. Every completed iteration is foldable, including finished iterations of the current turn: trim the current turn up to the last settled iteration with `{"through": "tN/iK"}`. A blocked attempt names only the live scope, so drop it and keep the settled ones. Keep active reproduction output, reads, anchors, edits, failures, and verification live until they settle.

A useful gist records the durable finding, rationale or consequence, and a workspace-relative `path:line`, symbol, or test. Omit the gist when the folded steps contain no reusable information. Refresh any preserved hash anchor before editing because writes make anchors stale.

Targets may be step ids, whole prior turns, or `through` / `from`+`to` / `since` ranges. A broader newer fold supersedes every fully covered narrower breadcrumb; equal scopes keep the newer gist. Partial overlaps remain separate.

### Recovery is lossless

Folding changes rendering, not storage. There is no destructive `unfold` command:

- Current conversation: `s = await session_state()`, select `s["transcript"]["turns"]` by numeric `position`, then filter `['iterations'][...]['blocks']` for the raw code/results.
- One folded native result: `ntr[tool_id]`.
- Another conversation: `await sessions()` to find its id, then `await session_state(id)` and filter the same path.

This recovers evidence without restoring it to the model wire. Filter in `python_execution`; never dump a full transcript back into context.

### The budget stays visible

The fold breadcrumb shows the reclaimed scope and token estimate in place. `session["utilization"]["now"]` reports total saved context and the scopes still represented on the wire.

## The net effect

The efficient path is:

1. Discover with `apropos` / `doc`.
2. Inspect with `struct_index`, then read one body only when needed.
3. Edit with `struct_patch`; fall back to anchored `patch` only when structure is unavailable.
4. Keep batch intermediates in `python_execution`.
5. Fold completed prior-turn noise while preserving durable evidence.

Vis spends context on decisions and proof, not repeated catalogs, whole files, intermediate results, or dead history.
