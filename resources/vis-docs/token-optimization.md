# Token optimization

Vis is built around one idea: **don't pay tokens for what you can address by reference.** The context window is an environment the model queries through code, so the expensive things — file contents, large results, history — stay outside the prompt until they're needed, and even then arrive in their cheapest useful form.

## Read structure before bytes

Before reading a file, the agent reads its **index** — a tree-sitter skeleton of the whole file: a `file · language · N lines` header, the file's `imports`/`require` dependencies, then every definition grouped by kind, each with line-ranged, patchable anchors:

```
core.clj · clojure · 524 lines

imports (3):
  clojure.string :as str  @6:1a2
  foo.bar :as bar         @7:9c4

definitions (29):
  constants:
    code-languages           @68:3de..78:8d3
  fn:
    private path-extension   [^String path]  @41:81b..52:39c
    detect-language          [^String path]  @54:a53..66:58d
  structs:
    Point                    @90:4a4..94:11e
```

The index costs a few tokens and tells the model exactly which range to read — instead of paging the whole file into context. The kind is named once as a section header (`fn:`, `constants:`) rather than repeated on every row, and nested definitions (a class's methods) sit under their parent. Every item carries a `<lineno>:<hash>` anchor, so the model can jump straight to an edit — `cat` that one span, or feed the anchor to `struct_patch` — without a second read. Alongside the text skeleton, `index` also returns a structured `definitions`/`imports` list (each row `{:name :kind :visibility :signature :doc :anchor :end-anchor :depth}`) that code can consume directly.

## Edit by name, not by diff

Structural editing targets a definition by name and replaces it in place:

```
struct_patch({"op": "replace", "target": "add", "code": "(defn add [a b c] (+ a b c))"})
```

`replace` · `insert_before` · `insert_after` · `append` · `add_doc` · `replace_doc` · `replace_node` · `rename` — all backed by tree-sitter, all re-parsed and rejected if an edit would break syntax. The model never has to reproduce surrounding lines just to anchor a change.

## Keep big values in vars, not the prompt

The agent writes code; intermediate results bind to vars in the sandbox. A 200 KB query result lives as a var and is summarized, filtered, or piped onward by more code — it never has to round-trip through the token budget. A compact **var index** tells the model what it has defined.

## Fold completed prior turns in place

A fold is **distillation, not deletion** — it drops steps off the **wire** only, never from the database. At the start of a new turn, the agent first understands the new intent, then folds completed earlier-turn work that is no longer relevant:

```
session_fold(["t2/i4"], "http timeout lives at src/vis/net/http.py:52")
```

The current turn is never foldable. Its reproduction output, reads, patch anchors, edits, failures, and verification remain live until the turn completes. The runtime rejects current and future scopes; there is no context-pressure nudge that can interrupt an active dependency chain.

The gist records what the completed work established: the finding, rationale or consequence, full workspace-relative `path:line`, and relevant symbol or test. Include a current `<lineno>:<hash>` anchor when useful, but refresh it before editing because writes make hash anchors stale. What disappears is prior-turn noise: search sweeps, raw dumps, dead ends, and superseded reads.

One fold can target several steps at once — a list, a whole turn (a bare `"tN"`), or a range: `{"through": "tN/iN"}` (up to a step), `{"since": "tN/iN"}` (from a step onward), or `{"from": …, "to": …}` (a window). A wider fold **supersedes** a finer one it already covers, so the ledger never accumulates overlapping breadcrumbs.

### The gist stays where the hunt was

The gist lands **in place** — a `# ⋯ folded <scopes> · saved ~<N>k tokens · ~<P>% of window · context <U>% · <gist>` breadcrumb replaces the step exactly where it collapsed, so the wire it reclaimed — in ~tokens and as a fraction of the model's per-call limit — plus the finding's anchors, are read in context as the model scans back through history. The `~<P>%` is the fold's OWN reduction, not an absolute "how full am I" level: that would baseline on the ever-growing `last_request_tokens` and so RISE across iterations even when the fold helped (fold → tool call → fold → the number climbs). Riding beside it, `context <U>%` DOES report the live window fullness — but straight from the provider's authoritative `saturation` (the same figure `session["utilization"]["now"]` shows), so it reads as a separate, absolute level and can't be mistaken for the fold's own reduction. The breadcrumb is written **once** and never re-transmitted.

### Folded history remains recoverable

A fold is a render-time **intent**, not a delete: it hides completed prior-turn steps from the wire, but the database keeps every one. Any folded **native** tool result is re-fetchable by its id via `ntr[...]`, and a whole past turn is readable via `session_state(id)`.

For the current session, recover turn `N` without dumping the transcript:

```python
state = await session_state()
turn = next(t for t in state["transcript"]["turns"] if t["position"] == N)
```

`turn["iterations"]` contains the original blocks, code, results, errors, and verification output. Pass a session id only when recovering a different conversation.

### One live budget, in the token meter

The only thing re-emitted each turn is a tiny budget line, carried **inside the token meter** — the model reads it in the same place it reads how full the window is:

- `session["utilization"]["now"]` — `saved <C>/<T> (<P>%) · live <scopes>`: how much of the wire is folded away and which scopes remain. `live` is accounting, not permission to fold the current turn. `<T>` counts only scopes still on the wire, so folds that have scrolled off the trailer never inflate the percentage.

## The net effect

Reading is the dominant cost in most agents. By making the agent read a file's index before its bytes, edit by name, and hold state in the environment, Vis spends tokens on decisions — not on re-transcribing the codebase every turn.
