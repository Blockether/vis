# Token optimization

Vis is built around one idea: **don't pay tokens for what you can address by reference.** The context window is an environment the model queries through code, so the expensive things — file contents, large results, history — stay outside the prompt until they're needed, and even then arrive in their cheapest useful form.

## Read structure before bytes

Before reading a file, the agent reads its **outline** — a tree-sitter skeleton of every definition with line-ranged, patchable anchors:

```
namespace foo.bar   @1:571..1:571
function add        @5:89f..5:89f
struct Point        @7:4a4..7:4a4
protocol Shape      @9:f90..9:f90
```

The outline costs a few tokens and tells the model exactly which range to read — instead of paging the whole file into context. Every item carries a `<lineno>:<hash>` anchor, so the model can jump straight to an edit without a second read.

## Edit by name, not by diff

Structural editing targets a definition by name and replaces it in place:

```
struct_patch({"op": "replace", "target": "add", "code": "(defn add [a b c] (+ a b c))"})
```

`replace` · `insert_before` · `insert_after` · `append` · `add_doc` · `replace_doc` · `replace_node` · `rename` — all backed by tree-sitter, all re-parsed and rejected if an edit would break syntax. The model never has to reproduce surrounding lines just to anchor a change.

## Keep big values in vars, not the prompt

The agent writes code; intermediate results bind to vars in the sandbox. A 200 KB query result lives as a var and is summarized, filtered, or piped onward by more code — it never has to round-trip through the token budget. A compact **var index** tells the model what it has defined.

## Fold spent history in place

A fold is **distillation, not deletion** — it drops steps off the **wire** only, never from the database, so it is always safe and always recoverable. Once a step's output has served its purpose, the agent replaces it with a one-line gist:

```
session_fold(["t2/i4"], "http timeout lives at http.py:52")
```

The whole step — the tool call **and** its output — collapses off the wire, so later requests stop paying for it. The gist is the fold's **rationale**: what the steps established and why the hunt is now safe to drop — the decision, the number, and the `file:line` **anchor** that makes the fact addressable later, so a reader scanning back sees *why* those steps went away. What it drops is the **noise**: search sweeps, raw dumps, dead ends, superseded reads.

One fold can target several steps at once — a list, a whole turn (a bare `"tN"`), or a range: `{"through": "tN/iN"}` (up to a step), `{"since": "tN/iN"}` (from a step onward), or `{"from": …, "to": …}` (a window). A wider fold **supersedes** a finer one it already covers, so the ledger never accumulates overlapping breadcrumbs.

### The gist stays where the hunt was

The gist lands **in place** — a `# ⋯ folded <scopes> · <gist>` breadcrumb replaces the step exactly where it collapsed, so the finding (with its anchors) is read in context as the model scans back through history. It is written **once** and never re-transmitted.

### Folding never loses anything

A fold is a render-time **intent**, not a delete: it hides steps from the wire, but the database keeps every one. Introspection stays available at all times — any folded **native** tool result is re-fetchable by its id via `ntr[...]` (this turn or a past turn, even after a restart clears the sandbox vars), and a whole past turn is readable via `session_state(id)`. Because a fold is reversible, the agent can collapse aggressively and re-fetch the rare detail it later needs.

### One live budget, in the token meter

The only thing re-emitted each turn is a tiny budget line, carried **inside the token meter** — the model reads it in the same place it reads how full the window is:

- `session["utilization"]["now"]` — `saved <C>/<T> (<P>%) · live <scopes>`: how much of the wire is folded away, and which steps are still live = the next fold candidates (compressed `tN/*` / `tA-tB/*` / `t*` / `tN/i1-i2,i5`). `<T>` counts only scopes still on the wire, so folds that have scrolled off the trailer never inflate the percentage. Scopes and counts only — no gists (those live once, in the breadcrumbs), no position (the `# tN/iN` step tag already carries it) — so re-emitting it each iteration costs a handful of tokens.

## The net effect

Reading is the dominant cost in most agents. By making the agent read outlines, edit by name, and hold state in the environment, Vis spends tokens on decisions — not on re-transcribing the codebase every turn.
