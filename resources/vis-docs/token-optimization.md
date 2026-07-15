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

Once a step's output has done its job, the agent replaces it with a one-line gist via `session_fold(["tN/iN"], "what it established")` — the whole step (the tool call **and** its output) collapses off the wire, so later requests stop paying for it. A fold can target a list of steps, a whole turn (a bare `"tN"`), or a range (`{"through": "tN/iN"}` / `{"since": "tN/iN"}` / `{"from": …, "to": …}`); a wider fold supersedes a finer one it covers.

The live ledger of what's collapsed rides **inside the token meter** — the model reads it in the same place it reads how full the window is — split by lifespan into two keys so history doesn't re-transmit every turn:

- `session["utilization"]["folds"]` — the **stable** list of surviving folds, `<at>: <gist>` with `at` the compressed scopes (`tN/*` / `tA-tB/*` / `t*` / `tN/i1-i2,i5`). It changes only when a fold lands, so the per-iteration context delta ships the heavy gists once per fold — not on every step.
- `session["utilization"]["now"]` — the **volatile but tiny** `saved <C>/<T> (<P>%) · live <scopes>`: how much of the wire is folded away, and which steps are still live = the next fold candidates. `<T>` counts only scopes still on the wire, so folds that have scrolled off the trailer never inflate the percentage. Scopes and counts only — no gists, no position (the `# tN/iN` step tag already carries that) — so re-emitting it each iteration costs a handful of tokens.

## The net effect

Reading is the dominant cost in most agents. By making the agent read outlines, edit by name, and hold state in the environment, Vis spends tokens on decisions — not on re-transcribing the codebase every turn.
