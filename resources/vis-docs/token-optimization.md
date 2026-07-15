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

## The net effect

Reading is the dominant cost in most agents. By making the agent read outlines, edit by name, and hold state in the environment, Vis spends tokens on decisions — not on re-transcribing the codebase every turn.
