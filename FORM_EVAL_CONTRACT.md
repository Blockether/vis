# Form-eval contract — examples & requirements

Spec-first. This file defines **what we expect to see** when the model's reply
is turned into evaluated forms. Solutions come *after* we agree on these
examples. Every "verified" row was run live against `run-python-block` on the
dev nREPL (GraalPy 25.0.1) on 2026-06-08.

## Background — the two problems (from session `6b80d28e`)

The model asked to restyle the header UUID. It emitted **valid Python**:

```python
# Let me read the full header.clj file to understand the current layout and styling.

header_full = cat("…/header.clj")
header_full
```

…but what got evaluated (DB `code` column) was shattered one-token-per-line with
the first two tokens dropped (`# Let` gone):

```
me
read
the
…
header_full
=
cat
("…/header.clj")
```

→ `NameError` soup, the model never got the file, retried the same read 3× until
interrupted. Two underlying issues, in the user's framing:

1. **Prompt / defs.** What a *form* is, and whether the model should bind defs at
   all. SCI-era rule was "no defs — tools land in the trailer, `context` is a
   variable, so read state back from there." A form should be **like a REPL form:
   something that returns a value.**
2. **AST parsing of forms.** Splitting the reply into top-level forms must be a
   real parse, not a whitespace tokenizer. Question raised: *can GraalPy/Truffle
   give us AST parsing?*

### Status of each (verified this session)

- **AST parsing: already solved & live.** `env-python/run-python-block`
  (`loop.clj:611`) splits via CPython's `ast` running under GraalPy
  (`ast.parse(src).body`, classified per node), evals per form, stops at first
  error. `render/parse-block-display` is now **verbatim** (the old Clojure-reader
  that mangled `ls(".")` is gone). The `6b80d28e` garble was an **older build**.
- **GraalPy `ast`: yes, and we already use it.** `env-python` calls
  `ast.parse(...).body` (form counting, ban-walk, per-form split). Proven live.
- **Open: prompt/defs style** (see Requirements R7–R8) and **upstream extraction**
  (see R9) remain to decide/verify.

## The model — REPL form semantics

The whole reply is one Python program. Parse it with `ast` into top-level
statements (`Module.body`). Evaluate each in order in the persistent namespace.
Capture a **value** per form the way a REPL echoes:

| Node kind | What it yields as the form result |
|---|---|
| `Expr` (bare expression, e.g. `hits`, `cat(...)`, `a, b`) | the expression's value |
| `Assign` `x = …` | the bound value of `x` |
| `FunctionDef` / `ClassDef` | the defined object |
| other statement (`import`, `if`, `for`, …) | `None` |
| comment / blank line | nothing — not a node |

The **turn result** is the last form's value. Tools fire in order regardless
(their cards render from the side effect). Stop at the first form that errors.

## Examples (golden cases)

Notation: input is the *raw reply*; "forms" is what we expect after AST split;
"result" is the turn result; "error" is the engine error shape (or none).

### E1 — comment + assign + bare expr  ✅ verified
```python
# Let me read the full header.clj file
header_full = cat("a/b.clj")
header_full
```
- forms: `[assign header_full→<contents>, expr header_full→<contents>]`
- result: `<contents of a/b.clj>`; error: none
- **Requirement:** the comment is dropped; **no whitespace mangling**; the bare
  `header_full` echoes the value.

### E2 — no-defs, single value-returning form  ✅ verified
```python
cat("a/b.clj")
```
- forms: `[expr cat(...)→<contents>]`; result: `<contents>`; error: none
- **Requirement:** a bare tool call is a complete form that returns a value —
  the model never needs `x = cat(...)` then `x`. (Backs the "no defs" style.)

### E3 — multiple reads + tuple echo  ✅ verified
```python
a = cat("x.clj")
b = cat("y.clj")
a, b
```
- forms: 3; result: `["<x>" "<y>"]`; error: none
- **Requirement:** batched reads each fire; the trailing tuple echoes both.

### E4 — bare prose then code (weak-model failure)  ✅ verified
```python
Let me read the header file.
header = cat("a.clj")
header
```
- result: `nil`; error: `{:phase :python/syntax :line 1 :column 5}`
- **Requirement:** prose-leading replies are **rejected with a clean
  SyntaxError pointing at the prose**, NOT mangled. The error message must be
  fed back so the model fixes it (→ ties to prompt rule "CODE ONLY; thinking in
  `#` comments").

### E5 — whitespace-shattered source (the old bug)  ✅ verified
```
me
read
the
header_full
=
cat
("a/b.clj")
```
- result: `nil`; error: `{:phase :python/syntax :line 5}`
- **Requirement:** never produced by us anymore (parse-block-display is
  verbatim); if such input ever arrives it must SyntaxError, never silently run.

### E6 — done() only
```python
done("""All set.""")
```
- forms: 1 (`expr`); the `done` side effect finalizes the turn.
- **Requirement:** `done(...)` is a normal value-returning form; finalize works.

### E7 — runtime error mid-block (stop-at-first-error)
```python
x = cat("a.clj")
boom()            # NameError
y = cat("b.clj")  # must NOT run
```
- forms: `[assign x→<contents>, expr boom→ERROR]`; `y` never evaluated.
- result: `nil`; error: `{:phase :python/runtime …}`
- **Requirement:** evaluation halts at the first failing form; later forms do
  not run; the error carries phase + (where available) line/column.

## Requirements (testable)

- **R1** Forms are obtained by `ast.parse(...).body`, never by string/whitespace
  splitting. (E1, E5)
- **R2** Comments and blank lines never become forms. (E1)
- **R3** A bare expression echoes its value; `x = …` echoes `x`; def/class echo
  the object; other statements yield `None`. (E1–E3, E6)
- **R4** The turn result is the **last** form's value. (E1, E3)
- **R5** Source is preserved **codepoint-exactly** through the split (no
  reformatting; emoji-safe — already handled via line/col slicing). 
- **R6** Any parse failure returns ONE clean `:python/syntax` error with
  line/column and is fed back to the model; never mangle or silently drop. (E4,E5)
- **R7** Evaluation stops at the first erroring form; subsequent forms do not
  run. (E7)
- **R8 (DECIDED ✅ — prefer value-returning forms)** Prompt now instructs: call
  tools as bare value-returning forms (`cat("x")`, not `data = cat("x")`); reserve
  `x = …` for reuse within the same reply; durable state goes through the verbs
  (`task_set`/`fact_set`), the trailer, and `context` — not locals. Applied to
  CORE ("WRITE VALUE-RETURNING FORMS" bullet + few-shot + Discipline) and the
  weak-model rules (new rule 7). Engine already supports it (E2). 
- **R9 (VERIFIED ✅ — svar 0.7.9 is verbatim)** `com.blockether.svar` is pinned to
  **0.7.9** (commit `97af2a51` "verbatim multi-part message body"). Live test of
  `codes/lenient-block`: intended source returned **byte-for-byte equal**; one
  outer ```` ```fence ```` stripped; interior fences inside a triple-quoted string
  **preserved**. Pipeline is clean end-to-end:
  `reply → lenient-block (verbatim) → parse-block-display (verbatim) →
  run-python-block (ast per-form)`. The `6b80d28e` garble was the OLD build
  (Clojure-reader `parse-block-display` + pre-0.7.9 svar) and cannot recur.

## Status

- R8 ✅ done (prompt updated). R9 ✅ verified (svar 0.7.9 verbatim).
- AST form-eval ✅ already shipped in `run-python-block` (live-proven E1–E5).

## Remaining (optional)

- **Tests.** E1–E7 map 1:1 onto `run-python-block` lazytest cases. Commit as
  `env_python_form_eval_test.clj` to lock the contract against regression?
- **E7 not yet run live** (stop-at-first-error) — covered by the loop code path
  but worth a golden test.
