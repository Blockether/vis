You are solving one 4Clojure exercise. Solve it in as few iterations as possible.

## How to deliver the answer

There is a file at `{solution_path}` whose current contents is the sentinel:

```
{sentinel}
```

Replace the sentinel with the source that should textually substitute for
every `__` in the tests (judge does `re-replace #"__" your-source`, then
evaluates). Use `z/patch` with `(z/source "...")` so reader macros
(`'`, `#()`, `@`, `^`) survive into the file:

```clojure
(z/patch [{{:path "solution.edn"
           :search '{sentinel}
           :replace (z/source "YOUR_ANSWER")}}])
```

`YOUR_ANSWER` is raw Clojure source; bytes inside the string land in the
file verbatim. Examples: `(z/source "'(1 2 3 4)")` for a literal list,
`(z/source "#(inc %)")` for a reader-fn, `(z/source ":a :b :c")` for
multiple bare forms substituting one `__`. For a result-value test like
`(= __ (conj '(2 3 4) 1))` write `(z/source "'(1 2 3 4)")` — NOT
`(z/source "(1 2 3 4)")` (that's a call of `1` as a function).

No Markdown fences, prose, comments, `(def ...)`, or test code in the file.

After the patch succeeds, finalize in a **clean** iteration (no extension
tool calls in that iteration):

```clojure
(turn-answer! [:ir [:p "solved"]])
```

## Restrictions

- `slurp` / `spit` / shell commands are banned. Use `v/cat` to read and
  `z/patch` to edit `solution.edn`.
- Respect any "Restricted symbols" list in the problem statement.
- Read the problem statement below carefully; it already contains the tests
  you must satisfy.

## Problem

{problem}
