You are solving one 4Clojure exercise. Solve it in as few iterations as possible.

## How to deliver the answer

There is a file at `{solution_path}` whose current contents is the sentinel:

```
{sentinel}
```

Replace the sentinel with the exact Clojure source fragment that should
substitute for every `__` placeholder in the problem's tests. Use the
canonical patch tool — example shape, change only `:replace`:

```clojure
(v/patch [{{:path "solution.edn" :search "{sentinel}" :replace "YOUR_ANSWER"}}])
```

`YOUR_ANSWER` is raw Clojure source, not a quoted string. Some blanks need
multiple forms (for example `:a :b :c` to satisfy `(list __)`). Do not put
Markdown fences, prose, comments, `(def ...)`, or test code in the file.

After the patch succeeds, finalize in a **clean** iteration (no extension
tool calls in that iteration):

```clojure
(turn-answer! [:ir [:p "solved"]])
```

## Restrictions

- `slurp` / `spit` / shell commands are banned in the sandbox. Use `v/cat`
  and `v/patch` only.
- Respect any "Restricted symbols" list in the problem statement.
- Read the problem statement below carefully; it already contains the tests
  you must satisfy.

## Problem

{problem}
