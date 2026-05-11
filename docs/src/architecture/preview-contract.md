# Result Payload Contract

Current contract:

- Tool payload lives under `:op/result`.
- Journal/TUI renderers show bounded previews automatically.
- Bind full reads/searches once, then inspect selected slices with plain Clojure.
- Read/search helpers do not mutate the original payload.

Example:

```clojure
(def file (v/cat "src/foo.clj"))
(subvec (get-in file [:op/result :lines]) 40 80)
```

Anti-patterns that caused conversation `ac0da8ae` failures:

```clojure
(get-in file [:result :lines]) ; wrong: nil path
(subvec (get-in file [:result :lines]) 40 80) ; nil -> subvec crash
(get-in lines) ; wrong: get-in needs a key path; slice bound vectors directly
```

Recovery rule: if an answer-alone preflight rejects an iteration, rerun the rejected observation forms without `(answer ...)`, observe success, then answer in a separate answer-only iteration.
