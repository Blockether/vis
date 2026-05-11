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
