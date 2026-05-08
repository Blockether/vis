# Preview Contract

`v/preview` projects large values into concise journal/TUI display.

Rules:

- Raw value remains under `:result`.
- EQL selects display fields.
- Rendering metadata controls human display only.
- Preview never mutates the original value.
- Bind full reads/searches when needed later, then preview selected slices.

Examples:

```clojure
(def file (v/cat "src/foo.clj"))
(v/preview file {:result [[:lines {:from 40 :to 80}]]})
```
