# vis-contract

The language-neutral contract lives in `resources/vis-contract/`. Each JSON
document has a same-named JSON Schema under `resources/vis-contract/schema/` and
is validated with Skjema before Clojure consumes it. The Python wheel packages
these source documents directly.

## Changing the contract

Edit the owning document and schema, then run the focused contract tests. The
project has no dependency on the Vis engine.
