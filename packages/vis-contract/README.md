# vis-contract

The language-neutral contract lives in `resources/vis-contract/`. Each JSON
document has a same-named JSON Schema under `resources/vis-contract/schema/` and
is validated with Skjema before Clojure consumes it. The `vis-agent` Python SDK
bundles these source documents directly; this directory is not a Python distribution.

## Changing the contract

Edit the owning document and schema, then run the focused contract tests. The
project has no dependency on the Vis engine.
