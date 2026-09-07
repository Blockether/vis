# vis-contract

`resources/vis-contract/` contains the language-neutral contracts. Each JSON
document has a matching schema under `resources/vis-contract/schema/`. Skjema
validates documents before Clojure uses them. The `vis-agent` Python SDK
includes these documents; this directory is not a Python distribution.

## Changing the contract

Edit the relevant document and schema, then run the affected contract tests.
This package does not depend on the Vis engine.
