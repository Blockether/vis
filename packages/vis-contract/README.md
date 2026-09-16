# vis-contract

`resources/vis-contract/schema/` contains the language-neutral JSON Schemas.
Skjema validates Clojure payloads; the `vis-agent` SDK ships these same schemas
for Python validation. Runtime readers derive field names, vocabulary and bounds
from schema definitions instead of maintaining separate catalogs.

## Changing a contract

Edit the relevant schema and its consumers, then run the affected contract tests.
Use standard schema keywords for structure, enums, defaults and constraints.
Operational annotations describe only behavior that validation cannot express;
callbacks, IO and mutable state belong in their implementation owners.
This package does not depend on the Vis engine.
