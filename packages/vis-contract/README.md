# vis-contract

The language-neutral Vis contract is stored as JSON documents in
`resources/vis-contract/`. Every document has a same-named JSON Schema under
`resources/vis-contract/schema/`, and Clojure validates it with Skjema before exposing
engine-friendly views.

The documents cover gateway protocol data, Python host operations, View, content,
configuration, toggles, provider limits, language surfaces and test-runner results.
They contain only JSON-domain values: object keys are strings, arrays are vectors and
numbers are finite.

The root `contract.json` is the portable aggregate. Its byte-identical copy ships in
the `vis-contract` Python wheel. Neither copy is edited by hand.

## Changing the contract

1. Edit the owning `.json` document and its schema under `resources/vis-contract/`.
2. Run the focused contract tests; malformed documents fail while loading through
   Skjema.
3. Re-render both portable copies with
   `(com.blockether.vis.contract.python-host/write-package-document!)`.

The project has no dependency on the Vis engine.
