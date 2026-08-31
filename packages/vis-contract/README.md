# vis-contract

The Vis extension **contract**, published on its own in both ecosystems:

| what | artifact | reads |
|---|---|---|
| Clojure | `com.blockether/vis-contract` | `resources/vis-contract/*.edn` directly |
| Python | `vis-contract` (PyPI, `python/`) | `contract.json`, rendered from the same EDN |

`resources/vis-contract/python-host.edn` declares every call the `vis` Python
module may make on its host — the polyglot global, arity and outside-host policy.
`resources/vis-contract/clojure-host.edn` is the executable dependency boundary: it
names current source inputs and freezes every forbidden Clojure edge and hand-written
JavaScript/Python wire value by source-file count while consumers move to the SDKs.

Nothing here requires a Vis namespace. The engine reads this project off its own
classpath, `vis-agent` depends on the wheel, and a tool in somebody else's
repository can read the same declaration without installing an agent.

The one part this project does not own is the View vocabulary: the engine's
`internal.view.spec` declares it and hands it to `package-document`, so the
closed vocabulary keeps exactly one definition.

## Changing the contract

1. Edit `resources/vis-contract/python-host.edn` (or `clojure-host.edn`).
2. Re-render the Python document:
   `(com.blockether.vis.contract.python-host/write-package-document!
     (com.blockether.vis.internal.view.spec/contract-vocabulary))`.
3. `python_package_test` and `contract.python-host-test` are what fail when you do not.

Both halves are versioned by the repository's `VIS_VERSION` — one document, one
version, two registries.
