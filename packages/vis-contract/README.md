# vis-contract

The Vis extension **contract**, published on its own in both ecosystems:

| what | artifact | reads |
|---|---|---|
| Clojure | `com.blockether/vis-contract` | `resources/vis-contract/*.edn` directly |
| Python | `vis-contract` (PyPI, `python/`) | `contract.json`, rendered from the same EDN |

`resources/vis-contract/python-host.edn` declares every call the `vis` Python
module may make on its host — the polyglot global the engine binds, the arity, and
what the op does when there is no Vis in the room. `resources/vis-contract/clojure-host.edn`
is its half for Clojure extensions: the facade they may require, and the frozen
list of internal namespaces that already reach past it.

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
