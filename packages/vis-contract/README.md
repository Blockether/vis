# vis-contract

The Vis **contract** as one declaration with language-native inputs:

| reader | input | source |
|---|---|---|
| Clojure | `com.blockether/vis-contract` | `resources/vis-contract/*.edn` directly |
| Python | `vis-contract` (PyPI, `python/`) | wheel-local `contract.json` |
| JavaScript generator | root `contract.json` | byte-identical to the Python copy |

`resources/vis-contract/python-host.edn` declares every call the `vis` Python
module may make on its host — the polyglot global, arity and outside-host policy.
`resources/vis-contract/clojure-host.edn` is the executable dependency boundary: it
names current source inputs and freezes every forbidden Clojure edge and hand-written
JavaScript/Python wire value by source-file count while consumers move to the SDKs.
`resources/vis-contract/gateway.edn` owns the built-in route table, protocol headers and
versions, event vocabularies, terminal/queue semantics and replay anchors.

Nothing here requires a Vis namespace. The engine reads this project off its own
classpath, `vis-agent` depends on the wheel, and a tool in somebody else's
repository can read the same declaration without installing an agent.

The one part this project does not own is the View vocabulary: the engine's
`internal.view.spec` declares it and hands it to `package-document`, so the
closed vocabulary keeps exactly one definition.

## Changing the contract

1. Edit the owning document under `resources/vis-contract/`.
2. Re-render the language-neutral and Python copies:
   `(com.blockether.vis.contract.python-host/write-package-document!
     (com.blockether.vis.internal.view.spec/contract-vocabulary))`.
3. The contract, package and gateway characterization tests fail on source or byte drift.

Both published ecosystems are versioned by the repository's `VIS_VERSION`; generated
artifacts consume the same owning declarations rather than maintaining a second vocabulary.
