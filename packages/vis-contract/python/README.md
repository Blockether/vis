# vis-contract

The contract half of [Vis](https://github.com/Blockether/vis): canonical gateway
routes/events, View, content, toggle and provider-limits vocabularies, plus everything a Vis
extension may ask its host for. `vis-agent` depends on this package; so can an SDK generator, linter,
test double or editor that never runs Vis at all.

```bash
pip install vis-contract
```

```python
import vis_contract

vis_contract.GATEWAY["routes"]  # every built-in method/path declaration
vis_contract.VERSION  # the host-operation contract version
vis_contract.OPS["shell"]["outside"]  # "local" — what the op does with no Vis
vis_contract.SHELL["spawn_ops"]  # ["run", "background"]
vis_contract.VIEW["kinds"]  # ["input", "live"] — one lifecycle, two policies
vis_contract.CONTENT["block_types"]  # canonical cross-channel content blocks
vis_contract.CONFIG["api_style_values"]  # accepted provider API styles
vis_contract.TOGGLE["types"]  # portable toggle kinds
vis_contract.PROVIDER["limits"]["statuses"]  # provider report statuses
vis_contract.SURFACE["capabilities"]  # language-tool capabilities
vis_contract.TEST_RUNNER["selector_keys"]  # shared test selectors


class MyHost:
    """A host of your own — a test double, another editor, a CI harness."""

    def state_get(self, key):
        return None

    ...


vis_contract.check_host(MyHost())  # TypeError names every op you did not answer
```

`vis_contract.Host` is a `typing.Protocol`: the calls, their arities and
their doc lines are the document's, and the package's own suite fails when the
protocol and `contract.json` stop agreeing.

The wheel-local `contract.json` is a byte-identical copy of the repository's portable aggregate. Its source documents and JSON Schemas live under [`resources/vis-contract/`](https://github.com/Blockether/vis/tree/main/packages/vis-contract/resources/vis-contract), and Skjema validates every source document before generation.

## License

Apache-2.0.
