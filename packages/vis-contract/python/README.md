# vis-contract

The contract half of [Vis](https://github.com/Blockether/vis): canonical gateway
routes/events, View, Activity, content, toggle and provider-limits vocabularies, plus everything a Vis
extension may ask its host for. `vis-agent` depends on this package; so can an SDK generator, linter,
test double or editor that never runs Vis at all.

```bash
pip install vis-contract
```

```python
from blockether import vis_contract

vis_contract.GATEWAY["routes"]  # every built-in method/path declaration
vis_contract.VERSION  # the host-operation contract version
vis_contract.OPS["shell"]["outside"]  # "local" — what the op does with no Vis
vis_contract.SHELL["spawn_ops"]  # ["run", "background"]
vis_contract.VIEW["kinds"]  # ["input", "live"] — one lifecycle, two policies
vis_contract.ACTIVITY["states"]  # engine-owned execution lifecycle
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

`vis_contract.Host` is a `typing.Protocol` checked against the canonical documents.
The wheel packages the JSON documents and schemas directly from
[`resources/vis-contract/`](https://github.com/Blockether/vis/tree/main/packages/vis-contract/resources/vis-contract).

## Validating portable data

```python
from blockether.vis_contract import schema, validate

validate("activity", "declaration", {"presenter": "tests", "label": "Run tests"})
validate(
    "view",
    "operator_action",
    {"action": "select", "node_id": "results", "item_ids": ["one"]},
)
activity_schema = schema("activity")
```

Validation uses the bundled JSON Schemas (Draft 2020-12), including local references;
it never retrieves a schema from the network. Invalid or non-JSON input raises
`ValueError` without echoing payload contents. Only document names and definitions
in the package are accepted. The SDK additionally enforces semantic constraints
such as Activity row-id uniqueness and UTF-8 receipt bounds. Fixtures under
`data/fixtures` are shared by the Python, Clojure and Companion consumer tests.

## License

Apache-2.0.
