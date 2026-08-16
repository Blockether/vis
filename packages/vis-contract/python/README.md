# vis-contract

The contract half of [Vis](https://github.com/Blockether/vis): everything a Vis
extension may ask its host for, as data — plus the protocol a host is checked
against. `vis-agent` (the API an extension imports) depends on this package; so
can a linter, a test double, or an editor that never runs Vis at all.

```bash
pip install vis-contract
```

```python
import vis_contract

vis_contract.VERSION  # the contract version
vis_contract.OPS["shell"]["outside"]  # "local" — what the op does with no Vis
vis_contract.SHELL["spawn_ops"]  # ["run", "background"]
vis_contract.HUMAN_INPUT["field_types"]  # the closed dialog vocabulary


class MyHost:
    """A host of your own — a test double, another editor, a CI harness."""

    def state_get(self, key):
        return None

    ...


vis_contract.check_host(MyHost())  # TypeError names every op you did not answer
```

`vis_contract.Host` is a `typing.Protocol`: the thirteen calls, their arities and
their doc lines are the document's, and the package's own suite fails when the
protocol and `contract.json` stop agreeing.

The document is generated from
[`resources/vis-contract/python-host.edn`](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/python-host.edn),
which the Clojure artifact `com.blockether/vis-contract` reads directly — the same
declaration in both ecosystems, at the same version.

## License

Apache-2.0.
