# vis-agent

The Python half of [Vis](https://github.com/Blockether/vis): the module a Vis
extension imports, packaged so it also runs where Vis is not.

```bash
pip install vis-agent
```

```python
import vis


def deploy(env):
    """Ship the current build to one environment."""
    spec = [
        vis.heading("Target"),
        vis.select("env", ["staging", "prod"], label="Where", default=env),
        vis.password("token", label="Deploy token", is_required=True),
    ]
    answer = vis.ask("Deploy", spec)
    if not answer:
        return vis.err("cancelled", answer.reason)
    run = vis.shell({"command": "./deploy.sh " + answer["env"]}).wait(600)
    vis.state["last_env"] = answer["env"]
    return vis.ok("deployed " + answer["env"], run["out"])


vis.extension(
    name="deployer",
    description="Ship a build from the session that decided to ship it.",
    alias="dep",
    symbols=[vis.symbol(deploy)],
)
```

## One file, two hosts

Everything the module does, it does through the host ops declared as data in
[`packages/vis-contract/resources/vis-contract/python-host.edn`](https://github.com/Blockether/vis/blob/main/packages/vis-contract/resources/vis-contract/python-host.edn)
and installed alongside as `vis-contract`, which ships that declaration as
`vis_contract/contract.json` and the `Host` protocol this package is written against.

Inside a Vis session the engine seeds those ops and they reach the live agent:
state is the extension's durable state, `vis.ask` opens a dialog on whichever
surface the human is using, `vis.shell` runs in the agent's sandbox.

Installed from PyPI there is no agent, so `vis._outside` serves the same ops the
way the contract says each behaves out here:

| op | outside |
| --- | --- |
| `state`, `log`, `notify`, `shell`, secrets, `host_env` | done locally — a JSON file under `~/.vis/outside`, stderr lines, a real subprocess, a process-local vault |
| `ask` | prompted in the TERMINAL: the same field tree, the same validators, the same `Answer` |
| `jailed_shell`, `jailed_shell_session` | refused by name — a jail is a property of the agent's process boundary, and nothing out here can enforce one |

So an extension file imports, type-checks, unit-tests and runs on a laptop or in
CI, and the code that ships is the code that was tested.

## Answering without a human

```python
import vis

vis.outside.answer_with({"env": "staging", "token": "hunter2"})
answer = vis.ask("Deploy", [vis.select("env", ["staging", "prod"])])
assert answer["env"] == "staging"
```

`VIS_OUTSIDE_ANSWERS` (a JSON object) primes the same values from the
environment, and `VIS_OUTSIDE_NONINTERACTIVE=1` makes an unanswerable ask return
`undeliverable` — exactly what a session with no surface mounted returns — instead
of blocking a build.

Other environment knobs: `VIS_OUTSIDE_HOME` moves the state file and the shell
logs (default `~/.vis/outside`).

## Testing live extensions

`vis.testing.LiveRecorder` is the shared in-memory host for extension tests. It
records extension envelopes without publishing fixture views into a real session,
materializes open/patch/state/close, and exposes `focus` and `close` for simulated
surface actions. Provider-specific tests keep only their provider snapshots and
assertions; `vis.testing.assert_tree` compares terminal view goldens at the exact
leaf that changed.

## Where the real documentation lives

`vis.ask`, the field builders, `vis.extension`, hooks, providers and network
filters are documented where they are defined, in `vis/__init__.py`, and in the
Vis docs (`doc("extending")` inside a session). The contract this package implements is one EDN document in the repository;
this package is one implementation of it, which is why they are kept apart.

Apache-2.0. Part of the Vis repository: `packages/vis-agent`.
