# vis-agent

Python SDK for [Vis](https://github.com/Blockether/vis). Add tools to an agent
or control its sessions from Python.

## Install

Requires Python 3.11+.

```bash
pip install vis-agent
```

The package is imported as `blockether.vis`. It does not include the Vis executable.

## Add a tool

Save as `.vis/extensions/greeter.py` in your project:

```python
import blockether.vis.extension as vis


def greet(name: str) -> str:
    """Greet someone by name."""
    return f"Hello, {name}!"


vis.register(
    vis.Extension(
        name="greeter",
        description="Greeting tools.",
        alias="greeter",
        symbols=[vis.Symbol(greet)],
    )
)
```

Start Vis or run `/reload` to make the tool available to the agent.

## Connect to a gateway

Use a running Vis gateway and its access token:

```python
import os
from blockether.vis.engine import GatewayClient

with GatewayClient(
    "https://gateway.example.com", token=os.environ["VIS_TOKEN"]
) as client:
    session = client.create_session(title="Python API")
    turn = session.send("Describe this project")
    result = turn.wait(timeout=120)
    print(result)
```

The gateway needs a configured model provider. Closing the client leaves its
sessions intact. A wait timeout does not cancel the turn; use `turn.cancel()`.

## Run a local engine

Requires a separately installed Vis executable with `sdk-stdio` support
(Linux or macOS) and a configured model provider.

```python
from blockether.vis.engine import LocalEngine

with LocalEngine(executable="/path/to/vis-agent", root="/path/to/project") as engine:
    session = engine.create_session(title="Local Python API")
    result = session.send("Describe this project").wait(timeout=120)
    print(result)
```

`LocalEngine` uses the same session API as `GatewayClient`. It starts a private
process and creates a temporary database; exiting the context stops the process
and removes the database.

## Documentation

- [Extensions: tools, input forms, hooks and providers](https://github.com/Blockether/vis/blob/main/resources/vis-docs/extending.md)
- [Gateway setup and API](https://github.com/Blockether/vis/blob/main/resources/vis-docs/gateway.md)
- [Vis installation](https://github.com/Blockether/vis#readme)

License: Apache-2.0.
