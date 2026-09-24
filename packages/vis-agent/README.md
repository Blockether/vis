# vis-agent

Python SDK for Vis extensions and agent sessions.

```sh
pip install vis-agent
```

Requires Python 3.11+. Install the Vis executable and configure a provider
separately for local use. The execution-layer API below is unreleased: build
the SDK and engine from the same source revision.

## Run a local agent

```python
from blockether.vis.engine import Agent

with Agent(project=".") as agent:
    result = agent.run("Summarize this project without changing files.")
    print(result["status"])
```

The SDK starts an owned `vis-agent stdio` process, sets up a temporary session
database and cleans it up on close. This is a Python transport, not an
interactive CLI command or an MCP server. Tasks can incur provider charges and
access project files. For a chosen executable or multiple sessions on one
process, use `LocalEngine`; see the [stdio guide](https://vis.blockether.com/python-sdk.html#use-stdio-from-the-python-sdk).

- [Python SDK guide](https://vis.blockether.com/python-sdk.html) — run a local
  agent, continue a conversation or connect to a shared gateway. `Agent` is
  available in SDK `0.2.3` and newer.
- [Decision models](https://vis.blockether.com/decision-models.html) — download Laya,
  train both heads offline, publish FP32 and select a version explicitly.
- [Python API reference](https://vis.blockether.com/python-sdk-api/) — browse
  classes, methods, signatures and types generated from the SDK source.
- [Extension guide](https://vis.blockether.com/extending.html) — add tools to Vis.
- [Documentation](https://vis.blockether.com) — browse all guides and reference pages.
