# vis-agent

Python SDK for Vis agent sessions and extensions.

```sh
pip install vis-agent
```

You need Python 3.12+ on Linux or macOS and a configured model provider. You do not
have to install Vis first: on the first local run, the SDK downloads the Vis release of
the same version into `~/.vis/sdk` and checks it against the release checksums. Set
`VIS_NO_AUTO_INSTALL=1` to turn this off.

## Run a local agent

```python
from blockether.vis.engine import Agent

with Agent(project=".") as agent:
    result = agent.run("Summarize this project without changing files.")
    print(result["status"])
```

The SDK starts a private Vis process with a temporary session database and removes both
when the block ends. Tasks can incur provider charges and can change project files. To
choose the executable or share one process between agents, use `LocalEngine`. See the
[stdio guide](https://vis.blockether.com/python-sdk.html#use-stdio-from-the-python-sdk).

- [Python SDK guide](https://vis.blockether.com/python-sdk.html) — run a local
  agent, continue a conversation or connect to a shared gateway.
- [Python API reference](https://vis.blockether.com/python-sdk-api/) — browse
  classes, methods, signatures and types generated from the SDK source.
- [Extension guide](https://vis.blockether.com/extending.html) — add tools to Vis.
- [Decision models](https://vis.blockether.com/decision-models.html) — download
  Laya or GLiNER2.5, train both heads offline, publish FP32 and select a version.
- [Documentation](https://vis.blockether.com) — browse all guides and reference pages.
