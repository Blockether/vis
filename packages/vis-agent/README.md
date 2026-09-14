# vis-agent

Python SDK for Vis extensions and agent sessions.

```sh
pip install vis-agent
```

Requires Python 3.11+. Install the Vis executable and configure a provider
separately for local use.

- [Python SDK guide](https://vis.blockether.com/python-sdk.html) — run a local
  agent, continue a conversation or connect to a shared gateway. `Agent` is
  available in SDK `0.2.3` and newer.
- [Python API reference](https://vis.blockether.com/python-sdk-api/) — browse
  classes, methods, signatures and types generated from the SDK source.
- [Extension guide](https://vis.blockether.com/extending.html) — add tools to Vis.
- [Documentation](https://vis.blockether.com) — browse all guides and reference pages.

The online API reference follows `main`, so it can include unreleased APIs that
are not yet available in the latest PyPI package. The SDK guide marks those APIs.

## Generate the API reference locally

From `packages/vis-agent` in a Vis checkout, create a virtual environment and
install the documentation dependencies:

```sh
python3 -m venv .venv
. .venv/bin/activate
python -m pip install -e '.[docs]'
python -m pdoc blockether.vis '!blockether\.vis\._' \
  --docformat google --no-show-source -o ../../target/python-sdk-api
```

Open `../../target/python-sdk-api/index.html` in your browser. [pdoc](https://pdoc.dev/)
generates the reference from public Python symbols, type annotations and
docstrings; you do not need to start Vis or configure a model provider. The docs
workflow regenerates and publishes it when SDK changes reach `main`.
