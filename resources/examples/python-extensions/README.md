# Python extension examples

Working single-file extensions for Vis's Python extension surface. To use
one, copy it into an extensions directory and start Vis (or run `/reload`
in a live session):

```
~/.vis/extensions/           global — loads in every project
<project>/.vis/extensions/   project-local — loads for that project only
```

| File | Demonstrates |
| --- | --- |
| `todo.py` | Model-facing **tools** + durable **`vis.state`** + a `/todos` **slash command** + a prompt fragment |
| `protected_paths.py` | **Op hooks** — a before-hook that blocks file mutations on protected paths |
| `pirate.py` | Dynamic **prompt fragment** (a callable, re-evaluated per turn) toggled by `/pirate` |
| `github_issues.py` | Real-world integration: **HTTP from the trusted context**, config via slash + state, secrets via env vars |
| `provider_acme.py` | **LLM provider** (`vis.provider`) — a router-callable provider from a `preset` + `get_token`, credentials via env vars |

The full authoring guide is the `python-extensions` page of Vis's own docs
(`vis docs` site, or ask Vis: *"how do I write a Python extension?"* — it
looks the page up via its `vis_docs` tool).

Python extensions contribute tools, prompts, slash commands, op hooks,
durable state, and LLM providers (`vis.provider(...)`). Channels,
persistence backends, and TUI rendering stay host-side Clojure.
