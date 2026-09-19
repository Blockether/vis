# Language extensions

Vis ships formatting, linting, tests, REPL evaluation and syntax checks for a few
languages. A language extension adds one more: you write a handful of Python
functions, and `format_code`, `lint_code`, `run_tests`, `repl_eval` and
`repl_start` start answering for your language too. The editors ask your syntax
handler before they write, so an edit that would break one of your files is
refused instead of saved.

Vis carries no parser of its own. Every syntax verdict it uses comes from a
surface like the one you are about to write, including the ones Vis ships with —
listed further down, and replaceable by yours.

## Before you start

Write this extension for the host — the Vis that runs in your project. It loads
your file and calls your handlers itself. An agent or client extension that
declares `language_tools` is refused before it connects, because there is no
engine on that side to route the tools to.

If this is your first extension, read [Extending Vis](extending.md) first, and
[Installing and sharing extensions](extension-packages.md) for where the file
belongs. The example below needs nothing but the Python standard library.

## Serve a language

A surface names one language and carries the handlers you implement. This one
gives Vis JSON formatting and a JSON syntax verdict:

```python
"""JSON formatting and syntax checks."""

from __future__ import annotations

import json
from pathlib import Path

import blockether.vis.extension as vis


def pretty(source):
    return json.dumps(json.loads(source), indent=2) + "\n"


def format_json(options):
    snippet = options.get("code")
    if snippet:
        return {"op": "json-format", "changed": pretty(snippet) != snippet}
    files = []
    for name in options.get("paths") or []:
        path = Path(name)
        before = path.read_text(encoding="utf-8")
        after = pretty(before)
        if after != before:
            path.write_text(after, encoding="utf-8")
        files.append({"path": str(path), "changed": after != before})
    return {
        "op": "json-format",
        "changed": sum(1 for file in files if file["changed"]),
        "files": files,
        "formatter": "json",
    }


def check_json(request):
    language = request["language"]
    try:
        json.loads(request["source"])
    except json.JSONDecodeError as error:
        finding = {
            "line": error.lineno,
            "col": error.colno,
            "kind": "parse",
            "message": error.msg,
        }
        return {"language": language, "is_clean": False, "findings": [finding]}
    return {"language": language, "is_clean": True, "findings": []}


vis.register_extension(
    vis.Extension(
        name="json-language",
        description="JSON formatting and syntax checks.",
        kind="language",
        language_tools=[
            vis.LanguageSurface(
                language="json",
                extensions=["json"],
                is_exact_syntax=True,
                format=format_json,
                syntax=check_json,
            )
        ],
    )
)
```

`language` is a lowercase name: a letter first, then letters, digits, `_`, `+`
or `-`. `extensions` lists the file suffixes this language owns, so Vis knows a
`.json` file is yours. Set `is_exact_syntax=True` when your handler really
parses the language rather than approximating it. Declare at least one handler —
the language gains exactly the capabilities you implement.

## What each handler serves

`format`, `lint`, `test` and `repl_eval` receive the options the tool was called
with: the `language`, the project directory as `cwd`, and either `code` for one
snippet or `path` and `paths` for files. `syntax` receives
`{"language": ..., "source": ...}`, and `balance` receives the source text
itself.

| Handler | Tool | What you return |
| --- | --- | --- |
| `format` | `format_code` | `op` names the formatter that ran; add `changed`, `files` and `formatter`. |
| `lint` | `lint_code` | `findings`, each with a `level` and a `message`; add `error`, `warning` and `info` counts. |
| `test` | `run_tests` | `mode`, either `"repl"` or `"cli"`; add `total`, `pass`, `fail`, `errored`, `skipped` and `failures`. |
| `repl_eval` | `repl_eval` | Your REPL's answer for one evaluation. |
| `repl_start` | the REPL lifecycle | Called as `repl_start(op, options)`, with `op` of `"start"`, `"status"`, `"stop"` or `"connect"`. |
| `syntax` | every writer that edits a file | `language`, `is_clean` and `findings`. |
| `balance` | the same writers | Repaired source text, or `None` to leave the edit refused. |

The tool names never change: people and models keep calling `format_code` and
`run_tests`, and Vis routes the call to the surface registered for that
language. A file call formats in place and reports per-file changes; a snippet
call reports whether the text changed, never the formatted text.

## Return results Vis can check

Every result is validated against the language-surface contract before anyone
sees it. A result that misses a required key comes back as a failure carrying
the schema explanation, so a broken handler cannot report a clean run it never
had. A `run_tests` result is completed first: the counts Vis can derive from
what you reported are filled in and the language is recorded for you, so a
runner that answers with its own numbers still returns the full result.

A surface declares no Activity of its own. `format_code`, `lint_code`,
`run_tests` and the REPL tools already present this work to people, so put the
counts, findings and failures in your result and let those tools show them. See
[Activity presentation](extension-api.md#activity-presentation) for the
declarations that do own their display.

## Guard edits with the syntax verdict

A language with a `syntax` handler is treated as code: `patch` and the sandbox
writers ask your handler whether the new content parses, and an edit that
introduces an error the file did not already have is refused with the line and
the finding you reported. Each finding carries `line`, `col` and a `kind` of
`"unclosed"`, `"unexpected"`, `"missing"` or `"parse"`; `end_line`, `end_col`,
`delimiter`, `expected`, `message` and `text` are optional detail.

`balance` is the second chance. When an edit would leave delimiters unpaired,
Vis offers your handler the source and writes back the repaired text you return;
returning `None` leaves the edit refused. If a handler raises or breaks the
result contract, Vis discards the answer instead of trusting it, and the file is
written unchecked: a language nothing judged is unguarded, exactly like a
language no surface claims. Keep your handler total and report a fault rather
than raising one.

## The surfaces Vis ships

Vis' own syntax verdicts come from three bundled extensions, written with exactly
the API on this page:

| Extension | Files | How it decides |
| --- | --- | --- |
| `language-surface` | `.json`, `.toml` | `json.loads` and `tomllib`, so a verdict matches the file a build reads |
| `language-surface-python` | `.py`, `.pyi`, `.pyw` | `compile`, the interpreter's own parser |
| `language-surface-clojure` | `.clj`, `.cljs`, `.cljc`, `.cljd`, `.cljr`, `.bb`, `.edn` | a scanner that follows comments, strings, regex and character literals and reports unpaired delimiters |

Vis refreshes them under `~/.vis/extensions-bundled/` when it starts and scans
that directory first, so a file of the same name in `~/.vis/extensions/` or
`<project>/.vis/extensions/` replaces the one Vis ships. Formatting, linting,
tests and the REPL for Clojure and Python stay with Vis' built-in packs: the
bundled surfaces own the syntax verdict and nothing else.

## Install it and try it

[Install the extension](extension-packages.md#install-a-package) where Vis loads
it and reload, then check that the session picked it up: the language tools in
the session prompt list your language with the tools it serves. Ask for the work
in chat ("format the changed JSON files"), or call the tool yourself:

```python
format_code({"language": "json", "paths": ["package.json"]})
```

If the call does not reach your handler, the error says which case you hit:

- `format_code: no handler for 'json'; available: clojure, python.` — the name
  in the call does not match the surface, or the extension did not load.
- `format_code: multiple handlers for 'json'; disable the duplicate language
  extension.` — two extensions serve the same language; keep one of them.
- `format_code: no language handler enabled.` — no surface serves this
  capability, so there is nothing to dispatch to.

## See also

- [Extending Vis](extending.md) — write, register and load the extension that carries your surface.
- [Extension API](extension-api.md) — every declaration field, including `language_tools`.
- [Installing and sharing extensions](extension-packages.md) — where extension files live and how to reload them.
