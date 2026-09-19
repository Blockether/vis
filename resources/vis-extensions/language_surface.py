"""Vis' base language surface: exact JSON and TOML syntax verdicts.

Vis carries no parser of its own. Every syntax verdict the write gate uses comes
from a language surface an extension registers, and this bundled extension serves
the two data languages the Python standard library parses exactly. Its siblings
`language-surface-python` and `language-surface-clojure` serve code.

A file of the same name in `~/.vis/extensions/` or `<project>/.vis/extensions/`
replaces this one, so you can serve JSON or TOML yourself without patching Vis.
"""

import blockether.vis.extension as vis
from vis_language_surface import data

_SURFACES = [
    vis.LanguageSurface(
        language="json",
        extensions=["json"],
        is_exact_syntax=True,
        syntax=data.json_syntax,
    )
]

if data.HAS_TOML:
    _SURFACES.append(
        vis.LanguageSurface(
            language="toml",
            extensions=["toml"],
            is_exact_syntax=True,
            syntax=data.toml_syntax,
        )
    )

vis.register_extension(
    vis.Extension(
        name="language-surface",
        description="Base language surface: exact JSON and TOML syntax verdicts for the write gate.",
        version="1.0.0",
        kind="language",
        language_tools=_SURFACES,
    )
)
