"""Python language surface: the interpreter's own compiler decides an edit.

`compile` answers whether a patch still parses, so Vis needs no Python parser of
its own. Formatting, linting, tests and the REPL stay with Vis' Python pack; this
extension owns the syntax verdict the write gate reads.

A file of the same name in `~/.vis/extensions/` or `<project>/.vis/extensions/`
replaces this one.
"""

import blockether.vis.extension as vis
from vis_language_surface import python as python_surface

vis.register_extension(
    vis.Extension(
        name="language-surface-python",
        description="Python language surface: exact syntax verdicts from the interpreter's compiler.",
        version="1.0.0",
        kind="language",
        language_tools=[
            vis.LanguageSurface(
                language="python",
                extensions=["py", "pyi", "pyw"],
                is_exact_syntax=True,
                syntax=python_surface.syntax,
            )
        ],
    )
)
