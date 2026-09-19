"""Clojure language surface: delimiter and literal verdicts in pure Python.

This is the half of the Clojure surface that does not need a JVM. It scans
Clojure, ClojureScript and EDN text for unbalanced delimiters and unterminated
literals, which is what the write gate must catch before a patch lands.
Formatting, linting, tests, the nREPL and delimiter repair stay with Vis' Clojure
pack, where the JVM tools live.

A file of the same name in `~/.vis/extensions/` or `<project>/.vis/extensions/`
replaces this one.
"""

import blockether.vis.extension as vis
from vis_language_surface import clojure as clojure_surface

vis.register_extension(
    vis.Extension(
        name="language-surface-clojure",
        description="Clojure language surface: delimiter and literal syntax verdicts for clj, cljs, cljc and edn.",
        version="1.0.0",
        kind="language",
        language_tools=[
            vis.LanguageSurface(
                language="clojure",
                extensions=["clj", "cljs", "cljc", "cljd", "cljr", "bb", "edn"],
                is_exact_syntax=False,
                syntax=clojure_surface.syntax,
            )
        ],
    )
)
