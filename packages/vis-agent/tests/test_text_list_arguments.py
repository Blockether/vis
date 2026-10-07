"""Regression coverage for #324: one bare string for a list of strings is one item."""

from __future__ import annotations

from collections.abc import Sequence
from pathlib import Path
from typing import Annotated, Literal, Optional

import blockether.vis.extension as vis


class Tools:
    """The parameter shapes of the `py.*` and `clj.*` language tools."""

    def lint_code(
        self,
        paths: Annotated[Sequence[str], "Files or directories to lint."] = (),
        *,
        cwd: str = "",
        namespaces: Annotated[Sequence[str], "Test namespaces to run."] = (),
    ) -> tuple:
        return tuple(paths), tuple(namespaces)


def rebuilt(fn, *args, **kwargs):
    return vis._call_arguments(fn, list(args), kwargs)


def test_bare_string_for_a_string_sequence_is_one_item():
    # Regression for Blockether/vis#324: `py.lint_code("/src/app.py")` named one
    # path for each character, and `/` made the lint walk the whole filesystem.
    tools = Tools()
    args, kwargs = rebuilt(tools.lint_code, "/src/app.py", namespaces="my.ns-test")
    assert args == [["/src/app.py"]]
    assert kwargs == {"namespaces": ["my.ns-test"]}
    assert tools.lint_code(*args, **kwargs) == (("/src/app.py",), ("my.ns-test",))
    assert rebuilt(tools.lint_code, paths="a.py")[1] == {"paths": ["a.py"]}


def test_lists_and_other_parameters_stay_as_they_arrived():
    tools = Tools()
    paths = ["a.py", "b.py"]
    args, kwargs = rebuilt(tools.lint_code, paths, cwd="/src")
    assert args[0] is paths
    assert kwargs == {"cwd": "/src"}


def test_container_and_item_annotations_decide_the_shape():
    def files(
        paths: list[str], tags: set[str] = frozenset(), pair: tuple[str, ...] = ()
    ):
        return paths, tags, pair

    def optional(paths: Optional[list[str]] = None, more: list[str] | None = None):  # noqa: UP045
        return paths, more

    def literal(modes: list[Literal["fast", "full"]] = ()):
        return modes

    assert rebuilt(files, "a.py", tags="x", pair="y") == (
        [["a.py"]],
        {"tags": {"x"}, "pair": ("y",)},
    )
    assert rebuilt(optional, "a.py", more="b.py") == ([["a.py"]], {"more": ["b.py"]})
    assert rebuilt(literal, "fast") == ([["fast"]], {})
    assert rebuilt(literal, "other") == (["other"], {})


def test_a_member_that_takes_the_string_as_it_is_wins():
    def either(paths: list[str] | str = (), path: str | list[str] = ""):
        return paths, path

    def anything(
        values: list = (), items: Sequence[object] = (), raw: list[bytes] = ()
    ):
        return values, items, raw

    assert rebuilt(either, "a.py", path="b.py") == (["a.py"], {"path": "b.py"})
    assert rebuilt(anything, "ab", items="cd", raw="ef") == (
        ["ab"],
        {"items": "cd", "raw": "ef"},
    )


def test_path_items_take_a_bare_string_unchanged():
    def walk(paths: list[str | Path] = ()):
        return paths

    assert rebuilt(walk, "src") == ([["src"]], {})
