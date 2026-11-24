"""Each `vis.ui` builder has one domain path: `vis.ui.<domain>` or `vis.ui.<domain>.<part>`."""

import inspect
import re
from collections import defaultdict

import blockether.vis.extension as vis

SEGMENT = re.compile(r"[a-z]+")


def _builder(node):
    # A domain is an object whose class calls a builder. A part is the builder.
    call = inspect.getattr_static(node, "__call__", None)
    return call.__func__ if isinstance(call, staticmethod) else node


def _parts(node):
    # A domain lists its class and instance names. A builder function lists the names
    # that code sets on it, such as `vis.ui.button.primary = ...`.
    names = getattr(node, "__dict__", {}) if callable(node) and type(node).__module__ != vis.__name__ else dir(node)
    return sorted(n for n in names if not n.startswith("_"))


def _walk(node, path):
    for name in _parts(node):
        child = getattr(node, name)
        yield f"{path}.{name}", name, child
        yield from _walk(child, f"{path}.{name}")


def test_each_domain_lists_all_its_parts():
    # A custom `__getattr__` or `__dir__` hides a part from the walk.
    domains = [vis.ui, *(node for _, _, node in _walk(vis.ui, "vis.ui") if type(node).__module__ == vis.__name__)]
    hidden = [
        type(one).__name__
        for one in domains
        if hasattr(type(one), "__getattr__") or type(one).__dir__ is not object.__dir__
    ]
    assert not hidden, f"list each part as a class attribute, without __getattr__ or __dir__: {hidden}"


def test_each_ui_builder_has_one_domain_path():
    # A flat name such as `table_column` or a second name for one builder is refused.
    paths = defaultdict(list)
    for path, name, node in _walk(vis.ui, "vis.ui"):
        assert SEGMENT.fullmatch(name), f"{path}: name a part with one lowercase word"
        assert path.count(".") <= 3, f"{path}: use vis.ui.<domain> or vis.ui.<domain>.<part>"
        if not callable(node):
            # A domain may only group its parts, such as `vis.ui.limit`.
            assert type(node).__module__ == vis.__name__, f"{path} builds nothing"
            assert any(not n.startswith("_") for n in dir(node)), f"{path} builds nothing"
            continue
        paths[_builder(node)].append(path)
    aliases = [found for found in paths.values() if len(found) > 1]
    assert not aliases, f"one builder has more than one path: {aliases}"

