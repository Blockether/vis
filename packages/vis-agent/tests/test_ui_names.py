"""Each `vis.ui` builder has one domain path: `vis.ui.<domain>` or `vis.ui.<domain>.<part>`."""

import re
from collections import defaultdict

import blockether.vis.extension as vis

SEGMENT = re.compile(r"[a-z]+")


def _builder(node):
    # A domain is an object whose class calls a builder. A part is the builder.
    call = type(node).__dict__.get("__call__")
    return call.__func__ if isinstance(call, staticmethod) else node


def _walk(node, path):
    for name in sorted(n for n in dir(type(node)) if not n.startswith("_")):
        child = getattr(node, name)
        yield f"{path}.{name}", name, child
        if not callable(child) or type(child).__module__ == vis.__name__:
            yield from _walk(child, f"{path}.{name}")


def test_each_ui_builder_has_one_domain_path():
    # A flat name such as `table_column` or a second name for one builder is refused.
    paths = defaultdict(list)
    for path, name, node in _walk(vis.ui, "vis.ui"):
        assert SEGMENT.fullmatch(name), f"{path}: name a part with one lowercase word"
        assert callable(node), f"{path} builds nothing"
        paths[_builder(node)].append(path)
    aliases = [found for found in paths.values() if len(found) > 1]
    assert not aliases, f"one builder has more than one path: {aliases}"

