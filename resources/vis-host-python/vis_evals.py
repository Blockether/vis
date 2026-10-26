"""Load the bundled `blockether.vis.evals` package and run one gateway operation.

The host gives the source of each module that `_gateway` uses. The package
`__init__` does not run, because it also imports the trajectory reader and its
SDK dependencies.
"""

import importlib.abc
import importlib.util
import json
import sys
import types

_PACKAGE = "blockether.vis.evals"


class _Sources(importlib.abc.MetaPathFinder, importlib.abc.Loader):
    """Import the package modules from the source texts that the host gives."""

    def __init__(self, sources):
        self.sources = sources

    def _name(self, fullname):
        prefix, _, name = fullname.rpartition(".")
        return name if prefix == _PACKAGE and name in self.sources else None

    def find_spec(self, fullname, path=None, target=None):
        if self._name(fullname) is None:
            return None
        return importlib.util.spec_from_loader(fullname, self)

    def create_module(self, spec):
        return None

    def exec_module(self, module):
        name = self._name(module.__name__)
        path = f"blockether/vis/evals/{name}.py"
        module.__file__ = path
        exec(compile(self.sources[name], path, "exec"), module.__dict__)


def _install(sources):
    for name in ("blockether", "blockether.vis", _PACKAGE):
        if name not in sys.modules or name == _PACKAGE:
            package = types.ModuleType(name)
            package.__path__ = []
            sys.modules[name] = package
    sys.meta_path.insert(0, _Sources(sources))


def evals_call(sources, operation, body):
    """Run `operation` on the JSON text `body` and give the JSON envelope."""
    if _PACKAGE not in sys.modules:
        _install(json.loads(sources))
    from blockether.vis.evals._gateway import handle

    return handle(operation, json.loads(body))
