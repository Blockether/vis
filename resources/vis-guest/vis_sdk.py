"""Install the canonical extension SDK with an explicit host or no host access."""

import importlib
import importlib.machinery
import sys
from types import ModuleType


class _SandboxHost:
    """Refuse extension host operations without creating an outside host."""

    def __getattr__(self, name):
        raise RuntimeError(
            f"Extension host operation {name!r} is unavailable in python_execution. "
            "You can inspect SDK types and construct declarations here; "
            "load an extension to use its host APIs."
        )


def install(source, host=None):
    """Load the bundled SDK without replacing other Blockether packages."""
    for name in ("blockether", "blockether.vis"):
        try:
            package = importlib.import_module(name)
        except ModuleNotFoundError as missing:
            if missing.name != name:
                raise
            package = ModuleType(name)
            package.__path__ = []
            package.__package__ = name
            package.__spec__ = importlib.machinery.ModuleSpec(
                name, None, is_package=True
            )
            sys.modules[name] = package
            if "." in name:
                sys.modules["blockether"].vis = package
        if not hasattr(package, "__path__"):
            raise ImportError(f"{name} must be a package")

    module = ModuleType("blockether.vis.extension")
    module.__package__ = "blockether.vis"
    module.__file__ = "blockether/vis/extension.py"
    module.__spec__ = importlib.machinery.ModuleSpec(module.__name__, None)
    module.__dict__["_host"] = _SandboxHost() if host is None else host
    previous = sys.modules.get(module.__name__)
    sys.modules[module.__name__] = module
    try:
        exec(compile(source, module.__file__, "exec"), module.__dict__)
    except BaseException:
        if previous is None:
            sys.modules.pop(module.__name__, None)
        else:
            sys.modules[module.__name__] = previous
        raise
    package.extension = module
    return module
