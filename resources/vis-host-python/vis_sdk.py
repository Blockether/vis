"""Install the one shared extension SDK with an explicit host or no host access."""

import importlib
import importlib.machinery
import os
import sys
from types import ModuleType

import vis_code

# The host stages the SDK engine next to this file, for `blockether.vis.engine`.
_ENGINE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "vis_engine")

# A native build ships the packages that the SDK engine imports here (#405).
_PACKAGES = os.path.join(sys.base_prefix, "lib", "vis-sdk-packages")

# The SDK module without a host and the code object that built it. The body takes
# about 15 ms to execute, and every sandbox session installs the same body. These
# sessions share `sys.modules`, so they can share this module like any import.
_SANDBOX = (None, None)


def _add_packages():
    """Add the bundled SDK packages last, so packages that the user installs win."""
    if os.path.isdir(_PACKAGES) and _PACKAGES not in sys.path:
        sys.path.append(_PACKAGES)


class _SandboxHost:
    """Refuse extension host operations without creating an outside host."""

    def __getattr__(self, name):
        raise RuntimeError(
            f"Extension host operation {name!r} is unavailable in python_execution. "
            "You can inspect SDK types and construct declarations here; "
            "load an extension to use its host APIs."
        )


def install(source, host=None):
    """Load the bundled SDK without replacing other Blockether packages.

    The staged SDK engine comes first in `blockether.vis`, also when an extension
    installs an older vis-agent. Without a host, the same source gives the module
    of the previous call.
    """
    global _SANDBOX
    _add_packages()
    for name in ("blockether", "blockether.vis"):
        try:
            package = importlib.import_module(name)
        except ModuleNotFoundError as missing:
            if missing.name != name:
                raise
            package = ModuleType(name)
            package.__path__ = [os.path.join(_ENGINE, *name.split("."))]
            package.__package__ = name
            package.__spec__ = importlib.machinery.ModuleSpec(
                name, None, is_package=True
            )
            sys.modules[name] = package
            if "." in name:
                sys.modules["blockether"].vis = package
        if not hasattr(package, "__path__"):
            raise ImportError(f"{name} must be a package")
    staged = os.path.join(_ENGINE, "blockether", "vis")
    if staged not in package.__path__:
        package.__path__ = [staged, *package.__path__]

    filename = "blockether/vis/extension.py"
    compiled = vis_code.code(source, filename)
    if host is None and _SANDBOX[0] is compiled:
        module = _SANDBOX[1]
        sys.modules[module.__name__] = module
        package.extension = module
        return module

    module = ModuleType("blockether.vis.extension")
    module.__package__ = "blockether.vis"
    module.__file__ = filename
    module.__spec__ = importlib.machinery.ModuleSpec(module.__name__, None)
    module.__dict__["_host"] = _SandboxHost() if host is None else host
    previous = sys.modules.get(module.__name__)
    sys.modules[module.__name__] = module
    try:
        exec(compiled, module.__dict__)
    except BaseException:
        if previous is None:
            sys.modules.pop(module.__name__, None)
        else:
            sys.modules[module.__name__] = previous
        raise
    if host is None:
        _SANDBOX = (compiled, module)
    package.extension = module
    return module
