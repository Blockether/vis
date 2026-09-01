"""Install a third-party package the first time a block imports it.

A sandbox block writes `import numpy` and means it: before CPython, Vis answered
that name with a hand-written reimplementation, and the model had to be told
which of them were real. Now the interpreter is the real one, so the answer is
the real wheel — fetched by the HOST, because the guest may not spawn a process
and may not reach the network on its own.

The finder goes LAST on `sys.meta_path`: it only ever sees a name nothing else
could resolve, so it can never shadow the standard library or a package that is
already installed. It refuses a dotted name (a package installs whole, and a
missing submodule of an installed package is a real error), refuses a stdlib
name, and remembers what it already failed to install so the second import of a
name that does not exist on PyPI costs nothing.
"""

import importlib
import sys
from importlib.machinery import PathFinder

#: Import name -> distribution name, for the ones that differ. pip installs a
#: DISTRIBUTION; `import` names a MODULE, and PyPI does not promise they match.
DISTRIBUTIONS = {
    "attr": "attrs",
    "bs4": "beautifulsoup4",
    "cv2": "opencv-python-headless",
    "dateutil": "python-dateutil",
    "docx": "python-docx",
    "dotenv": "python-dotenv",
    "fitz": "pymupdf",
    "google": "protobuf",
    "jwt": "pyjwt",
    "mpl_toolkits": "matplotlib",
    "OpenSSL": "pyopenssl",
    "PIL": "pillow",
    "pptx": "python-pptx",
    "PyPDF2": "pypdf",
    "serial": "pyserial",
    "skimage": "scikit-image",
    "sklearn": "scikit-learn",
    "yaml": "PyYAML",
    "zoneinfo": "tzdata",
}


class _VisAutoInstall:
    """The `sys.meta_path` finder that turns a missing import into a pip run."""

    def __init__(self, install):
        self.install = install
        self.tried = set()
        self.busy = set()

    def _wanted(self, fullname, path):
        if path is not None or "." in fullname:
            return None
        if fullname.startswith("_") or fullname in self.tried or fullname in self.busy:
            return None
        if fullname in getattr(sys, "stdlib_module_names", ()):
            return None
        return DISTRIBUTIONS.get(fullname, fullname)

    def find_spec(self, fullname, path=None, target=None):
        distribution = self._wanted(fullname, path)
        if distribution is None:
            return None
        self.busy.add(fullname)
        try:
            installed = self.install(distribution)
        except Exception:
            installed = False
        finally:
            self.busy.discard(fullname)
        self.tried.add(fullname)
        if not installed:
            return None
        importlib.invalidate_caches()
        return PathFinder.find_spec(fullname, None, target)


def install(host_install):
    """Put ONE finder on `sys.meta_path` for this process, answering it.

    Sessions share the interpreter and share the package directory pip installs
    into, so a finder per session would be several finders racing on one
    directory for the same wheel. A later session only re-points the callable at
    its own host tool.
    """
    for finder in sys.meta_path:
        if isinstance(finder, _VisAutoInstall):
            finder.install = host_install
            return finder
    finder = _VisAutoInstall(host_install)
    sys.meta_path.append(finder)
    return finder
