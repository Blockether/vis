"""The harness of the app function: it serves the web app of one project directory.

The app function starts this file with the Python of the project, so it uses only the
standard library. Before each request it compares the stamp of the source files. After
a change it loads the modules of the project again, so the edit of a session shows in
the next request. The packages of the image stay loaded.
"""

from __future__ import annotations

import hashlib
import importlib
import os
import resource
import signal
import sys
import traceback
from collections.abc import Callable, Iterable
from typing import Any
from wsgiref.simple_server import WSGIRequestHandler, make_server

# The directory of a project that holds its modules.
SOURCE = "src"
# The directories that hold no source, and the largest number of files that a stamp reads.
SKIPPED, FILES = {"__pycache__", ".git", ".venv", "node_modules"}, 5000
# The largest file that the code of the project can write.
FILE_BYTES = 16_000_000
TEXT = [("Content-Type", "text/plain; charset=utf-8"), ("Cache-Control", "no-store")]


def stamp(root: str) -> str:
    """Return a text that changes when a source file of the project changes."""
    digest, count = hashlib.sha256(), 0
    for folder, folders, names in os.walk(os.path.join(root, SOURCE)):
        folders[:] = sorted(name for name in folders if name not in SKIPPED)
        for name in sorted(names):
            path = os.path.join(folder, name)
            try:
                facts = os.stat(path)
            except OSError:
                continue
            digest.update(f"{path} {facts.st_mtime_ns} {facts.st_size}\n".encode())
            count += 1
            if count >= FILES:
                return digest.hexdigest()[:16]
    return digest.hexdigest()[:16]


class Project:
    """The WSGI app of a project. It loads the app again after a change of the source."""

    def __init__(self, root: str, entry: str) -> None:
        self.root, self.stamp = root, ""
        self.module, _, self.name = entry.partition(":")
        self.app: Callable[..., Iterable[bytes]] | None = None
        self.failure = ""

    def load(self) -> None:
        current = stamp(self.root)
        if current == self.stamp:
            return
        for name, module in list(sys.modules.items()):
            if (getattr(module, "__file__", None) or "").startswith(self.root + "/"):
                del sys.modules[name]
        importlib.invalidate_caches()
        self.stamp = current
        try:
            self.app, self.failure = getattr(importlib.import_module(self.module), self.name), ""
        except Exception:  # the page shows the error of the project
            self.app, self.failure = None, traceback.format_exc()

    def __call__(self, environ: dict[str, Any], start_response: Callable[..., Any]) -> Iterable[bytes]:
        self.load()
        try:
            if self.app is None:
                raise RuntimeError(self.failure)
            return self.app(environ, start_response)
        except Exception:
            start_response("500 Internal Server Error", TEXT)
            return [(self.failure or traceback.format_exc()).encode()]


class Quiet(WSGIRequestHandler):
    def log_message(self, *args: Any) -> None:  # the app function writes the log
        pass


def main(root: str, port: str, entry: str) -> None:
    # The code of the project cannot fill /tmp with one file or with a core file.
    signal.signal(signal.SIGXFSZ, signal.SIG_IGN)
    resource.setrlimit(resource.RLIMIT_FSIZE, (FILE_BYTES, FILE_BYTES))
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    os.umask(0o077)
    sys.path.insert(0, os.path.join(root, SOURCE))
    project = Project(root, entry)
    project.load()
    make_server("127.0.0.1", int(port), project, handler_class=Quiet).serve_forever()


if __name__ == "__main__":
    main(*sys.argv[1:4])
