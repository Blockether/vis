"""The journals and review apps of one process, for each session key and root.

The extension records a checkpoint around each block and each patch. The SDK
program records one each time it polls. Both use one `Recorder`, so each journal
has one writer. A root can change during a session, for example in a draft, so
each root of a session has its own journal.
"""

from __future__ import annotations

import os
import threading
from collections.abc import Callable, Iterable, Mapping
from typing import Any

from vis_lang_interface.tree import listing

from vis_review.journal import Entry, Journal

# Characters of code that a block checkpoint keeps as its label.
MOST_LABEL = 80


def block_label(args) -> str:
    """The label of a block checkpoint: the first line of code that is not a comment."""
    first = args[0] if isinstance(args, (list, tuple)) and args else args
    code = first.get("code") if isinstance(first, Mapping) else first
    for line in str(code or "").splitlines():
        text = line.strip()
        if text and not text.startswith("#"):
            return text[:MOST_LABEL]
    return "Code block"


def patch_label(args, root: str) -> str:
    """The label of a patch checkpoint: the path that the patch wrote, under `root`."""
    if not isinstance(args, (list, tuple)) or not args or not isinstance(args[0], str):
        return "Patch"
    path = os.path.expanduser(args[0])
    if os.path.isabs(path):
        path = os.path.relpath(os.path.realpath(path), root)
    return "Patch " + path


class Recorder:
    """Journals by `(key, root)`, and the open review app of each journal."""

    def __init__(self, base=None, files: Callable[[str], Iterable[str]] = listing):
        self._base = base
        self._files = files
        self._journals: dict[tuple[str, str], Journal] = {}
        self._apps: dict[tuple[str, str], Any] = {}
        self._lock = threading.Lock()

    def journal(self, key: str, root) -> Journal:
        """The journal of `key` under `root`, loaded once."""
        real = os.path.realpath(os.fspath(root))
        with self._lock:
            found = self._journals.get((key, real))
            if found is None:
                found = Journal(real, key, self._base, files=self._files)
                self._journals[(key, real)] = found
            return found

    def record(self, key: str, root, kind: str, label: str) -> Entry | None:
        """Record what changed under `root`, and repaint the open app when it did."""
        journal = self.journal(key, root)
        entry = journal.checkpoint(kind, label)
        if entry is not None:
            app = self.app(key, journal.root)
            if app is not None:
                app.refresh()
        return entry

    def app(self, key: str, root) -> Any:
        """The open review app of `key` under `root`, or None."""
        with self._lock:
            return self._apps.get((key, os.path.realpath(os.fspath(root))))

    def keep(self, key: str, app) -> None:
        """Remember `app`, so each new checkpoint of its journal repaints it."""
        with self._lock:
            self._apps[(key, app.journal.root)] = app
