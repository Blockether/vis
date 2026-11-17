"""The journals and review apps of one process, for each session key and root.

The extension records a checkpoint around each block and each patch. The SDK
program records one each time it polls. Both use one `Recorder`, so each journal
has one writer. A root can change during a session, for example with `/cd`, so
each root of a session has its own journal.
"""

from __future__ import annotations

import os
import re
import threading
from collections.abc import Callable, Iterable, Mapping
from typing import Any

from vis_lang_interface.tree import listing

from vis_review.journal import Entry, Journal

# Characters of code that a block checkpoint keeps as its label.
MOST_LABEL = 80


def _block_code(args) -> str:
    first = args[0] if isinstance(args, (list, tuple)) and args else args
    return str((first.get("code") if isinstance(first, Mapping) else first) or "")


def _patch_path(args, root: str) -> str | None:
    """The path that a patch wrote, under `root`; None when the call does not name it."""
    if not isinstance(args, (list, tuple)) or not args or not isinstance(args[0], str):
        return None
    path = os.path.expanduser(args[0])
    if os.path.isabs(path):
        path = os.path.relpath(os.path.realpath(path), root)
    return path


def block_label(args) -> str:
    """The label of a block checkpoint: the first line of code that is not a comment."""
    for line in _block_code(args).splitlines():
        text = line.strip()
        if text and not text.startswith("#"):
            return text[:MOST_LABEL]
    return "Code block"


def patch_label(args, root: str) -> str:
    """The label of a patch checkpoint: the path that the patch wrote, under `root`."""
    path = _patch_path(args, root)
    return "Patch" if path is None else "Patch " + path


def block_wrote(args) -> Callable[[str], bool]:
    """True for a path whose file name is in the code of the block.

    Code does not declare the files that it writes. A name in the code is the only
    sign, so a file that the block made through another program has none.
    """
    code = _block_code(args)

    def wrote(rel: str) -> bool:
        name = re.escape(os.path.basename(rel))
        return bool(name and re.search(rf"(?<![\w.-]){name}(?![\w-])", code))

    return wrote


def patch_wrote(args, root: str) -> Callable[[str], bool]:
    """True for the path that the patch wrote."""
    path = _patch_path(args, root)
    named = None if path is None else path.replace(os.sep, "/")
    return lambda rel: rel == named


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

    def record(
        self,
        key: str,
        root,
        kind: str,
        label: str,
        wrote: Callable[[str], bool] | None = None,
    ) -> Entry | None:
        """Record what changed under `root`. Repaint the open app when files changed.

        A commit or a push changes no file and still changes what the app shows, so
        an app that is behind git gets a paint too. `wrote` is true for a path that
        the step shows it wrote; the journal then keeps that path as an own change.
        """
        journal = self.journal(key, root)
        entry = journal.checkpoint(kind, label, wrote)
        app = self.app(key, journal.root)
        if app is not None and (entry is not None or app.is_behind()):
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
