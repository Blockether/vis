"""A write journal for one root: checkpoints of file contents, diffs between them and undo.

A checkpoint records the files that changed since the previous checkpoint, as
content digests in a shared `Store`. The first checkpoint of a journal is the
baseline: it records every file. The state at checkpoint N is the baseline with
the changes of checkpoints 2 to N applied, so any two checkpoints can be compared
and any checkpoint can be brought back.

The journal is one JSON line for each checkpoint in
`<base>/<root digest>/<key>.jsonl`, appended after the blobs of its contents are
written. A line that an interrupted write cut short is skipped when the journal
loads. One journal has one writer: the process that owns its key.

An undo is a checkpoint too, so an undo can be undone. Each undo first records
the changes that no checkpoint holds yet, so nothing is lost. Running the same
undo again changes nothing, and an interrupted undo finishes when it runs again.
"""

from __future__ import annotations

import contextlib
import difflib
import hashlib
import json
import os
import re
import tempfile
import threading
import time
from collections.abc import Callable, Iterable, Mapping
from dataclasses import dataclass, field
from pathlib import Path

from vis_lang_interface import process
from vis_lang_interface.changes import line_changes
from vis_lang_interface.tree import Tree, listing, stamp_of

from vis_review.store import Store

KINDS = ("baseline", "outside", "block", "patch", "undo", "manual")
# Seconds after which the file list is read again, to see files that git now lists.
REFRESH_S = 300
# A text side larger than this shows no line diff; its counts still come from line_changes.
MOST_DIFF_BYTES = 1024 * 1024
_BINARY_PROBE = 8000


@dataclass(frozen=True)
class Entry:
    """One checkpoint: the files that changed since the previous one."""

    seq: int
    at: float
    kind: str
    label: str
    files: Mapping[str, str | None] = field(default_factory=dict)


@dataclass(frozen=True)
class FileDiff:
    """How one file differs between two checkpoints."""

    path: str
    status: str  # added, deleted or modified
    added: int
    removed: int
    is_binary: bool
    patch: str
    before: str | None
    after: str | None
    note: str = ""


@dataclass(frozen=True)
class Undone:
    """What one undo did."""

    entry: Entry | None
    restored: tuple[str, ...]
    conflicts: tuple[str, ...] = ()
    missing: tuple[str, ...] = ()


def base_dir() -> Path:
    """`$VIS_HOME/review`, or `~/.vis/review`."""
    return Path(os.environ.get("VIS_HOME") or Path.home() / ".vis") / "review"


def _safe(key: str) -> str:
    return re.sub(r"[^A-Za-z0-9._-]", "_", key)[:120] or "default"


def _covers(_path) -> bool:
    return True


class Journal:
    """Checkpoints of every file under `root`, written by the owner of `key`.

    `listing(root)` gives the files to watch; by default the files that git sees,
    never ignored ones. `clock()` gives the time of each checkpoint.
    """

    def __init__(
        self,
        root,
        key: str,
        base=None,
        *,
        files: Callable[[str], Iterable[str]] = listing,
        clock: Callable[[], float] = time.time,
    ):
        self.root = os.path.realpath(os.fspath(root))
        self.key = key
        base = Path(base) if base is not None else base_dir()
        self.store = Store(base)
        folder = base / hashlib.sha256(self.root.encode()).hexdigest()[:16]
        self.path = folder / f"{_safe(key)}.jsonl"
        self._folder = folder
        self._files = files
        self._clock = clock
        self._lock = threading.RLock()
        self._tree: Tree | None = None
        self.entries: list[Entry] = []
        self.state: dict[str, str] = {}
        self._load()

    # Reading -----------------------------------------------------------------

    def _load(self) -> None:
        try:
            lines = self.path.read_text(encoding="utf-8").splitlines()
        except FileNotFoundError:
            return
        for line in lines:
            try:
                raw = json.loads(line)
                entry = Entry(
                    int(raw["seq"]),
                    float(raw["at"]),
                    str(raw["kind"]),
                    str(raw.get("label", "")),
                    dict(raw.get("files") or {}),
                )
            except (ValueError, KeyError, TypeError):
                # A line cut short by an interrupted write holds nothing it promised.
                continue
            self._apply(entry)

    def _apply(self, entry: Entry) -> None:
        self.entries.append(entry)
        for rel, sha in entry.files.items():
            if sha is None:
                self.state.pop(rel, None)
            else:
                self.state[rel] = sha

    def entry(self, seq: int) -> Entry:
        """The checkpoint with number `seq`."""
        for entry in self.entries:
            if entry.seq == seq:
                return entry
        raise KeyError(f"no checkpoint {seq}")

    def state_at(self, seq: int) -> dict[str, str]:
        """`{path: digest}` of every file as checkpoint `seq` left it; 0 is the empty state."""
        state: dict[str, str] = {}
        for entry in self.entries:
            if entry.seq > seq:
                break
            for rel, sha in entry.files.items():
                if sha is None:
                    state.pop(rel, None)
                else:
                    state[rel] = sha
        return state

    @property
    def last(self) -> int:
        """The number of the newest checkpoint, or 0 before the baseline."""
        return self.entries[-1].seq if self.entries else 0

    # Writing -----------------------------------------------------------------

    def checkpoint(self, kind: str = "manual", label: str = "") -> Entry | None:
        """Record the files that changed since the newest checkpoint.

        The first checkpoint is the baseline and records every file. Later ones
        answer None when nothing changed.
        """
        if kind not in KINDS:
            raise ValueError(f"kind must be one of {KINDS}, not {kind!r}")
        with self._lock:
            delta = self._scan()
            if not self.entries:
                return self._append("baseline", label or "Baseline", delta)
            if not delta:
                return None
            return self._append(kind, label, delta)

    def _scan(self) -> dict[str, str | None]:
        """`{path: digest or None}` of each file whose content differs from `state`."""
        tree = self._tree
        if tree is None or time.monotonic() - tree.listed_at > REFRESH_S:
            fresh = Tree(self.root, self._files(self.root), _covers, previous=tree)
            self._tree = fresh
            if tree is None:
                # A new process trusts no stamp: read every file and every recorded one.
                inside = os.path.join(self.root, "")
                candidates = dict.fromkeys(fresh.seen)
                candidates.update(dict.fromkeys(inside + rel for rel in self.state))
                return self._read(candidates)
        changed = self._tree.changes(_covers)
        delta = self._read(changed)
        self._tree.settle(changed)
        return delta

    def _read(self, paths: Iterable[str]) -> dict[str, str | None]:
        delta: dict[str, str | None] = {}
        for path in paths:
            rel = os.path.relpath(path, self.root).replace(os.sep, "/")
            if rel.startswith("../"):
                continue
            try:
                with open(path, "rb") as source:
                    data = source.read()
            except FileNotFoundError:
                if rel in self.state:
                    delta[rel] = None
                continue
            except (IsADirectoryError, PermissionError, OSError):
                continue
            sha = self.store.put(data)
            if self.state.get(rel) != sha:
                delta[rel] = sha
        return delta

    def _append(self, kind: str, label: str, files: Mapping[str, str | None]) -> Entry:
        entry = Entry(
            self.last + 1, self._clock(), kind, label, dict(sorted(files.items()))
        )
        self.path.parent.mkdir(parents=True, exist_ok=True)
        root_note = self._folder / "ROOT"
        if not root_note.exists():
            root_note.write_text(self.root + "\n", encoding="utf-8")
        line = json.dumps(
            {
                "seq": entry.seq,
                "at": entry.at,
                "kind": entry.kind,
                "label": entry.label,
                "files": entry.files,
            },
            separators=(",", ":"),
        )
        with open(self.path, "a", encoding="utf-8") as out:
            out.write(line + "\n")
            out.flush()
            os.fsync(out.fileno())
        self._apply(entry)
        return entry

    # Diffs -------------------------------------------------------------------

    def diff(
        self,
        start: int,
        end: int | None = None,
        paths: Iterable[str] | None = None,
        context: int = 3,
    ) -> list[FileDiff]:
        """How each file differs from checkpoint `start` to `end` (default: the newest).

        `context` is the number of unchanged lines around each change.
        """
        before = self.state_at(start)
        after = self.state if end is None else self.state_at(end)
        wanted = None if paths is None else set(paths)
        changed = sorted(
            rel
            for rel in set(before) | set(after)
            if before.get(rel) != after.get(rel) and (wanted is None or rel in wanted)
        )
        return [
            self.file_diff(rel, before.get(rel), after.get(rel), context)
            for rel in changed
        ]

    def changes_of(self, seq: int) -> list[FileDiff]:
        """What checkpoint `seq` itself changed."""
        entry = self.entry(seq)
        return self.diff(seq - 1, seq, entry.files)

    def against(self, ref: str, context: int = 3) -> list[FileDiff] | None:
        """How each file differs from git commit `ref`; None where git cannot tell.

        The new side is the newest checkpoint, so take a checkpoint first.
        """
        old = git_files(self.root, ref)
        if old is None:
            return None
        before = {
            rel: self.store.put(data) for rel, data in old.items() if data is not None
        }
        changed = sorted(rel for rel in old if before.get(rel) != self.state.get(rel))
        return [
            self.file_diff(rel, before.get(rel), self.state.get(rel), context)
            for rel in changed
        ]

    def file_diff(
        self, rel: str, before: str | None, after: str | None, context: int = 3
    ) -> FileDiff:
        """One file from content `before` to content `after` (digests; None is absent)."""
        status = (
            "added" if before is None else "deleted" if after is None else "modified"
        )
        old = b"" if before is None else self.store.get(before)
        new = b"" if after is None else self.store.get(after)
        if old is None or new is None:
            return FileDiff(
                rel,
                status,
                0,
                0,
                True,
                _header(rel, status) + "Binary files differ\n",
                before,
                after,
                "The store keeps no copy of a file this large.",
            )
        old_text, new_text = _text(old), _text(new)
        if old_text is None or new_text is None:
            return FileDiff(
                rel,
                status,
                0,
                0,
                True,
                _header(rel, status) + "Binary files differ\n",
                before,
                after,
            )
        added, removed = line_changes(old_text, new_text)
        if len(old) > MOST_DIFF_BYTES or len(new) > MOST_DIFF_BYTES:
            return FileDiff(
                rel,
                status,
                added,
                removed,
                False,
                "",
                before,
                after,
                "The file is too large to show its lines.",
            )
        return FileDiff(
            rel,
            status,
            added,
            removed,
            False,
            unified(rel, status, old_text, new_text, context),
            before,
            after,
        )

    # Undo --------------------------------------------------------------------

    def restore(self, seq: int, paths: Iterable[str] | None = None) -> Undone:
        """Bring the files back to how checkpoint `seq` left them; `paths` limits it."""
        with self._lock:
            self.checkpoint("outside", "Changes before an undo")
            if seq < 1 or seq > self.last:
                raise KeyError(f"no checkpoint {seq}")
            target = self.state_at(seq)
            wanted = None if paths is None else set(paths)
            plan = {
                rel: target.get(rel)
                for rel in sorted(set(target) | set(self.state))
                if target.get(rel) != self.state.get(rel)
                and (wanted is None or rel in wanted)
            }
            return self._write(plan, (), f"Restore to #{seq}")

    def revert(self, seq: int, paths: Iterable[str] | None = None) -> Undone:
        """Undo only what checkpoint `seq` changed; `paths` limits it.

        A file that changed again after `seq` is a conflict and stays as it is.
        """
        with self._lock:
            self.checkpoint("outside", "Changes before an undo")
            entry = self.entry(seq)
            if entry.kind == "baseline":
                raise ValueError(
                    "The baseline has no change to undo; restore a checkpoint instead."
                )
            before = self.state_at(seq - 1)
            wanted = None if paths is None else set(paths)
            plan, conflicts = {}, []
            for rel, after in entry.files.items():
                if wanted is not None and rel not in wanted:
                    continue
                if self.state.get(rel) == before.get(rel):
                    continue
                if self.state.get(rel) != after:
                    conflicts.append(rel)
                    continue
                plan[rel] = before.get(rel)
            return self._write(plan, tuple(conflicts), f"Undo #{seq}")

    def _write(self, plan: Mapping[str, str | None], conflicts, label: str) -> Undone:
        written: dict[str, str | None] = {}
        missing = []
        stamps = {}
        for rel, sha in plan.items():
            path = self._inside(rel)
            if sha is None:
                with contextlib.suppress(FileNotFoundError):
                    os.unlink(path)
            else:
                data = self.store.get(sha)
                if data is None:
                    missing.append(rel)
                    continue
                _write_file(path, data)
            written[rel] = sha
            stamps[path] = stamp_of(path)
        if self._tree is not None:
            self._tree.settle(stamps)
        entry = self._append("undo", label, written) if written else None
        return Undone(entry, tuple(written), tuple(conflicts), tuple(missing))

    def _inside(self, rel: str) -> str:
        path = os.path.realpath(os.path.join(self.root, rel))
        if not path.startswith(os.path.join(self.root, "")):
            raise ValueError(f"{rel} is outside {self.root}")
        return path


def _write_file(path: str, data: bytes) -> None:
    """Replace `path` with `data` in one step, keeping the mode of the old file."""
    folder = os.path.dirname(path)
    os.makedirs(folder, exist_ok=True)
    try:
        mode = os.stat(path).st_mode & 0o7777
    except FileNotFoundError:
        mode = None
    fd, temporary = tempfile.mkstemp(dir=folder, prefix=".vis-review-")
    try:
        with os.fdopen(fd, "wb") as out:
            out.write(data)
        if mode is not None:
            os.chmod(temporary, mode)
        else:
            os.chmod(temporary, 0o666 & ~_umask())
        os.replace(temporary, path)
    except BaseException:
        with contextlib.suppress(OSError):
            os.unlink(temporary)
        raise


def _git(root: str, *args: str, timeout_s: int = 60) -> str | None:
    """What `git args` printed in `root`, or None when it failed or is missing."""
    env = {"GIT_CONFIG_GLOBAL": os.devnull, "GIT_OPTIONAL_LOCKS": "0"}
    try:
        done = process.run(["git", *args], cwd=root, timeout_s=timeout_s, env=env)
    except (process.ToolMissing, process.ToolTimeout, OSError):
        return None
    return done.out if done.is_ok else None


def base_ref(root: str) -> str | None:
    """The commit where HEAD left the default branch, like T3 "Automatic"; None if unknown."""
    default = _git(root, "rev-parse", "--abbrev-ref", "origin/HEAD")
    names = [default.strip()] if default and default.strip() != "origin/HEAD" else []
    for name in [*names, "origin/main", "origin/master", "main", "master"]:
        base = _git(root, "merge-base", "HEAD", name)
        if base and base.strip():
            return base.strip()
    return None


def git_files(root: str, ref: str) -> dict[str, bytes | None] | None:
    """`{path: content at ref, or None}` of each file under `root` that differs from `ref`.

    Paths are relative to `root`. Git writes the old files into a temporary
    work tree and a temporary index, so the repository itself never changes.
    Returns None where `root` is no git work tree or `ref` is no commit.
    """
    prefix = _git(root, "rev-parse", "--show-prefix")
    top = _git(root, "rev-parse", "--show-toplevel")
    commit = _git(root, "rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}")
    if prefix is None or top is None or not commit:
        return None
    prefix, top = prefix.strip(), top.strip()
    status = _git(
        root, "diff", "--name-status", "-z", "--no-renames", "--relative", ref
    )
    others = _git(root, "ls-files", "-z", "--others", "--exclude-standard")
    if status is None or others is None:
        return None
    fields = status.split("\0")
    files: dict[str, bytes | None] = dict.fromkeys(filter(None, others.split("\0")))
    at_ref = [
        rel for code, rel in zip(fields[::2], fields[1::2]) if rel and code != "A"
    ]
    files.update(
        dict.fromkeys(rel for code, rel in zip(fields[::2], fields[1::2]) if rel)
    )
    if at_ref:
        old = _restored(top, commit.strip(), [prefix + rel for rel in at_ref], prefix)
        if old is None:
            return None
        files.update(old)
    return files


def _restored(
    top: str, commit: str, names: list[str], prefix: str
) -> dict[str, bytes] | None:
    """`{path under prefix: content}` of `names` at `commit`; None when git failed."""
    with tempfile.TemporaryDirectory(prefix="vis-review-") as work:
        tree = os.path.join(work, "tree")
        os.mkdir(tree)
        listed = os.path.join(work, "paths")
        Path(listed).write_bytes(b"\0".join(name.encode() for name in names))
        env = {
            "GIT_CONFIG_GLOBAL": os.devnull,
            "GIT_INDEX_FILE": os.path.join(work, "index"),
        }
        try:
            done = process.run(
                [
                    "git",
                    "--literal-pathspecs",
                    f"--work-tree={tree}",
                    "restore",
                    f"--source={commit}",
                    "--worktree",
                    "--pathspec-file-nul",
                    f"--pathspec-from-file={listed}",
                ],
                cwd=top,
                timeout_s=120,
                env=env,
                read_write=[work],
            )
        except (process.ToolMissing, process.ToolTimeout, OSError):
            return None
        if not done.is_ok:
            return None
        out: dict[str, bytes] = {}
        for name in names:
            with contextlib.suppress(OSError):
                out[name[len(prefix) :]] = Path(tree, name).read_bytes()
        return out


def _umask() -> int:
    mask = os.umask(0)
    os.umask(mask)
    return mask


def _text(data: bytes) -> str | None:
    if b"\0" in data[:_BINARY_PROBE]:
        return None
    try:
        return data.decode("utf-8")
    except UnicodeDecodeError:
        return None


def _header(rel: str, status: str) -> str:
    head = f"diff --git a/{rel} b/{rel}\n"
    if status == "added":
        head += "new file mode 100644\n"
    elif status == "deleted":
        head += "deleted file mode 100644\n"
    return head


def unified(rel: str, status: str, old: str, new: str, context: int = 3) -> str:
    """A git style patch of one file from `old` to `new`."""
    old_lines = _lines(old)
    new_lines = _lines(new)
    body = list(
        difflib.unified_diff(
            old_lines,
            new_lines,
            "/dev/null" if status == "added" else f"a/{rel}",
            "/dev/null" if status == "deleted" else f"b/{rel}",
            n=context,
        )
    )
    return _header(rel, status) + "".join(body)


def _lines(text: str) -> list[str]:
    lines = text.splitlines(keepends=True)
    if lines and not lines[-1].endswith(("\n", "\r")):
        lines[-1] += "\n\\ No newline at end of file\n"
    return lines
