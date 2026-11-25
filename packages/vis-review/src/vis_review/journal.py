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
from bisect import bisect_left
from collections import Counter, OrderedDict
from collections.abc import Callable, Iterable, Mapping
from dataclasses import dataclass, field, replace
from pathlib import Path

from vis_lang_interface import process
from vis_lang_interface.changes import line_changes
from vis_lang_interface.tree import Tree, listing, stamp_of

from vis_review.store import Store

KINDS = ("baseline", "outside", "block", "patch", "undo", "manual", "team")
# Kinds that the session itself wrote; "team" holds the changes of its subagents,
# "outside" the edits of others.
OWN_KINDS = ("block", "patch", "undo", "team")
# Kinds that record a step of the session. A step takes time, so its changes can
# hold the work of others: a pull, or the edit of a session that runs beside it.
STEP_KINDS = ("block", "patch")
# The most paths that one git command names; above it, git lists the whole tree.
MOST_NAMED_PATHS = 400
# Seconds after which the file list is read again, to see files that git now lists.
REFRESH_S = 300
# A text side larger than this shows no line diff; its counts still come from line_changes.
MOST_DIFF_BYTES = 1024 * 1024
_BINARY_PROBE = 8000
# Diffs that a journal keeps, by path, digests and context: a repaint diffs nothing again.
MOST_CACHED_DIFFS = 256
# Lines of both sides above which git computes the diff; difflib is quadratic there.
GIT_DIFF_LINES = 2000
# Lines of one side times lines of the other up to which difflib matches a region.
MOST_DIFFLIB_CELLS = 4_000_000
# Nested regions after which no more anchors are looked for.
MOST_DIFF_DEPTH = 64


@dataclass(frozen=True)
class Entry:
    """One checkpoint: the files that changed since the previous one."""

    seq: int
    at: float
    kind: str
    label: str
    files: Mapping[str, str | None] = field(default_factory=dict)
    turn: int | None = None
    iteration: int | None = None
    call: str | None = None
    commit: str | None = None
    started_at: float | None = None


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


def _entry_line(entry: Entry) -> str:
    extra = {
        "turn": entry.turn,
        "iteration": entry.iteration,
        "call": entry.call,
        "commit": entry.commit,
        "from": entry.started_at,
    }
    return (
        json.dumps(
            {
                "seq": entry.seq,
                "at": entry.at,
                "kind": entry.kind,
                "label": entry.label,
                "files": entry.files,
                **{key: value for key, value in extra.items() if value is not None},
            },
            separators=(",", ":"),
        )
        + "\n"
    )


def base_dir() -> Path:
    """`$VIS_HOME/review`, or `~/.vis/review`."""
    return Path(os.environ.get("VIS_HOME") or Path.home() / ".vis") / "review"


def _safe(key: str) -> str:
    return re.sub(r"[^A-Za-z0-9._-]", "_", key)[:120] or "default"


# The part of a journal key between the key of a leader and the key of its subagent.
TEAM_MARK = ".agent."


def member_key(leader: str, member: str) -> str:
    """The journal key of subagent `member` of leader `leader`.

    The journal of the leader reads the journals of these keys: the own changes of
    its subagents are its own changes too.
    """
    return f"{_safe(leader)}{TEAM_MARK}{_safe(member)}"


def _status(before: str | None, after: str | None) -> str:
    return "added" if before is None else "deleted" if after is None else "modified"


def _undo_label(rel: str) -> str:
    return f"Undo the own changes of {rel}"


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
        self._diffs: OrderedDict[tuple, FileDiff] = OrderedDict()
        self._diffs_lock = threading.Lock()
        # The time and the git HEAD of the last scan: where the next step starts.
        self._scanned_at: float | None = None
        self._head: str | None = None
        # The git directory of the root: None before the first look, "" without git.
        self._git_dir: str | None = None
        # The git directory that holds the branches: another one in a linked work tree.
        self._common_dir = ""
        # Git blob names by content digest. A content never changes, so nor does its name.
        self._oids: dict[str, str] = {}
        # Merged contents by the digests of their three sides; None when the sides conflict.
        self._merges: OrderedDict[tuple, str | None] = OrderedDict()
        # The own changes of each subagent journal, by its path, with the stamp of the read.
        self._members: dict[
            Path, tuple[tuple, list[tuple[float, float, str, str | None]]]
        ] = {}
        # Own spans and their rendered diffs, by journal and team revision.
        self._own: tuple | None = None
        self._own_views: OrderedDict[tuple, tuple[tuple[FileDiff, bool], ...]] = (
            OrderedDict()
        )
        self._merge_checked: tuple | None = None
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
                    turn=raw.get("turn"),
                    iteration=raw.get("iteration"),
                    call=raw.get("call"),
                    commit=raw.get("commit"),
                    started_at=raw.get("from"),
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

    def checkpoint(
        self,
        kind: str = "manual",
        label: str = "",
        wrote: Callable[[str], bool] | None = None,
        *,
        started_at: float | None = None,
        ended_at: float | None = None,
        turn: int | None = None,
        iteration: int | None = None,
        call: str | None = None,
    ) -> Entry | None:
        """Record the files that changed since the newest checkpoint.

        The first checkpoint is the baseline and records every file. Later ones
        answer None when nothing changed. `wrote` is true for a path that the step
        shows it wrote; see `_of_others`. With a time window, a newer file is
        left for the next checkpoint. Call identifiers also apply to outside
        changes that this checkpoint observes.
        """
        if kind not in KINDS:
            raise ValueError(f"kind must be one of {KINDS}, not {kind!r}")
        with self._lock:
            since, head = self._scanned_at, self._head
            try:
                return self._checkpoint(
                    kind,
                    label,
                    wrote,
                    since,
                    head,
                    started_at,
                    ended_at,
                    {"turn": turn, "iteration": iteration, "call": call},
                )
            except BaseException:
                # The scan settled the file stamps before the checkpoint was written.
                # Forget them: the next checkpoint reads every file and loses no change.
                self._tree = None
                self._scanned_at, self._head = since, head
                raise

    def _checkpoint(self, kind, label, wrote, since, head, start, end, metadata):
        # A scan can have a time window only for a tool call.
        timed = kind in STEP_KINDS and start is not None
        delta, stamps, deferred = self._scan(end if timed else None)
        self._scanned_at = end if timed and end is not None else self._clock()
        self._head = self._head_now()
        between = set()
        if timed:
            # A deletion has no time. The step keeps a deletion that its code names,
            # and one of a file that the session changed before: a shell command
            # such as `rm -rf out` names no file.
            gone = [rel for rel in delta if not stamps[rel]]
            own = self._own_changes() if gone and self.entries else {}
            between = {
                rel
                for rel in delta
                if (
                    stamps[rel][0] < start * 1_000_000_000
                    if stamps[rel]
                    else not (wrote and wrote(rel)) and rel not in own
                )
            }
        baseline_entry = None
        if not self.entries:
            # There is no preimage before the first after hook. Git supplies
            # it for tracked paths; do not claim an edit with no known preimage.
            old = git_files(self.root, "HEAD") if timed else None
            if old is None:
                return self._append(
                    "baseline", label or "Baseline", delta, at=start, **metadata
                )
            for rel, data in old.items():
                if data is None or rel in delta or rel in deferred:
                    continue
                try:
                    os.stat(self._inside(rel))
                except FileNotFoundError:
                    delta[rel] = None
                    if wrote is None or not wrote(rel):
                        between.add(rel)
                except OSError:
                    continue
            # A newer tracked file can match HEAD and so be absent from git diff.
            unknown = deferred - old.keys()
            if unknown:
                place = _place(self.root, "HEAD")
                if place is not None:
                    prefix, top, commit = place
                    tracked = self._oids_at(commit, sorted(unknown))
                    paths = sorted(unknown & tracked.keys()) if tracked else []
                    if paths:
                        old.update(
                            _restored(
                                top, commit, [prefix + rel for rel in paths], prefix
                            )
                            or {}
                        )
            baseline = {
                rel: sha
                for rel, sha in delta.items()
                if rel in between or rel not in old
            }
            for rel in (set(delta) - between) | deferred:
                data = old.get(rel)
                if data is not None:
                    baseline[rel] = self.store.put(data)
                elif rel in old:
                    baseline.pop(rel, None)
            baseline_entry = self._append(
                "baseline", "Baseline", baseline, at=start, **metadata
            )
            delta = {
                rel: sha for rel, sha in delta.items() if self.state.get(rel) != sha
            }
        if not delta:
            return baseline_entry
        start = start if timed else since
        ours = self._of_team(delta, since, wrote if kind in STEP_KINDS else None)
        if ours:
            team = self._append(
                "team",
                "Changes of subagents",
                {rel: delta.pop(rel) for rel in ours},
                start,
                at=end,
                **metadata,
            )
            if not delta:
                return team
        if kind in STEP_KINDS:
            theirs = self._of_others(delta, start, head, wrote) | (
                between & delta.keys()
            )
            if theirs:
                other = self._append(
                    "outside",
                    "Changes of others during a step",
                    {rel: delta.pop(rel) for rel in theirs},
                    at=end if timed else None,
                    **metadata,
                )
                if not delta:
                    return other
        return self._append(
            kind,
            label,
            delta,
            start if kind in STEP_KINDS else None,
            at=end,
            **metadata,
        )

    def _head_now(self) -> str | None:
        """The commit of HEAD. The reflog gives it without a git process."""
        if not self._git_dirs():
            return None
        return _reflog_head(os.path.join(self._git_dir, "logs", "HEAD")) or _line(
            _git(self.root, "rev-parse", "--verify", "--quiet", "HEAD")
        )

    def _git_dirs(self) -> bool:
        """Find the git directories of the root one time. False without git."""
        if self._git_dir is None:
            both = _git(
                self.root,
                "rev-parse",
                "--path-format=absolute",
                "--git-dir",
                "--git-common-dir",
            )
            rows = (both or "").split("\n")
            if len(rows) >= 2 and rows[0] and rows[1]:
                self._git_dir, self._common_dir = rows[0], rows[1]
            else:
                # A git before 2.31 does not know `--path-format`.
                found = _line(_git(self.root, "rev-parse", "--absolute-git-dir"))
                self._git_dir = self._common_dir = found or ""
        return bool(self._git_dir)

    def refs_stamp(self) -> tuple:
        """A value that changes when HEAD or a base branch moves; `()` without git.

        A commit, a push and a fetch change what git shows, and they change no file
        of the root. The stamp reads only file times, so each step can ask for it.
        """
        if not self._git_dirs():
            return ()
        common = self._common_dir
        paths = [
            os.path.join(self._git_dir, "logs", "HEAD"),
            os.path.join(common, "packed-refs"),
        ]
        for ref in _BASE_REFS.values():
            paths += [os.path.join(common, ref), os.path.join(common, "logs", ref)]
        return tuple(_file_stamp(path) for path in paths)

    def _of_others(
        self,
        delta: Mapping[str, str | None],
        since: float | None,
        head: str | None,
        wrote: Callable[[str], bool] | None = None,
    ) -> set[str]:
        """The paths of `delta` that the step did not write itself.

        Git brought a file when HEAD moved to a commit that has the file as it is
        now, and no commit made during the step changed it. Another session wrote
        a file when its journal recorded the same content as its own change
        during the step. Two steps that run together both see such a file, and
        the journals do not name its writer. So a session keeps a file that it
        changed before, and a file that the step shows it wrote (`wrote`).
        """
        if since is None:
            since = self.entries[-1].at
        theirs = self._brought_by_git(delta, since, head)
        claimed = self._peer_claims(since)
        contested = {rel for rel in delta if (rel, delta[rel]) in claimed} - theirs
        if contested:
            before = self._own_changes()
            theirs |= {
                rel
                for rel in contested - before.keys()
                if wrote is None or not wrote(rel)
            }
        return theirs

    def _brought_by_git(
        self, delta: Mapping[str, str | None], since: float, head: str | None
    ) -> set[str]:
        new = self._head
        if not head or not new or head == new:
            return set()
        names = ("--name-only", "-z", "--no-renames", "--relative")
        moved = _names(_git(self.root, "diff", *names, head, new))
        fresh = _names(
            _git(
                self.root,
                "log",
                f"--since=@{int(since)}",
                "--format=",
                *names,
                f"{head}..{new}",
            )
        )
        if moved is None or fresh is None:
            return set()
        maybe = sorted((set(delta) & moved) - fresh)
        at_new = self._oids_at(new, maybe) if maybe else {}
        if at_new is None:
            return set()
        return {rel for rel in maybe if at_new.get(rel) == self._oid(delta[rel])}

    def _of_team(
        self,
        delta: Mapping[str, str | None],
        since: float | None,
        wrote: Callable[[str], bool] | None = None,
    ) -> set[str]:
        """The paths of `delta` that a subagent journal recorded as its own change since `since`.

        A path that the step shows it wrote (`wrote`) stays with the step.
        """
        claimed = self._team_claims(self.entries[-1].at if since is None else since)
        return {
            rel
            for rel in delta
            if (rel, delta[rel]) in claimed and (wrote is None or not wrote(rel))
        }

    def _team_claims(self, since: float | None = None) -> set[tuple[str, str | None]]:
        """`(path, digest)` of each own change of a subagent journal, since `since` or ever."""
        return {
            (rel, sha)
            for rel, changes in self._team_changes().items()
            for _start, at, sha in changes
            if since is None or at >= since
        }

    def team_stamp(self) -> tuple:
        """A value that changes when a subagent journal of this session changes."""
        stamps = []
        for path in self._members_paths():
            try:
                info = path.stat()
            except OSError:
                continue
            stamps.append((path.name, info.st_mtime_ns, info.st_size))
        return tuple(sorted(stamps))

    def _members_paths(self) -> list[Path]:
        try:
            return list(self._folder.glob(_safe(self.key) + TEAM_MARK + "*.jsonl"))
        except OSError:
            return []

    def _team_changes(self) -> dict[str, list[tuple[float, float, str | None]]]:
        """`{path: [(step start, step end, digest)]}` of the own changes of the subagent journals."""
        changes: dict[str, list[tuple[float, float, str | None]]] = {}
        for path in self._members_paths():
            for start, at, rel, sha in self._member_changes(path):
                changes.setdefault(rel, []).append((start, at, sha))
        return changes

    def _member_changes(self, path: Path) -> list[tuple[float, float, str, str | None]]:
        """`(step start, step end, path, digest)` of each own change in the subagent journal at `path`.

        A step starts at its `from` time, else at the previous checkpoint of the journal.
        """
        try:
            info = path.stat()
            stamp = (info.st_mtime_ns, info.st_size)
            with self._lock:
                known = self._members.get(path)
            if known and known[0] == stamp:
                return known[1]
            lines = path.read_text(encoding="utf-8").splitlines()
        except OSError:
            return []
        changes = []
        start = None
        for line in lines:
            try:
                raw = json.loads(line)
                at = float(raw["at"])
                if raw["kind"] in OWN_KINDS:
                    begin = float(raw.get("from", at if start is None else start))
                    changes.extend(
                        (begin, at, rel, sha)
                        for rel, sha in (raw.get("files") or {}).items()
                    )
                start = at
            except (ValueError, KeyError, TypeError, AttributeError):
                continue
        with self._lock:
            self._members[path] = (stamp, changes)
        return changes

    @staticmethod
    def _team_step(
        changes: list[tuple[float, float, str | None]] | None,
        since: float | None,
        at: float,
        old: str | None,
        new: str | None,
    ) -> tuple[str | None] | None:
        """`(digest,)` that a subagent wrote in a change of others from `old` to `new`, or None.

        The digest is None for a deleted file.

        The change was seen at `at`, and the checkpoint before it at `since`. A
        subagent step that ran in that time and wrote `new` made the change. A
        subagent step that ended in that time and wrote another content made the
        change first, and others edited the file after it.
        """
        if not changes:
            return None
        ran = [
            (end, sha)
            for start, end, sha in changes
            if start <= at and (since is None or end >= since)
        ]
        if any(sha == new for _end, sha in ran):
            return (new,)
        ended = [(end, sha) for end, sha in ran if end <= at and sha != old]
        return (max(ended, key=lambda e: e[0])[1],) if ended else None

    def _peer_claims(self, since: float) -> set[tuple[str, str | None]]:
        """`(path, digest)` of each own change that another journal of this root recorded since `since`."""
        claims: set[tuple[str, str | None]] = set()
        try:
            peers = [path for path in self._folder.glob("*.jsonl") if path != self.path]
        except OSError:
            return claims
        for path in peers:
            try:
                if path.stat().st_mtime < since - 1:
                    continue
                lines = path.read_text(encoding="utf-8").splitlines()
            except OSError:
                continue
            for line in reversed(lines):
                try:
                    raw = json.loads(line)
                    if float(raw["at"]) < since:
                        break
                    if raw["kind"] in OWN_KINDS:
                        claims.update(dict(raw.get("files") or {}).items())
                except (ValueError, KeyError, TypeError):
                    continue
        return claims

    def _oid(self, sha: str | None) -> str | None:
        """The git blob name of the content with digest `sha`; "" when the store has no copy."""
        if sha is None:
            return None
        found = self._oids.get(sha)
        if found is None:
            data = self.store.get(sha)
            if data is None:
                return ""
            found = hashlib.sha1(
                b"blob %d\0" % len(data) + data, usedforsecurity=False
            ).hexdigest()
            self._oids[sha] = found
        return found

    def _oids_at(self, commit: str, rels: list[str]) -> dict[str, str] | None:
        """`{path: git blob name}` of `rels` at `commit`; None when git failed."""
        named = rels if len(rels) <= MOST_NAMED_PATHS else []
        listed = _git(self.root, "ls-tree", "-r", "-z", commit, "--", *named)
        if listed is None:
            return None
        found = {}
        for row in filter(None, listed.split("\0")):
            head, _, rel = row.partition("\t")
            found[rel] = head.split()[2]
        return found

    def _scan(self, ended_at: float | None = None):
        """Read only paths with changed stamps; leave newer paths unsettled."""
        tree = self._tree
        if tree is None or time.monotonic() - tree.listed_at > REFRESH_S:
            files = self._files(self.root)
            if tree is not None:
                # A removed path may no longer be listed at this refresh.
                files = dict.fromkeys(os.fspath(path) for path in files)
                files.update(
                    dict.fromkeys(os.path.join(self.root, rel) for rel in self.state)
                )
            self._tree = Tree(self.root, files, _covers, previous=tree)
        if tree is None:
            # A new process trusts no stamp: read every file and every recorded one.
            candidates = dict(self._tree.seen)
            for rel in self.state:
                path = os.path.join(self.root, rel)
                if path not in candidates:
                    candidates[path] = stamp_of(path)
        else:
            candidates = self._tree.changes(_covers)
        limit = None if ended_at is None else ended_at * 1_000_000_000
        delta: dict[str, str | None] = {}
        stamps = {}
        settled = {}
        deferred = set()
        for path, stamp in candidates.items():
            rel = os.path.relpath(path, self.root).replace(os.sep, "/")
            if rel.startswith("../"):
                continue
            if limit is not None and stamp is not None and stamp[0] > limit:
                deferred.add(rel)
                if tree is None:
                    self._tree.seen[path] = None
                continue
            if stamp is None:
                try:
                    os.stat(path)
                except FileNotFoundError:
                    if rel in self.state:
                        delta[rel] = None
                        stamps[rel] = None
                except OSError:
                    self._tree.seen[path] = None
                    continue
                else:
                    self._tree.seen[path] = None
                    continue
            else:
                try:
                    with open(path, "rb") as source:
                        data = source.read()
                except OSError:
                    # A file disturbed during the scan still needs a scan.
                    self._tree.seen[path] = None
                    continue
                if stamp_of(path) != stamp:
                    deferred.add(rel)
                    self._tree.seen[path] = None
                    continue
                sha = self.store.put(data)
                if self.state.get(rel) != sha:
                    delta[rel] = sha
                    stamps[rel] = stamp
            settled[path] = stamp
        self._tree.settle(settled)
        return delta, stamps, deferred

    def _append(
        self,
        kind: str,
        label: str,
        files: Mapping[str, str | None],
        start: float | None = None,
        *,
        at: float | None = None,
        turn: int | None = None,
        iteration: int | None = None,
        call: str | None = None,
    ) -> Entry:
        """Write one checkpoint. `start` is the time when a step started, if known."""
        entry = Entry(
            self.last + 1,
            self._clock() if at is None else at,
            kind,
            label,
            dict(sorted(files.items())),
            turn=turn,
            iteration=iteration,
            call=call,
            started_at=start,
        )
        self.path.parent.mkdir(parents=True, exist_ok=True)
        root_note = self._folder / "ROOT"
        if not root_note.exists():
            root_note.write_text(self.root + "\n", encoding="utf-8")
        with self.path.open("a", encoding="utf-8") as out:
            out.write(_entry_line(entry))
            out.flush()
            os.fsync(out.fileno())
        self._apply(entry)
        return entry

    def snapshots(self) -> list[Entry]:
        """The changes after the baseline or the last merge, oldest first."""
        with self._lock:
            return [e for e in self.entries if e.kind not in ("baseline", "merge")]

    def merge_point(self) -> Entry | None:
        """The most recent merge, if there is one."""
        with self._lock:
            first = self.entries[0] if self.entries else None
            return first if first is not None and first.kind == "merge" else None

    def refresh_merge(self) -> Entry | None:
        """Prune when the tip of the default branch has all the own contents."""
        with self._lock:
            key, spans, _mixed = self._own_state()
            if not spans:
                return None
            stamp = (key, self.refs_stamp())
            if stamp == self._merge_checked:
                return None
            base = base_ref(self.root)
            if base is None:
                self._merge_checked = stamp
                return None
            place = _place(self.root, base.branch)
            if place is None:
                return None
            commit = place[2]
            at_ref = self._oids_at(commit, list(spans))
            if at_ref is None:
                return None
            if self._matches(at_ref, spans):
                return self._compact(self._last_own_seq(), commit)
            self._merge_checked = stamp
            return None

    def mark_merged(self, commit: str) -> Entry | None:
        """Replace the snapshots present in `commit` with a single merge point."""
        with self._lock:
            place = _place(self.root, commit)
            if place is None:
                return None
            commit = place[2]
            previous = self.merge_point()
            if previous is not None and previous.commit == commit:
                return None
            at_ref = self._oids_at(commit, [])
            if at_ref is None:
                return None
            spans = self._own_changes()
            if spans and self._matches(at_ref, spans):
                return self._compact(self._last_own_seq(), commit)
            # A commit can contain an earlier checkpoint while a later one is open.
            for index in range(len(self.entries) - 2, 0, -1):
                entry = self.entries[index]
                if entry.kind not in OWN_KINDS and entry.kind != "outside":
                    continue
                spans, _mixed = self._own_spans(self.entries[: index + 1])
                if spans and self._matches(at_ref, spans):
                    return self._compact(entry.seq, commit)
            return None

    def _last_own_seq(self) -> int:
        """The last checkpoint with an own change, including a subagent's change."""
        team = self._team_changes()
        for index in range(len(self.entries) - 1, 0, -1):
            entry = self.entries[index]
            if entry.kind in OWN_KINDS:
                return entry.seq
            if entry.kind == "outside":
                older = self.entries[index - 1].at
                before = self.state_at(entry.seq - 1) if team else {}
                if any(
                    self._team_step(
                        team.get(rel), older, entry.at, before.get(rel), sha
                    )
                    is not None
                    for rel, sha in entry.files.items()
                ):
                    return entry.seq
        return self.last

    def _matches(self, at_ref, spans) -> bool:
        return all(
            at_ref.get(rel) == self._oid(after)
            for rel, (_before, after) in spans.items()
        )

    def _compact(self, seq: int, commit: str) -> Entry:
        """Replace the old history in one rename, then remove unreferenced blobs."""
        mark = Entry(
            seq,
            self.entry(seq).at,
            "merge",
            "Merged",
            self.state_at(seq),
            commit=commit,
        )
        retained = [mark, *(entry for entry in self.entries if entry.seq > seq)]
        fd, temporary = tempfile.mkstemp(dir=self.path.parent, prefix=".merge-")
        try:
            with os.fdopen(fd, "w", encoding="utf-8") as out:
                for entry in retained:
                    out.write(_entry_line(entry))
                out.flush()
                os.fsync(out.fileno())
            os.replace(temporary, self.path)
        except BaseException:
            with contextlib.suppress(FileNotFoundError):
                os.unlink(temporary)
            # If the rename finished before an interrupt, use the durable state.
            self.entries, self.state = [], {}
            self._load()
            self._own = self._merge_checked = None
            self._own_views.clear()
            raise
        self.entries = retained
        self._own = self._merge_checked = None
        self._own_views.clear()
        self.store.sweep()
        return mark

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
        return [] if entry.kind == "merge" else self.diff(seq - 1, seq, entry.files)

    def own_diff(self, context: int = 3) -> list[FileDiff]:
        """Diff the first before and last after of each path with an own change."""
        with self._lock:
            key, spans, mixed = self._own_state()
            return self._own_view((key, context), spans, mixed, context)

    def own_files(
        self, ref: str | None = None
    ) -> list[tuple[str, str, str | None, str | None]] | None:
        """List own file changes without building patches.

        With `ref`, omit changes that are already in that commit. Return None
        when git cannot read the ref.
        """
        commit = None
        if ref is not None:
            place = _place(self.root, ref)
            if place is None:
                return None
            commit = place[2]
        with self._lock:
            spans = self._own_changes()
            if commit is not None:
                spans = self._open_spans(commit, spans)
                if spans is None:
                    return None
            return [
                (rel, _status(before, after), before, after)
                for rel, (before, after) in spans.items()
            ]

    def own_file_diff(
        self, rel: str, before: str | None, after: str | None, context: int = 3
    ) -> FileDiff:
        """Build a patch for one `own_files` row, with its outside-edit note."""
        with self._lock:
            _key, spans, mixed = self._own_state()
            has_outside = rel in mixed and spans.get(rel) == (before, after)
        return self._own_patch(rel, before, after, context, has_outside)

    def _own_patch(self, rel, before, after, context, has_outside) -> FileDiff:
        diff = self.file_diff(rel, before, after, context)
        if has_outside:
            note = "This diff includes changes made between this session's edits."
            diff = replace(diff, note=" ".join(filter(None, (diff.note, note))))
        return diff

    def own_paths(self) -> list[str]:
        """The paths that the session itself changed, in order."""
        return list(self._own_changes())

    def _own_changes(self) -> dict[str, tuple[str | None, str | None]]:
        with self._lock:
            return self._own_state()[1]

    def _own_state(self):
        key = (self.last, self.team_stamp())
        if self._own is None or self._own[0] != key:
            spans, mixed = self._own_spans(self.entries)
            self._own = (key, spans, mixed)
        return self._own

    def _own_spans(self, entries):
        state = {}
        spans = {}
        mixed = set()
        outside = set()
        since = None
        team = self._team_changes()
        for entry in entries:
            for rel, sha in entry.files.items():
                old = state.get(rel)
                step = (
                    self._team_step(team.get(rel), since, entry.at, old, sha)
                    if entry.kind == "outside"
                    else None
                )
                if entry.kind == "undo" and entry.label == _undo_label(rel):
                    spans.pop(rel, None)
                    mixed.discard(rel)
                    outside.discard(rel)
                elif entry.kind in OWN_KINDS or step is not None:
                    spans[rel] = (
                        spans.get(rel, (old,))[0],
                        sha if step is None else step[0],
                    )
                    if rel in outside:
                        mixed.add(rel)
                        outside.discard(rel)
                if (
                    rel in spans
                    and entry.kind not in OWN_KINDS
                    and (step is None or step[0] != sha)
                ):
                    outside.add(rel)
                state[rel] = sha
            since = entry.at
        return (
            {rel: span for rel, span in sorted(spans.items()) if span[0] != span[1]},
            mixed,
        )

    def own_open(self, ref: str, context: int = 3) -> list[FileDiff] | None:
        """Own snapshot changes whose final content is not in commit `ref`."""
        place = _place(self.root, ref)
        if place is None:
            return None
        with self._lock:
            key, spans, mixed = self._own_state()
            commit = place[2]
            cache_key = (key, context, commit)
            if cache_key not in self._own_views:
                spans = self._open_spans(commit, spans)
                if spans is None:
                    return None
            return self._own_view(cache_key, spans, mixed, context)

    def _open_spans(self, commit, spans):
        """Keep only spans whose after-content differs from `commit`."""
        at_ref = self._oids_at(commit, list(spans)) if spans else {}
        if at_ref is None:
            return None
        return {
            rel: span
            for rel, span in spans.items()
            if (
                span[1] is not None
                if rel not in at_ref
                else at_ref[rel] != self._oid(span[1])
            )
        }

    def _own_view(self, key, spans, mixed, context):
        found = self._own_views.get(key)
        if found is None:
            # A new checkpoint may change only one span. Keep the diffs that
            # still have the same sides, even when this view exceeds the LRU.
            known = {
                (diff.path, diff.before, diff.after): (diff, has_outside)
                for previous, view in self._own_views.items()
                if previous[1] == context
                for diff, has_outside in view
            }
            result = []
            for rel, (before, after) in spans.items():
                has_outside = rel in mixed
                cached = known.get((rel, before, after))
                if cached is not None and cached[1] == has_outside:
                    diff = cached[0]
                else:
                    diff = self._own_patch(rel, before, after, context, has_outside)
                result.append((diff, has_outside))
            found = tuple(result)
            self._own_views[key] = found
            if len(self._own_views) > 2:
                self._own_views.popitem(last=False)
        else:
            self._own_views.move_to_end(key)
        return [diff for diff, _ in found]

    def _carry(
        self, span: tuple[str | None, str | None], old: str | None, new: str | None
    ) -> tuple[str | None, str | None]:
        """`span` with the edit of another session from `old` to `new` on both sides.

        The span stays as it is when the edit changes the lines of an own change.
        """
        before, after = span
        merged = (
            self._merged(old, before, new),
            new if after == old else self._merged(old, after, new),
        )
        return span if None in merged else merged

    def _merged(
        self, base: str | None, ours: str | None, theirs: str | None
    ) -> str | None:
        """The digest of `ours` with the change from `base` to `theirs`; None on a conflict."""
        if ours == base:
            return theirs
        if theirs == base:
            return ours
        key = (base, ours, theirs)
        if key in self._merges:
            return self._merges[key]
        lines = [self._lines_of(sha) for sha in key]
        found = None
        if None not in lines:
            merged = _merge(
                lines[0], _hunks(lines[0], lines[1]), _hunks(lines[0], lines[2])
            )
            if merged is not None:
                found = self.store.put("".join(merged).encode("utf-8"))
        self._merges[key] = found
        while len(self._merges) > MOST_CACHED_DIFFS:
            self._merges.popitem(last=False)
        return found

    def _lines_of(self, sha: str | None) -> list[str] | None:
        """The lines of the text with digest `sha`; None when absent, binary or too large."""
        data = None if sha is None else self.store.get(sha)
        if data is None or len(data) > MOST_DIFF_BYTES:
            return None
        text = _text(data)
        return None if text is None else text.splitlines(keepends=True)

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
        key = (rel, before, after, context)
        with self._diffs_lock:
            found = self._diffs.get(key)
            if found is not None:
                self._diffs.move_to_end(key)
                return found
        found = self._file_diff(rel, before, after, context)
        with self._diffs_lock:
            self._diffs[key] = found
            while len(self._diffs) > MOST_CACHED_DIFFS:
                self._diffs.popitem(last=False)
        return found

    def _file_diff(
        self, rel: str, before: str | None, after: str | None, context: int
    ) -> FileDiff:
        status = _status(before, after)
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
            self.entry(seq)
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
            if entry.kind in ("baseline", "merge"):
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

    def undo_own(self, rel: str) -> Undone:
        """Take the own changes out of `rel` and keep the edits of others in it.

        A file where another session changed the lines of an own change is a
        conflict and stays as it is.
        """
        with self._lock:
            self.checkpoint("outside", "Changes before an undo")
            if rel not in self._own_changes():
                return Undone(None, ())
            span = self._undo_span(rel)
            if span is None or span[1] != self.state.get(rel):
                return Undone(None, (), (rel,))
            return self._write({rel: span[0]}, (), _undo_label(rel))

    def _undo_span(self, rel: str) -> tuple[str | None, str | None] | None:
        """Apply the outside edits to both sides for this file only."""
        team = self._team_changes().get(rel)
        span = None
        old = None
        since = None
        for entry in self.entries:
            if rel in entry.files:
                sha = entry.files[rel]
                step = (
                    self._team_step(team, since, entry.at, old, sha)
                    if entry.kind == "outside"
                    else None
                )
                if entry.kind == "undo" and entry.label == _undo_label(rel):
                    span = None
                elif entry.kind in OWN_KINDS or step is not None:
                    span = (
                        (span[0] if span else old),
                        sha if step is None else step[0],
                    )
                    if step is not None and step[0] != sha:
                        span = self._carry(span, step[0], sha)
                elif span is not None:
                    span = self._carry(span, old, sha)
                old = sha
            since = entry.at
        return span

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


def _line(out: str | None) -> str | None:
    """The one line that git printed, or None."""
    return (out or "").strip() or None


def _names(out: str | None) -> set[str] | None:
    """The paths of a NUL separated git list; None when git failed."""
    if out is None:
        return None
    return {name.strip("\n") for name in out.split("\0")} - {""}


def _reflog_head(path: str) -> str | None:
    """The commit that the newest line of the reflog at `path` moved to, or None."""
    try:
        with open(path, "rb") as log:
            size = log.seek(0, os.SEEK_END)
            span = 1024
            while True:
                log.seek(max(0, size - span))
                tail = log.read().rstrip(b"\n")
                if b"\n" in tail or span >= size:
                    break
                span *= 4
    except OSError:
        return None
    fields = tail.rpartition(b"\n")[2].split(b" ", 2)
    new = fields[1].decode("ascii", "replace") if len(fields) == 3 else ""
    return new if len(new) >= 40 and set(new) <= set("0123456789abcdef") else None


def _place(root: str, ref: str) -> tuple[str, str, str] | None:
    """`(prefix of root in the work tree, top of the work tree, commit of ref)`, or None."""
    out = _git(
        root, "rev-parse", "--show-prefix", "--show-toplevel", f"{ref}^{{commit}}"
    )
    rows = (out or "").split("\n")
    if len(rows) < 3 or not rows[1] or not rows[2]:
        return None
    return rows[0], rows[1], rows[2]


# The branches that can be the base, in order, and the git ref of each.
_BASE_REFS = {
    "origin/HEAD": "refs/remotes/origin/HEAD",
    "origin/main": "refs/remotes/origin/main",
    "origin/master": "refs/remotes/origin/master",
    "main": "refs/heads/main",
    "master": "refs/heads/master",
}


def _file_stamp(path: str) -> tuple[int, int] | None:
    """`(time, size)` of the file at `path`, or None when there is no file."""
    try:
        stat = os.stat(path)
    except OSError:
        return None
    return stat.st_mtime_ns, stat.st_size


@dataclass(frozen=True)
class Base:
    """The commit where HEAD left the default branch, and the name of that branch."""

    commit: str
    branch: str

    @property
    def label(self) -> str:
        """How a person reads the base, for example "origin/main@362588b1"."""
        return f"{self.branch}@{self.commit[:8]}"


def base_ref(root: str) -> Base | None:
    """Where HEAD left the default branch, for "Automatic"; None if unknown."""
    for name in _BASE_REFS:
        commit = _line(_git(root, "merge-base", "HEAD", name))
        if commit:
            branch = name
            if name == "origin/HEAD":
                branch = _line(_git(root, "rev-parse", "--abbrev-ref", name)) or name
            return Base(commit, branch)
    return None


def git_files(root: str, ref: str) -> dict[str, bytes | None] | None:
    """`{path: content at ref, or None}` of each file under `root` that differs from `ref`.

    Paths are relative to `root`. Git writes the old files into a temporary
    work tree and a temporary index, so the repository itself never changes.
    Returns None where `root` is no git work tree or `ref` is no commit.
    """
    place = _place(root, ref)
    if place is None:
        return None
    prefix, top, commit = place
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
        old = _restored(top, commit, [prefix + rel for rel in at_ref], prefix)
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
    if len(old_lines) + len(new_lines) > GIT_DIFF_LINES:
        hunks = _git_hunks(old, new, context)
        if hunks is not None:
            names = (
                "/dev/null" if status == "added" else f"a/{rel}",
                "/dev/null" if status == "deleted" else f"b/{rel}",
            )
            lines = f"--- {names[0]}\n+++ {names[1]}\n" if hunks else ""
            return _header(rel, status) + lines + hunks
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


def _git_hunks(old: str, new: str, context: int) -> str | None:
    """The hunks of `git diff --no-index` from `old` to `new`; None without git."""
    with tempfile.TemporaryDirectory(prefix="vis-review-diff-") as work:
        paths = [os.path.join(work, "old"), os.path.join(work, "new")]
        for path, text in zip(paths, (old, new)):
            Path(path).write_text(text, encoding="utf-8", newline="")
        command = ["git", "diff", "--no-index", "--no-color", "--no-ext-diff"]
        command += ["--text", f"-U{context}", "--", *paths]
        env = {"GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_NOSYSTEM": "1"}
        try:
            done = process.run(command, timeout_s=60, env=env, read_write=[work])
        except (process.ToolMissing, process.ToolTimeout, OSError):
            return None
    # Exit 1 means the files differ; 0 means they are the same.
    if done.exit_code not in (0, 1):
        return None
    start = done.out.find("\n@@ ")
    return "" if start < 0 else done.out[start + 1 :]


def _hunks(old: list[str], new: list[str]) -> list[tuple[int, int, tuple[str, ...]]]:
    """`(start, end, new lines)` of each block of `old` that `new` changes.

    Difflib matches a small region. It is quadratic, so lines that each side of a
    larger region holds once split it first, as in a patience diff. A large region
    without such a line stays one changed block.
    """
    pairs: list[tuple[int, int]] = []
    _match(old, 0, len(old), new, 0, len(new), pairs, 0)
    hunks = []
    i = j = 0
    for k, m in [*pairs, (len(old), len(new))]:
        if k > i or m > j:
            hunks.append((i, k, tuple(new[j:m])))
        i, j = k + 1, m + 1
    return hunks


def _match(old, lo, hi, new, blo, bhi, pairs, depth) -> None:
    """Append the equal line pairs of `old[lo:hi]` and `new[blo:bhi]` to `pairs`, in order."""
    while lo < hi and blo < bhi and old[lo] == new[blo]:
        pairs.append((lo, blo))
        lo, blo = lo + 1, blo + 1
    tail = []
    while lo < hi and blo < bhi and old[hi - 1] == new[bhi - 1]:
        hi, bhi = hi - 1, bhi - 1
        tail.append((hi, bhi))
    if lo < hi and blo < bhi:
        if (hi - lo) * (bhi - blo) <= MOST_DIFFLIB_CELLS:
            matcher = difflib.SequenceMatcher(
                None, old[lo:hi], new[blo:bhi], autojunk=False
            )
            for a, b, size in matcher.get_matching_blocks():
                pairs.extend((lo + a + n, blo + b + n) for n in range(size))
        elif depth < MOST_DIFF_DEPTH:
            anchors = _anchors(old, lo, hi, new, blo, bhi)
            for i, j in anchors:
                _match(old, lo, i, new, blo, j, pairs, depth + 1)
                pairs.append((i, j))
                lo, blo = i + 1, j + 1
            if anchors:
                _match(old, lo, hi, new, blo, bhi, pairs, depth + 1)
    pairs.extend(reversed(tail))


def _anchors(old, lo, hi, new, blo, bhi) -> list[tuple[int, int]]:
    """The longest run of line pairs, in order on both sides, of lines each region holds once."""
    in_old, in_new = Counter(old[lo:hi]), Counter(new[blo:bhi])
    at = {old[i]: i for i in range(lo, hi) if in_old[old[i]] == 1}
    unique = [
        (at[line], j)
        for j, line in enumerate(new[blo:bhi], blo)
        if in_new[line] == 1 and line in at
    ]
    # Patience sorting: the longest increasing run of old positions, by new position.
    tops: list[int] = []
    ends: list[int] = []
    before: list[int | None] = []
    for n, (i, _j) in enumerate(unique):
        k = bisect_left(tops, i)
        before.append(ends[k - 1] if k else None)
        if k == len(tops):
            tops.append(i)
            ends.append(n)
        else:
            tops[k], ends[k] = i, n
    run = []
    n = ends[-1] if ends else None
    while n is not None:
        run.append(unique[n])
        n = before[n]
    return run[::-1]


def _merge(base: list[str], *sides: list[tuple]) -> list[str] | None:
    """`base` with the hunks of each side; None when two hunks change the same lines."""
    out: list[str] = []
    at, last = 0, None
    for start, end, lines in sorted(hunk for side in sides for hunk in side):
        if start < at or (start == end and last == (start, end)):
            return None
        out += base[at:start] + list(lines)
        at, last = end, (start, end)
    return out + base[at:]


def _lines(text: str) -> list[str]:
    lines = text.splitlines(keepends=True)
    if lines and not lines[-1].endswith(("\n", "\r")):
        lines[-1] += "\n\\ No newline at end of file\n"
    return lines
