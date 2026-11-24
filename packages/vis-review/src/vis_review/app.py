"""The review app: checkpoints, changed files, the diff with findings, checks and undo.

`ReviewApp` works the same at both doors. `opener(declaration, on_event)` opens
the app (or adopts it by key) and answers its `vis.LiveAttachment` handle. The
door also gives `call` for the language tools and `ask` for the AI review. A
comment of a person stays a draft of the diff: the next message of the session
takes it.

Every paint renders the whole app and replaces only the nodes that changed. The
first node never changes, so each node keeps its place.
"""

from __future__ import annotations

import dataclasses
import hashlib
import json
import logging
import os
import threading
import time
import traceback
from collections import OrderedDict
from collections.abc import Callable, Mapping
from typing import Any

import blockether.vis.extension as vis
from blockether.vis import _contracts
from pydantic import BaseModel

from vis_review import ai, checks, git
from vis_review.journal import FileDiff, Journal, base_ref

LOG = logging.getLogger("vis_review")
ui = vis.ui

# Characters of patch text the diff node shows; the contract allows 1 MB.
MOST_PATCH_CHARS = 900_000
# File names of the cut files that the notes list.
MOST_CUT_NAMES = 5
# Notes below the diff, and the characters of one note.
MOST_NOTES = 20
MOST_NOTE_CHARS = 2000
# Unchanged lines around each change; full files show every line, as "Expand all" does.
CONTEXT_LINES = 3
FULL_CONTEXT = 1_000_000
# Checkpoints and files that a toolbar menu lists, newest checkpoint first.
MOST_MENU_ITEMS = 40
_AUTHOR = {"kind": "extension", "name": "vis-review"}
EVENT_FIELDS = ("type", "node_id", "values", "item_ids")
BUTTONS = (
    "session",
    "all",
    "latest",
    "uncommitted",
    "branch",
    "full",
    "merged",
    "refresh",
    "checks",
    "tests",
    "ai",
    "undo-file",
    "undo",
    "restore",
    "git-commit",
    "git-commit-push",
    "git-push",
    "git-push-branch",
    "git-pull",
    "git-rebase",
    "git-merge",
    "git-pr",
    "git-abort",
)
# The key of each button, the same in every surface. A terminal shows them in its
# key legend instead of a button row; the Companion keeps the button too.
KEYS = {
    "session": "1",
    "all": "2",
    "latest": "3",
    "uncommitted": "4",
    "branch": "5",
    "full": "F",
    "merged": "M",
    "refresh": "R",
    "checks": "C",
    "tests": "T",
    "ai": "A",
    "undo-file": "U",
    "undo": "Z",
    "restore": "B",
    "git-commit": "K",
    "git-commit-push": "J",
    "git-push": "P",
    "git-push-branch": "N",
    "git-pull": "L",
    "git-rebase": "E",
    "git-merge": "G",
    "git-pr": "Q",
    "git-abort": "X",
}
_GIT_LABELS = {
    "git-commit": "Commit",
    "git-commit-push": "Commit and push",
    "git-push": "Push",
    "git-push-branch": "Push to branch",
    "git-pull": "Pull with rebase",
    "git-rebase": "Rebase onto base",
    "git-merge": "Merge into base and push",
    "git-pr": "Create pull request",
    "git-abort": "Abort merge or rebase",
}
_GIT_INPUTS = {
    "git-commit": ("message", "Commit message"),
    "git-commit-push": ("message", "Commit message"),
    "git-push-branch": ("branch", "Remote branch name"),
}
_GIT_ACTIONS = {
    "git-commit": git.commit,
    "git-commit-push": git.commit_and_push,
    "git-push": git.push,
    "git-push-branch": git.push_to_branch,
    "git-pull": git.pull,
    "git-rebase": git.rebase,
    "git-merge": git.merge,
    "git-pr": git.create_pr,
    "git-abort": git.abort,
}


class CommitMessage(BaseModel):
    """A proposed commit message that a person can edit before use."""

    message: str


# Buttons that call a tool or a model: they run outside the lock.
SLOW = ("checks", "tests", "ai")
# Git scopes: the "Uncommitted" scope and the "Automatic" base branch.
_AGAINST = {"HEAD": "uncommitted changes", "base": "changes against the base branch"}
# Nodes that one view holds at most: the view bound.
MOST_NODES = _contracts.definition("view", "live_view")["x-vis-max-nodes"]
# The table of the changed pictures, and the rows that it holds at most.
IMAGE_TABLE = "image-files"
MOST_IMAGE_ROWS = ui.limit.bound("table", "rows")
# Changed binary files that the app knows as a picture or not, newest first.
MOST_KNOWN_FILES = 4 * MOST_IMAGE_ROWS
# Decoded picture bytes that the app shows together, old and new: the view bound.
MOST_PICTURE_BYTES = _contracts.definition("view", "live_view")[
    "x-vis-max-picture-bytes"
]
# The order of findings when the threads cannot hold them all.
_LEVEL_RANK = {"error": 0, "warning": 1}
# Decoded picture bytes that the image cache keeps, newest first.
MOST_CACHED_PICTURE_BYTES = 2 * MOST_PICTURE_BYTES
MOST_CACHED_IMAGES = 256
# The name of a checks run, by whether it runs the tests too.
_RUN_NAMES = {True: "Tests and checks", False: "Checks"}
# The scope fields that the app saves next to its journal, so a restart keeps them.
_KEPT = (
    "start",
    "end",
    "selected",
    "path",
    "is_full",
    "against",
    "is_own",
    "ai_summary",
)


def event_dict(event) -> dict[str, Any]:
    """A plain dict of one event, from a wire dict or an SDK event object."""
    if isinstance(event, Mapping):
        return {k: event.get(k) for k in EVENT_FIELDS if event.get(k) is not None}
    return {
        k: getattr(event, k)
        for k in EVENT_FIELDS
        if getattr(event, k, None) is not None
    }


class RawApp(vis.LiveAttachment):
    """A live app handle that gives each event as the raw wire dict."""

    def _wire_event(self, raw):
        return dict(raw)


def extension_opener(
    declaration: dict,
    on_event: Callable[[Any], None],
    on_reload: Callable[[dict], None] | None = None,
) -> RawApp:
    """Open the app through the extension host.

    `on_reload` is the hook that opens the app again after a reload of the extension.
    """
    return RawApp(declaration, flush_ms=0, on_event=on_event, on_reload=on_reload)


class ReviewApp:
    """One review app over one journal."""

    def __init__(
        self,
        journal: Journal,
        opener: Callable[[dict, Callable[[Any], None]], Any],
        *,
        call: Callable[..., object] | None = None,
        ask: Callable[..., object] | None = None,
        key: str = "review",
        title: str = "Review",
        clock: Callable[[], float] = time.time,
        is_session_scoped: bool = False,
    ):
        self.journal = journal
        self._opener = opener
        self._call = call
        self._ask = ask
        self.key = key
        self.title = title
        self._clock = clock
        self.start = 1
        self.end: int | None = None
        self.selected: int | None = None
        self.path: str | None = None
        self.is_full = False
        # "HEAD" or "base" compares the files with a git commit, None with checkpoints.
        self.against: str | None = None
        # What the last "against" paint compared with, for a person: "HEAD" or the base label.
        self.against_ref: str | None = None
        # A session door knows which checkpoints the session wrote itself; its first
        # range then shows only those changes, never the edits of other sessions.
        self.is_session_scoped = is_session_scoped
        self.is_own = is_session_scoped
        # "This session" leaves out the changes that the base branch has, until a person asks.
        self.is_merged_shown = False
        # The label of the base that the last "This session" paint compared with, or None.
        self.own_base: str | None = None
        # The stamp of the git refs at the last paint: a later stamp can show other changes.
        self._refs: tuple = ()
        self.findings: list[checks.Finding] = []
        # The date of each finding that the last paint showed: the time of its first paint.
        self._found_at: dict[checks.Finding, int] = {}
        self.ai_summary = ""
        self.notice = ("Ready", "idle")
        self.view = None
        self._painted: dict[str, dict] = {}
        self._lock = threading.RLock()
        self._busy: str | None = None
        self._git_state = git.Status()
        self._git_stamp: tuple | None = None
        self._git_form: str | None = None
        self._git_message = ""
        # Image nodes by (path, old digest, new digest): a paint reads no blob twice.
        self._images: OrderedDict[tuple, dict | None] = OrderedDict()
        # Whether a changed binary file is a picture, by the key of its image node.
        self._pictures: dict[tuple, bool] = {}
        # The row ids of the pictures that a person opened, oldest first.
        self._open: list[str] = []
        # The scope and the last results, saved at each paint for the next process.
        self._state_path = journal.path.with_suffix(".view.json")
        self._saved: dict | None = None
        self._load_state()

    # Opening and painting ----------------------------------------------------

    def open(self, *, checkpoint: bool = True) -> ReviewApp:
        """Open the app, or adopt the open app with the same key, and paint it."""
        if checkpoint:
            self.journal.checkpoint("manual", "Review opened")
        with self._lock:
            nodes = self.render()
            declaration = ui.app(
                self.title,
                *nodes,
                description="What changed, the checks and undo.",
                key=self.key,
                icon="review",
                shortcuts=self._shortcuts(),
                surfaces={"inline": False, "session": "sidebar"},
                # Each root has its own key; the person sets up one Review app.
                settings_name="review",
            )
            self.view = self._opener(declaration, self.on_event)
            self._painted = _picture(self.view, nodes)
            self._paint(nodes)
            return self

    def refresh(self, notice: tuple[str, str] | None = None) -> None:
        """Check the base and then paint the newest journal state."""
        self.journal.refresh_merge()
        self._repaint(notice)

    def _repaint(self, notice: tuple[str, str] | None = None) -> None:
        with self._lock:
            if notice is not None:
                self.notice = notice
            if self.view is not None:
                self._paint(self.render())

    def is_behind(self) -> bool:
        """True when what the app shows changed without a checkpoint of its journal.

        A git ref moved, and the app compares with git. Or a subagent recorded a
        change, and the app shows the own changes.
        """
        is_git = self.against is not None or (
            self._is_own_range() and not self.is_merged_shown
        )
        return self.view is not None and (
            (is_git and self.journal.refs_stamp() != self._refs)
            or (self._is_own_range() and self.journal.team_stamp() != self._team)
        )

    def _paint(self, nodes: list[dict]) -> None:
        painted = list(self._painted.values())
        with self.view.batch():
            for step, *args in _steps(painted, nodes, _ids(painted)) or []:
                if step == "drop":
                    self.view.drop(*args)
                elif step == "add":
                    self.view.add(args[0], after=args[1])
                elif step == "select":
                    self.view.node(args[0]).select(*args[1])
                else:
                    self.view.node(args[0]).set(**args[1])
        self._painted = {node["id"]: node for node in nodes}
        self._keep_state()

    def _state(self) -> dict:
        """What a new process needs to show the same review: the scope and the last results."""
        text, tone = self.notice
        return {
            **{name: getattr(self, name) for name in _KEPT},
            "is_merged_shown": self.is_merged_shown,
            # A run does not continue after a restart.
            "notice": ["Ready", "idle"] if tone == "running" else [text, tone],
            "findings": [dataclasses.asdict(finding) for finding in self.findings],
        }

    def _keep_state(self) -> None:
        """Save the state when it changed. One rename, so a reader sees a whole file."""
        state = self._state()
        if state == self._saved:
            return
        part = self._state_path.with_suffix(".part")
        try:
            part.parent.mkdir(parents=True, exist_ok=True)
            part.write_text(json.dumps(state), encoding="utf-8")
            os.replace(part, self._state_path)
        except OSError as error:
            LOG.warning("vis-review: the state of the app is not saved: %s", error)
            return
        self._saved = state

    def _load_state(self) -> None:
        """Take the state that the last process saved. A file that does not fit is ignored."""
        try:
            state = json.loads(self._state_path.read_text(encoding="utf-8"))
            findings = [checks.Finding(**finding) for finding in state["findings"]]
            kept = {name: state[name] for name in _KEPT}
            text, tone = state["notice"]
        except (OSError, ValueError, KeyError, TypeError):
            return
        last = self.journal.last

        def number(value, least: int) -> bool:
            return type(value) is int and least <= value <= last

        if not (
            number(kept["start"], 1)
            and (kept["end"] is None or number(kept["end"], kept["start"]))
            and (kept["selected"] is None or number(kept["selected"], 0))
            and (kept["path"] is None or isinstance(kept["path"], str))
            and (kept["against"] is None or kept["against"] in _AGAINST)
            and all(type(kept[name]) is bool for name in ("is_full", "is_own"))
            and all(
                isinstance(value, str) for value in (kept["ai_summary"], text, tone)
            )
        ):
            return
        for name, value in kept.items():
            setattr(self, name, value)
        # A state that an older version saved has no such field.
        self.is_merged_shown = state.get("is_merged_shown") is True
        # Only a session door knows which checkpoints the session wrote itself.
        self.is_own = self.is_own and self.is_session_scoped
        self.findings, self.notice = findings, (text, tone)

    def log(self, message: str, level: str = "info", detail: str | None = None) -> None:
        """Write one entry to the Log panel of the app; a closed app only logs here."""
        getattr(LOG, {"warn": "warning"}.get(level, level))("vis-review: %s", message)
        if self.view is None:
            return
        try:
            self.view.log(message, level, detail).flush()
        except Exception:  # noqa: BLE001 - the log must never break the app
            LOG.exception("vis-review: the app log did not take an entry")

    # Rendering ---------------------------------------------------------------

    def diffs(self) -> list[FileDiff]:
        """The diffs of the chosen range and file."""
        diffs = self._scope_diffs()
        return [d for d in diffs if self.path is None or d.path == self.path]

    def all_findings(
        self, diffs: list[FileDiff], *, cut: set[str] | None = None
    ) -> list[checks.Finding]:
        """Diff checks except on cut files, then the findings of the last runs."""
        paths = {d.path for d in diffs} | (cut or set())
        visible = diffs if not cut else [d for d in diffs if d.path not in cut]
        found = checks.diff_checks(visible, self.journal.store.get)
        return found + [f for f in self.findings if not f.path or f.path in paths]

    def render(self) -> list[dict]:
        """Every node of the app for the current state: the diff first."""
        self._refs = self.journal.refs_stamp()
        self._team = self.journal.team_stamp()
        self._sync_git()
        if self.selected is not None and all(
            e.seq != self.selected or e.kind == "merge" for e in self.journal.entries
        ):
            self._first_range(is_own=self.is_session_scoped)
        if self.against is None and self._is_own_range():
            paths, diffs, patch, cut = self._own_render()
        else:
            diffs_all = self._scope_diffs()
            paths = [d.path for d in diffs_all]
            self._keep_path(paths)
            diffs = [d for d in diffs_all if self.path is None or d.path == self.path]
            patch, cut = _patch_of(diffs)
        end = self.end or self.journal.last
        cut_paths = set(cut)
        findings = self.all_findings(diffs, cut=cut_paths)
        shown = {d.path for d in diffs if d.patch and d.path not in cut_paths}
        newest = self.journal.entries[-1].at if self.journal.entries else 0
        now = int(newest * 1000) or 1
        self._found_at = {f: self._found_at.get(f, now) for f in findings}
        threads, left = _threads(findings, shown, self._found_at)
        notice = self._notice()
        notes = self._notes([*_left_out(cut, left), *_off_lines(findings, shown)])
        nodes = [
            self._diff(
                patch, threads, paths, bool(diffs or cut), end, is_cut=bool(cut)
            ),
            *([notice] if notice else []),
            *self._git_nodes(),
            *([notes] if notes else []),
        ]
        # The group of the images and its table are two nodes.
        room = MOST_NODES - len(list(_contracts._wire_tree(nodes))) - 2
        images = self.image_nodes(diffs, room)
        if images:
            nodes.append(ui.group("images", *images, label="Changed images"))
        return nodes

    def _keep_path(self, paths: list[str]) -> None:
        if self.path is not None and self.path not in paths:
            self.path = None

    def _own_render(self) -> tuple[list[str], list[FileDiff], str, list[str]]:
        self.own_base = None
        base = None if self.is_merged_shown else base_ref(self.journal.root)
        rows = None if base is None else self.journal.own_files(base.commit)
        if rows is None:
            rows = self.journal.own_files()
        else:
            self.own_base = base.label
        paths = [row[0] for row in rows]
        self._keep_path(paths)
        diffs, parts, cut = [], [], []
        size = 0
        for path, _status, before, after in rows:
            if self.path is not None and path != self.path:
                continue
            if cut or size >= MOST_PATCH_CHARS:
                cut.append(path)
                continue
            diff = self.journal.own_file_diff(path, before, after, self._context())
            text = _patch_text(diff)
            if size + len(text) > MOST_PATCH_CHARS:
                cut.append(path)
            else:
                diffs.append(diff)
                parts.append(text)
                size += len(text)
        return paths, diffs, "".join(parts), cut

    def _own_patch(self, most: int) -> str:
        """The own patch text, built only until it holds `most` characters."""
        parts, size = [], 0
        for path, _status, before, after in self.journal.own_files():
            if size >= most:
                break
            text = _patch_text(self.journal.own_file_diff(path, before, after))
            parts.append(text)
            size += len(text)
        return "".join(parts)

    def _context(self) -> int:
        return FULL_CONTEXT if self.is_full else CONTEXT_LINES

    def _baseline(self) -> int:
        point = self.journal.merge_point()
        return point.seq if point is not None else 1

    def _scope_diffs(self) -> list[FileDiff]:
        """Every diff of the scope: a checkpoint range, or a git commit to now."""
        if self.against is not None:
            base = None if self.against == "HEAD" else base_ref(self.journal.root)
            ref = base.commit if base else "HEAD" if self.against == "HEAD" else None
            found = None if ref is None else self.journal.against(ref, self._context())
            if found is not None:
                self.against_ref = base.label if base else ref
                return found
            self.notice = (f"Git cannot show {_AGAINST[self.against]} here", "warn")
            self.against = None
        self.own_base = None
        if self._is_own_range():
            base = None if self.is_merged_shown else base_ref(self.journal.root)
            found = base and self.journal.own_open(base.commit, self._context())
            if found is not None and base:
                self.own_base = base.label
                return found
            return self.journal.own_diff(self._context())
        paths = None
        if self.selected is not None and self.end == self.selected:
            paths = self.journal.entry(self.selected).files
        return self.journal.diff(
            max(self.start, self._baseline()),
            self.end,
            paths=paths,
            context=self._context(),
        )

    def _merged_count(self, paths: list[str]) -> int:
        """The own changed files that "This session" leaves out: the base branch has them."""
        if self.own_base is None:
            return 0
        return len(set(self.journal.own_paths()).difference(paths))

    def _is_own_range(self) -> bool:
        """True when the app shows only the changes of its own session."""
        return self.is_own and self.start <= 1 and self.end is None

    def _is_all(self) -> bool:
        """True when the app shows the whole journal: the first range."""
        return (
            self.start <= 1
            and self.end is None
            and self.against is None
            and not self.is_own
        )

    def _range_text(self, end: int) -> str:
        if self.against is not None:
            return _AGAINST[self.against]
        if self._is_own_range():
            return "this session"
        return "all changes" if self._is_all() else f"#{self.start}..#{end}"

    def _notice(self) -> dict | None:
        """The state of the last action, or None while there is nothing to tell.

        It is the first status of the app, so a surface can show it in the header.
        It has no detail: `_notes` gives the texts of a result.
        """
        text, tone = self.notice
        return None if tone == "idle" else ui.status("notice", text=text, tone=tone)

    def _notes(self, off_lines: list[str]) -> dict | None:
        """The texts of the last results below the diff, or None when there is none.

        A finding on a shown line is a thread of the diff; the others are listed here,
        after the AI summary and the files and findings that the diff leaves out.
        """
        parts = [part for part in (self.ai_summary, *off_lines) if part]
        if not parts:
            return None
        more = len(parts) - MOST_NOTES
        texts = parts[:MOST_NOTES] + ([f"{more} more not shown"] if more > 0 else [])
        return ui.group(
            "notes",
            *(
                ui.paragraph(f"note-{n}", text[:MOST_NOTE_CHARS])
                for n, text in enumerate(texts)
            ),
            label="Review notes",
        )

    def _shortcuts(self) -> list[dict]:
        """One key for each button. A shortcut has no label, so it shows the button's own text."""
        return [
            ui.app.shortcut(KEYS[button], node_id=button)
            for button in BUTTONS
            if button not in ("session", "merged") or self.is_session_scoped
        ]

    def _scope_label(self) -> str:
        """The name of the shown range: the label of the scope menu."""
        if self.against is not None:
            return {"HEAD": "Uncommitted", "base": "Against base branch"}[self.against]
        if self.end is not None:
            return f"Checkpoint #{self.selected or self.end}"
        if self._is_own_range():
            return "This session"
        return "All changes" if self._is_all() else f"#{self.start} to now"

    def _toolbar(self, paths: list[str], has_diffs: bool) -> list[dict]:
        """The diff toolbar of a T3 Code review: the scope menu, refresh and one more menu.

        A nested group is a submenu. The choice that holds now is selected, like a T3 Code
        radio item; a control is off only when it cannot do anything now.
        """
        has_change = self.selected is not None and self.selected > 1
        own = self._is_own_range() and self.against is None
        checkpoints = self._checkpoint_buttons()
        files = [
            ui.button(f"file:{path}", path, is_selected=path == self.path)
            for path in paths[:MOST_MENU_ITEMS]
        ]
        scope = [
            *(
                [
                    ui.button(
                        "session", "This session", is_selected=own and self.path is None
                    )
                ]
                if self.is_session_scoped
                else []
            ),
            ui.button(
                "all", "All changes", is_selected=self._is_all() and self.path is None
            ),
            ui.button("uncommitted", "Uncommitted", is_selected=self.against == "HEAD"),
            ui.button(
                "latest",
                "Latest change",
                is_disabled=self.journal.last <= self._baseline(),
                is_selected=self.selected == self.journal.last,
            ),
            ui.button(
                "branch", "Against base branch", is_selected=self.against == "base"
            ),
            *(
                [ui.group(*checkpoints, id="checkpoints", label="Checkpoint")]
                if checkpoints
                else []
            ),
        ]
        merged = self._merged_count(paths)
        more = [
            ui.button("checks", "Run checks", is_disabled=not has_diffs),
            ui.button(
                "tests", "Run tests", is_disabled=not has_diffs or self._call is None
            ),
            ui.button(
                "ai", "AI review", is_disabled=not has_diffs or self._ask is None
            ),
            ui.button(
                "full", "Show changes only" if self.is_full else "Show full files"
            ),
            *(
                [
                    ui.button(
                        "merged",
                        "Hide merged changes"
                        if self.is_merged_shown
                        else f"Include merged changes ({merged})"
                        if merged
                        else "Include merged changes",
                        is_disabled=not own,
                    )
                ]
                if self.is_session_scoped
                else []
            ),
            *(
                [
                    ui.group(
                        ui.button(
                            "file-all", "All files", is_selected=self.path is None
                        ),
                        *files,
                        id="file",
                        label=self.path or "All files",
                    )
                ]
                if files
                else []
            ),
            ui.button(
                "undo-file",
                "Undo file",
                variant="destructive",
                is_disabled=self.path is None or self.against is not None,
            ),
            ui.button(
                "undo",
                "Undo checkpoint",
                variant="destructive",
                is_disabled=not has_change,
            ),
            ui.button(
                "restore",
                "Restore to here",
                variant="destructive",
                is_disabled=self.selected is None,
            ),
        ]
        return [
            ui.group(*scope, id="scope", label=self._scope_label()),
            ui.button("refresh", "Refresh", icon="refresh"),
            self._git_toolbar(),
            ui.group(*more, id="actions", label="More actions", icon="more"),
        ]

    def _checkpoint_buttons(self) -> list[dict]:
        baseline = self.journal.entries[:1]
        if baseline and baseline[0].kind == "merge":
            baseline = []
        snapshots = [*baseline, *self.journal.snapshots()]
        groups: dict[tuple[int, int | None], list[dict]] = {}
        items = []
        for entry in reversed(snapshots[-MOST_MENU_ITEMS:]):
            turn, iteration = (
                getattr(entry, "turn", None),
                getattr(entry, "iteration", None),
            )
            label = entry.label or entry.kind
            if entry.kind == "outside" and not entry.files:
                continue
            if turn is not None:
                at = f"t{turn}" + (f"/i{iteration}" if iteration is not None else "")
                label = f"{at} · {label}"
            item = ui.button(
                f"checkpoint-{entry.seq}",
                f"#{entry.seq} {label}"[:300],
                is_selected=entry.seq == self.selected,
            )
            if turn is not None:
                groups.setdefault((turn, iteration), []).append(item)
            else:
                items.append(item)
        point = self.journal.merge_point()
        return [
            *(
                ui.group(
                    *nodes,
                    id=f"snapshots-{turn}-{iteration}",
                    label=f"Turn {turn}"
                    + (f" · Iteration {iteration}" if iteration is not None else ""),
                )
                for (turn, iteration), nodes in groups.items()
            ),
            *items,
            *(
                [
                    ui.button(
                        "merge-point",
                        f"Merged at {point.commit[:12]}",
                        is_disabled=True,
                    )
                ]
                if point is not None and point.commit
                else []
            ),
        ]

    def _sync_git(self) -> None:
        # Commands that write or contact a remote run only from an app event.
        stamp = (self._refs, self._team, self.journal.last)
        if stamp != self._git_stamp and not (self._busy or "").startswith("git-"):
            self._git_state = git.status(self.journal)
            self._git_stamp = stamp

    def _git_enabled(self) -> set[str]:
        state = self._git_state
        if not state.ok or self._busy:
            return set()
        if state.operation:
            return {"git-abort"} if state.operation in ("merge", "rebase") else set()
        buttons = set()
        if state.own_dirty:
            buttons.add("git-commit")
            if state.upstream and not state.behind:
                buttons.add("git-commit-push")
        if state.branch:
            if state.upstream and state.ahead and not state.behind:
                buttons.add("git-push")
            if state.has_origin:
                buttons.add("git-push-branch")
            if (
                state.has_gh
                and state.upstream
                and state.ahead_of_base
                and not (state.behind or state.ahead)
            ):
                buttons.add("git-pr")
            if not state.dirty:
                if state.upstream:
                    buttons.add("git-pull")
                if state.base:
                    buttons.add("git-rebase")
                    if (
                        state.has_origin
                        and state.ahead_of_base
                        and state.branch != state.base.removeprefix("origin/")
                    ):
                        buttons.add("git-merge")
        return buttons

    def _git_toolbar(self) -> dict:
        state = self._git_state
        available = self._git_enabled()
        label = "No Git work tree"
        if state.ok:
            label = state.branch or "Detached HEAD"
            if state.upstream:
                label += f" ↑{state.ahead} ↓{state.behind}"
            if state.operation:
                label += f" · {state.operation}"
        return ui.group(
            *(
                ui.button(node, text, is_disabled=node not in available)
                for node, text in _GIT_LABELS.items()
            ),
            id="git-actions",
            label=f"Git · {label}"[:300],
        )

    def _git_nodes(self) -> list[dict]:
        if not self._git_form:
            return []
        field, label = _GIT_INPUTS[self._git_form]
        return [
            ui.group(
                "git-form",
                (ui.multiline if field == "message" else ui.plaintext)(
                    field,
                    id=field,
                    label=label,
                    value=self._git_message,
                    is_required=True,
                ),
                *(
                    [
                        ui.button(
                            "git-suggest",
                            "Suggest message",
                            is_disabled=bool(self._busy),
                        )
                    ]
                    if field == "message" and self._ask is not None
                    else []
                ),
                ui.button(
                    f"submit-{self._git_form}",
                    _GIT_LABELS[self._git_form],
                    action="submit",
                    variant="primary",
                    is_disabled=bool(self._busy),
                ),
                ui.button("git-cancel", "Cancel"),
                label=_GIT_LABELS[self._git_form],
            )
        ]

    def _old_revision(self) -> str:
        """What the diff compares with: a git ref or the first checkpoint of the range."""
        if self.against is not None:
            return self.against_ref or self.against
        point = self.journal.merge_point()
        if self.own_base is None and point is not None and self.start <= point.seq:
            return f"{point.commit[:12]}" if point.commit else f"#{point.seq}"
        return self.own_base or f"#{max(self.start, 0)}"

    def _diff(
        self,
        patch: str,
        threads: list[dict],
        paths: list[str],
        has_diffs: bool,
        end: int,
        *,
        is_cut: bool,
    ) -> dict:
        """The diff node of `patch` with its toolbar and `threads`, one for each finding on a line."""
        return ui.diff(
            "diff",
            patch
            or (
                "The diff exceeds the view limit.\n"
                if is_cut
                else "Every change of this session is merged.\n"
                if not has_diffs and self._merged_count(paths)
                else "No change in this range.\n"
            ),
            old_revision=self._old_revision(),
            new_revision=f"#{end}"
            if self.end is not None and not self.against
            else "now",
            threads=threads,
            children=self._toolbar(paths, has_diffs),
        )

    def image_nodes(self, diffs: list[FileDiff], room: int) -> list[dict]:
        """The table of the changed pictures, then each picture that a person opened.

        A row of the table is no node and holds no bytes, so each changed picture
        has a row and none shows until a person presses its row. The view has room
        for `room` open pictures and for `MOST_PICTURE_BYTES`: when a new picture
        needs that room, the pictures that were opened first close.
        """
        files: dict[str, FileDiff] = {}
        for diff in diffs:
            if len(files) == MOST_IMAGE_ROWS:
                break
            if diff.is_binary and self._is_picture(diff):
                files[_image_id(diff.path)] = diff
        if not files:
            return []
        budget = MOST_PICTURE_BYTES
        shown: dict[str, dict] = {}
        closed: set[str] = set()
        for row_id in reversed(self._open):
            # A picture of another range or file stays open for its return.
            if (diff := files.get(row_id)) is None:
                continue
            node = self._image(_image_key(diff), diff)
            size = _contracts.node_picture_bytes(node)
            if closed or len(shown) == room or size > budget:
                closed.add(row_id)
                continue
            budget -= size
            shown[row_id] = dict(node, label=_file_label(diff))
        self._open = [row_id for row_id in self._open if row_id not in closed]
        table = ui.table(
            IMAGE_TABLE,
            columns=[ui.table.column("file", "File")],
            rows=[ui.table.row(i, [_file_label(diff)]) for i, diff in files.items()],
            is_selectable=True,
            selected_ids=[row_id for row_id in files if row_id in shown],
        )
        return [table, *(shown[row_id] for row_id in files if row_id in shown)]

    def _is_picture(self, diff: FileDiff) -> bool:
        """True when a surface can show the binary file `diff`: one read for each version."""
        key = _image_key(diff)
        if key not in self._pictures:
            self._pictures[key] = self._image(key, diff) is not None
            while len(self._pictures) > MOST_KNOWN_FILES:
                del self._pictures[next(iter(self._pictures))]
        return self._pictures[key]

    def _image(self, key: tuple, diff: FileDiff) -> dict | None:
        """The image node of `diff` from the cache, which keeps the newest bytes."""
        if key in self._images:
            self._images.move_to_end(key)
            return self._images[key]
        node = image_node(diff, self.journal.store.get)
        self._images[key] = node
        cached = sum(
            _contracts.node_picture_bytes(n or {}) for n in self._images.values()
        )
        while len(self._images) > 1 and (
            cached > MOST_CACHED_PICTURE_BYTES or len(self._images) > MOST_CACHED_IMAGES
        ):
            _, old = self._images.popitem(last=False)
            cached -= _contracts.node_picture_bytes(old or {})
        return node

    # Events ------------------------------------------------------------------

    def on_event(self, event) -> None:
        """The `on_event` callback: one event from a person, from any thread."""
        fields = event_dict(event)
        try:
            self.dispatch(fields)
        except Exception as error:  # noqa: BLE001 - one bad event must not end the app
            what = fields.get("node_id") or fields.get("type") or "an event"
            self.log(
                f"{what} failed: {type(error).__name__}: {error}",
                "error",
                traceback.format_exc(),
            )
            self.refresh((f"{type(error).__name__}: {error}"[:300], "error"))

    def dispatch(self, event: dict) -> None:
        """Do what one event asks and paint the result."""
        kind, node = event.get("type"), event.get("node_id")
        if kind == "draft":
            self._seen()
            return
        if kind == "activate" and node in _GIT_ACTIONS:
            self._activate_git(node)
            return
        if kind == "submit" and isinstance(node, str):
            action = node.removeprefix("submit-")
            if action in _GIT_INPUTS and node != action:
                value = (event.get("values") or {}).get(_GIT_INPUTS[action][0])
                if isinstance(value, str):
                    self._run_git(action, value)
                return
        if kind == "activate" and node == "git-suggest":
            self._suggest_commit()
            return
        if kind == "activate" and node == "refresh":
            self.journal.checkpoint("manual", "Refresh")
            with self._lock:
                self._git_stamp = None
            self.refresh()
            return
        if kind == "activate" and node in SLOW:
            self._slow(node)
            return
        with self._lock:
            if kind == "activate" and node == "git-cancel":
                self._git_form = None
                self._repaint()
            elif kind == "activate" and str(node).startswith("checkpoint-"):
                self._choose_checkpoint([str(node).removeprefix("checkpoint-")])
            elif kind == "activate" and (
                node == "file-all" or str(node).startswith("file:")
            ):
                path = str(node).removeprefix("file:")
                self.path = None if node == "file-all" or path == self.path else path
                self._repaint()
            elif kind == "activate" and node in BUTTONS:
                getattr(self, "_" + node.replace("-", "_"))()
            elif kind == "select" and node == IMAGE_TABLE:
                self._press_images(list(event.get("item_ids") or []))

    def _activate_git(self, node: str) -> None:
        with self._lock:
            if node not in self._git_enabled():
                return
            if node in _GIT_INPUTS:
                self._git_form, self._git_message = node, ""
                self._repaint()
                return
        self._run_git(node)

    def _run_git(self, node: str, value: str | None = None) -> None:
        with self._lock:
            if (
                node in _GIT_INPUTS and self._git_form != node
            ) or node not in self._git_enabled():
                return
            self._busy = node
            self._repaint((f"{_GIT_LABELS[node]} in progress", "running"))
        try:
            done = (
                _GIT_ACTIONS[node](self.journal, value)
                if value is not None
                else _GIT_ACTIONS[node](self.journal)
            )
            if node in ("git-pull", "git-rebase", "git-merge", "git-abort"):
                self.journal.checkpoint("outside", done.message)
            if node in (
                "git-commit",
                "git-commit-push",
                "git-push",
                "git-push-branch",
                "git-pull",
                "git-rebase",
                "git-merge",
                "git-abort",
            ):
                self.journal.refresh_merge()
            state = git.status(self.journal)
        except BaseException:
            with self._lock:
                self._busy = self._git_stamp = None
            raise
        with self._lock:
            self._busy = None
            if done.ok:
                self._git_form = None
            self._git_state = state
            self._git_stamp = (
                self.journal.refs_stamp(),
                self.journal.team_stamp(),
                self.journal.last,
            )
            text = done.message
            if done.conflicts:
                text += (
                    ": "
                    + ", ".join(done.conflicts[:5])
                    + ". Use Abort or resolve them in Git."
                )
            self.log(text[:300], "info" if done.ok else "warn")
            self._repaint((text[:300], "ok" if done.ok else "warn"))

    def _suggest_commit(self) -> None:
        with self._lock:
            form = self._git_form
            if (
                self._busy
                or self._ask is None
                or form not in ("git-commit", "git-commit-push")
            ):
                return
            self._busy = "git-suggest"
        try:
            patch = self._own_patch(ai.MOST_PROMPT_CHARS)
            answer = self._ask(
                "Suggest a short Git commit message for the change below. Do not edit files.\n\n"
                + patch[: ai.MOST_PROMPT_CHARS],
                CommitMessage,
            )
        finally:
            with self._lock:
                self._busy = None
        with self._lock:
            if self._git_form == form and answer is not None:
                self._git_message = answer.message.strip()[:2000]
                self._repaint()

    def _press_images(self, ids: list[str]) -> None:
        """A press on one row opens or closes its picture; a longer selection is the open set."""
        if len(ids) == 1:
            is_open = ids[0] in self._open
            self._open = [i for i in self._open if i != ids[0]] + ids * (not is_open)
        else:
            self._open = ids
        self._repaint()

    def _seen(self) -> None:
        """A person works on a comment: the result of the last action leaves the app."""
        with self._lock:
            # A run that is not done keeps its state.
            if not self._busy and self.notice[1] not in ("idle", "running"):
                self._repaint(("Ready", "idle"))

    def _choose_checkpoint(self, ids) -> None:
        seq = int(ids[0]) if ids else None
        if seq is not None and all(
            e.seq != seq or e.kind == "merge" for e in self.journal.entries
        ):
            return
        if seq is None or seq == self.selected or seq <= 1:
            self.selected = (
                seq if seq is not None and seq <= 1 and seq != self.selected else None
            )
            self.start, self.end = 1, None
        else:
            self.selected = seq
            self.start, self.end = seq - 1, seq
        self.against = None
        self._clear()
        self._repaint(
            (
                "Showing "
                + (self._range_text(0) if self.end is None else f"checkpoint #{seq}"),
                "idle",
            )
        )

    def _clear(self) -> None:
        self.findings = []
        self.ai_summary = ""

    def _all(self) -> None:
        self._first_range(is_own=False)
        self._repaint(("Showing all changes", "idle"))

    def _session(self) -> None:
        self._first_range(is_own=self.is_session_scoped)
        self._repaint(("Showing the changes of this session", "idle"))

    def _first_range(self, *, is_own: bool) -> None:
        self.start, self.end, self.selected, self.path = 1, None, None, None
        self.against, self.is_own = None, is_own
        self._clear()

    def _uncommitted(self) -> None:
        self._compare("HEAD")

    def _branch(self) -> None:
        self._compare("base")

    def _compare(self, against: str) -> None:
        """Compare the files with a git commit; a checkpoint first takes in new edits."""
        self.journal.checkpoint("manual", "Refresh")
        self.against, self.selected = against, None
        self._clear()
        self.notice = (f"Showing {_AGAINST[against]}", "idle")
        self._repaint(self.notice)

    def _latest(self) -> None:
        self._choose_checkpoint([str(self.journal.last)])

    def _full(self) -> None:
        # The same changes in another picture: the result of the last run stays.
        self.is_full = not self.is_full
        self._repaint()

    def _merged(self) -> None:
        self.is_merged_shown = not self.is_merged_shown
        self._repaint()

    def _checks(self, *, is_tested: bool = False) -> None:
        name = _RUN_NAMES[is_tested]
        with self._lock:
            diffs, scope = self.diffs(), self._scope()
            self._repaint((f"Running {name.lower()}", "running"))
        started = time.perf_counter()
        found = self._language_findings(diffs, is_tested)
        with self._lock:
            if scope != self._scope():
                return
            self._take_checks(found, diffs, f"{name} ran in {_ms(started)} ms")

    def _language_findings(
        self, diffs: list[FileDiff], is_tested: bool
    ) -> list[checks.Finding]:
        """The findings of the language tools on `diffs`, or a note that none are here."""
        if self._call is None:
            return [
                checks.Finding(
                    "tool",
                    "info",
                    "",
                    0,
                    "No language tools here; only the diff checks ran.",
                    "language",
                )
            ]
        paths = [d.path for d in diffs if d.status != "deleted"]
        return checks.language_checks(
            self.journal.root, paths, self._call, is_tested=is_tested
        )

    def _take_checks(
        self, found: list[checks.Finding], diffs: list[FileDiff], ran: str
    ) -> None:
        """Keep the findings of one checks run, log the run and paint its result."""
        self.findings = [f for f in self.findings if f.source == "ai"] + found
        counts = checks.summary(self.all_findings(diffs))
        for finding in found:
            if finding.check == "tool":
                self.log(finding.message, "warn")
        self.log(
            f"{ran} on {len(diffs)} files: {counts['error']} errors, "
            f"{counts['warning']} warnings"
        )
        self._repaint(
            (
                f"Checks done: {counts['error']} errors, {counts['warning']} warnings",
                _tone(counts),
            )
        )

    def _tests(self) -> None:
        self._checks(is_tested=True)

    def _ai(self) -> None:
        with self._lock:
            diffs, scope = self.diffs(), self._scope()
            shown = self.all_findings(diffs)
            known = [f for f in shown if f.source != "ai"]
            # The model checks each finding of the last AI review again.
            earlier = [f for f in shown if f.source == "ai"]
            self._repaint(("AI review in progress", "running"))
        started = time.perf_counter()
        summary, found = ai.review(self.journal.root, diffs, self._ask, known, earlier)
        self.log(f"The AI review ran in {_ms(started)} ms: {len(found)} findings")
        with self._lock:
            if scope != self._scope():
                return
            # A finding of a file that this review did not read stays.
            self.findings = [f for f in self.findings if f not in earlier] + found
            self.ai_summary = summary
            count = f"{len(found)} finding{'' if len(found) == 1 else 's'}"
            self._repaint((f"AI review done: {count}", "warn" if found else "ok"))

    def _scope(self) -> tuple:
        """What the shown results belong to: a result for another scope is dropped."""
        return (self.start, self.end, self.path)

    def _slow(self, node: str) -> None:
        """Run a slow button outside the lock, so a block hook never waits for it."""
        with self._lock:
            if self._busy:
                self.log(f"Refused {node}: {self._busy} is still running", "warn")
                self._repaint((f"Wait: {self._busy} is still running", "warn"))
                return
            self._busy = node
        try:
            getattr(self, "_" + node)()
        finally:
            with self._lock:
                self._busy = None

    def _undo_file(self) -> None:
        if self.path is None:
            return
        if self.end is not None and self.selected and self.selected > 1:
            done = self.journal.revert(self.selected, [self.path])
        elif self._is_own_range() and self.against is None:
            done = self.journal.undo_own(self.path)
        else:
            done = self.journal.restore(max(self.start, self._baseline()), [self.path])
        self._after_undo(done)

    def _undo(self) -> None:
        if self.selected and self.selected > 1:
            self._after_undo(self.journal.revert(self.selected))

    def _restore(self) -> None:
        if self.selected:
            self._after_undo(self.journal.restore(self.selected))

    def _after_undo(self, done) -> None:
        parts = [f"{len(done.restored)} files changed back"]
        if done.conflicts:
            parts.append("changed again later, kept: " + ", ".join(done.conflicts[:5]))
        if done.missing:
            parts.append("too large to keep: " + ", ".join(done.missing[:5]))
        tone = "warn" if done.conflicts or done.missing else "ok"
        if tone == "warn":
            self.log("Undo kept some files: " + "; ".join(parts[1:]), "warn")
        self.start, self.end, self.selected = 1, None, None
        self._clear()
        self._repaint(
            ("; ".join(parts) + (f" (#{done.entry.seq})" if done.entry else ""), tone)
        )


# The marks of GitHub for a changed file. Each other status is a modified file.
CHANGE_MARKS = {"added": "+", "deleted": "-"}
MODIFIED_MARK = "•"


def _file_label(diff: FileDiff) -> str:
    """The path of a changed file after the mark of its status."""
    return f"{CHANGE_MARKS.get(diff.status, MODIFIED_MARK)} {diff.path}"


def _image_id(path: str) -> str:
    """The id of the image node of the file `path`, and of its row in the table."""
    return "image-" + hashlib.sha1(path.encode()).hexdigest()[:12]


def _image_key(diff: FileDiff) -> tuple:
    """The cache key of the image node of `diff`: the path and its two versions."""
    return (diff.path, diff.before, diff.after)


def image_node(diff: FileDiff, blob: Callable[[str], bytes | None]) -> dict | None:
    """The image node of one changed picture, a note when it cannot show, or None.

    A modified picture compares its old and new version. An added or deleted one
    shows one picture. None means that the file is no picture that a surface draws.
    The node has no label: the app adds the mark and the path of the file.
    """
    old = blob(diff.before) if diff.before else None
    new = blob(diff.after) if diff.after else None
    is_old = bool(old) and ui.picture.type(old) is not None
    is_new = bool(new) and ui.picture.type(new) is not None
    if not (is_old or is_new):
        return None
    node_id = _image_id(diff.path)
    try:
        if is_new:
            return ui.image(
                node_id,
                new,
                alt=f"The new version of {diff.path}"[:300],
                before=old if is_old else None,
            )
        return ui.image(node_id, old, alt=f"The deleted file {diff.path}"[:300])
    except ValueError as error:
        return ui.status(
            node_id,
            text=f"{diff.path}: no preview",
            detail=str(error),
            tone="warn",
        )


def _tone(counts: Mapping[str, int]) -> str:
    """The tone of a result: error, warn or ok, by its worst finding."""
    if counts["error"]:
        return "error"
    return "warn" if counts["warning"] else "ok"


def _ms(started: float) -> int:
    """Whole milliseconds since `started`, a `time.perf_counter()` value."""
    return round((time.perf_counter() - started) * 1000)


def _patch_text(diff: FileDiff) -> str:
    return (
        diff.patch
        or f"diff --git a/{diff.path} b/{diff.path}\n# {diff.note or 'No line diff.'}\n"
    )


def _patch_of(diffs: list[FileDiff]) -> tuple[str, list[str]]:
    """The patch text of `diffs` within the node limit, and the files left out."""
    parts, size, cut = [], 0, []
    for diff in diffs:
        text = _patch_text(diff)
        if size + len(text) > MOST_PATCH_CHARS:
            cut.append(diff.path)
            continue
        parts.append(text)
        size += len(text)
    return "".join(parts), cut


def _where(finding) -> str:
    """Where a finding is: `path:line`, the path, or `-`."""
    if finding.line:
        return f"{finding.path}:{finding.line}"
    return finding.path or "-"


def _left_out(cut: list[str], left: int) -> list[str]:
    """What the diff leaves out, one text each: the files over the size limit and the
    count of findings over the thread limit."""
    more = len(cut) - MOST_CUT_NAMES
    names = ", ".join(cut[:MOST_CUT_NAMES]) + (f" and {more} more" if more > 0 else "")
    return [
        *([f"Too large to show: {names}"] if cut else []),
        *([f"{left} more finding{'' if left == 1 else 's'} not shown"] if left else []),
    ]


def _off_lines(findings, shown: set[str]) -> list[str]:
    """The findings that no shown diff line holds, one text each: the notice lists them."""
    return [
        f"{f.level} {f.check} {_where(f)}: {f.message[:500]}"
        for f in findings
        # The file list already marks an added binary file.
        if (f.line < 1 or f.path not in shown) and f.check != "binary-file"
    ]


def _threads(findings, paths, dates) -> tuple[list[dict], int]:
    """One thread for each finding on a line of a shown file, dated by `dates`, and
    the count of findings that the bound leaves out. Errors come first, then warnings.

    A finding keeps the date of its first paint. A date that stays the same keeps
    the diff node the same, so a paint keeps the comments a person is writing.
    """
    on_lines = [
        (n, finding)
        for n, finding in enumerate(findings)
        if finding.line >= 1 and finding.path in paths
    ]
    on_lines.sort(key=lambda pair: _LEVEL_RANK.get(pair[1].level, 2))
    kept, left = vis.ui.limit.fit(on_lines, "diff", "threads")
    threads = []
    for n, finding in kept:
        threads.append(
            {
                "id": f"finding-{n}",
                "anchor": {"path": finding.path, "side": "new", "line": finding.line},
                "messages": [
                    {
                        "id": f"finding-{n}-1",
                        "author": _AUTHOR,
                        "created_at": dates[finding],
                        "body": f"{finding.level} {finding.check}: {finding.message}"[
                            :20000
                        ],
                    }
                ],
            }
        )
    return threads, left


def _shown_nodes(view) -> dict[str, dict] | None:
    """The nodes that `view` shows by id, or None when it has no picture yet."""
    try:
        picture = view.state() or {}
    except Exception:  # noqa: BLE001 - an app without a picture is new
        return None
    nodes = picture.get("nodes") or []
    return {node.get("id"): node for node in nodes if isinstance(node, dict)}


# The keys of a diff that `set` replaces. Each other change needs a new node.
_DIFF_SET = {"patch", "old_revision", "new_revision"}


def _ids(nodes: list[dict]) -> set[str]:
    """The id of each node in `nodes`, at any depth."""
    return {
        i for node in nodes for i in (node["id"], *_ids(node.get("children") or []))
    }


def _steps(
    painted: list[dict], nodes: list[dict], known: set[str], is_top: bool = True
) -> list[tuple] | None:
    """The view calls that turn the nodes `painted` into `nodes`: `("drop", id)`,
    `("add", node, after)`, `("set", id, changes)` and `("select", id, row_ids)`.

    A node changes in place when it can, so a long diff does not go to each surface
    again as a whole node. None when the group of the nodes must come as a new node: a
    new first node has no node to stand after, a node of `known` comes from another
    group, or the nodes changed their order. The top list has no group: its new first
    node goes to the end, so each node after it comes again, after it.
    """
    held = {node["id"]: node for node in painted}
    kept = [node["id"] for node in nodes if node["id"] in held]
    if not is_top and kept != [i for i in held if i in set(kept)]:
        return None
    steps: list[tuple] = [("drop", i) for i in held if i not in set(kept)]
    before = None
    is_behind = False
    for node in nodes:
        old = held.get(node["id"])
        if old != node or is_behind:
            inner = None if old is None or is_behind else _node_steps(old, node, known)
            if inner is None:
                is_moved = old is None and bool(_ids([node]) & known)
                if not is_top and (before is None or is_moved):
                    return None
                is_behind = is_behind or before is None
                drop = [] if old is None else [("drop", node["id"])]
                inner = [*drop, ("add", node, before)]
            steps += inner
        before = node["id"]
    return steps


def _node_steps(old: dict, node: dict, known: set[str]) -> list[tuple] | None:
    """The view calls that change node `old` into `node` in place. None when `node`
    must come as a new node."""
    if old.keys() != node.keys() or old["type"] != node["type"]:
        return None
    changed = {key for key in node if node[key] != old[key]}
    steps: list[tuple] = []
    if "children" in changed:
        inner = _steps(old["children"], node["children"], known, False)
        if inner is None:
            return None
        steps += inner
        changed.discard("children")
    if changed == {"selected_ids"}:
        steps.append(("select", node["id"], node["selected_ids"]))
    elif changed:
        if node["type"] != "diff" or not changed <= _DIFF_SET:
            return None
        steps.append(("set", node["id"], {key: node[key] for key in changed}))
    return steps


def _painted_from(shown: dict[str, dict], nodes: list[dict]) -> dict[str, dict]:
    """The shown nodes, each cut to the keys that its rendered node sets.

    A shown node that this render does not have stays whole: the first paint drops it.
    """
    rendered = {node["id"]: node for node in nodes}
    painted = {
        node_id: {key: shown[node_id].get(key) for key in node}
        for node_id, node in rendered.items()
        if node_id in shown
    }
    painted.update(
        (node_id, old)
        for node_id, old in shown.items()
        if node_id and node_id not in rendered
    )
    return painted


def _picture(view, nodes: list[dict]) -> dict[str, dict]:
    """The nodes that an opened or adopted app shows, cut to the keys the render sets."""
    shown = _shown_nodes(view)
    if shown is None:
        return {node["id"]: node for node in nodes}
    return _painted_from(shown, nodes)
