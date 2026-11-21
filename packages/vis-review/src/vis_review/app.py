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

from vis_review import ai, checks
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
}
# Buttons that call a tool or a model: they run outside the lock.
SLOW = ("checks", "tests", "ai")
# Git scopes: the "Uncommitted" scope and the "Automatic" base branch.
_AGAINST = {"HEAD": "uncommitted changes", "base": "changes against the base branch"}
# Changed pictures the app shows at most.
MOST_IMAGES = 8
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
        # Image nodes by (path, old digest, new digest): a paint reads no blob twice.
        self._images: OrderedDict[tuple, dict | None] = OrderedDict()
        # The scope and the last results, saved at each paint for the next process.
        self._state_path = journal.path.with_suffix(".view.json")
        self._saved: dict | None = None
        self._load_state()

    # Opening and painting ----------------------------------------------------

    def open(self) -> ReviewApp:
        """Open the app, or adopt the open app with the same key, and paint it."""
        with self._lock:
            self.journal.checkpoint("manual", "Review opened")
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
        """Paint the newest journal state; a range that ends at the newest follows it."""
        with self._lock:
            if notice is not None:
                self.notice = notice
            if self.view is not None:
                self._paint(self.render())

    def is_behind(self) -> bool:
        """True when the app compares with git, and a git ref moved since the last paint."""
        is_git = self.against is not None or (
            self._is_own_range() and not self.is_merged_shown
        )
        return (
            self.view is not None and is_git and self.journal.refs_stamp() != self._refs
        )

    def _paint(self, nodes: list[dict]) -> None:
        painted = list(self._painted.values())
        with self.view.batch():
            for step, *args in _steps(painted, nodes, _ids(painted)) or []:
                if step == "drop":
                    self.view.drop(*args)
                elif step == "add":
                    self.view.add(args[0], after=args[1])
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

    def all_findings(self, diffs: list[FileDiff]) -> list[checks.Finding]:
        """Diff checks of `diffs`, then the findings that the last runs gave."""
        shown = {d.path for d in diffs}
        found = checks.diff_checks(diffs, self.journal.store.get)
        return found + [f for f in self.findings if not f.path or f.path in shown]

    def render(self) -> list[dict]:
        """Every node of the app for the current state: the diff first."""
        self._refs = self.journal.refs_stamp()
        diffs_all = self._scope_diffs()
        if all(d.path != self.path for d in diffs_all):
            # An undo or a new range can take the chosen file out of the list.
            self.path = None
        diffs = [d for d in diffs_all if self.path is None or d.path == self.path]
        findings = self.all_findings(diffs)
        end = self.end or self.journal.last
        images = self.image_nodes(diffs)
        patch, cut = _patch_of(diffs)
        shown = {d.path for d in diffs if d.patch and d.path not in cut}
        newest = self.journal.entries[-1].at if self.journal.entries else 0
        now = int(newest * 1000) or 1
        self._found_at = {f: self._found_at.get(f, now) for f in findings}
        threads, left = _threads(findings, shown, self._found_at)
        notice = self._notice()
        notes = self._notes([*_left_out(cut, left), *_off_lines(findings, shown)])
        return [
            self._diff(patch, threads, diffs, diffs_all, end),
            *([notice] if notice else []),
            *([notes] if notes else []),
            *([ui.group("images", *images, label="Changed images")] if images else []),
        ]

    def _context(self) -> int:
        return FULL_CONTEXT if self.is_full else CONTEXT_LINES

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
        return self.journal.diff(self.start, self.end, context=self._context())

    def _merged_count(self, diffs_all: list[FileDiff]) -> int:
        """The own changed files that "This session" leaves out: the base branch has them."""
        if self.own_base is None:
            return 0
        return len(set(self.journal.own_paths()) - {d.path for d in diffs_all})

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
            ui.shortcut(KEYS[button], node_id=button)
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

    def _toolbar(self, diffs_all: list[FileDiff], has_diffs: bool) -> list[dict]:
        """The diff toolbar of a T3 Code review: the scope menu, refresh and one more menu.

        A nested group is a submenu. The choice that holds now is selected, like a T3 Code
        radio item; a control is off only when it cannot do anything now.
        """
        has_change = self.selected is not None and self.selected > 1
        own = self._is_own_range() and self.against is None
        checkpoints = [
            ui.button(
                f"checkpoint-{e.seq}",
                f"#{e.seq} {e.label or e.kind}",
                is_selected=e.seq == self.selected,
            )
            for e in reversed(self.journal.entries[-MOST_MENU_ITEMS:])
        ]
        files = [
            ui.button(f"file:{d.path}", d.path, is_selected=d.path == self.path)
            for d in diffs_all[:MOST_MENU_ITEMS]
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
                is_disabled=self.journal.last <= 1,
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
        merged = self._merged_count(diffs_all)
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
            ui.group(*more, id="actions", label="More actions", icon="more"),
        ]

    def _old_revision(self) -> str:
        """What the diff compares with: a git ref or the first checkpoint of the range."""
        if self.against is not None:
            return self.against_ref or self.against
        return self.own_base or f"#{max(self.start, 0)}"

    def _diff(
        self, patch: str, threads: list[dict], diffs, diffs_all, end: int
    ) -> dict:
        """The diff node of `patch` with its toolbar and `threads`, one for each finding on a line."""
        return ui.diff(
            "diff",
            patch
            or (
                "Every change of this session is merged.\n"
                if self._merged_count(diffs_all)
                else "No change in this range.\n"
            ),
            old_revision=self._old_revision(),
            new_revision=f"#{end}"
            if self.end is not None and not self.against
            else "now",
            threads=threads,
            children=self._toolbar(diffs_all, bool(diffs)),
        )

    def image_nodes(self, diffs: list[FileDiff]) -> list[dict]:
        """One image node for each changed picture: old and new side by side."""
        nodes: list[dict] = []
        budget = MOST_PICTURE_BYTES
        for diff in diffs:
            if not diff.is_binary:
                continue
            if len(nodes) == MOST_IMAGES:
                nodes.append(
                    ui.status(
                        "images-more",
                        label="More images",
                        text=f"Only the first {MOST_IMAGES} changed images show.",
                        tone="idle",
                    )
                )
                break
            node = self._image((diff.path, diff.before, diff.after), diff)
            if node is None:
                continue
            size = _contracts.node_picture_bytes(node)
            if size > budget:
                node = ui.status(
                    node["id"],
                    label=node.get("label"),
                    text=f"{diff.path}: no preview",
                    detail="The changed images of this range are too large together.",
                    tone="warn",
                )
            else:
                budget -= size
            nodes.append(node)
        return nodes

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
        if kind == "activate" and node in SLOW:
            self._slow(node)
            return
        with self._lock:
            if kind == "activate" and str(node).startswith("checkpoint-"):
                self._choose_checkpoint([str(node).removeprefix("checkpoint-")])
            elif kind == "activate" and (
                node == "file-all" or str(node).startswith("file:")
            ):
                path = str(node).removeprefix("file:")
                self.path = None if node == "file-all" or path == self.path else path
                self.refresh()
            elif kind == "activate" and node in BUTTONS:
                getattr(self, "_" + node.replace("-", "_"))()

    def _seen(self) -> None:
        """A person works on a comment: the result of the last action leaves the app."""
        with self._lock:
            # A run that is not done keeps its state.
            if not self._busy and self.notice[1] not in ("idle", "running"):
                self.refresh(("Ready", "idle"))

    def _choose_checkpoint(self, ids) -> None:
        seq = int(ids[0]) if ids else None
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
        self.refresh(
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
        self.refresh(("Showing all changes", "idle"))

    def _session(self) -> None:
        self._first_range(is_own=self.is_session_scoped)
        self.refresh(("Showing the changes of this session", "idle"))

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
        self.refresh(self.notice)

    def _latest(self) -> None:
        self._choose_checkpoint([str(self.journal.last)])

    def _full(self) -> None:
        # The same changes in another picture: the result of the last run stays.
        self.is_full = not self.is_full
        self.refresh()

    def _merged(self) -> None:
        self.is_merged_shown = not self.is_merged_shown
        self.refresh()

    def _refresh(self) -> None:
        self.journal.checkpoint("manual", "Refresh")
        self.refresh()

    def _checks(self, *, is_tested: bool = False) -> None:
        name = _RUN_NAMES[is_tested]
        with self._lock:
            diffs, scope = self.diffs(), self._scope()
            self.refresh((f"Running {name.lower()}", "running"))
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
        self.refresh(
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
            self.refresh(("AI review in progress", "running"))
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
            self.refresh((f"AI review done: {count}", "warn" if found else "ok"))

    def _scope(self) -> tuple:
        """What the shown results belong to: a result for another scope is dropped."""
        return (self.start, self.end, self.path)

    def _slow(self, node: str) -> None:
        """Run a slow button outside the lock, so a block hook never waits for it."""
        with self._lock:
            if self._busy:
                self.log(f"Refused {node}: {self._busy} is still running", "warn")
                self.refresh((f"Wait: {self._busy} is still running", "warn"))
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
            done = self.journal.restore(max(self.start, 1), [self.path])
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
        self.refresh(
            ("; ".join(parts) + (f" (#{done.entry.seq})" if done.entry else ""), tone)
        )


def image_node(diff: FileDiff, blob: Callable[[str], bytes | None]) -> dict | None:
    """The image node of one changed picture, a note when it cannot show, or None.

    A modified picture compares its old and new version. An added or deleted one
    shows one picture. None means that the file is no picture that a surface draws.
    """
    old = blob(diff.before) if diff.before else None
    new = blob(diff.after) if diff.after else None
    is_old = bool(old) and ui.picture_type(old) is not None
    is_new = bool(new) and ui.picture_type(new) is not None
    if not (is_old or is_new):
        return None
    node_id = "image-" + hashlib.sha1(diff.path.encode()).hexdigest()[:12]
    label = f"{diff.path} ({diff.status})"
    try:
        if is_new:
            return ui.image(
                node_id,
                new,
                alt=f"The new version of {diff.path}"[:300],
                label=label,
                before=old if is_old else None,
            )
        return ui.image(
            node_id, old, alt=f"The deleted file {diff.path}"[:300], label=label
        )
    except ValueError as error:
        return ui.status(
            node_id,
            label=label,
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


def _patch_of(diffs: list[FileDiff]) -> tuple[str, list[str]]:
    """The patch text of `diffs` within the node limit, and the files left out."""
    parts, size, cut = [], 0, []
    for diff in diffs:
        text = (
            diff.patch
            or f"diff --git a/{diff.path} b/{diff.path}\n# {diff.note or 'No line diff.'}\n"
        )
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
    kept, left = vis.ui.fit(on_lines, "diff", "threads")
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
    `("add", node, after)` and `("set", id, changes)`.

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
    if changed:
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
