"""The review app: checkpoints, changed files, the diff with findings, checks and undo.

`ReviewApp` works the same at both doors. `opener(declaration, on_event)` opens
the app (or adopts it by key) and answers its `vis.LiveAttachment` handle. The
door also gives `call` for the language tools, `ask` for the AI review and `send`
for the review comments that a person sends to the session.

Every paint renders the whole app and replaces only the nodes that changed. The
first node never changes, so each node keeps its place.
"""

from __future__ import annotations

import logging
import threading
import time
from collections.abc import Callable, Mapping
from typing import Any

import blockether.vis.extension as vis

from vis_review import ai, checks
from vis_review.journal import FileDiff, Journal

LOG = logging.getLogger("vis_review")
ui = vis.ui

# Characters of patch text the diff node shows; the contract allows 1 MB.
MOST_PATCH_CHARS = 900_000
# Rows the checkpoint table shows, newest first.
MOST_CHECKPOINTS = 200
_TONES = {"error": "error", "warning": "warn", "info": "idle"}
_AUTHOR = {"kind": "extension", "name": "vis-review"}
EVENT_FIELDS = ("type", "node_id", "values", "submission_id", "comments", "item_ids")
BUTTONS = ("all", "refresh", "checks", "tests", "ai", "undo-file", "undo", "restore")
# Buttons that call a tool or a model: they run outside the lock.
SLOW = ("checks", "tests", "ai")


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

    def _host_event(self, raw):
        if raw.get("type") == "validate":
            self._judge(raw)
            return
        for callback in list(self._listeners):
            callback(dict(raw))


def extension_opener(declaration: dict, on_event: Callable[[Any], None]) -> RawApp:
    """Open the app through the extension host."""
    return RawApp(declaration, flush_ms=0, on_event=on_event)


class ReviewApp:
    """One review app over one journal."""

    def __init__(
        self,
        journal: Journal,
        opener: Callable[[dict, Callable[[Any], None]], Any],
        *,
        call: Callable[..., object] | None = None,
        ask: Callable[..., object] | None = None,
        send: Callable[[str], object] | None = None,
        key: str = "review",
        title: str = "Review",
        clock: Callable[[], float] = time.time,
    ):
        self.journal = journal
        self._opener = opener
        self._call = call
        self._ask = ask
        self._send = send
        self.key = key
        self.title = title
        self._clock = clock
        self.start = 1
        self.end: int | None = None
        self.selected: int | None = None
        self.path: str | None = None
        self.findings: list[checks.Finding] = []
        self.ai_summary = ""
        self.notice = ("Ready", "idle")
        self.view = None
        self._painted: dict[str, dict] = {}
        self._lock = threading.RLock()
        self._busy: str | None = None

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
                surfaces={"inline": False, "session": "sidebar"},
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

    def _paint(self, nodes: list[dict]) -> None:
        with self.view.batch():
            before = None
            for node in nodes:
                node_id = node["id"]
                if self._painted.get(node_id) != node:
                    if node_id in self._painted:
                        self.view.drop(node_id)
                    self.view.add(node, after=before)
                    self._painted[node_id] = node
                before = node_id

    # Rendering ---------------------------------------------------------------

    def diffs(self) -> list[FileDiff]:
        """The diffs of the chosen range and file."""
        diffs = self.journal.diff(self.start, self.end)
        return [d for d in diffs if self.path is None or d.path == self.path]

    def all_findings(self, diffs: list[FileDiff]) -> list[checks.Finding]:
        """Diff checks of `diffs`, then the findings that the last runs gave."""
        shown = {d.path for d in diffs}
        found = checks.diff_checks(diffs, self.journal.store.get)
        return found + [f for f in self.findings if not f.path or f.path in shown]

    def render(self) -> list[dict]:
        """Every node of the app for the current state."""
        diffs_all = self.journal.diff(self.start, self.end)
        if all(d.path != self.path for d in diffs_all):
            # An undo or a new range can take the chosen file out of the list.
            self.path = None
        diffs = [d for d in diffs_all if self.path is None or d.path == self.path]
        findings = self.all_findings(diffs)
        counts = checks.summary(findings)
        per_file: dict[str, int] = {}
        for finding in findings:
            per_file[finding.path] = per_file.get(finding.path, 0) + 1
        end = self.end or self.journal.last
        scope = (
            "all changes"
            if self.start <= 1 and self.end is None
            else f"#{self.start}..#{end}"
        )
        stats = [
            {"id": "range", "label": "Range", "value_text": scope},
            {"id": "files", "label": "Files", "value_text": str(len(diffs_all))},
            {
                "id": "added",
                "label": "Added",
                "value_text": f"+{sum(d.added for d in diffs_all)}",
                "tone": "ok",
            },
            {
                "id": "removed",
                "label": "Removed",
                "value_text": f"-{sum(d.removed for d in diffs_all)}",
                "tone": "error",
            },
            {
                "id": "errors",
                "label": "Errors",
                "value_text": str(counts["error"]),
                "tone": "error" if counts["error"] else "ok",
            },
            {
                "id": "warnings",
                "label": "Warnings",
                "value_text": str(counts["warning"]),
                "tone": "warn" if counts["warning"] else "ok",
            },
        ]
        text, tone = self.notice
        notice = ui.status("notice", text=text, tone=tone)
        if self.ai_summary:
            notice = ui.status(
                "notice", text=text, tone=tone, detail=self.ai_summary[:2000]
            )
        has_change = self.selected is not None and self.selected > 1
        buttons = ui.group(
            ui.button(
                "all",
                "All changes",
                is_disabled=self.start <= 1 and self.end is None and self.path is None,
            ),
            ui.button("refresh", "Refresh"),
            ui.button("checks", "Run checks", variant="primary", is_disabled=not diffs),
            ui.button(
                "tests", "Run tests", is_disabled=not diffs or self._call is None
            ),
            ui.button("ai", "AI review", is_disabled=not diffs or self._ask is None),
            ui.button(
                "undo-file",
                "Undo file",
                variant="destructive",
                is_disabled=self.path is None,
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
            direction="row",
            id="actions",
        )
        rows = [
            ui.table_row(
                str(e.seq),
                [
                    f"#{e.seq}",
                    e.kind,
                    e.label or "-",
                    str(len(e.files)),
                    time.strftime("%H:%M:%S", time.localtime(e.at)),
                ],
            )
            for e in reversed(self.journal.entries[-MOST_CHECKPOINTS:])
        ]
        checkpoints = ui.table(
            "checkpoints",
            [
                ui.table_column(c, label)
                for c, label in (
                    ("seq", "#"),
                    ("kind", "Kind"),
                    ("label", "Label"),
                    ("files", "Files"),
                    ("at", "Time"),
                )
            ],
            rows=rows,
            is_selectable=True,
            label="Checkpoints",
            selected_ids=[] if self.selected is None else [str(self.selected)],
        )
        files = ui.table(
            "files",
            [
                ui.table_column(c, label)
                for c, label in (
                    ("path", "File"),
                    ("status", "Status"),
                    ("added", "+"),
                    ("removed", "-"),
                    ("findings", "Findings"),
                )
            ],
            rows=[
                ui.table_row(
                    d.path,
                    [
                        d.path,
                        d.status,
                        f"+{d.added}",
                        f"-{d.removed}",
                        str(per_file.get(d.path, 0)),
                    ],
                    **(
                        {"tone": "error"}
                        if any(
                            f.level == "error" and f.path == d.path for f in findings
                        )
                        else {}
                    ),
                )
                for d in diffs_all
            ],
            is_selectable=True,
            label="Changed files",
            selected_ids=[] if self.path is None else [self.path],
        )
        patch, cut = _patch_of(diffs)
        diff = ui.diff(
            "diff",
            patch or "No change in this range.\n",
            old_revision=f"#{max(self.start, 0)}",
            new_revision=f"#{end}" if self.end is not None else "now",
            label=("Diff, cut to fit: " + ", ".join(cut[:5])) if cut else "Diff",
            threads=_threads(
                findings,
                {d.path for d in diffs if d.patch},
                int((self.journal.entries[-1].at if self.journal.entries else 0) * 1000)
                or 1,
            ),
        )
        table = ui.table(
            "findings",
            [
                ui.table_column(c, label)
                for c, label in (
                    ("level", "Level"),
                    ("check", "Check"),
                    ("where", "Where"),
                    ("message", "Message"),
                )
            ],
            rows=[
                ui.table_row(
                    f"f{n}",
                    [
                        f.level,
                        f.check,
                        f"{f.path}:{f.line}" if f.line else f.path or "-",
                        f.message[:500],
                    ],
                    tone=_TONES[f.level],
                )
                for n, f in enumerate(findings)
            ],
            label="Findings",
        )
        root = ui.status("root", text=f"Changes under {self.journal.root}", tone="idle")
        return [
            root,
            ui.stat("summary", stats=stats),
            notice,
            buttons,
            checkpoints,
            files,
            diff,
            table,
        ]

    # Events ------------------------------------------------------------------

    def on_event(self, event) -> None:
        """The `on_event` callback: one event from a person, from any thread."""
        try:
            self.dispatch(event_dict(event))
        except Exception as error:  # noqa: BLE001 - one bad event must not end the app
            LOG.exception("vis-review: event %r failed", event)
            self.refresh((f"{type(error).__name__}: {error}"[:300], "error"))

    def dispatch(self, event: dict) -> None:
        """Do what one event asks and paint the result."""
        kind, node = event.get("type"), event.get("node_id")
        if kind == "activate" and node in SLOW:
            self._slow(node)
            return
        with self._lock:
            if kind == "select" and node == "checkpoints":
                self._choose_checkpoint(event.get("item_ids") or [])
            elif kind == "select" and node == "files":
                ids = event.get("item_ids") or []
                self.path = None if not ids or ids[0] == self.path else str(ids[0])
                self.refresh()
            elif kind == "activate" and node in BUTTONS:
                getattr(self, "_" + node.replace("-", "_"))()
            elif kind == "submit":
                self._comments(event)

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
        self._clear()
        self.refresh(
            (
                "Showing "
                + ("all changes" if self.end is None else f"checkpoint #{seq}"),
                "idle",
            )
        )

    def _clear(self) -> None:
        self.findings = []
        self.ai_summary = ""

    def _all(self) -> None:
        self.start, self.end, self.selected, self.path = 1, None, None, None
        self._clear()
        self.refresh(("Showing all changes", "idle"))

    def _refresh(self) -> None:
        entry = self.journal.checkpoint("manual", "Refresh")
        self.refresh(
            (
                "Recorded checkpoint #%d" % entry.seq if entry else "No new change",
                "idle",
            )
        )

    def _checks(self, *, is_tested: bool = False) -> None:
        with self._lock:
            diffs, scope = self.diffs(), self._scope()
            self.refresh(
                (
                    "Running tests and checks" if is_tested else "Running checks",
                    "running",
                )
            )
        found: list[checks.Finding] = []
        if self._call is not None:
            paths = [d.path for d in diffs if d.status != "deleted"]
            found = checks.language_checks(
                self.journal.root, paths, self._call, is_tested=is_tested
            )
        else:
            found = [
                checks.Finding(
                    "tool",
                    "info",
                    "",
                    0,
                    "No language tools here; only the diff checks ran.",
                    "language",
                )
            ]
        with self._lock:
            if scope != self._scope():
                return
            self.findings = [f for f in self.findings if f.source == "ai"] + found
            counts = checks.summary(self.all_findings(diffs))
            tone = "error" if counts["error"] else "warn" if counts["warning"] else "ok"
            self.refresh(
                (
                    f"Checks done: {counts['error']} errors, {counts['warning']} warnings",
                    tone,
                )
            )

    def _tests(self) -> None:
        self._checks(is_tested=True)

    def _ai(self) -> None:
        with self._lock:
            diffs, scope = self.diffs(), self._scope()
            known = self.all_findings(diffs)
            self.refresh(("The AI review is running", "running"))
        summary, found = ai.review(self.journal.root, diffs, self._ask, known)
        with self._lock:
            if scope != self._scope():
                return
            self.findings = [f for f in self.findings if f.source != "ai"] + found
            self.ai_summary = summary
            self.refresh(
                (f"AI review done: {len(found)} findings", "warn" if found else "ok")
            )

    def _scope(self) -> tuple:
        """What the shown results belong to: a result for another scope is dropped."""
        return (self.start, self.end, self.path)

    def _slow(self, node: str) -> None:
        """Run a slow button outside the lock, so a block hook never waits for it."""
        with self._lock:
            if self._busy:
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
        self.start, self.end, self.selected = 1, None, None
        self._clear()
        self.refresh(
            ("; ".join(parts) + (f" (#{done.entry.seq})" if done.entry else ""), tone)
        )

    def _comments(self, event: dict) -> None:
        comments = event.get("comments") or []
        submission = event.get("submission_id")
        lines = []
        for comment in comments:
            anchor = comment.get("anchor") or {}
            where = (
                f"{anchor.get('path')}:{anchor.get('line')}" if anchor else "the change"
            )
            lines.append(f"- {where}: {str(comment.get('body', '')).strip()}")
        if not lines:
            return
        end = self.end or self.journal.last
        text = (
            f"Review comments on checkpoints #{self.start}..#{end} under {self.journal.root}:\n"
            + "\n".join(lines)
        )
        if self._send is None:
            if submission:
                self.view.set_status(
                    submission, "failed", note="This app cannot send to a session."
                )
            return
        self._send(text)
        if submission:
            self.view.set_status(submission, "answered", note="Sent to the session.")
        self.refresh(("Sent the comments to the session", "ok"))


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


def _threads(findings, paths, now: int) -> list[dict]:
    """One thread for each finding on a line of a shown file, dated by the newest checkpoint.

    A date that stays the same keeps the diff node the same, so a paint keeps the
    comments a person is writing.
    """
    threads = []
    for n, finding in enumerate(findings):
        if finding.line < 1 or finding.path not in paths:
            continue
        threads.append(
            {
                "id": f"finding-{n}",
                "anchor": {"path": finding.path, "side": "new", "line": finding.line},
                "messages": [
                    {
                        "id": f"finding-{n}-1",
                        "author": _AUTHOR,
                        "created_at": now,
                        "body": f"{finding.level} {finding.check}: {finding.message}"[
                            :20000
                        ],
                    }
                ],
            }
        )
    return threads


def _picture(view, nodes: list[dict]) -> dict[str, dict]:
    """The nodes that an opened or adopted app shows, cut to the keys the render sets."""
    try:
        picture = view.state() or {}
    except Exception:  # noqa: BLE001 - an app without a picture is new
        return {node["id"]: node for node in nodes}
    shown = {
        node.get("id"): node
        for node in picture.get("nodes") or []
        if isinstance(node, dict)
    }
    painted = {}
    for node in nodes:
        old = shown.get(node["id"])
        if old is not None:
            painted[node["id"]] = {key: old.get(key) for key in node}
    return painted
