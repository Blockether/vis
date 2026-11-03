"""The review app: checkpoints, changed files, the diff with findings, checks and undo.

`ReviewApp` works the same at both doors. `opener(declaration, on_event)` opens
the app (or adopts it by key) and answers its `vis.LiveAttachment` handle. The
door also gives `call` for the language tools, `ask` for the AI review and `send`
for the review comments that a person sends to the session.

Every paint renders the whole app and replaces only the nodes that changed. The
first node never changes, so each node keeps its place.
"""

from __future__ import annotations

import hashlib
import logging
import threading
import time
import traceback
from collections import Counter, OrderedDict
from collections.abc import Callable, Mapping
from typing import Any

import blockether.vis.extension as vis
from blockether.vis import _contracts

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
# Changed pictures the app shows at most.
MOST_IMAGES = 8
# Decoded picture bytes that the app shows together, old and new: the view bound.
MOST_PICTURE_BYTES = _contracts.definition("view", "live_view")[
    "x-vis-max-picture-bytes"
]
# Decoded picture bytes that the image cache keeps, newest first.
MOST_CACHED_PICTURE_BYTES = 2 * MOST_PICTURE_BYTES
MOST_CACHED_IMAGES = 256
# The name of a checks run, by whether it runs the tests too.
_RUN_NAMES = {True: "Tests and checks", False: "Checks"}


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
        # Image nodes by (path, old digest, new digest): a paint reads no blob twice.
        self._images: OrderedDict[tuple, dict | None] = OrderedDict()

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
            # A node that the render no longer has, like the images of an old range.
            for node_id in set(self._painted) - {node["id"] for node in nodes}:
                self.view.drop(node_id)
                del self._painted[node_id]

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
        end = self.end or self.journal.last
        images = self.image_nodes(diffs)
        return [
            ui.status("root", text=f"Changes under {self.journal.root}", tone="idle"),
            ui.stat(
                "summary", stats=_stats(self._range_text(end), diffs_all, findings)
            ),
            self._notice(),
            self._buttons(bool(diffs)),
            self._checkpoints(),
            _files_table(diffs_all, findings, self.path),
            self._diff(diffs, findings, end),
            *([ui.group("images", *images, label="Changed images")] if images else []),
            _findings_table(findings),
        ]

    def _is_all(self) -> bool:
        """True when the app shows the whole journal: the first range."""
        return self.start <= 1 and self.end is None

    def _range_text(self, end: int) -> str:
        return "all changes" if self._is_all() else f"#{self.start}..#{end}"

    def _notice(self) -> dict:
        text, tone = self.notice
        if self.ai_summary:
            return ui.status(
                "notice", text=text, tone=tone, detail=self.ai_summary[:2000]
            )
        return ui.status("notice", text=text, tone=tone)

    def _buttons(self, has_diffs: bool) -> dict:
        """The action buttons; each one is off when it cannot do anything now."""
        has_change = self.selected is not None and self.selected > 1
        return ui.group(
            ui.button(
                "all", "All changes", is_disabled=self._is_all() and self.path is None
            ),
            ui.button("refresh", "Refresh"),
            ui.button(
                "checks", "Run checks", variant="primary", is_disabled=not has_diffs
            ),
            ui.button(
                "tests", "Run tests", is_disabled=not has_diffs or self._call is None
            ),
            ui.button(
                "ai", "AI review", is_disabled=not has_diffs or self._ask is None
            ),
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

    def _checkpoints(self) -> dict:
        """The checkpoint table, newest first, with the chosen checkpoint selected."""
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
        return ui.table(
            "checkpoints",
            _columns(
                ("seq", "#"),
                ("kind", "Kind"),
                ("label", "Label"),
                ("files", "Files"),
                ("at", "Time"),
            ),
            rows=rows,
            is_selectable=True,
            label="Checkpoints",
            selected_ids=_selected(self.selected),
        )

    def _diff(self, diffs: list[FileDiff], findings, end: int) -> dict:
        """The diff node of `diffs`, with one thread for each finding on a line."""
        patch, cut = _patch_of(diffs)
        newest = self.journal.entries[-1].at if self.journal.entries else 0
        return ui.diff(
            "diff",
            patch or "No change in this range.\n",
            old_revision=f"#{max(self.start, 0)}",
            new_revision=f"#{end}" if self.end is not None else "now",
            label=("Diff, cut to fit: " + ", ".join(cut[:5])) if cut else "Diff",
            threads=_threads(
                findings,
                {d.path for d in diffs if d.patch},
                int(newest * 1000) or 1,
            ),
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
            known = self.all_findings(diffs)
            self.refresh(("The AI review is running", "running"))
        started = time.perf_counter()
        summary, found = ai.review(self.journal.root, diffs, self._ask, known)
        self.log(f"The AI review ran in {_ms(started)} ms: {len(found)} findings")
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
            self.log("Refused the comments: this app cannot send to a session", "warn")
            if submission:
                self.view.set_status(
                    submission, "failed", note="This app cannot send to a session."
                )
            return
        self._send(text)
        if submission:
            self.view.set_status(submission, "answered", note="Sent to the session.")
        self.refresh(("Sent the comments to the session", "ok"))


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


def _columns(*pairs: tuple[str, str]) -> list[dict]:
    """Table columns from `(id, label)` pairs."""
    return [ui.table_column(column, label) for column, label in pairs]


def _selected(item) -> list[str]:
    """The selected ids of a table: none, or the one chosen item."""
    return [] if item is None else [str(item)]


def _stats(scope: str, diffs: list[FileDiff], findings) -> list[dict]:
    """The summary numbers: range, files, lines and the findings by level."""
    counts = checks.summary(findings)
    return [
        {"id": "range", "label": "Range", "value_text": scope},
        {"id": "files", "label": "Files", "value_text": str(len(diffs))},
        {
            "id": "added",
            "label": "Added",
            "value_text": f"+{sum(d.added for d in diffs)}",
            "tone": "ok",
        },
        {
            "id": "removed",
            "label": "Removed",
            "value_text": f"-{sum(d.removed for d in diffs)}",
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


def _files_table(diffs: list[FileDiff], findings, path: str | None) -> dict:
    """The changed files, each with its findings; a file with an error is red."""
    per_file = Counter(finding.path for finding in findings)
    failed = {finding.path for finding in findings if finding.level == "error"}
    return ui.table(
        "files",
        _columns(
            ("path", "File"),
            ("status", "Status"),
            ("added", "+"),
            ("removed", "-"),
            ("findings", "Findings"),
        ),
        rows=[
            ui.table_row(
                d.path,
                [
                    d.path,
                    d.status,
                    f"+{d.added}",
                    f"-{d.removed}",
                    str(per_file[d.path]),
                ],
                **({"tone": "error"} if d.path in failed else {}),
            )
            for d in diffs
        ],
        is_selectable=True,
        label="Changed files",
        selected_ids=_selected(path),
    )


def _where(finding) -> str:
    """Where a finding is: `path:line`, the path, or `-`."""
    if finding.line:
        return f"{finding.path}:{finding.line}"
    return finding.path or "-"


def _findings_table(findings) -> dict:
    """Every finding, toned by its level."""
    return ui.table(
        "findings",
        _columns(
            ("level", "Level"),
            ("check", "Check"),
            ("where", "Where"),
            ("message", "Message"),
        ),
        rows=[
            ui.table_row(
                f"f{n}",
                [f.level, f.check, _where(f), f.message[:500]],
                tone=_TONES[f.level],
            )
            for n, f in enumerate(findings)
        ],
        label="Findings",
    )


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


def _shown_nodes(view) -> dict[str, dict] | None:
    """The nodes that `view` shows by id, or None when it has no picture yet."""
    try:
        picture = view.state() or {}
    except Exception:  # noqa: BLE001 - an app without a picture is new
        return None
    nodes = picture.get("nodes") or []
    return {node.get("id"): node for node in nodes if isinstance(node, dict)}


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
