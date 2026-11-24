"""Open a Board app and paint the latest workspace board."""

import logging
import threading
import time
from pathlib import Path

import blockether.vis.extension as vis

from vis_board.store import (
    DEFAULT_KIND,
    DEFAULT_PRIORITY,
    GROUPS,
    KINDS,
    PRIORITIES,
    Store,
    board_node,
    edit,
    matching,
    move,
)

LOG = logging.getLogger(__name__)
SURFACES = {"inline": False, "session": "chat"}
ANY = "any"
NO_ONE = "none"
WEEK_MS = 7 * 24 * 60 * 60 * 1000
FILTERS = {
    "query": "",
    "assignees": [],
    "epic": ANY,
    "label": ANY,
    "priority": ANY,
    "kind": ANY,
    "group": GROUPS[0],
    "flagged": False,
    "recent": False,
    "hide_done": False,
}
GROUP_WORDS = {"none": "No rows", "assignee": "Assignee", "epic": "Epic", "priority": "Priority"}
NEW_FIELDS = ("new_title", "new_column", "new_assignee", "new_priority", "new_kind", "new_text")


def _option(value, label):
    return vis.ui.select.option(value, label)


def _words(values):
    return [_option(value, value[:1].upper() + value[1:]) for value in values]


def tools_node():
    """The search and the verb of the app band."""
    return vis.ui.toolbar(
        "tools",
        vis.ui.plaintext(
            "query", label="Search issues", placeholder="Search issues", icon="search"
        ),
        vis.ui.button("create", "Create issue", icon="plus", variant="primary"),
    )


def _labels(data):
    return sorted({label for issue in data["issues"] for label in issue.get("labels", [])})


def _choices(data):
    """The options that change with the board. A change rebuilds the filters."""
    return (
        tuple((one["id"], one["name"]) for one in data["people"]),
        tuple((one["id"], one["title"]) for one in data["epics"]),
        tuple(_labels(data)),
    )


def filters_node(data, filters):
    """The filters of the board, with the values that the person chose."""
    people, epics, labels = _choices(data)
    return vis.ui.toolbar(
        "filters",
        vis.ui.select.many(
            "assignees",
            [_option(i, name) for i, name in people] + [_option(NO_ONE, "Unassigned")],
            label="Assignee",
            placeholder="Assignee",
            value=filters["assignees"],
        ),
        vis.ui.select(
            "epic",
            [_option(ANY, "All epics")] + [_option(i, title) for i, title in epics],
            label="Epic",
            value=filters["epic"],
        ),
        vis.ui.select(
            "label",
            [_option(ANY, "All labels")] + [_option(one, one) for one in labels],
            label="Label",
            value=filters["label"],
        ),
        vis.ui.select(
            "priority",
            [_option(ANY, "All priorities")] + _words(PRIORITIES),
            label="Priority",
            value=filters["priority"],
        ),
        vis.ui.select(
            "kind",
            [_option(ANY, "All types")] + _words(KINDS),
            label="Type",
            value=filters["kind"],
        ),
        vis.ui.select(
            "group",
            [_option(one, GROUP_WORDS.get(one, one)) for one in GROUPS],
            label="Group by",
            value=filters["group"],
        ),
        vis.ui.checkbox("flagged", label="Flagged", value=filters["flagged"]),
        vis.ui.checkbox("recent", label="Updated this week", value=filters["recent"]),
        vis.ui.checkbox("hide_done", label="Hide done", value=filters["hide_done"]),
    )


def create_node(data, *, is_open=False):
    """The dialog that makes a new issue."""
    return vis.ui.dialog(
        "new-issue",
        "Create issue",
        vis.ui.plaintext("new_title", label="Title", is_required=True, max_length=200),
        vis.ui.row(
            "new-place",
            vis.ui.select(
                "new_column",
                [_option(one["id"], one["title"]) for one in data["columns"]],
                label="Column",
                value=data["columns"][0]["id"],
            ),
            vis.ui.select(
                "new_assignee",
                [_option(NO_ONE, "Unassigned")]
                + [_option(one["id"], one["name"]) for one in data["people"]],
                label="Assignee",
                value=NO_ONE,
            ),
        ),
        vis.ui.row(
            "new-kind",
            vis.ui.select(
                "new_priority", _words(PRIORITIES), label="Priority", value=DEFAULT_PRIORITY
            ),
            vis.ui.select("new_kind", _words(KINDS), label="Type", value=DEFAULT_KIND),
        ),
        vis.ui.multiline("new_text", label="Description"),
        vis.ui.button("new-submit", "Create", action="submit", variant="primary"),
        is_open=is_open,
    )


def shown(data, filters, now_ms=None):
    """The issues that match the filters of the app."""
    now_ms = time.time_ns() // 1_000_000 if now_ms is None else now_ms
    pick = lambda key: None if filters[key] == ANY else filters[key]  # noqa: E731
    return matching(
        data["issues"],
        query=filters["query"] or "",
        assignees=[None if one == NO_ONE else one for one in filters["assignees"]],
        epic=pick("epic"),
        flagged=filters["flagged"],
        label=pick("label"),
        priority=pick("priority"),
        kind=pick("kind"),
        since=now_ms - WEEK_MS if filters["recent"] else None,
        hidden_columns={one["id"] for one in data["columns"] if one.get("is_done")}
        if filters["hide_done"]
        else (),
    )


class BoardApp:
    """Show one person's view of the workspace board."""

    def __init__(self, store, *, opener=vis.attach):
        self.store = store
        self.opener = opener
        self.lock = threading.RLock()
        self.view = None
        self._stop = threading.Event()
        self._file_version = None
        self.filters = dict(FILTERS)
        self._choices = None
        self._data = None

    def _stamp(self):
        try:
            stat = self.store.path.stat()
            return stat.st_ino, stat.st_mtime_ns
        except FileNotFoundError:
            return None

    def open(self, *, on_reload=None, watch=True):
        """Open the board, or connect to the board with the same key."""
        with self.lock:
            version = self._stamp()
            data = self.store.read()
            self._choices = _choices(data)
            declaration = vis.ui.app(
                "Board",
                tools_node(),
                filters_node(data, self.filters),
                board_node(data, shown(data, self.filters)),
                create_node(data),
                description="Plan and track work in this workspace.",
                key="board",
                icon="board",
                surfaces=SURFACES,
                settings_name="board",
            )
            self.view = self.opener(
                declaration,
                on_event=self.on_event,
                on_reload=on_reload,
            )
            self._file_version = version
            self._paint(data)
        if watch:
            threading.Thread(target=self._watch, name="vis-board", daemon=True).start()
        return self

    def stop(self):
        """Stop watching the board file."""
        self._stop.set()

    def _watch(self):
        """Repaint after another session or process saves this board."""
        while not self._stop.wait(0.75):
            try:
                with self.lock:
                    self.view.state()
                    if self.view.is_interrupted:
                        return
                    if self._stamp() != self._file_version:
                        self.refresh()
            except vis.Interrupted:
                return
            except Exception:
                with self.lock:
                    self._log_failure("Refresh board")
                return

    def _paint(self, data):
        self._data = data
        with self.view.batch():
            if (choices := _choices(data)) != self._choices:
                self._choices = choices
                self.view.drop("filters")
                self.view.add(filters_node(data, self.filters), after="tools")
            self.view["issues"].set(
                columns=data["columns"],
                people=data["people"],
                epics=data["epics"],
                group_by=self.filters["group"],
            )
            self.view["issues"].replace(shown(data, self.filters))

    def _open_create(self):
        """Open a new Create dialog with the latest columns and people."""
        data = self._data or self.store.read()
        with self.view.batch():
            self.view.drop("new-issue")
            self.view.add(create_node(data, is_open=True), after="issues")

    def _create(self, values):
        assignee = values.get("new_assignee")
        change = {
            "title": str(values.get("new_title") or "").strip(),
            "column": values.get("new_column"),
            "assignee": None if assignee in (None, NO_ONE) else assignee,
            "priority": values.get("new_priority") or DEFAULT_PRIORITY,
            "kind": values.get("new_kind") or DEFAULT_KIND,
        }
        if text := str(values.get("new_text") or "").strip():
            change["text"] = text
        self.store.change(lambda data: edit(data, change))
        self.view["new-issue"].set(is_open=False)
        self.refresh()

    def refresh(self):
        """Paint the latest store contents."""
        with self.lock:
            version = self._stamp()
            self._paint(self.store.read())
            self._file_version = version

    def _log_failure(self, action):
        LOG.exception("%s failed", action)
        try:
            self.view.log(f"{action} failed.", "error").flush()
        except Exception:
            LOG.exception("Cannot write to the board log")

    def on_event(self, event):
        """Handle one action. Keep the app open if the event fails."""
        with self.lock:
            try:
                if event.type == "change" and event.node_id in FILTERS:
                    self.filters[event.node_id] = (
                        FILTERS[event.node_id] if event.value is None else event.value
                    )
                    self._paint(self._data or self.store.read())
                elif event.type == "activate" and event.node_id == "create":
                    self._open_create()
                elif event.type == "submit" and event.node_id == "new-submit":
                    self._create(event.values or {})
                elif event.node_id == "issues" and event.type == "edit":
                    self.store.change(
                        lambda data: edit(
                            data,
                            event.values or {},
                            event.item_id,
                            remove=event.is_removed,
                        )
                    )
                    self.refresh()
                elif event.node_id == "issues" and event.type == "move":
                    self.store.change(
                        lambda data: move(
                            data,
                            event.item_id,
                            event.column,
                            event.index,
                            visible={one["id"] for one in shown(data, self.filters)},
                        )
                    )
                    self.refresh()
            except Exception:
                self._log_failure("Update board")
                # The host applies a move before it tells the owner. Restore its view
                # from the store if the write failed.
                if event.type == "move":
                    try:
                        self.refresh()
                    except Exception:
                        self._log_failure("Restore board")


class WorkspaceBoards:
    """Keep the app handles that this extension runtime opened."""

    def __init__(self):
        self._apps = {}
        self._lock = threading.Lock()

    def open(self, session_id, workspace, on_reload):
        """Open or reconnect the board of a session."""
        with self._lock:
            if old := self._apps.get(session_id):
                old.stop()
            app = BoardApp(Store(workspace)).open(on_reload=on_reload)
            self._apps[session_id] = app

    def refresh(self, workspace):
        """Paint open apps of this runtime now. Other runtimes watch the file."""
        path = Path(workspace).resolve()
        with self._lock:
            apps = [
                item for item in self._apps.items() if item[1].store.workspace == path
            ]
        for session_id, app in apps:
            try:
                app.refresh()
            except vis.Interrupted:
                app.stop()
                with self._lock:
                    if self._apps.get(session_id) is app:
                        del self._apps[session_id]
            except Exception:
                LOG.exception("Cannot refresh the open board")


BOARDS = WorkspaceBoards()
