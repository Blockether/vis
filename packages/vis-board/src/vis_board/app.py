"""Open the global Board app and paint the boards of all projects."""

import logging
import threading
import time
from pathlib import Path

import blockether.vis.extension as vis

from vis_board import start
from vis_board.store import (
    DEFAULT_KIND,
    DEFAULT_PRIORITY,
    GROUPS,
    KINDS,
    PRIORITIES,
    Store,
    board_dir,
    board_node,
    columns,
    edit,
    matching,
    move,
    reference_matches,
)

CHANGED = "board.changed"
"""The `vis.events` topic of a saved board. The data names the project folder."""

LOG = logging.getLogger(__name__)
ANY = "any"
ALL = "all"
NO_ONE = "none"
WEEK_MS = 7 * 24 * 60 * 60 * 1000
FILTERS = {
    "project": ALL,
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
GROUP_WORDS = {
    "none": "No rows",
    "assignee": "Assignee",
    "epic": "Epic",
    "priority": "Priority",
}


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
    return sorted(
        {label for issue in data["issues"] for label in issue.get("labels", [])}
    )


def _choices(data):
    """The options that change with the board. A change rebuilds the filters."""
    return (
        tuple((one["id"], one["name"]) for one in data["people"]),
        tuple((one["id"], one["title"]) for one in data["epics"]),
        tuple(_labels(data)),
    )


def _project_options(folders):
    """One option for each project folder. A repeated name also shows its parent."""
    names = [folder.name for folder in folders]
    return [
        _option(
            str(folder),
            folder.name
            if names.count(folder.name) == 1
            else f"{folder.name} ({folder.parent})",
        )
        for folder in folders
    ]


def filters_node(data, filters, folders=()):
    """The filters of the board, with the values that the person chose."""
    people, epics, labels = _choices(data)
    return vis.ui.toolbar(
        "filters",
        vis.ui.select(
            "project",
            [_option(ALL, "All projects")] + _project_options(folders),
            label="Project",
            value=filters["project"],
        ),
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


def _project_field(folders, project):
    """The project of a new issue: a choice, or a folder path when Vis knows none."""
    if not folders:
        return vis.ui.plaintext(
            "new_project", label="Project folder", is_required=True, max_length=4096
        )
    known = {str(folder) for folder in folders}
    return vis.ui.select(
        "new_project",
        _project_options(folders),
        label="Project",
        value=project if project in known else str(folders[0]),
    )


def create_node(data, *, folders=(), project=ALL, is_open=False):
    """The dialog that makes a new issue."""
    return vis.ui.dialog(
        "new-issue",
        "Create issue",
        _project_field(folders, project),
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
                "new_priority",
                _words(PRIORITIES),
                label="Priority",
                value=DEFAULT_PRIORITY,
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


def merged(boards):
    """One board of the issues, people and epics of `(store, data)` pairs."""
    people, epics, issues = {}, {}, []
    for _, data in boards:
        for one in data["people"]:
            people.setdefault(one["id"], one)
        for one in data["epics"]:
            epics.setdefault(one["id"], one)
        issues.extend(data["issues"])
    return {
        "columns": columns(),
        "people": list(people.values()),
        "epics": list(epics.values()),
        "issues": issues,
    }


def vis_projects():
    """The folders of the Vis projects that are not archived. None when Vis does not answer."""
    try:
        page = vis._host_layer().routes.get_projects()
    except Exception:
        LOG.debug("Cannot list the Vis projects", exc_info=True)
        return []
    return [
        Path(one["workspace_root"])
        for one in page.get("projects") or []
        if one.get("workspace_root") and not one.get("archived_at")
    ]


class BoardApp:
    """Show one person's view of the boards of all projects."""

    def __init__(self, *, home=None, opener=vis.attach, projects=vis_projects):
        self.home = home
        self.opener = opener
        self.projects = projects
        self.lock = threading.RLock()
        self.view = None
        self._subscription = None
        self._file_version = None
        self.filters = dict(FILTERS)
        self._choices = None
        self._boards = []
        self._known = []

    def _stamp(self):
        """The name, inode and change time of each saved board."""
        stamps = []
        for path in sorted(board_dir(self.home).glob("*.json")):
            try:
                stat = path.stat()
            except FileNotFoundError:
                continue
            stamps.append((path.name, stat.st_ino, stat.st_mtime_ns))
        return tuple(stamps)

    def _read(self):
        """Read each saved board of a project."""
        return [(store, store.read()) for store in Store.all(self.home)]

    def folders(self):
        """The project folders: those with a board, then the other Vis projects."""
        seen = {store.workspace for store, _ in self._boards}
        more = []
        for folder in self._known:
            path = folder.resolve()
            if path not in seen:
                seen.add(path)
                more.append(path)
        return [store.workspace for store, _ in self._boards] + sorted(
            more, key=lambda one: (one.name.casefold(), str(one))
        )

    def _visible(self):
        """The boards that the project filter shows."""
        project = self.filters["project"]
        return [
            pair
            for pair in self._boards
            if project == ALL or str(pair[0].workspace) == project
        ]

    def _store(self, folder):
        return Store(folder, self.home)

    def _owner(self, issue_id):
        """The store of the board that holds issue `issue_id`."""
        for store, data in self._boards:
            if any(reference_matches(one, issue_id) for one in data["issues"]):
                return store
        raise ValueError(f"No issue with ID or key {issue_id!r}.")

    def open(self, *, on_reload=None, watch=True):
        """Open the board, or connect to the board with the same key."""
        if watch:
            # Subscribe before the first read, so that no save falls between them.
            self._subscription = vis.events.subscribe(CHANGED, self._changed)
        with self.lock:
            version = self._stamp()
            self._boards = self._read()
            self._known = list(self.projects())
            data = merged(self._visible())
            self._choices = (_choices(data), tuple(self.folders()))
            declaration = vis.ui.app(
                "Board",
                tools_node(),
                filters_node(data, self.filters, self.folders()),
                board_node(data, shown(data, self.filters)),
                create_node(
                    data, folders=self.folders(), project=self.filters["project"]
                ),
                description="Plan and track the work of all projects.",
                key="board",
                icon="board",
                scope="global",
            )
            self.view = self.opener(
                declaration,
                on_event=self.on_event,
                on_reload=on_reload,
            )
            self._file_version = version
            self._paint()
        return self

    def stop(self):
        """Stop following the saved boards."""
        if self._subscription is not None:
            self._subscription.close()

    def _changed(self, event):
        """Repaint after a tool of a session saved a board."""
        try:
            with self.lock:
                if self.view is None:
                    return
                self.view.state()
                if self.view.is_interrupted:
                    self.stop()
                elif self._stamp() != self._file_version:
                    self.refresh()
        except vis.Interrupted:
            self.stop()
        except Exception:
            with self.lock:
                self._log_failure("Refresh board")

    def _paint(self):
        data = merged(self._visible())
        with self.view.batch():
            if (choices := (_choices(data), tuple(self.folders()))) != self._choices:
                self._choices = choices
                self.view.drop("filters")
                self.view.add(
                    filters_node(data, self.filters, self.folders()), after="tools"
                )
            self.view["issues"].set(
                columns=data["columns"],
                people=data["people"],
                epics=data["epics"],
                group_by=self.filters["group"],
            )
            self.view["issues"].replace(shown(data, self.filters))

    def _open_create(self):
        """Open a new Create dialog with the latest projects, columns and people."""
        self._known = list(self.projects())
        data = merged(self._visible())
        with self.view.batch():
            self.view.drop("new-issue")
            self.view.add(
                create_node(
                    data,
                    folders=self.folders(),
                    project=self.filters["project"],
                    is_open=True,
                ),
                after="issues",
            )

    def _target(self):
        """The board of a new issue that the board itself makes: the shown project."""
        if self.filters["project"] != ALL:
            return self._store(self.filters["project"])
        if len(self._boards) == 1:
            return self._boards[0][0]
        raise ValueError("Choose a project first.")

    def _create(self, values):
        folder = str(values.get("new_project") or "").strip()
        if not folder or not Path(folder).expanduser().is_dir():
            raise ValueError("The project folder does not exist.")
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
        self._store(Path(folder).expanduser()).change(lambda data: edit(data, change))
        self.view["new-issue"].set(is_open=False)
        self.refresh()

    def refresh(self):
        """Paint the latest saved boards."""
        with self.lock:
            version = self._stamp()
            self._boards = self._read()
            self._paint()
            self._file_version = version

    def _log_failure(self, action):
        LOG.exception("%s failed", action)
        try:
            self.view.log(f"{action} failed.", "error").flush()
        except Exception:
            LOG.exception("Cannot write to the board log")

    def _edit(self, event):
        store = self._owner(event.item_id) if event.item_id else self._target()
        store.change(
            lambda data: edit(
                data, event.values or {}, event.item_id, remove=event.is_removed
            )
        )

    def _move(self, event):
        visible = {one["id"] for one in shown(merged(self._visible()), self.filters)}
        self._owner(event.item_id).change(
            lambda data: move(
                data, event.item_id, event.column, event.index, visible=visible
            )
        )

    def _column(self, issue_id):
        """The column and the ID of issue `issue_id`, or None."""
        for _, data in self._boards:
            for one in data["issues"]:
                if reference_matches(one, issue_id):
                    return one["column"], one["id"]
        return None

    def _steps(self, before, item_id):
        """The SDLC step of an issue that moved: `start`, `finish` or None."""
        after = self._column(item_id)
        if before is None or after is None or before[0] == after[0]:
            return None
        if after[0] == start.START_COLUMN:
            return "start", after[1]
        if after[0] == start.DONE_COLUMN:
            return "finish", after[1]
        return None

    def _step(self, step, issue_id):
        """Start or finish issue `issue_id` as the settings say. Log a failure."""
        log = lambda text: self.view.log(text, "error").flush()  # noqa: E731
        try:
            with self.lock:
                store = self._owner(issue_id)
            plan = start.plan()
            if step == "start":
                self.view.log("Starting the work of the issue.").flush()
                record = start.start(store, issue_id, plan, log=log)
                where = record["workspace"] + (
                    f" ({record['branch']})" if record.get("branch") else ""
                )
                self.view.log(f"Started in {where}.", "ok").flush()
            else:
                start.finish(store, issue_id, plan, log=log)
            self.refresh()
        except Exception:
            self._log_failure("Start" if step == "start" else "Finish")

    def on_event(self, event):
        """Handle one action. Keep the app open if the event fails."""
        step = None
        with self.lock:
            before = (
                self._column(event.item_id)
                if event.node_id == "issues" and event.item_id
                else None
            )
            try:
                if event.type == "change" and event.node_id in FILTERS:
                    self.filters[event.node_id] = (
                        FILTERS[event.node_id] if event.value is None else event.value
                    )
                    self._paint()
                elif event.type == "activate" and event.node_id == "create":
                    self._open_create()
                elif event.type == "submit" and event.node_id == "new-submit":
                    self._create(event.values or {})
                elif event.node_id == "issues" and event.type == "edit":
                    self._edit(event)
                    self.refresh()
                    step = self._steps(before, event.item_id)
                elif event.node_id == "issues" and event.type == "move":
                    self._move(event)
                    self.refresh()
                    step = self._steps(before, event.item_id)
            except Exception:
                self._log_failure("Update board")
                # The host applies a move before it tells the owner. Restore its view
                # from the store if the write failed.
                if event.type == "move":
                    try:
                        self.refresh()
                    except Exception:
                        self._log_failure("Restore board")
        # A Start can take a while: the board stays free for other events meanwhile.
        if step:
            self._step(*step)


class GlobalBoard:
    """Keep the one Board app that this extension runtime opened."""

    def __init__(self):
        self.app = None
        self._lock = threading.Lock()

    def open(self, on_reload):
        """Open or reconnect the Board app."""
        with self._lock:
            if self.app:
                self.app.stop()
            self.app = BoardApp().open(on_reload=on_reload)


BOARD = GlobalBoard()
