"""Open a Board app and paint the latest workspace board."""

import logging
import threading
from pathlib import Path

import blockether.vis.extension as vis

from vis_board.store import Store, board_node, edit, move

LOG = logging.getLogger(__name__)
SURFACES = {"inline": False, "session": "chat"}


class BoardApp:
    """Show one person's view of the workspace board."""

    def __init__(self, store, *, opener=vis.attach):
        self.store = store
        self.opener = opener
        self.lock = threading.RLock()
        self.view = None
        self._stop = threading.Event()
        self._file_version = None

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
            declaration = vis.ui.app(
                "Board",
                board_node(data, data["issues"]),
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
        with self.view.batch():
            self.view["issues"].set(
                columns=data["columns"], people=data["people"], epics=data["epics"]
            )
            self.view["issues"].replace(data["issues"])

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
                if event.node_id == "issues" and event.type == "edit":
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
                            data, event.item_id, event.column, event.index
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
