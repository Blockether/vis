"""Open app models through the two doors: an extension and an SDK program.

Both doors answer the same `vis.LiveAttachment` handle, so `Runner` drives an
app the same way in both. Only the place where the envelopes go differs:

- In an extension, the envelopes go to the extension host (no `door`).
- In an SDK program, `Session.attach(declaration, owner=..., on_event=...)`
  of `blockether.vis.engine` posts each envelope to
  `POST /v1/sessions/:sid/apps` and long-polls the `events` op in a thread.
"""

from __future__ import annotations

import logging
import threading
from collections.abc import Callable, MutableMapping
from typing import Any

import blockether.vis.extension as vis

from live_apps.runtime import Runner

LOG = logging.getLogger("live_apps")


class App(vis.LiveAttachment):
    """A live app handle that gives the runner each event as the raw dict.

    The raw dict keeps every field of the wire event (`action`, `key`,
    `item_ids`), also when this SDK version does not know them yet.
    """

    def _host_event(self, raw):
        if raw.get("type") == "validate":
            # A validate question is for the handle: its validate= functions answer.
            self._judge(raw)
            return
        for callback in list(self._listeners):
            callback(dict(raw))

    def set_shortcuts(self, shortcuts):
        """Replace the shortcuts of the app; an empty list removes them.

        The op joins the open batch. `LiveAttachment.set_shortcuts` sends its
        own patch, but a patch that drops a bound node must replace the
        shortcuts in the same patch.
        """
        return self._push({"op": "shortcuts", "shortcuts": list(shortcuts)})


def open_app(declaration: dict, on_event: Callable[[Any], None], door=None) -> App:
    """Open (or adopt by key) one app declaration and bind `on_event`."""
    request = dict(declaration)
    request.pop("flush_ms", None)
    return App(request, flush_ms=0, on_event=on_event, door=door)


def run_effect(
    runner: Runner, effect: dict, *, open_app_key: Callable[[str], Any] | None = None
) -> None:
    """Run one effect that a model asked for, on the app of `runner`."""
    do = effect.get("do")
    view = runner.view
    if do == "set_status":
        view.set_status(
            effect["submission_id"], effect["status"], note=effect.get("note")
        )
    elif do == "reply":
        view.reply(
            effect["node_id"], effect["body"], submission_id=effect.get("submission_id")
        )
    elif do == "open":
        if open_app_key is None:
            raise ValueError("this runner cannot open other apps")
        open_app_key(effect["app"])
    else:
        raise ValueError(f"unknown effect {do!r}")


class Apps:
    """Every runner of one owner in one session, by app key.

    `opener` opens a declaration; `store` keeps the model states. Opening a
    key that runs already answers its runner; opening it again after a
    restart adopts the open app and paints the saved state on it.
    """

    def __init__(
        self, opener, store: MutableMapping[str, Any] | None = None, *, prefix: str = ""
    ):
        from live_apps import apps

        self.models = apps()
        self._opener = opener
        self._store = _Prefixed(store if store is not None else {}, prefix)
        self.runners: dict[str, Runner] = {}
        self._lock = threading.RLock()

    def open(self, key: str) -> Runner:
        with self._lock:
            if key not in self.models:
                raise KeyError(
                    f"no app {key!r}; choose one of {', '.join(self.models)}"
                )
            runner = self.runners.get(key)
            if runner is not None and runner.view is not None and runner.view._is_open:
                return runner
            runner = Runner(
                self.models[key],
                self._opener,
                store=self._store,
                on_effect=lambda r, effect: run_effect(
                    r, effect, open_app_key=self.open
                ),
            ).open()
            runner.start()
            self.runners[key] = runner
            return runner

    def stop(self) -> None:
        for runner in self.runners.values():
            runner.stop()


class _Prefixed(MutableMapping):
    """A view of `inner` whose keys start with `prefix`."""

    def __init__(self, inner, prefix):
        self._inner = inner
        self._prefix = prefix

    def __getitem__(self, key):
        return self._inner[self._prefix + key]

    def __setitem__(self, key, value):
        self._inner[self._prefix + key] = value

    def __delitem__(self, key):
        del self._inner[self._prefix + key]

    def __iter__(self):
        return (
            k[len(self._prefix) :]
            for k in list(self._inner)
            if k.startswith(self._prefix)
        )

    def __len__(self):
        return sum(1 for _ in self)

    def get(self, key, default=None):
        try:
            return self[key]
        except KeyError:
            return default
