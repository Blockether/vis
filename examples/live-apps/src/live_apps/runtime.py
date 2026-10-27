"""Run one pure app model as a live app through any door.

An app model is a module with these names:

- `KEY`, `TITLE` and `DESCRIPTION`: the app identity and its texts.
- `initial()`: the first state, a JSON-safe dict.
- `reduce(state, event)`: the next state. It never changes `state` in place.
- `render(state)`: the list of view nodes for that state.
- `shortcuts(state)` (optional): the keyboard shortcuts for that state.
- `effects(old, new)` (optional): side effects for the runner, as dicts.
- `READS` (optional): node ids that the reducer reads from the picture. A
  table gives its `selected_ids`, an input gives its `value`. They arrive in
  `event["read"]`, so a press sees what the person chose just before it.
- `TICK_SECONDS` (optional): the runner gives the model a `tick` event at this
  interval, for apps that change without a person.

The runner opens the app, turns each human event into a reducer event, renders
the new state and sends only the difference. The model never talks to a host,
so the same model runs in an extension, in an SDK program and in a test.
"""

from __future__ import annotations

import copy
import logging
import threading
from collections.abc import Callable, Mapping, MutableMapping
from typing import Any

LOG = logging.getLogger("live_apps")

MAX_NODES = 64
# The engine bound for one view: every node of the tree counts, rows do not.

INPUT_TYPES = frozenset(
    (
        "plaintext",
        "password",
        "multiline",
        "select",
        "multiselect",
        "checkbox",
        "range",
        "otp",
    )
)

# The keys that one typed handle can set in place, for each node type.
SETTABLE = {
    "paragraph": ("text",),
    "heading": ("text", "level"),
    "code": ("text", "language"),
    "status": ("text", "tone", "detail", "label"),
    "button": ("label", "is_disabled"),
    "card": ("title", "text", "tone", "meta", "is_selectable"),
    "progress": ("value", "done", "total", "label"),
    "spinner": ("text", "variant", "is_active"),
}

ENGINE_OWNED = {
    "thread": ("messages", "draft", "is_resolved", "anchor"),
    "diff": ("threads",),
}

ITEMS = {
    "stat": "stats",
    "steps": "steps",
    "table": "rows",
    "link": "links",
    "log": "lines",
}


def node_id(node: Mapping[str, Any]) -> str:
    """The id of a node: inputs may give only a name."""
    return str(node.get("id") or node.get("name") or "")


def walk(nodes):
    """Every node of a tree, parents first."""
    for node in nodes or []:
        yield node
        yield from walk(node.get("children"))


def count_nodes(nodes) -> int:
    """How many nodes count against the engine bound."""
    return sum(1 for _ in walk(nodes))


def check_tree(nodes) -> None:
    """Refuse a render that the differ cannot address or the engine refuses."""
    seen = set()
    for node in walk(nodes):
        ident = node_id(node)
        if not ident:
            raise ValueError(
                f"every node needs an id, got {node.get('type')} without one"
            )
        if ident in seen:
            raise ValueError(f"node id {ident!r} is used twice")
        seen.add(ident)
    total = count_nodes(nodes)
    if total > MAX_NODES:
        raise ValueError(f"a render has {total} nodes; the maximum is {MAX_NODES}")


# -- the differ ----------------------------------------------------------------
#
# `diff(old, new)` answers a list of steps. Each step is a dict with `do`:
#
# - set:     {"do": "set", "id", "type", "keys": {...}}
# - value:   {"do": "value", "id", "value"}
# - items:   {"do": "items", "id", "type", "upsert": [...], "remove": [...], "clear": bool}
# - groups:  {"do": "groups", "id", "groups": [...]}
# - lines:   {"do": "lines", "id", "lines": [...], "clear": bool}
# - select:  {"do": "select", "id", "ids"}
# - drop:    {"do": "drop", "id"}
# - add:     {"do": "add", "node", "after"}


def _props(node):
    return {k: v for k, v in node.items() if k not in ("children",)}


def _same_shape(a, b):
    return node_id(a) == node_id(b) and a.get("type") == b.get("type")


def _diff_items(old, new):
    kind = new["type"]
    key = ITEMS[kind]
    ident = node_id(new)
    before = list(old.get(key) or [])
    after = list(new.get(key) or [])
    if kind == "log":
        if after[: len(before)] == before:
            tail = after[len(before) :]
            return (
                [{"do": "lines", "id": ident, "lines": tail, "clear": False}]
                if tail
                else []
            )
        return [{"do": "lines", "id": ident, "lines": after, "clear": True}]
    steps = []
    old_ids = [item["id"] for item in before]
    new_ids = [item["id"] for item in after]
    kept = [i for i in old_ids if i in set(new_ids)]
    if kind == "link" or kept != [i for i in new_ids if i in set(old_ids)]:
        # The order changed: a collection keeps insertion order, so rebuild it.
        steps.append(
            {
                "do": "items",
                "id": ident,
                "type": kind,
                "upsert": after,
                "remove": [],
                "clear": True,
            }
        )
    else:
        by_id = {item["id"]: item for item in before}
        upsert = [item for item in after if by_id.get(item["id"]) != item]
        # A new item lands at the end; only a pure append keeps the order.
        new_only = [i for i in new_ids if i not in by_id]
        if new_only and new_ids[-len(new_only) :] != new_only:
            steps.append(
                {
                    "do": "items",
                    "id": ident,
                    "type": kind,
                    "upsert": after,
                    "remove": [],
                    "clear": True,
                }
            )
        else:
            remove = [i for i in old_ids if i not in set(new_ids)]
            if upsert or remove:
                steps.append(
                    {
                        "do": "items",
                        "id": ident,
                        "type": kind,
                        "upsert": upsert,
                        "remove": remove,
                        "clear": False,
                    }
                )
    if kind == "table" and (old.get("groups") or []) != (new.get("groups") or []):
        old_groups = {g["id"]: g for g in old.get("groups") or []}
        changed = [g for g in new.get("groups") or [] if old_groups.get(g["id"]) != g]
        if set(old_groups) - {g["id"] for g in new.get("groups") or []}:
            return None
        steps.append({"do": "groups", "id": ident, "groups": changed})
    return steps


def _diff_node(old, new):
    """Steps that turn node `old` into `new` in place, or None to replace it."""
    kind = new.get("type")
    ident = node_id(new)
    if kind == "group":
        if _props(old) != _props(new):
            return None
        return _diff_children(old.get("children") or [], new.get("children") or [])
    if kind in INPUT_TYPES:
        rest_old = {k: v for k, v in old.items() if k != "value"}
        rest_new = {k: v for k, v in new.items() if k != "value"}
        if rest_old != rest_new:
            return None
        if old.get("value") != new.get("value"):
            return [{"do": "value", "id": ident, "value": new.get("value")}]
        return []
    if kind in ITEMS:
        key = ITEMS[kind]
        # The person owns a selection. A render that names `selected_ids` sets it.
        skip = (key, "groups", "selected_ids")
        rest_old = {k: v for k, v in old.items() if k not in skip}
        rest_new = {k: v for k, v in new.items() if k not in skip}
        if rest_old != rest_new:
            return None
        steps = _diff_items(old, new)
        if steps is None:
            return None
        chosen = new.get("selected_ids")
        if kind == "table" and chosen is not None and chosen != old.get("selected_ids"):
            steps.append({"do": "select", "id": ident, "ids": list(chosen)})
        return steps
    if kind in ENGINE_OWNED:
        # People write these messages and threads: the render declares only the
        # frame, and the owner answers with `reply`.
        owned = ENGINE_OWNED[kind]
        same = {k: v for k, v in old.items() if k not in owned} == {
            k: v for k, v in new.items() if k not in owned
        }
        return [] if same else None
    if old == new:
        return []
    settable = SETTABLE.get(kind)
    if settable is None:
        return None
    changed = {k for k in set(old) | set(new) if old.get(k) != new.get(k)}
    # A typed handle cannot remove a key, so a removed key replaces the node.
    if not changed <= set(settable) or any(
        k not in new or new[k] is None for k in changed
    ):
        return None
    keys = {k: new[k] for k in sorted(changed)}
    return [{"do": "set", "id": ident, "type": kind, "keys": keys, "node": new}]


def _diff_children(old, new):
    """Steps for one list of siblings, or None when the parent must be replaced."""
    prefix = 0
    while prefix < min(len(old), len(new)) and _same_shape(old[prefix], new[prefix]):
        prefix += 1
    if prefix == 0 and (old or new):
        return None
    steps = []
    for index in range(prefix):
        inner = _diff_node(old[index], new[index])
        if inner is None:
            if index == 0:
                return None
            steps += [
                {"do": "drop", "id": node_id(old[index])},
                {"do": "add", "node": new[index], "after": node_id(new[index - 1])},
            ]
        else:
            steps += inner
    if prefix < len(old) or prefix < len(new):
        steps += [{"do": "drop", "id": node_id(node)} for node in old[prefix:]]
        anchor = node_id(new[prefix - 1])
        for node in new[prefix:]:
            steps.append({"do": "add", "node": node, "after": anchor})
            anchor = node_id(node)
    return steps


def diff(old, new):
    """The steps that turn render `old` into render `new`.

    The first root node must keep its id and type: it is the anchor that every
    replaced or added node stands after.
    """
    steps = _diff_children(list(old), list(new))
    if steps is None:
        raise ValueError("the first root node of an app must keep its id and type")
    # Drops go first: a node that moves to another group is dropped there before
    # it is added here, so one patch never holds the same id twice.
    drops = [step for step in steps if step["do"] == "drop"]
    return drops + [step for step in steps if step["do"] != "drop"]


def apply_steps(view, steps) -> None:
    """Send `steps` through the typed handles of `view`, as one patch."""
    with view.batch():
        for step in steps:
            do = step["do"]
            if do == "drop":
                view.drop(step["id"])
            elif do == "add":
                view.add(step["node"], after=step["after"])
            elif do == "value":
                view[step["id"]].set(step["value"])
            elif do == "set":
                _apply_set(view[step["id"]], step["type"], step["keys"], step["node"])
            elif do == "lines":
                handle = view[step["id"]]
                if step["clear"]:
                    handle.clear()
                if step["lines"]:
                    handle.write(list(step["lines"]))
            elif do == "groups":
                for group in step["groups"]:
                    view[step["id"]].group(
                        group["id"],
                        label=group.get("label"),
                        tone=group.get("tone"),
                        order=group.get("order"),
                        is_open=group.get("is_open"),
                    )
            elif do == "items":
                _apply_items(view[step["id"]], step)
            elif do == "select":
                view[step["id"]].select(*step["ids"])
            else:
                raise ValueError(f"unknown step {do!r}")


def _apply_set(handle, kind, keys, node):
    """One typed `set` call. `node` is the full new node: it gives a text or a
    title that did not change, because those handles need one in every call.
    Reading the engine here would flush the batch halfway."""
    if kind in ("paragraph", "heading", "code"):
        handle.set(
            node.get("text", ""), **{k: v for k, v in keys.items() if k != "text"}
        )
    elif kind == "card":
        handle.set(node.get("title"), **{k: v for k, v in keys.items() if k != "title"})
    elif kind == "status":
        handle.set(node.get("text"), **{k: v for k, v in keys.items() if k != "text"})
    else:
        handle.set(**keys)


def _apply_items(handle, step):
    kind = step["type"]
    if step["clear"]:
        handle.clear()
    if step["remove"]:
        handle.remove(*step["remove"])
    for item in step["upsert"]:
        if kind == "table":
            handle.upsert(
                item["id"],
                item.get("cells") or [],
                tone=item.get("tone"),
                parent=item.get("parent"),
            )
        elif kind == "stat":
            handle.set(
                item["id"],
                item.get("value_text", ""),
                item.get("label", item["id"]),
                tone=item.get("tone"),
            )
        elif kind == "steps":
            handle.set(
                item["id"],
                item.get("label", item["id"]),
                tone=item.get("tone"),
                detail=item.get("detail"),
                value=item.get("value"),
            )
        elif kind == "link":
            handle.add(
                item["id"],
                item["label"],
                item["target"],
                target_kind=item.get("target_kind"),
                tone=item.get("tone"),
            )


# -- events --------------------------------------------------------------------

EVENT_FIELDS = (
    "type",
    "node_id",
    "seq",
    "value",
    "values",
    "is_set",
    "submission_id",
    "comments",
    "action",
    "key",
    "item_ids",
)


def event_dict(event) -> dict[str, Any]:
    """A plain dict of one `LiveEvent` (or of a dict), for a pure reducer."""
    if isinstance(event, Mapping):
        return {k: event.get(k) for k in EVENT_FIELDS if event.get(k) is not None}
    return {
        k: getattr(event, k)
        for k in EVENT_FIELDS
        if getattr(event, k, None) is not None
    }


# -- the runner ----------------------------------------------------------------

Opener = Callable[[dict, Callable[[Any], None]], Any]
# `opener(declaration, on_event)` opens or adopts the app and answers its handle.


class MemoryStore(dict):
    """A store that keeps app states in this process only."""


class Runner:
    """Drive one app model through one live app handle.

    `opener` opens the app: `vis.live(...)` in an extension, or the SDK door of a
    session. `store` keeps the model state under the app key, so an owner that
    starts again adopts the app with its state. `on_effect(runner, effect)`
    runs each effect that the model asks for.
    """

    def __init__(
        self,
        app,
        opener: Opener,
        *,
        store: MutableMapping[str, Any] | None = None,
        on_effect: Callable[[Runner, dict], None] | None = None,
        key: str | None = None,
    ):
        self.app = app
        self.key = key or app.KEY
        self._opener = opener
        self._store = MemoryStore() if store is None else store
        self._on_effect = on_effect
        self._lock = threading.RLock()
        self.view = None
        self.state = None
        self._rendered: list = []
        self._shortcuts: list = []
        self.errors: list[str] = []

    # -- declaring -------------------------------------------------------------

    def _render(self, state):
        nodes = self.app.render(state)
        check_tree(nodes)
        return nodes

    def _shortcuts_of(self, state):
        fn = getattr(self.app, "shortcuts", None)
        return list(fn(state)) if fn else []

    def declaration(self, state) -> dict:
        """The `vis.ui.app(...)` declaration of `state`."""
        import blockether.vis.extension as vis

        options = {}
        shortcuts = self._shortcuts_of(state)
        if shortcuts:
            options["shortcuts"] = shortcuts
        return vis.ui.app(
            self.app.TITLE,
            *self._render(state),
            description=getattr(self.app, "DESCRIPTION", None),
            key=self.key,
            **options,
        )

    def open(self):
        """Open the app, or adopt the open app with the same key."""
        with self._lock:
            saved = self._store.get(self.key)
            self.state = (
                copy.deepcopy(saved) if saved is not None else self.app.initial()
            )
            self._rendered = self._render(self.state)
            self._shortcuts = self._shortcuts_of(self.state)
            self.view = self._opener(self.declaration(self.state), self.on_event)
            if saved is not None:
                # An adopted app keeps its picture. Paint the saved state on it.
                self._resync()
            self._save()
            return self

    def _resync(self):
        """Paint the saved state over the picture of an adopted app.

        The differ compares the picture, cut down to the keys the render
        declares, with the render. A thread keeps the messages that people
        and the owner wrote.
        """
        picture = self.view.state() or {}
        declared = {node_id(node): node for node in walk(self._rendered)}
        baseline = _project(picture.get("nodes") or [], declared)
        if not baseline or node_id(baseline[0]) != node_id(self._rendered[0]):
            raise ValueError(
                "the adopted app has another first node; open it under a new key"
            )
        target = self._rendered
        self._rendered = baseline
        self._shortcuts = list(picture.get("shortcuts") or [])
        self._paint(self.state)
        self._rendered = target

    # -- events ----------------------------------------------------------------

    def on_event(self, event) -> None:
        """The `on_event` callback: one human event, from any thread."""
        try:
            self.dispatch(event_dict(event))
        except Exception as error:  # noqa: BLE001 - one bad event must not end the app
            LOG.exception("live app %s: event %r failed", self.key, event)
            self.errors.append(f"{type(error).__name__}: {error}")

    def dispatch(self, event: dict) -> None:
        """Reduce one event, paint the new state and run its effects."""
        with self._lock:
            if self.view is None:
                raise RuntimeError("open the app first")
            self._learn_human(event)
            if getattr(self.app, "READS", ()) and event.get("type") != "tick":
                event = dict(event, read=self._read())
            old = self.state
            new = self.app.reduce(copy.deepcopy(old), event)
            if new == old:
                return
            self._paint(new)
            self.state = new
            self._save()
            effects = getattr(self.app, "effects", None)
            for effect in effects(old, new) if effects else []:
                if self._on_effect is None:
                    LOG.warning(
                        "live app %s: no effect handler for %r", self.key, effect
                    )
                else:
                    self._on_effect(self, effect)

    def _learn_human(self, event):
        """Take a human-owned value into the baseline, so it is not sent back."""
        if event.get("type") == "change":
            for node in walk(self._rendered):
                if node_id(node) == event.get("node_id"):
                    node["value"] = event.get("value")
        if event.get("type") == "select":
            for node in walk(self._rendered):
                if node_id(node) == event.get("node_id"):
                    node["selected_ids"] = list(event.get("item_ids") or [])

    def _read(self):
        """The current selection or value of each node in `READS`."""
        picture = self.view.state() or {}
        read = {}
        for node in walk(picture.get("nodes")):
            ident = node_id(node)
            if ident in self.app.READS:
                if node.get("type") == "table":
                    read[ident] = list(node.get("selected_ids") or [])
                else:
                    read[ident] = node.get("value")
        # The person owns these values: the next render must not send them back.
        for node in walk(self._rendered):
            ident = node_id(node)
            if ident in read:
                key = "selected_ids" if node.get("type") == "table" else "value"
                node[key] = read[ident]
        return read

    def _paint(self, state):
        nodes = self._render(state)
        steps = diff(self._rendered, nodes)
        shortcuts = self._shortcuts_of(state)
        setter = getattr(self.view, "set_shortcuts", None)
        kept = {node_id(node) for node in walk(nodes)}
        bound = [
            s
            for s in self._shortcuts
            if s.get("node_id") is None or s["node_id"] in kept
        ]
        if setter is not None and steps and bound != self._shortcuts:
            # The host refuses a patch that drops a node that a shortcut still
            # names. `LiveAttachment.set_shortcuts` sends its own patch, so the
            # shortcuts of the dropped nodes go first.
            setter(bound)
            self._shortcuts = bound
        with self.view.batch():
            if steps:
                apply_steps(self.view, steps)
            if shortcuts != self._shortcuts:
                if setter is None:
                    LOG.warning(
                        "live app %s: this SDK cannot change shortcuts yet", self.key
                    )
                else:
                    setter(shortcuts)
        self._rendered = nodes
        self._shortcuts = shortcuts

    def _save(self):
        self._store[self.key] = copy.deepcopy(self.state)

    def start(self):
        """Start the tick loop of a model with `TICK_SECONDS`. Answers the thread."""
        seconds = getattr(self.app, "TICK_SECONDS", None)
        if not seconds:
            return None
        self._stop = threading.Event()

        def loop():
            # A failed tick keeps the old state, so the next tick tries again.
            # The gateway can restart; the door then adopts the app again.
            failing = False
            while not self._stop.wait(seconds):
                try:
                    self.dispatch({"type": "tick"})
                except Exception as error:  # noqa: BLE001 - logged, the next tick retries
                    if not failing:
                        LOG.warning(
                            "live app %s: a tick failed, the loop retries: %s",
                            self.key,
                            error,
                        )
                    failing = True
                    continue
                if failing:
                    LOG.info("live app %s: the tick loop works again", self.key)
                failing = False

        thread = threading.Thread(target=loop, name=f"live-app-{self.key}", daemon=True)
        thread.start()
        self._ticker = thread
        return thread

    def stop(self):
        """Stop the tick loop, when one runs."""
        stop = getattr(self, "_stop", None)
        if stop is not None:
            stop.set()

    def close(self, **ending):
        """Close the app; its record stays readable."""
        self.stop()
        with self._lock:
            if self.view is not None:
                return self.view.close(**ending)
            return None


def _project(nodes, declared):
    """Picture nodes with only the keys that the render declares for each id."""
    out = []
    for node in nodes:
        want = declared.get(node_id(node))
        if want is None:
            out.append({"type": node.get("type"), "id": node_id(node)})
            continue
        lean = {k: node[k] for k in want if k in node and k != "children"}
        if "validate" in want:
            # A function never crosses to the engine: take it from the render.
            lean["validate"] = want["validate"]
        if node.get("children") is not None:
            lean["children"] = _project(node["children"], declared)
        out.append(lean)
    return out
