"""Vis entrypoint: the `/apps` command opens the proof apps in a session.

`/apps` opens the gallery; `/apps <name> ...` opens each named app. Run it again after a
restart: the app comes back with the same id and its saved state. In the TUI,
C-x L opens the gallery and C-x i the incident app. The attachments key moves to C-x e.
"""

import logging
import threading

import blockether.vis.extension as vis
from live_apps import apps
from live_apps.host import Apps, open_app

LOG = logging.getLogger("live_apps")
_SESSIONS = {}
_LOCK = threading.Lock()


def _apps_of(session_id):
    with _LOCK:
        found = _SESSIONS.get(session_id)
        if found is None:
            found = Apps(open_app, vis.state, prefix=f"apps:{session_id}:")
            _SESSIONS[session_id] = found
        return found


def run_apps(ctx):
    """Open the gallery, or each app that the arguments name."""
    args = ctx.get("args")
    # The host gives the words after the command as a list.
    words = list(args) if isinstance(args, (list, tuple)) else str(args or "").split()
    names = [str(word).strip() for word in words if str(word).strip()] or ["gallery"]
    session_id = str(ctx.get("session_id") or "")
    if not session_id:
        return vis.err("Open /apps in a session", "The apps live in one session.")
    known = apps()
    unknown = [name for name in names if name not in known]
    if unknown:
        return vis.err(
            f"No app {', '.join(unknown)}", "Choose one of: " + ", ".join(known)
        )
    opened = []
    for name in dict.fromkeys(names):
        try:
            runner = _apps_of(session_id).open(name)
        except Exception as error:  # noqa: BLE001 - shown to the person and logged
            LOG.exception("/apps %s failed", name)
            return vis.err(
                f"{known[name].TITLE} did not open", f"{type(error).__name__}: {error}"
            )
        opened.append({"name": name, "view_id": runner.view.id, "key": runner.key})
    titles = ", ".join(known[item["name"]].TITLE for item in opened)
    description = (
        known[opened[0]["name"]].DESCRIPTION
        if len(opened) == 1
        else f"{len(opened)} apps are open."
    )
    return vis.ok(
        f"{titles} {'is' if len(opened) == 1 else 'are'} open",
        description,
        {"apps": opened},
    )


vis.register_extension(
    vis.Extension(
        name="vis-live-apps",
        description="Proof apps for live apps: counter, todo, kanban, 2048, incidents and a deploy wizard.",
        slash_commands=[
            vis.SlashCommand(
                "apps",
                run_apps,
                doc="Open the live apps gallery or one app.",
                usage="/apps [counter|todo|kanban|2048|incident|wizard ...]",
            )
        ],
        keybindings=[
            # A new key on a free key.
            vis.Keybinding("L", command="/apps", label="Live apps"),
            # A changed key: the built-in attachments verb moves from C-x i to C-x e.
            vis.Keybinding("e", action="focus-attachments"),
            # The key that the move frees. The TUI applies moves before commands.
            vis.Keybinding("i", command="/apps incident", label="Incidents"),
        ],
    )
)
