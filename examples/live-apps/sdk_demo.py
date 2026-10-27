"""SDK program: open the proof apps in a gateway session and serve their events.

Run it from this directory with the Python of the Vis SDK:

    VIS_GATEWAY_TOKEN=... python sdk_demo.py --url http://127.0.0.1:8090 counter todo

Without `--session` it creates a session. Without app names it opens every app.
It serves events until Ctrl+C. Run it again with the same `--session` and
`--state`: it adopts the open apps by key and paints their saved state.
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import sys
import threading
from collections.abc import MutableMapping
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / "src"))

from blockether.vis.engine import GatewayClient  # noqa: E402
from live_apps import apps  # noqa: E402
from live_apps.host import Apps  # noqa: E402

LOG = logging.getLogger("live_apps")
OWNER = "live-apps-demo"


class JsonStore(MutableMapping):
    """App states in one JSON file, written after each change."""

    def __init__(self, path: Path):
        self.path = path
        self._lock = threading.Lock()
        self._data = json.loads(path.read_text()) if path.exists() else {}

    def _write(self):
        tmp = self.path.with_suffix(".tmp")
        tmp.write_text(json.dumps(self._data, indent=1, sort_keys=True))
        tmp.replace(self.path)

    def __getitem__(self, key):
        return self._data[key]

    def __setitem__(self, key, value):
        with self._lock:
            self._data[key] = value
            self._write()

    def __delitem__(self, key):
        with self._lock:
            del self._data[key]
            self._write()

    def __iter__(self):
        return iter(list(self._data))

    def __len__(self):
        return len(self._data)


def opener_of(session, owner: str):
    """Open each app declaration through the SDK app door of `session`."""

    def open_one(declaration, on_event):
        return session.attach(
            dict(declaration, flush_ms=0), owner=owner, on_event=on_event
        )

    return open_one


def main(argv=None) -> int:
    known = apps()
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("names", nargs="*", help="apps to open: " + ", ".join(known))
    parser.add_argument(
        "--url", default=os.environ.get("VIS_GATEWAY_URL", "http://127.0.0.1:8090")
    )
    parser.add_argument(
        "--session", help="an existing session id; without it, a new session"
    )
    parser.add_argument(
        "--state",
        default="live-apps-state.json",
        help="the JSON file of the app states",
    )
    parser.add_argument(
        "--owner", default=OWNER, help="the program name that owns the apps"
    )
    parser.add_argument(
        "--seconds", type=float, help="stop after this many seconds (default: Ctrl+C)"
    )
    args = parser.parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(name)s %(message)s")

    names = args.names or [name for name in known if name != "gallery"]
    unknown = [name for name in names if name not in known]
    if unknown:
        parser.error(f"no app {', '.join(unknown)}; choose from {', '.join(known)}")

    token = os.environ.get("VIS_GATEWAY_TOKEN") or None
    with GatewayClient(args.url, token=token) as client:
        session = (
            client.session(args.session) if args.session else client.create_session()
        )
        store = JsonStore(Path(args.state))
        opened = Apps(opener_of(session, args.owner), store, prefix=f"{session.id}:")
        for name in names:
            runner = opened.open(name)
            print(
                f"{name}: app {runner.view.id} is open in session {session.id}",
                flush=True,
            )
        stop = threading.Event()
        try:
            stop.wait(args.seconds)
        except KeyboardInterrupt:
            pass
        finally:
            opened.stop()
        errors = [
            f"{key}: {error}"
            for key, runner in opened.runners.items()
            for error in runner.errors
        ]
        for line in errors:
            print("error", line, file=sys.stderr)
        print(
            f"served apps {', '.join(opened.runners)}; the apps stay open in the session",
            flush=True,
        )
        return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
