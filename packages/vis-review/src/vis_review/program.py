"""The SDK program: review a root from outside Vis, in a session or as a global app.

    python -m vis_review --session <id> [--root PATH]
    python -m vis_review --global [--root PATH]

The program records a checkpoint each time it sees a change, every `--every`
seconds, so it catches each change but cannot name the block that made it. Run
the `vis-review` extension instead to get one checkpoint for each block and patch.
Language checks run the language packages that this program can import.
"""

from __future__ import annotations

import argparse
import importlib
import logging
import os
import threading
from collections.abc import Callable
from typing import Any

from vis_review.app import ReviewApp
from vis_review.recorder import Recorder

LOG = logging.getLogger("vis_review")
OWNER = "vis-review"
# The language package and class of each tool name, as the extensions name them.
LOCAL_TOOLS = {
    "py": ("vis_lang_python.tools", "PythonTools"),
    "clj": ("vis_lang_clojure.tools", "ClojureTools"),
}


def session_opener(session, owner: str = OWNER) -> Callable[[dict, Callable], Any]:
    """Open the app through the app door of an SDK `session`."""

    def open_one(declaration, on_event):
        return session.attach(
            dict(declaration, flush_ms=0), owner=owner, on_event=on_event
        )

    return open_one


def global_opener(client, owner: str = OWNER) -> Callable[[dict, Callable], Any]:
    """Open the app as a global app through the global door of an SDK `client`."""

    def open_one(declaration, on_event):
        return client.attach_app(
            dict(declaration, flush_ms=0, scope="global"),
            owner=owner,
            on_event=on_event,
        )

    return open_one


def local_call(
    root: str, tools: dict[str, tuple[str, str]] = LOCAL_TOOLS
) -> Callable[..., object]:
    """Call a language tool from a package that this program imports."""
    made: dict[str, object] = {}

    def call(tool: str, verb: str, **arguments):
        if tool not in made:
            module_name, class_name = tools[tool]
            try:
                module = importlib.import_module(module_name)
            except ImportError:
                raise LookupError(
                    f"Install {module_name.split('.')[0].replace('_', '-')} to run the {tool} checks here."
                ) from None
            made[tool] = getattr(module, class_name)(workspace_root=lambda: root)
        return getattr(made[tool], verb)(**arguments)

    return call


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    place = parser.add_mutually_exclusive_group(required=True)
    place.add_argument("--session", help="the id of the session that the app opens in")
    place.add_argument(
        "--global",
        dest="is_global",
        action="store_true",
        help="open a global app, which belongs to no session",
    )
    parser.add_argument("--root", default=os.getcwd(), help="the folder to review")
    parser.add_argument(
        "--url", default=os.environ.get("VIS_GATEWAY_URL", "http://127.0.0.1:8090")
    )
    parser.add_argument(
        "--every", type=float, default=2.0, help="seconds between two looks"
    )
    parser.add_argument("--seconds", type=float, help="stop after this many seconds")
    args = parser.parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(name)s %(message)s")
    from blockether.vis.engine import GatewayClient

    root = os.path.realpath(args.root)
    with GatewayClient(
        args.url, token=os.environ.get("VIS_GATEWAY_TOKEN") or None
    ) as client:
        key = "program-global" if args.is_global else f"program-{args.session}"
        recorder = Recorder()
        journal = recorder.journal(key, root)

        def ask(request, model):
            return client.create_session(root=root).run(request, response_model=model)

        if args.is_global:
            opener, send = global_opener(client), None
        else:
            session = client.session(args.session)
            opener, send = session_opener(session), session.send
        app = ReviewApp(
            journal,
            opener,
            call=local_call(root),
            ask=ask,
            send=send,
            key="review-" + os.path.basename(root),
        ).open()
        recorder.keep(key, app)
        print(
            f"Review of {root} is open ({'global' if args.is_global else args.session})",
            flush=True,
        )
        stop = threading.Event()
        deadline = None if args.seconds is None else args.seconds
        waited = 0.0
        try:
            while not stop.wait(args.every):
                recorder.record(key, root, "outside", "Seen by the program")
                waited += args.every
                if deadline is not None and waited >= deadline:
                    break
        except KeyboardInterrupt:
            pass
    return 0
