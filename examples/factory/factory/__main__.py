"""Start the factory with its configured table, memories and Vis gateway."""

from __future__ import annotations

import logging
from pathlib import Path

from factory import settings
from factory.api import App
from factory.gateway import Gateway
from factory.memory import Memories
from factory.runs import Starter
from factory.server import serve
from factory.tables import open_table


def main() -> None:
    """Open the stores and serve the factory until it stops."""
    logging.basicConfig(level=logging.INFO)
    Path(settings.PROJECT).mkdir(parents=True, exist_ok=True)
    Path(settings.WORKFLOWS).mkdir(parents=True, exist_ok=True)
    app = App(open_table("factory"), Memories(), Gateway(), Starter())
    try:
        serve(app)
    finally:
        app.stop()


if __name__ == "__main__":
    main()
