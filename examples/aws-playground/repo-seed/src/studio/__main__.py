"""Run the studio.

`python -m studio [port]` serves the page on this machine.
`python -m studio render [directory]` writes each song as a MIDI file.
"""

from __future__ import annotations

import sys
from pathlib import Path
from wsgiref.simple_server import make_server

from studio import songs
from studio.app import application


def render(directory: Path) -> None:
    """Write the MIDI file of each song."""
    directory.mkdir(parents=True, exist_ok=True)
    for item in songs.CATALOG:
        file = directory / f"{item.id}.mid"
        file.write_bytes(songs.midi_file(item))
        print(file)


def main(arguments: list[str]) -> None:
    if arguments[:1] == ["render"]:
        render(Path(arguments[1] if len(arguments) > 1 else "out"))
        return
    port = int(arguments[0]) if arguments else 8090
    with make_server("127.0.0.1", port, application) as server:
        print(f"MIDI studio: http://127.0.0.1:{port}/", flush=True)
        server.serve_forever()


if __name__ == "__main__":
    main(sys.argv[1:])
