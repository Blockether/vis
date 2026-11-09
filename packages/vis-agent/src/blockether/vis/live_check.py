"""Check a live view without Vis: `python -m blockether.vis.live_check [FILE]`.

The command reads one live view as JSON from FILE, or from stdin when FILE is
`-` or not given. It checks the view with the check that the SDK, the test
recorder and the engine use. It prints one line for the faults of the view and
one line for each node with an error card. The exit
code is 0 for a valid view, 1 for a view with a problem and 2 when the input is
not a view.

`vis.testing.LiveRecorder(...).view_json()` gives a view to check.
"""

from __future__ import annotations

import argparse
import json
import sys

from blockether.vis import _contracts


def problems(view: object) -> list[str]:
    """One line for the faults of a live view and one line for each node with an
    error card; empty when the view is valid.

    A view that came from a host carries engine stamps, such as button clicks, so
    the check reads it as a stored view, not as a declaration.
    """
    return _contracts.live_view_faults(view, is_declaration=False).lines()


def main(argv: list[str] | None = None) -> int:
    """Run the command and answer its exit code."""
    parser = argparse.ArgumentParser(
        prog="python -m blockether.vis.live_check",
        description="Check a live view JSON file as Vis checks it.",
    )
    parser.add_argument(
        "file", nargs="?", default="-", help="a JSON file, or - for stdin"
    )
    args = parser.parse_args(argv)
    try:
        if args.file == "-":
            text = sys.stdin.read()
        else:
            with open(args.file, encoding="utf-8") as source:
                text = source.read()
    except OSError as error:
        print(f"live_check: cannot read {args.file}: {error.strerror}", file=sys.stderr)
        return 2
    if not text.strip():
        print(
            "live_check: the input is empty; give one live view as JSON",
            file=sys.stderr,
        )
        return 2
    try:
        view = json.loads(text)
    except json.JSONDecodeError as error:
        print(
            f"live_check: the input is not JSON: {error.msg} at line {error.lineno}",
            file=sys.stderr,
        )
        return 2
    found = problems(view)
    for line in found:
        print(line)
    if not found:
        print("live_check: the view is valid")
    return 1 if found else 0


if __name__ == "__main__":
    raise SystemExit(main())
