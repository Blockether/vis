"""Print the full report of one Harbor round and write it as JSON.

Run it with ``python -m blockether.vis.evals.report JOBS``. `JOBS` is one Harbor job
folder or a folder of jobs. Read `reports` for the content of the report.
"""

from __future__ import annotations

import argparse
import json
import sys
from collections.abc import Sequence
from pathlib import Path

from ._gateway import plain
from .reports import REPORT_FILE, render_report, round_report, write_report

__all__ = ["main"]


def main(argv: Sequence[str] | None = None) -> int:
    """Print the report of a round and write its JSON. Return 1 without trials."""
    parser = argparse.ArgumentParser(prog="python -m blockether.vis.evals.report")
    parser.add_argument(
        "jobs", type=Path, help="a Harbor job folder or a folder of jobs"
    )
    parser.add_argument("--tasks", type=Path, help="the dataset folder of the tasks")
    parser.add_argument("--model", help="count only attempts of this model or route")
    parser.add_argument("--round", dest="round_name", help="the name of the round")
    parser.add_argument("--labels", type=Path, help="the cause labels file")
    parser.add_argument("--judge", type=Path, help="the job folder of a judge")
    parser.add_argument("--stage-labels", type=Path, help="the stage labels file")
    parser.add_argument("--json", action="store_true", help="print JSON, not text")
    parser.add_argument("--no-write", action="store_true", help="do not write a file")
    args = parser.parse_args(argv)
    try:
        report = round_report(
            args.jobs,
            args.tasks,
            model=args.model,
            round_name=args.round_name,
            labels=args.labels,
            judge_dir=args.judge,
            stage_labels=args.stage_labels,
        )
    except (FileNotFoundError, ValueError) as error:
        print(error, file=sys.stderr)
        return 1
    if args.json:
        print(json.dumps(plain(report), indent=2))
    else:
        print(render_report(report), end="")
    if not args.no_write:
        path = write_report(report, args.jobs / REPORT_FILE)
        print(f"Report: {path}", file=sys.stderr if args.json else sys.stdout)
    return 0


if __name__ == "__main__":
    sys.exit(main())
