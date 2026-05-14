#!/usr/bin/env python3
"""Download Rich 4Clojure problems EDN and materialize normalized JSON."""
from __future__ import annotations

import argparse
import json
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path

DEFAULT_URL = "https://raw.githubusercontent.com/PEZ/rich4clojure/main/problems.edn"


def repo_root() -> Path:
    return Path(__file__).resolve().parents[3]


def edn_to_json(edn_path: Path) -> list[dict]:
    form = r"""
(require '[clojure.edn :as edn]
         '[charred.api :as json])
(let [xs (edn/read-string (slurp *in*))]
  (println (json/write-json-str xs)))
"""
    proc = subprocess.run(
        ["clojure", "-M", "-e", form],
        cwd=repo_root(),
        input=edn_path.read_text(),
        text=True,
        capture_output=True,
        check=True,
    )
    return json.loads(proc.stdout)


def normalize(raw: list[dict], source_url: str) -> list[dict]:
    problems: list[dict] = []
    for row in raw:
        pid = row.get("_id")
        if pid is None:
            continue
        problems.append(
            {
                "id": int(pid),
                "title": row.get("title") or f"Problem {pid}",
                "difficulty": row.get("difficulty") or "Unknown",
                "tags": row.get("tags") or [],
                "description": row.get("description") or "",
                "tests": row.get("tests") or [],
                "restricted": row.get("restricted") or [],
                "extra_requires": row.get("extra-requires") or [],
                "source_url": source_url,
            }
        )
    problems.sort(key=lambda p: p["id"])
    return problems


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--url", default=DEFAULT_URL)
    p.add_argument("--out", type=Path, default=Path(__file__).with_name("problems.json"))
    args = p.parse_args()

    with tempfile.TemporaryDirectory() as d:
        edn_path = Path(d) / "problems.edn"
        with urllib.request.urlopen(args.url, timeout=30) as response:
            edn_path.write_bytes(response.read())
        problems = normalize(edn_to_json(edn_path), args.url)

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(problems, indent=2, ensure_ascii=False) + "\n")
    print(f"wrote {len(problems)} problems → {args.out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
