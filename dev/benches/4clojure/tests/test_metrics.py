#!/usr/bin/env python3
from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
METRICS = ROOT / "metrics.py"


class FourClojureMetricsTest(unittest.TestCase):
    def test_counts_pass_rate_and_errors(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            tmp = Path(d)
            preds = tmp / "predictions.jsonl"
            preds.write_text(
                "\n".join(
                    [
                        json.dumps({"id": 1, "difficulty": "Elementary", "empty": False, "passed": True, "elapsed_s": 1.0}),
                        json.dumps({"id": 2, "difficulty": "Elementary", "empty": True, "passed": False, "eval_error": "empty-solution", "elapsed_s": 3.0}),
                    ]
                )
                + "\n"
            )
            out = tmp / "summary.json"
            subprocess.run(
                [sys.executable, str(METRICS), "--predictions", str(preds), "--total", "2", "--out", str(out)],
                check=True,
                capture_output=True,
                text=True,
            )
            summary = json.loads(out.read_text())
            self.assertEqual(summary["passed"], 1)
            self.assertEqual(summary["pass_pct"], 50.0)
            self.assertEqual(summary["empty_solutions"], 1)
            self.assertEqual(summary["errors"], 1)


if __name__ == "__main__":
    unittest.main()
