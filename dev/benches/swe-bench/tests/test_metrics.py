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


class MetricsTest(unittest.TestCase):
    def run_metrics(self, report: dict, report_name: str = "vis+zai.run-1.json") -> dict:
        with tempfile.TemporaryDirectory() as d:
            tmp = Path(d)
            preds = tmp / "predictions.jsonl"
            preds.write_text(
                "\n".join(
                    [
                        json.dumps(
                            {
                                "instance_id": "task-1",
                                "model_patch": "diff --git a/x b/x\n",
                                "elapsed_s": 1.0,
                                "tokens": 100,
                                "cost_usd": 0.01,
                            }
                        ),
                        json.dumps(
                            {
                                "instance_id": "task-2",
                                "model_patch": "",
                                "empty": True,
                                "elapsed_s": 3.0,
                            }
                        ),
                    ]
                )
                + "\n"
            )
            (tmp / report_name).write_text(json.dumps(report))
            out = tmp / "summary.json"
            subprocess.run(
                [
                    sys.executable,
                    str(METRICS),
                    "--predictions",
                    str(preds),
                    "--report-dir",
                    str(tmp),
                    "--run-id",
                    "run-1",
                    "--total",
                    "2",
                    "--out",
                    str(out),
                ],
                check=True,
                capture_output=True,
                text=True,
            )
            return json.loads(out.read_text())

    def test_reads_official_swebench_report_filename(self) -> None:
        summary = self.run_metrics(
            {
                "resolved_instances": 1,
                "resolved_ids": ["task-1"],
                "schema_version": 2,
            }
        )
        self.assertEqual(summary["resolved"], 1)
        self.assertEqual(summary["resolved_pct"], 50.0)
        self.assertEqual(summary["patches_emitted"], 1)

    def test_count_valued_resolved_instances_is_not_treated_as_ids(self) -> None:
        summary = self.run_metrics({"resolved_instances": 1})
        self.assertEqual(summary["resolved"], 0)


if __name__ == "__main__":
    unittest.main()
