#!/usr/bin/env python3
from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = ROOT.parents[2]
EVALUATOR = ROOT / "eval_one.clj"
PROBLEMS = ROOT / "problems.json"


class FourClojureEvaluatorTest(unittest.TestCase):
    def run_eval(self, problem: dict, solution: str) -> dict:
        with tempfile.TemporaryDirectory() as d:
            tmp = Path(d)
            problem_path = tmp / "problem.json"
            solution_path = tmp / "solution.edn"
            problem_path.write_text(json.dumps(problem))
            solution_path.write_text(solution)
            proc = subprocess.run(
                ["clojure", "-M", str(EVALUATOR), str(problem_path), str(solution_path)],
                cwd=REPO_ROOT,
                check=True,
                capture_output=True,
                text=True,
                timeout=30,
            )
            return json.loads(proc.stdout)

    def test_accepts_correct_solution(self) -> None:
        problem = json.loads(PROBLEMS.read_text())[0]
        result = self.run_eval(problem, "true")
        self.assertTrue(result["passed"])

    def test_rejects_restricted_symbol(self) -> None:
        problem = next(p for p in json.loads(PROBLEMS.read_text()) if p["id"] == 19)
        result = self.run_eval(problem, "last")
        self.assertFalse(result["passed"])
        self.assertEqual(result["error"], "restricted-symbol")


if __name__ == "__main__":
    unittest.main()
