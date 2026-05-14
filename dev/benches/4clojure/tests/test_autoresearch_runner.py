#!/usr/bin/env python3
"""Self-test for the autoresearch runner aggregation + trace parsing.

Exercises edge cases that previously almost slipped through (negative
context_score on probe failure, div-by-zero on empty workloads, name
drift between emitted metrics and the metric_keys whitelist).
"""
from __future__ import annotations

import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

import autoresearch_runner as ar  # noqa: E402


def _outcome(**overrides):
    base = {
        "kind": "4clojure",
        "id": 1,
        "title": "x",
        "difficulty": "Elementary",
        "passed": True,
        "iterations": 5,
        "iter_source": "envelope",
        "iterations_score": 5,
        "timed_out": False,
        "returncode": 0,
        "tokens_total": 100,
        "prompt_tokens": 80,
        "output_tokens": 20,
        "reasoning_tokens": 0,
        "cost_usd": 0.01,
        "prompt_chars": 1500,
        "verdict": {"passed": True},
        "trace_events": 10,
        "trace_last_phase": "iteration-final",
        "z_patch_calls": 0,
        "v_patch_calls": 1,
        "block_errors": 0,
        "error_iterations": [],
        "elapsed_s": 50.0,
    }
    base.update(overrides)
    return base


class AggregateTest(unittest.TestCase):
    def test_empty_outcomes_is_safe(self) -> None:
        s = ar.aggregate([])
        self.assertEqual(s["task_count"], 0)
        self.assertEqual(s["pass_count"], 0)
        self.assertEqual(s["pass_rate"], 0.0)
        self.assertEqual(s["iter_score"], 0)
        self.assertIsNone(s["mean_iterations_pass"])
        self.assertIsNone(s["max_iterations"])
        self.assertEqual(s["mean_prompt_chars"], 0)
        self.assertEqual(s["z_patch_share"], 0.0)
        self.assertEqual(s["block_errors"], 0)

    def test_all_fail_keeps_meaningful_score(self) -> None:
        outs = [_outcome(passed=False, iterations=None, iterations_score=30) for _ in range(3)]
        s = ar.aggregate(outs)
        self.assertEqual(s["iter_score"], 90)
        self.assertEqual(s["pass_count"], 0)
        self.assertEqual(s["pass_rate"], 0.0)
        self.assertIsNone(s["mean_iterations_pass"])

    def test_mixed_pass_fail(self) -> None:
        outs = [
            _outcome(iterations=3, iterations_score=3, cost_usd=0.01),
            _outcome(iterations=7, iterations_score=7, cost_usd=0.02),
            _outcome(passed=False, iterations=None, iterations_score=30, cost_usd=0.0),
        ]
        s = ar.aggregate(outs)
        self.assertEqual(s["iter_score"], 40)
        self.assertEqual(s["pass_count"], 2)
        self.assertAlmostEqual(s["mean_iterations_pass"], 5.0)
        self.assertEqual(s["max_iterations"], 7)
        self.assertAlmostEqual(s["total_cost_usd"], 0.03)
        self.assertAlmostEqual(s["pass_rate"], 66.67, places=2)

    def test_z_patch_share_no_patches(self) -> None:
        outs = [_outcome(z_patch_calls=0, v_patch_calls=0)]
        s = ar.aggregate(outs)
        self.assertEqual(s["z_patch_calls"], 0)
        self.assertEqual(s["v_patch_calls"], 0)
        self.assertEqual(s["z_patch_share"], 0.0)

    def test_z_patch_share_only_z(self) -> None:
        outs = [_outcome(z_patch_calls=2, v_patch_calls=0)]
        s = ar.aggregate(outs)
        self.assertEqual(s["z_patch_share"], 100.0)

    def test_z_patch_share_mixed(self) -> None:
        outs = [_outcome(z_patch_calls=3, v_patch_calls=1)]
        s = ar.aggregate(outs)
        self.assertEqual(s["z_patch_share"], 75.0)

    def test_block_errors_aggregate(self) -> None:
        outs = [
            _outcome(block_errors=2),
            _outcome(block_errors=0),
            _outcome(block_errors=5),
        ]
        s = ar.aggregate(outs)
        self.assertEqual(s["block_errors"], 7)


class TraceParseTest(unittest.TestCase):
    def test_classify_patch_extension(self) -> None:
        self.assertEqual(ar._classify_patch_extension("com.blockether.vis.ext.foundation.core"), "v")
        self.assertEqual(ar._classify_patch_extension("com.blockether.vis.ext.lang-clojure.core"), "z")
        self.assertEqual(ar._classify_patch_extension("com.blockether.vis.ext.lang_clojure.patch"), "z")
        self.assertIsNone(ar._classify_patch_extension(""))
        self.assertIsNone(ar._classify_patch_extension(None))
        self.assertIsNone(ar._classify_patch_extension("com.blockether.vis.ext.exa.core"))

    def test_parse_empty_trace(self) -> None:
        r = ar.parse_trace_stream("")
        self.assertEqual(r["events"], 0)
        self.assertEqual(r["iterations_observed"], 0)
        self.assertEqual(r["v_patch_calls"], 0)
        self.assertEqual(r["z_patch_calls"], 0)
        self.assertEqual(r["block_errors"], 0)

    def test_parse_counts_patches_and_errors(self) -> None:
        frames = [
            '{"event":"trace-chunk","payload":{"phase":"provider-call","iteration":1}}',
            '{"event":"trace-chunk","payload":{"phase":"tool-start","iteration":1,"tool-event":{"phase":"tool-start","op":"patch","extension":"com.blockether.vis.ext.foundation.core","symbol":"patch"}}}',
            '{"event":"trace-chunk","payload":{"phase":"form-result","iteration":1,"info":{"status":"error","iteration":1,"ref":"r/1/b/1"}}}',
            '{"event":"trace-chunk","payload":{"phase":"tool-start","iteration":2,"tool-event":{"phase":"tool-start","op":"patch","extension":"com.blockether.vis.ext.lang-clojure.core","symbol":"patch"}}}',
            '{"event":"trace-chunk","payload":{"phase":"form-result","iteration":2,"info":{"status":"done","iteration":2,"ref":"r/2/b/1"}}}',
            '{"event":"result","payload":{"iteration-count":2,"tokens":{"total":1234},"cost":{"total-cost":0.05}}}',
        ]
        r = ar.parse_trace_stream("\n".join(frames))
        self.assertEqual(r["v_patch_calls"], 1)
        self.assertEqual(r["z_patch_calls"], 1)
        self.assertEqual(r["block_errors"], 1)
        self.assertIn(1, r["error_iterations"])
        self.assertEqual(r["iterations_observed"], 2)
        self.assertIsNotNone(r["result"])
        self.assertEqual(r["result"]["iteration-count"], 2)


class PickWorkloadTest(unittest.TestCase):
    def test_n_zero_refuses(self) -> None:
        original = ar.N_TASKS
        ar.N_TASKS = 0
        try:
            with self.assertRaises(RuntimeError):
                ar.pick_workload(0)
        finally:
            ar.N_TASKS = original

    def test_fixed_workload_is_default_and_stable(self) -> None:
        original = ar.ROTATE
        ar.ROTATE = False
        try:
            a = ar.pick_workload(0)
            b = ar.pick_workload(17)
            c = ar.pick_workload(99)
        finally:
            ar.ROTATE = original
        self.assertEqual(len(a), len(ar.FIXED_WORKLOAD))
        self.assertEqual([t["id"] for t in a], [e["id"] for e in ar.FIXED_WORKLOAD])
        self.assertEqual([t["id"] for t in a], [t["id"] for t in b])
        self.assertEqual([t["id"] for t in b], [t["id"] for t in c])

    def test_rotation_changes_with_offset(self) -> None:
        original = ar.ROTATE
        ar.ROTATE = True
        try:
            a = ar.pick_workload(0)
            b = ar.pick_workload(1)
        finally:
            ar.ROTATE = original
        sig_a = [t["id"] for t in a]
        sig_b = [t["id"] for t in b]
        self.assertNotEqual(sig_a, sig_b)

    def test_workload_signature_is_stable(self) -> None:
        wl = [
            {"kind": "4clojure",  "id": 5},
            {"kind": "filewrite", "id": "fw-005"},
        ]
        self.assertEqual(ar.workload_signature(wl), "4clojure:5,filewrite:fw-005")


class WallEstimateTest(unittest.TestCase):
    def test_minimum_is_one(self) -> None:
        self.assertEqual(ar.estimate_iters_from_wall(0.0), 1)
        self.assertEqual(ar.estimate_iters_from_wall(1.5), 1)
        # User rule of thumb: 10 s → 1 iter (calibrated against real 12.5 s).
        self.assertEqual(ar.estimate_iters_from_wall(12.0), 1)

    def test_scales_with_wall(self) -> None:
        # ~7 s/iter after a ~5 s bootstrap. 60 s → round((60-5)/7) = 8.
        self.assertEqual(ar.estimate_iters_from_wall(60.0), 8)
        # 240 s timeout → round((240-5)/7) = 34 (caps at FAIL_PENALTY upstream).
        self.assertGreaterEqual(ar.estimate_iters_from_wall(240.0), 30)


if __name__ == "__main__":
    unittest.main()
