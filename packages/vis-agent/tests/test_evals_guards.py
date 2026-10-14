import collections
import csv
import hashlib
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import evals_atif as atif
from blockether.vis.evals import grade
from blockether.vis.evals.grade import cache_metric_failures, usage_percent
from blockether.vis.evals.harbor import WORKSPACE_FILESYSTEM_FILE

PROVIDER = "test-provider"
TASKS = Path(__file__).resolve().parents[3] / "e2e"

if not TASKS.is_dir():
    raise unittest.SkipTest("the Vis e2e tasks are not in this checkout")


def load_scenarios(scenario_ids):
    """Load e2e tasks as the dicts that the guards read, with their directory."""
    tasks = []
    for task_id in sorted(scenario_ids):
        directory = TASKS / task_id
        checks = json.loads((directory / "tests" / "checks.json").read_text())
        checks["id"] = task_id
        checks["prompt"] = (directory / "instruction.md").read_text()
        checks["_dir"] = str(directory)
        registrations = directory / "environment" / WORKSPACE_FILESYSTEM_FILE
        if registrations.is_file():
            checks["workspace_filesystem"] = json.loads(registrations.read_text())
        tasks.append(checks)
    return tasks


def seed_files(sc, work):
    """Copy the seed workspace of the task dict ``sc`` into ``work``."""
    seed = Path(sc["_dir"]) / "environment" / "workspace"
    shutil.copytree(seed, work, dirs_exist_ok=True)


def grade_run(
    sc, document, *, provider=PROVIDER, model="test-model", reasoning_effort=None
):
    """Grade the ATIF ``document`` of one run in a seeded workspace, like one trial."""
    trial = atif.trial(
        document, provider=provider, model=model, reasoning_effort=reasoning_effort
    )
    with tempfile.TemporaryDirectory() as work:
        if "_dir" in sc:
            seed_files(sc, work)
        return grade.grade_trial(sc, work, trial).result


class ScenarioFilesTest(unittest.TestCase):
    def test_seeds_the_registered_fixture_directory(self):
        # HostEnvironment writes the vis.yml registration; test_environment covers it.
        scenario = load_scenarios(["py-project-paths"])[0]
        self.assertEqual({"library": "library"}, scenario["workspace_filesystem"])
        with tempfile.TemporaryDirectory() as work:
            seed_files(scenario, work)
            self.assertIn("return 0", (Path(work) / "library/value.py").read_text())

    def test_helper_reuse_scenario_repeats_one_audit_across_blocks(self):
        scenario = load_scenarios(["session-helper-reuse"])[0]
        self.assertEqual(2, scenario["want_helper_reuse"])
        self.assertIs(True, scenario["measurement"])
        self.assertIn("one log per python_execution block", scenario["prompt"])
        self.assertEqual(["187", "23", "charlie", "18.5"], scenario["want_answer"])
        with tempfile.TemporaryDirectory() as work:
            seed_files(scenario, work)
            logs = sorted((Path(work) / "reports").glob("*.log"))
            events = [
                [
                    line
                    for line in log.read_text().splitlines()
                    if line.strip() and not line.startswith("#")
                ]
                for log in logs
            ]
            self.assertEqual(5, len(logs))
            self.assertEqual(187, sum(len(rows) for rows in events))
            self.assertEqual(
                23, sum(row.count("level=error") for rows in events for row in rows)
            )
            charlie = events[logs.index(Path(work) / "reports" / "charlie.log")]
            latency = [int(row.split("ms=")[1]) for row in charlie]
            self.assertEqual(18.5, round(sum(latency) / len(latency), 1))

    def test_namespace_rename_uses_anchored_edits_and_checks_the_moved_source(self):
        scenario = load_scenarios(["clj-ns-rename"])[0]
        self.assertEqual(["patch"], scenario["want_tools"])
        self.assertNotIn("symbol_rename", scenario["prompt"])
        self.assertIn("(ns foo.core)", scenario["want"]["src/foo/core.clj"])

    def test_extension_probe_scenario_requires_the_registered_extension_tools(self):
        scenario = load_scenarios(["extension-contract-discovery"])[0]
        self.assertEqual(
            ["contract_probe.record", "contract_probe.status"], scenario["want_tools"]
        )
        self.assertIn("contract_probe", scenario["prompt"])
        self.assertIn("shell", scenario["forbid_tools"])


class LargeStdoutScenariosTest(unittest.TestCase):
    def test_generated_stdout_and_exact_answer_oracles(self):
        cases = {
            "stdout-lookup-middle": ("lookup",),
            "stdout-ledger-reduce": ("ledger",),
            "stdout-jsonl-filter": ("jsonl",),
            "stdout-unicode": ("unicode",),
            "stdout-dual-source": ("alpha", "beta"),
            "stdout-incident-join": ("incident",),
        }
        scenarios = {sc["id"]: sc for sc in load_scenarios(set(cases))}
        self.assertEqual(set(cases), set(scenarios))
        for sid, modes in cases.items():
            with self.subTest(scenario=sid), tempfile.TemporaryDirectory() as work:
                sc = scenarios[sid]
                seed_files(sc, work)
                script = Path(work) / "emit.py"
                self.assertTrue(script.is_file())
                expected_outputs = sc["want_stdout_recovery"]
                if not isinstance(expected_outputs, list):
                    expected_outputs = [expected_outputs]
                self.assertEqual(len(modes), len(expected_outputs))
                saved = {}
                for mode, expected in zip(modes, expected_outputs, strict=True):
                    raw = subprocess.run(
                        [sys.executable, str(script), mode],
                        cwd=work,
                        capture_output=True,
                        text=True,
                        timeout=15,
                        check=True,
                    ).stdout
                    self.assertGreater(len(raw), expected["min_chars"])
                    self.assertTrue(raw.startswith(expected["head"]))
                    self.assertTrue(raw.rstrip().endswith(expected["tail"]))
                    self.assertIn(expected["middle"], raw)
                    position = raw.index(expected["middle"]) / len(raw)
                    self.assertGreater(position, 0.2)
                    self.assertLess(position, 0.8)
                    self.assertIsNone(expected["goal_status"])
                    self.assertTrue(
                        grade.stdout_recovery_failures(
                            [
                                {
                                    "scope": "t1/i1/f1",
                                    "iteration": 1,
                                    "tool_call_id": "call_x",
                                    "stdout": raw,
                                }
                            ],
                            [],
                            expected,
                        )
                    )
                    saved[mode] = raw
                self.assertIn("read_session()", sc["want_forms"])
                self.assertIn("target data row", sc["prompt"])
                for key in sc["want_answer_json"]:
                    self.assertIn(key, sc["prompt"])
                self.assertTrue(sc["want_requested_route"])
                self.assertTrue(grade.structured_failures(sc, work, "{}", []))
                rows = next(iter(saved.values())).splitlines()[1:-1]
                if sid == "stdout-lookup-middle":
                    self.assertIn(
                        "ticket:<id> prefix is not a key=value field", sc["prompt"]
                    )
                    record = next(
                        row for row in rows if row.startswith("ticket:03101|")
                    )
                    ticket, *parts = record.split("|")
                    answer = {
                        "ticket": ticket.split(":")[1],
                        **dict(part.split("=", 1) for part in parts),
                    }
                elif sid == "stdout-ledger-reduce":
                    beta = [row.split("|") for row in rows if "|acct=beta|" in row]
                    answer = {
                        "beta_settled_net": sum(
                            int(row[2].split("=")[1])
                            for row in beta
                            if row[3] == "status=settled"
                        ),
                        "beta_reversed_count": sum(
                            row[3] == "status=reversed" for row in beta
                        ),
                    }
                elif sid == "stdout-jsonl-filter":
                    filtered = [
                        row
                        for line in rows
                        if (row := json.loads(line))["region"] == "east"
                        and row["priority"] == "high"
                        and row["latency"] >= 900
                    ]
                    top = sorted(
                        filtered, key=lambda row: (-row["latency"], row["id"])
                    )[:3]
                    answer = {
                        "matched_count": len(filtered),
                        "top_three": [
                            {"id": row["id"], "latency": row["latency"]} for row in top
                        ],
                    }
                elif sid == "stdout-unicode":
                    self.assertIn(
                        "metric as a JSON number, never a string", sc["prompt"]
                    )
                    self.assertIn(
                        "The final assistant message must contain only that JSON object",
                        sc["prompt"],
                    )
                    self.assertIn(
                        "answer is not a single JSON value",
                        grade.structured_failures(
                            sc,
                            work,
                            "Proof:\n" + json.dumps(sc["want_answer_json"]),
                            [],
                        ),
                    )
                    record = next(row for row in rows if row.startswith("記録02701|"))
                    answer = {
                        "label": record.split("|label=")[1].split("|")[0],
                        "metric": int(record.split("|metric=")[1]),
                        "utf8_bytes": len(saved["unicode"].encode("utf-8")),
                        "emoji_rows": sum("|label=🙂|" in row for row in rows),
                    }
                elif sid == "stdout-dual-source":
                    a = next(row for row in rows if row.startswith("A03450|"))
                    b = next(
                        row
                        for row in saved["beta"].splitlines()
                        if row.startswith("B02150|")
                    )
                    value = a.split("|value=")[1]
                    weight = b.split("|weight=")[1]
                    answer = {
                        "alpha_value": value,
                        "beta_weight": weight,
                        "sum": int(value) + int(weight),
                    }
                else:
                    events = [
                        row.split("|") for row in rows if "|req=critical-72|" in row
                    ]
                    errors = [row for row in events if "status=error" in row]
                    answer = {
                        "first_failed_component": errors[0][2].split("=")[1],
                        "failed_attempts": [
                            int(row[4].split("=")[1]) for row in errors
                        ],
                        "final_status": events[-1][3].split("=")[1],
                        "final_attempt": int(events[-1][4].split("=")[1]),
                    }
                self.assertEqual(sc["want_answer_json"], answer)


class NaturalStdoutScenariosTest(unittest.TestCase):
    def test_prompts_leave_recovery_choice_to_the_model(self):
        cases = {
            "stdout-natural-lookup": "stdout-lookup-middle",
            "stdout-natural-ledger": "stdout-ledger-reduce",
        }
        scenarios = {
            sc["id"]: sc for sc in load_scenarios(set(cases.values()) | set(cases))
        }
        for sid, source in cases.items():
            with self.subTest(scenario=sid), tempfile.TemporaryDirectory() as work:
                sc = scenarios[sid]
                self.assertEqual(
                    scenarios[source]["want_answer_json"], sc["want_answer_json"]
                )
                self.assertNotIn("read_session", sc["prompt"])
                if sid == "stdout-natural-ledger":
                    self.assertIn("No code fence, prose, or explanation", sc["prompt"])
                    answer = json.dumps(sc["want_answer_json"])
                    self.assertIn(
                        "answer is not a single JSON value",
                        grade.structured_failures(
                            sc, work, f"```json\n{answer}\n```\n\nExplanation", []
                        ),
                    )
                self.assertNotIn("want_stdout_recovery", sc)
                self.assertTrue(sc["want_requested_route"])
                seed_files(sc, work)
                self.assertEqual(
                    (Path(work) / "emit.py").read_bytes(),
                    (
                        Path(scenarios[source]["_dir"])
                        / "environment/workspace/emit.py"
                    ).read_bytes(),
                )


class LargeInputScenariosTest(unittest.TestCase):
    def test_generated_inputs_and_compact_oracles(self):
        inputs = {
            "large-csv-groups": ("sales.csv",),
            "large-crossfile-join": ("accounts.csv", "events.csv"),
            "large-jsonl-integrity": ("audit.jsonl",),
            "large-two-ledgers": ("reconcile-left.csv", "reconcile-right.csv"),
        }
        scenarios = {sc["id"]: sc for sc in load_scenarios(set(inputs))}
        self.assertEqual(set(inputs), set(scenarios))
        for sid, names in inputs.items():
            with self.subTest(scenario=sid), tempfile.TemporaryDirectory() as work:
                sc = scenarios[sid]
                seed_files(sc, work)
                data = Path(work) / "data"
                self.assertFalse((Path(work) / "generate.py").exists())
                for name in names:
                    self.assertGreater((data / name).stat().st_size, 32768)
                self.assertLess(sc["max_form_output_chars"], 1200)
                self.assertLess(sc["max_total_output_chars"], 2300)
                self.assertTrue(sc["want_requested_route"])
                self.assertTrue(grade.structured_failures(sc, work, "{}", []))

                def csv_rows(name, root=data):
                    with (root / name).open(newline="") as stream:
                        return list(csv.DictReader(stream))

                if sid == "large-csv-groups":
                    self.assertIn("import csv", sc["prompt"])
                    paid = [
                        row for row in csv_rows("sales.csv") if row["status"] == "paid"
                    ]
                    answer = {
                        "paid_count": len(paid),
                        "net_by_region": {
                            region: sum(
                                int(row["amount"])
                                for row in paid
                                if row["region"] == region
                            )
                            for region in ("north", "south", "east", "west")
                        },
                    }
                elif sid == "large-crossfile-join":
                    self.assertIn("project_root_path / 'data'", sc["prompt"])
                    self.assertIn(
                        "not relative to the current working directory", sc["prompt"]
                    )
                    self.assertIn("events.kind marks failed events", sc["prompt"])
                    self.assertIn("no status column", sc["prompt"])
                    accounts = {
                        row["account_id"]: row for row in csv_rows("accounts.csv")
                    }
                    failed = [
                        row
                        for row in csv_rows("events.csv")
                        if row["kind"] == "failed"
                        and accounts[row["account_id"]]["tier"] == "gold"
                        and accounts[row["account_id"]]["region"] == "east"
                    ]
                    counts = collections.Counter(row["account_id"] for row in failed)
                    answer = {
                        "failed_events": len(failed),
                        "affected_accounts": len(counts),
                        "top_three": [
                            {"account_id": account, "failures": count}
                            for account, count in sorted(
                                counts.items(), key=lambda item: (-item[1], item[0])
                            )[:3]
                        ],
                    }
                elif sid == "large-jsonl-integrity":
                    invalid = []
                    valid = []
                    for index, line in enumerate(
                        (data / "audit.jsonl").read_text().splitlines(), 1
                    ):
                        try:
                            valid.append(json.loads(line))
                        except json.JSONDecodeError:
                            invalid.append(index)
                    errors = [row for row in valid if row["status"] == "error"]
                    answer = {
                        "invalid_lines": len(invalid),
                        "first_invalid_line": invalid[0],
                        "error_events": len(errors),
                        "unique_error_records": len({row["record"] for row in errors}),
                    }
                else:

                    def invoice_totals(name):
                        totals = collections.defaultdict(int)
                        for row in csv_rows(name):
                            totals[row["invoice"]] += int(row["amount"])
                        return totals

                    left = invoice_totals("reconcile-left.csv")
                    right = invoice_totals("reconcile-right.csv")
                    shared = left.keys() & right.keys()
                    answer = {
                        "left_only": len(left.keys() - right.keys()),
                        "right_only": len(right.keys() - left.keys()),
                        "mismatched_shared": sum(
                            left[key] != right[key] for key in shared
                        ),
                        "net_delta": sum(right[key] - left[key] for key in shared),
                    }
                    self.assertTrue(sc["prompt"].startswith("/goal "))
                    self.assertIn("Do not print the full goal result", sc["prompt"])
                    self.assertIn("Skip the CSV header", sc["prompt"])
                    self.assertEqual(1100, sc["max_form_output_chars"])
                    self.assertEqual(2200, sc["max_total_output_chars"])
                    self.assertTrue(sc["want_goal_complete"])
                    self.assertTrue(sc["want_cache_read"])
                    self.assertTrue(sc["want_cache_metrics"])
                self.assertEqual(sc["want_answer_json"], answer)


class CacheMetricValidationTest(unittest.TestCase):
    def usage(self):
        return {
            "fold_count": 1,
            "input_tokens": 30_000,
            "input_cache_read_tokens": 19_000,
            "prompt_cache_reusable_tokens": 20_000,
            "prompt_cache_reused_tokens": 18_500,
            "prompt_cache_sample_count": 3,
            "prompt_cache_estimated_sample_count": 1,
            "prompt_cache_rebuild_count": 1,
            "prompt_cache_expired_count": 0,
            "cache_read_share_percent": 63,
            "reusable_prefix_coverage_percent": 93,
        }

    def test_accepts_cross_layer_totals_and_one_fold(self):
        self.assertEqual(
            [],
            cache_metric_failures(
                self.usage(),
                {"input": 30_000, "cached": 19_000},
                provider_call_count=4,
                folded_prefix=True,
            ),
        )

    def test_rejects_provider_percentage_and_fold_drift(self):
        usage = self.usage()
        usage.update(
            {
                "fold_count": 0,
                "input_tokens": 29_999,
                "cache_read_share_percent": 62,
                "prompt_cache_estimated_sample_count": 0,
                "prompt_cache_rebuild_count": 0,
            }
        )
        failures = cache_metric_failures(
            usage,
            {"input": 30_000, "cached": 19_000},
            provider_call_count=4,
            folded_prefix=True,
        )
        self.assertTrue(any("usage input" in failure for failure in failures))
        self.assertTrue(any("cache-read share" in failure for failure in failures))
        self.assertTrue(any("estimated samples" in failure for failure in failures))
        self.assertTrue(any("rebuilds" in failure for failure in failures))
        self.assertTrue(any("recorded folds" in failure for failure in failures))

    def test_fold_count_ignores_printed_receipts(self):
        self.assertEqual([], grade.fold_count_failures({"fold_count": 1}))
        for count in (None, False, True, 0, 2):
            with self.subTest(count=count):
                self.assertTrue(
                    grade.fold_count_failures(
                        {"fold_count": count, "stdout": "folded through t1/i2"}
                    )
                )

    def test_percentage_rounds_half_up_like_gateway(self):
        self.assertEqual(13, usage_percent(1, 8))
        self.assertEqual(0, usage_percent(0, 0))
        self.assertEqual(100, usage_percent(9, 8))


class ContextBudgetTest(unittest.TestCase):
    CODEX = {"auto_compress_above": 183600, "model_input_limit": 204000}

    def usage(self, **changes):
        health = {
            "budget_tokens": 183600,
            "model_input_limit": 204000,
            "reminder_tokens": 137700,
        }
        return {"health": {**health, **changes}}

    def failures(self, usage=None, printed=CODEX, answer=None, expected=None):
        outputs = (
            []
            if printed is None
            else [{"stdout": json.dumps(printed, sort_keys=True) + "\n"}]
        )
        return grade.context_budget_failures(
            self.usage() if usage is None else usage,
            outputs,
            json.dumps(self.CODEX) if answer is None else answer,
            expected,
        )

    def run_scenario(self, health):
        scenario = load_scenarios(["context-budget"])[0]
        route = {"provider": "openai-codex", "model": "gpt-6-sol"}
        code = (
            "print(json.dumps({'auto_compress_above': session['utilization']"
            "['auto_compress_above'], 'model_input_limit': session['utilization']"
            "['model_input_limit']}, sort_keys=True))"
        )
        printed = json.dumps(self.CODEX, sort_keys=True)
        document = atif.trajectory(
            atif.step(1, atif.block("t1/i1/f1", code, stdout=printed + "\n"), **route),
            answer=printed,
            session_id="00000000-0000-0000-0000-000000000000",
            session_usage={"health": health},
            **route,
        )
        return grade_run(scenario, document, **route)

    def test_printed_answered_and_persisted_budgets_agree(self):
        self.assertEqual([], self.failures())
        self.assertEqual([], self.failures(expected=self.CODEX))
        fenced = "```json\n" + json.dumps(self.CODEX) + "\n```"
        self.assertEqual([], self.failures(answer=fenced))

    def test_budget_keeps_a_ten_percent_reserve(self):
        wide = {"auto_compress_above": 200000, "model_input_limit": 204000}
        self.assertEqual(
            ["budget_tokens 200000 exceeds 90% of limit 204000"],
            self.failures(
                self.usage(budget_tokens=200000, reminder_tokens=150000),
                printed=wide,
                answer=json.dumps(wide),
            ),
        )

    def test_reminder_is_three_quarters_of_the_budget(self):
        self.assertEqual(
            ["reminder_tokens 137699 is not 75% of 183600"],
            self.failures(self.usage(reminder_tokens=137699)),
        )

    def test_pinned_route_values_are_exact(self):
        opus = {"auto_compress_above": 250000, "model_input_limit": 872000}
        failures = self.failures(expected=opus)
        self.assertEqual(1, len(failures), failures)
        self.assertTrue(failures[0].startswith("persisted budget"))

    def test_model_visible_and_answered_budgets_must_match_health(self):
        stale = {"auto_compress_above": 200000, "model_input_limit": 1000000}
        for printed, answer, expected in (
            (stale, None, "model-visible budget"),
            (None, None, "no sandbox form printed"),
            (self.CODEX, "183600", "does not report the persisted budget"),
            (self.CODEX, json.dumps(stale), "does not report the persisted budget"),
        ):
            with self.subTest(printed=printed, answer=answer):
                failures = self.failures(printed=printed, answer=answer)
                self.assertEqual(1, len(failures), failures)
                self.assertIn(expected, failures[0])

    def test_missing_health_or_limit_fails(self):
        self.assertEqual(["usage has no persisted request health"], self.failures({}))
        self.assertEqual(
            ["health model_input_limit is not a positive integer: None"],
            self.failures(self.usage(model_input_limit=None)),
        )

    def test_scenario_checks_the_pinned_route_through_the_grader(self):
        result = self.run_scenario(self.usage()["health"])
        self.assertTrue(result["correct"], result["detail"])
        self.assertIn(
            "context-budget=183600/204000 tokens, reminder 137700", result["evidence"]
        )
        wide = self.usage(budget_tokens=200000, reminder_tokens=150000)["health"]
        result = self.run_scenario(wide)
        self.assertFalse(result["correct"])
        self.assertTrue(
            any("exceeds 90% of limit" in item for item in result["detail"]),
            result["detail"],
        )


class StdoutGoalRecoveryTest(unittest.TestCase):
    def test_scenario_requires_goal_cache_and_exact_read_back(self):
        scenario = load_scenarios(["stdout-goal-cache"])[0]
        self.assertTrue(scenario["prompt"].startswith("/goal "))
        self.assertTrue(scenario["want_goal_complete"])
        self.assertTrue(scenario["want_cache_metrics"])
        self.assertTrue(scenario["want_cache_read"])
        self.assertIn("read_session()", scenario["want_forms"])
        self.assertIn("later iteration", scenario["prompt"])
        self.assertIn("trailing newline", scenario["prompt"])
        self.assertIn("Do not inspect", scenario["prompt"])

    def test_requires_later_session_backed_exact_output_not_just_the_raw_print(self):
        expected = {
            "min_chars": 25,
            "head": "HEAD-",
            "middle": "record-03500;",
            "tail": "-END",
        }
        raw = "HEAD-" + "x" * 30 + "record-03500;" + "-END\n"
        digest = hashlib.sha256(raw.encode()).hexdigest()
        outputs = [
            {
                "scope": "t1/i1/f1",
                "iteration": 1,
                "tool_call_id": "call_abc123",
                "stdout": raw,
            },
            {
                "scope": "t1/i2/f1",
                "iteration": 2,
                "stdout": f"LEN: {len(raw)}\nSHA256: {digest}\nrecord-03500;\nGOAL_STATUS: active\n",
            },
        ]
        readers = [
            {
                "scope": "t1/i2/f1",
                "iteration": 2,
                "code": (
                    "r = await read_session(); "
                    'b = next(b for t in r["transcript"]["turns"] '
                    'for i in t["iterations"] for b in i["blocks"] '
                    'if b.get("scope") == "t1/i1" and '
                    'b.get("svar_tool_call_id") == "call_abc123"); '
                    's = b["stdout"]; '
                    'print("SHA256:", hashlib.sha256(s.encode()).hexdigest())'
                ),
            }
        ]
        self.assertEqual([], grade.stdout_recovery_failures(outputs, readers, expected))
        proof_later = {**outputs[1], "scope": "t1/i3/f1", "iteration": 3}
        staged = [
            outputs[0],
            {**outputs[1], "stdout": "saved for next block"},
            proof_later,
        ]
        self.assertEqual([], grade.stdout_recovery_failures(staged, readers, expected))
        self.assertTrue(
            grade.stdout_recovery_failures(
                staged, [{**readers[0], "iteration": 4}], expected
            )
        )
        self.assertTrue(grade.stdout_recovery_failures(outputs[:1], readers, expected))
        self.assertTrue(grade.stdout_recovery_failures(outputs, [], expected))
        self.assertTrue(grade.stdout_recovery_failures(outputs[1:], readers, expected))
        self.assertTrue(
            grade.stdout_recovery_failures(
                outputs,
                [
                    {
                        **readers[0],
                        "code": readers[0]["code"].replace("call_abc123", "call_other"),
                    }
                ],
                expected,
            )
        )
        self.assertTrue(
            grade.stdout_recovery_failures(
                outputs,
                [{**readers[0], "code": readers[0]["code"].replace("t1/i1", "t1/i9")}],
                expected,
            )
        )
        self.assertTrue(
            grade.stdout_recovery_failures(
                [
                    outputs[0],
                    {
                        **outputs[1],
                        "stdout": outputs[1]["stdout"].replace(digest, "0" * 64),
                    },
                ],
                readers,
                expected,
            )
        )

    def test_multiple_raw_outputs_require_distinct_matching_readbacks(self):
        originals = [
            {
                "scope": f"t1/i{index}/f1",
                "iteration": index,
                "tool_call_id": f"call_{index}",
                "stdout": f"HEAD-{index}-" + "x" * 30 + f"MIDDLE-{index}-TAIL\n",
            }
            for index in (1, 2)
        ]
        expected = [
            {
                "min_chars": 25,
                "head": f"HEAD-{index}-",
                "middle": f"MIDDLE-{index}",
                "tail": "-TAIL",
                "goal_status": None,
            }
            for index in (1, 2)
        ]
        readers = [
            {
                "scope": f"t1/i{index + 2}/f1",
                "iteration": index + 2,
                "code": (
                    "r = await read_session(); "
                    f'b = next(b for t in r["transcript"]["turns"] '
                    f'for i in t["iterations"] for b in i["blocks"] '
                    f'if b.get("scope") == "t1/i{index}" and '
                    f'b.get("svar_tool_call_id") == "call_{index}"); '
                    's = b["stdout"]; print("LEN:", len(s))'
                ),
            }
            for index in (1, 2)
        ]
        readbacks = [
            {
                "scope": readers[index - 1]["scope"],
                "iteration": index + 2,
                "stdout": (
                    f"LEN: {len(originals[index - 1]['stdout'])}\n"
                    f"SHA256: {hashlib.sha256(originals[index - 1]['stdout'].encode()).hexdigest()}\n"
                    f"MIDDLE-{index}\n"
                ),
            }
            for index in (1, 2)
        ]
        self.assertEqual(
            [], grade.stdout_recovery_failures(originals + readbacks, readers, expected)
        )
        self.assertTrue(
            grade.stdout_recovery_failures(originals + readbacks[:1], readers, expected)
        )
        self.assertTrue(
            grade.stdout_recovery_failures(
                originals + readbacks,
                [
                    readers[0],
                    {
                        **readers[1],
                        "code": readers[1]["code"].replace("call_2", "call_1"),
                    },
                ],
                expected,
            )
        )
        self.assertTrue(
            grade.stdout_recovery_failures(
                originals
                + [readbacks[0], {**readbacks[1], "stdout": readbacks[0]["stdout"]}],
                readers,
                expected,
            )
        )
        self.assertTrue(
            grade.stdout_recovery_failures(originals + readbacks, readers, expected * 2)
        )


class ActivityToolEvidenceTest(unittest.TestCase):
    def test_ls_is_detected_from_the_activity_receipt(self):
        scenario = load_scenarios(["ls-source-root-discovery"])[0].copy()
        scenario.update(want_answer=[], want_forms=[])
        listing = atif.block("t1/i1/f1", activity=atif.receipt(atif.row("ls-1", "ls")))
        for blocks, expected in [((), False), ((listing,), True)]:
            with self.subTest(blocks=len(blocks)):
                document = atif.trajectory(
                    atif.step(1, *blocks), answer="Listed directory"
                )
                result = grade_run(scenario, document)
                self.assertEqual(expected, result["correct"])
                if expected:
                    self.assertIn("tools=ls×1", result["detail"])


class FormOutputBudgetTest(unittest.TestCase):
    def test_budget_checks_every_output_without_truncating_it(self):
        # #234: a correct answer can still waste context by dumping full metadata.
        cases = [
            (None, 24757, None, True),
            (6000, 5999, None, True),
            (6000, 6000, None, True),
            (6000, 24757, None, False),
            (0, 0, None, True),
            (0, 1, None, False),
            (6000, 6001, "recorded failure", False),
            (-1, 0, None, False),
            ("6000", 0, None, False),
            (True, 0, None, False),
        ]
        for limit, size, error, expected in cases:
            scenario = load_scenarios(["ls-source-root-discovery"])[0].copy()
            scenario.update(
                want={}, wantnot={}, want_answer=[], want_forms=[], want_tools=[]
            )
            if limit is not None:
                scenario["max_form_output_chars"] = limit
            output = "x" * size
            blocks = [
                atif.block("t1/i1/f1", stdout=output, error=error),
                atif.block("t1/i1/f2"),
            ]
            document = atif.trajectory(atif.step(1, *blocks), answer="Ready")
            with self.subTest(limit=limit, size=size, error=error):
                result = grade_run(scenario, document)
                self.assertEqual(expected, result["correct"])
                if not expected:
                    self.assertIn("max_form_output_chars", " ".join(result["detail"]))
                if type(limit) is int and limit >= 0:
                    self.assertIn(
                        f"max-form-output={size}/{limit} chars", result["evidence"]
                    )
                if error:
                    self.assertIn("recorded failure", result["err_msgs"])
                self.assertTrue(
                    any(
                        item.startswith(f"stdout={size} total chars;")
                        for item in result["evidence"]
                    ),
                    result["evidence"],
                )


class ReasoningEffortTest(unittest.TestCase):
    def run_scenario(
        self,
        effort=None,
        evaluation=None,
        scenario_id="py-fix-body",
        *,
        session_usage=None,
        provider=None,
        error=None,
    ):
        scenario = load_scenarios([scenario_id])[0].copy()
        scenario.update(want={}, wantnot={})
        document = atif.trajectory(
            answer="done",
            provider=provider or PROVIDER,
            session_id="00000000-0000-0000-0000-000000000000",
            eval=evaluation,
            session_usage=session_usage,
            error=error,
        )
        return grade_run(scenario, document, reasoning_effort=effort)

    def evaluation(self):
        return {
            "is_valid": True,
            "invalid_reasons": [],
            "reasoning_effort": {
                "requested": "low",
                "iterations": [
                    {
                        "requested": "low",
                        "effective": "low",
                        "provider": PROVIDER,
                        "model": "test-model",
                        "selected": {"provider": PROVIDER, "model": "test-model"},
                        "is_fallback": False,
                    }
                ],
            },
        }

    def test_unset_effort_keeps_the_default_validation(self):
        result = self.run_scenario()
        self.assertTrue(result["correct"])
        self.assertNotIn("reasoning-effort", " ".join(result["evidence"]))

    def test_low_is_reported_with_route(self):
        result = self.run_scenario("low", self.evaluation())
        self.assertTrue(result["correct"], result["detail"])
        self.assertIn("reasoning-effort=low", result["evidence"])
        self.assertIn(
            f"requested-route={PROVIDER}/test-model calls=1", result["evidence"]
        )

    def test_missing_or_invalid_evaluation_fails_even_with_zero_exit(self):
        for evaluation in (None, {"is_valid": False, "invalid_reasons": []}):
            with self.subTest(evaluation=evaluation):
                result = self.run_scenario("low", evaluation)
                self.assertFalse(result["correct"])
                self.assertTrue(any("reasoning" in item for item in result["detail"]))

    def test_mismatched_effort_or_route_evidence_fails(self):
        for change in (
            {"effective": "high"},
            {"requested": "high"},
            {"provider": "other-provider"},
            {"model": "other-model"},
            {"is_fallback": True},
            {"selected": {"provider": "other-provider", "model": "test-model"}},
        ):
            with self.subTest(change=change):
                evaluation = self.evaluation()
                evaluation["reasoning_effort"]["iterations"][0].update(change)
                result = self.run_scenario("low", evaluation)
                self.assertFalse(result["correct"])

    def test_requested_route_is_not_presented_as_an_observed_route(self):
        result = self.run_scenario("low", self.evaluation(), provider="other-provider")
        self.assertFalse(result["correct"])
        self.assertIn(
            f"requested-route={PROVIDER}/test-model calls=1", result["evidence"]
        )
        self.assertTrue(any("other-provider" in item for item in result["detail"]))

    def test_missing_usage_preserves_the_scenario_result(self):
        result = self.run_scenario(
            "low", self.evaluation(), scenario_id="context-folding"
        )
        self.assertFalse(result["correct"])
        self.assertIn("the trajectory has no session usage", result["detail"])

    def test_a_provider_error_is_counted_and_not_converged(self):
        result = self.run_scenario(
            "low", self.evaluation(), error="Provider model unavailable"
        )
        self.assertFalse(result["converged"])
        self.assertEqual(1, result["errors"])
        self.assertEqual(["Provider model unavailable"], result["err_msgs"])


class HelperReuseEvidenceTest(unittest.TestCase):
    def test_helper_called_by_later_forms_passes(self):
        forms = [
            "def summarize(path):\n    return len(open(path).read())\nprint(summarize('a'))",
            "print(summarize('b'))",
            "print([summarize(name) for name in ('c', 'd')])",
        ]
        metrics, failures = grade.helper_reuse_evidence(forms, 2)
        self.assertEqual([], failures)
        self.assertEqual(1, metrics["helpers_defined"])
        self.assertEqual(2, metrics["reuse_forms"])
        self.assertEqual(0, metrics["retyped_helpers"])
        self.assertEqual(0, metrics["lambda_helpers"])

    def test_named_lambda_helper_counts_like_a_def(self):
        forms = [
            "audit = lambda name: len(open(name).read())\nprint(audit('a'))",
            "print(audit('b'))",
            "print(audit('c'))",
        ]
        metrics, failures = grade.helper_reuse_evidence(forms, 2)
        self.assertEqual([], failures)
        self.assertEqual(1, metrics["helpers_defined"])
        self.assertEqual(1, metrics["lambda_helpers"])
        self.assertEqual(2, metrics["reuse_forms"])

    def test_retyped_lambda_helper_fails_like_a_retyped_def(self):
        body = "audit = lambda name: name.strip()\n"
        forms = [body + "print(audit('a'))", body + "print(audit('b'))"]
        metrics, failures = grade.helper_reuse_evidence(forms, 1)
        self.assertEqual(1, metrics["retyped_helpers"])
        self.assertEqual(
            ["helper 'audit' was retyped in a later form instead of called"],
            failures,
        )

    def test_defining_and_calling_inside_one_form_is_not_reuse(self):
        forms = ["def summarize(path):\n    return path\nprint(summarize('a'))"]
        metrics, failures = grade.helper_reuse_evidence(forms, True)
        self.assertEqual(0, metrics["reuse_forms"])
        self.assertIn("reused in 0 later form(s)", failures[0])

    def test_retyped_definition_fails_even_when_a_later_form_calls_it(self):
        body = "def summarize(path):\n    return path.strip()\n"
        forms = [body + "print(summarize('a'))", body + "print(summarize('b'))"]
        metrics, failures = grade.helper_reuse_evidence(forms, 1)
        self.assertEqual(1, metrics["retyped_helpers"])
        self.assertEqual(1, metrics["reuse_forms"])
        self.assertEqual(
            ["helper 'summarize' was retyped in a later form instead of called"],
            failures,
        )

    def test_reuse_needs_a_call_not_a_mention(self):
        forms = [
            "def summarize(path):\n    return path\n",
            "summarize = 3\nprint(summarize)",
        ]
        metrics, failures = grade.helper_reuse_evidence(forms, True)
        self.assertEqual(0, metrics["reused_helpers"])
        self.assertTrue(failures)

    def test_unparsable_form_is_skipped(self):
        forms = [
            "def summarize(path):\n    return path\n",
            "print(summarize(",
            "print(summarize('b'))",
        ]
        _, failures = grade.helper_reuse_evidence(forms, 1)
        self.assertEqual([], failures)

    def test_missing_helper_and_invalid_requirements_are_reported(self):
        _, failures = grade.helper_reuse_evidence(["print(1)"], True)
        self.assertEqual(["no sandbox form defined a helper to reuse"], failures)
        for want in (0, -1, "2", False, None):
            with self.subTest(want=want):
                metrics, failures = grade.helper_reuse_evidence([], want)
                self.assertEqual({}, metrics)
                self.assertEqual(
                    ["want_helper_reuse must be true or a positive integer"], failures
                )


class DiscoveryEvaluationTest(unittest.TestCase):
    # #232/#234: syntactic markers and tool presence hid incorrect workflows.
    def run_document(self, document, **checks):
        scenario = {"id": "evaluation", "lang": "python", "prompt": "test", **checks}
        return grade_run(scenario, document)

    def run_blocks(self, *blocks, answer="ready", tokens=atif.TOKENS, **checks):
        """Grade a run that runs each block in its own iteration, then answers."""
        steps = [atif.step(index, block) for index, block in enumerate(blocks, 1)]
        document = atif.trajectory(*steps, answer=answer, tokens=tokens)
        return self.run_document(document, **checks)

    def activity(
        self,
        state,
        operation="probe.read",
        row_id="call-1",
        *,
        ref="t1/i1/f1",
        error=None,
    ):
        """Return one block whose Activity receipt holds one row."""
        receipt = atif.receipt(atif.row(row_id, operation, state))
        return atif.block(ref, activity=receipt, error=error)

    def test_caught_activity_failure_cannot_pass(self):
        result = self.run_blocks(self.activity("failed"), want_tools=["probe.read"])
        self.assertEqual(1, result["errors"])
        self.assertEqual(1, result["activity_failures"])
        self.assertEqual(1, result["caught_activity_failures"])
        self.assertEqual(0, result["surfaced_errors"])
        self.assertFalse(result["correct"])

    def test_a_group_row_holds_the_calls_that_it_collapsed(self):
        # A finished block collapses its per-call rows into ONE group row. The
        # receipt keeps those calls as children of the group row.
        calls = [atif.row("call-1", "shell"), atif.row("call-2", "shell", sequence=2)]
        group = atif.row("group-call-1", "shell", children=calls)
        result = self.run_blocks(
            atif.block("t1/i1/f1", activity=atif.receipt(group)), want_tools=["shell"]
        )
        self.assertEqual(0, result["incomplete_activities"])
        self.assertEqual(0, result["errors"])
        self.assertTrue(result["correct"], result["detail"])

    def test_each_block_keeps_its_own_failed_and_running_rows(self):
        failed = atif.receipt(
            atif.row("call-2", "shell", "failed"),
            atif.row("group-call-2", "shell", sequence=2),
        )
        stale = atif.receipt(atif.row("call-1", "shell", "running"))
        result = self.run_blocks(
            atif.block("t1/i1/f1", activity=failed),
            atif.block("t1/i2/f1", activity=stale),
            want_tools=["shell"],
        )
        self.assertEqual(1, result["activity_failures"])
        self.assertEqual(1, result["incomplete_activities"])
        self.assertFalse(result["correct"])

    def test_running_or_cancelled_activity_is_not_success(self):
        for state in ("running", "cancelled"):
            with self.subTest(state=state):
                result = self.run_blocks(
                    self.activity(state), want_tools=["probe.read"]
                )
                self.assertFalse(result["correct"])
                self.assertGreater(result["errors"], 0)

    def test_a_succeeded_row_counts_once(self):
        result = self.run_blocks(
            self.activity("succeeded"),
            want_tools=["probe.read"],
            want_activity_sequence=["probe.read"],
        )
        self.assertTrue(result["correct"], result["detail"])
        self.assertEqual(0, result["errors"])
        self.assertEqual(1, result["activity_successes"])

    def test_surfaced_failure_is_not_labelled_caught(self):
        result = self.run_blocks(
            self.activity("failed", error="failed"), answer="recovered"
        )
        self.assertEqual(1, result["activity_failures"])
        self.assertEqual(0, result["caught_activity_failures"])
        self.assertEqual(1, result["surfaced_errors"])

    def test_exact_answer_rejects_mislabelled_numbers_extra_prose_and_booleans(self):
        expected = {"cards": 2, "active_jobs": 7, "elapsed": 750}
        for answer in (
            '{"cards":7,"active_jobs":2,"elapsed":750}',
            "2 7 750",
            '{"cards":true,"active_jobs":7,"elapsed":750}',
            '{"cards":2,"active_jobs":7,"elapsed":750,"collector":"collector-40"}',
        ):
            with self.subTest(answer=answer):
                result = self.run_blocks(answer=answer, want_answer_json=expected)
                self.assertFalse(result["correct"])
        result = self.run_blocks(answer=json.dumps(expected), want_answer_json=expected)
        self.assertTrue(result["correct"], result["detail"])
        self.assertEqual(
            [],
            grade.structured_failures(
                {
                    "want_answer_json": {
                        "paid_count": 10909,
                        "regions": {"north": 32972},
                    }
                },
                ".",
                '{"paid_count": 10909, "regions": {"north": 32972.0}}',
                [],
            ),
        )

    def test_exact_answer_accepts_only_goal_completion_wrapper_for_goal_scenarios(self):
        expected = {"left_only": 150, "net_delta": 931}
        goal_answer = f"Goal complete: {json.dumps(expected)}"
        self.assertEqual(
            [],
            grade.structured_failures(
                {"want_answer_json": expected, "want_goal_complete": True},
                ".",
                goal_answer,
                [],
            ),
        )
        for answer, goal in (
            (goal_answer, False),
            (f"Completed: {json.dumps(expected)}", True),
            (goal_answer + " more prose", True),
        ):
            with self.subTest(answer=answer, goal=goal):
                self.assertTrue(
                    grade.structured_failures(
                        {"want_answer_json": expected, "want_goal_complete": goal},
                        ".",
                        answer,
                        [],
                    )
                )

    def test_operation_sequence_rejects_missing_extra_or_reordered_calls(self):
        expected = ["probe.cards", "probe.cards", "probe.monitor"]
        for operations in (expected[:2], expected[::-1], expected + ["probe.monitor"]):
            with self.subTest(operations=operations):
                rows = [
                    atif.row(str(index), operation, sequence=index + 1)
                    for index, operation in enumerate(operations)
                ]
                block = atif.block("t1/i1/f1", activity=atif.receipt(*rows))
                result = self.run_blocks(
                    block, answer="done", want_activity_sequence=expected
                )
                self.assertFalse(result["correct"])

    def test_total_output_is_separate_from_peak_and_tokens_are_reported(self):
        blocks = [
            atif.block("t1/i1/f1", stdout="x" * 6),
            atif.block("t1/i2/f1", stdout="y" * 5),
        ]
        result = self.run_blocks(
            *blocks,
            answer="done",
            tokens={"input": 100, "cached": 75, "output": 20, "total": 120},
            max_form_output_chars=6,
            max_total_output_chars=10,
        )
        self.assertFalse(result["correct"])
        self.assertEqual(6, result["max_form_output_chars"])
        self.assertEqual(11, result["total_output_chars"])
        self.assertEqual(25, result["tokens"]["uncached"])
        self.assertEqual(75.0, result["tokens"]["cached_input_percent"])

    def test_exact_jsonl_rejects_extra_rows_wrong_values_or_missing_files(self):
        with tempfile.TemporaryDirectory() as work:
            path = Path(work) / "calls.jsonl"
            scenario = {
                "want_json_files": {"calls.jsonl": [{"key": "atlas"}, {"key": None}]}
            }
            self.assertTrue(grade.structured_failures(scenario, work, "", []))
            for records in (
                [{"key": "wrong"}, {"key": None}],
                [{"key": "atlas"}],
                [{"key": "atlas"}, {"key": None}, {"key": None}],
            ):
                path.write_text("\n".join(json.dumps(row) for row in records))
                self.assertTrue(grade.structured_failures(scenario, work, "", []))
            path.write_text('{"key":"atlas"}\n{"key":null}\n')
            self.assertEqual([], grade.structured_failures(scenario, work, "", []))

    def test_aliases_count_but_comments_strings_and_uninvoked_functions_do_not(self):
        rules = {"signatures": ["probe.read"]}
        for code in (
            "# inspect.signature(probe.read)",
            'print("inspect.signature(probe.read)")',
            "def unused():\n    return inspect.signature(probe.read)",
        ):
            with self.subTest(code=code):
                _, failures = grade.discovery_evidence([code], [], rules)
                self.assertTrue(failures)
        metrics, failures = grade.discovery_evidence(
            ["from inspect import signature as sig\nf = probe.read\nprint(sig(f))"],
            [],
            rules,
        )
        self.assertEqual([], failures)
        self.assertEqual(1, metrics["signature_calls"])

    def test_duplicate_and_known_contract_discovery_are_rejected(self):
        code = "import inspect\nprint(inspect.signature(probe.read))"
        metrics, failures = grade.discovery_evidence(
            [code, code], [], {"signatures": ["probe.read"]}
        )
        self.assertEqual(1, metrics["redundant_discovery"])
        self.assertTrue(failures)
        _, failures = grade.discovery_evidence([code], [], {"known": True})
        self.assertTrue(failures)
        _, failures = grade.discovery_evidence(
            ["print(probe.read())"], [], {"known": True}
        )
        self.assertEqual([], failures)

    def test_forbidden_source_access_cannot_pass(self):
        for code in (
            'from pathlib import Path\nprint(Path(".vis/extensions/probe.py").read_text())',
            "import probe",
            'open("source.py").read()',
            'exec("print(1)")',
        ):
            with self.subTest(code=code):
                _, failures = grade.discovery_evidence([code], [], {"known": True})
                self.assertTrue(failures)

    def test_missing_impossible_and_noninteger_token_counts_fail(self):
        for tokens in (
            {},
            {"input": 8, "cached": 9, "output": 1},
            {"input": True, "cached": 0, "output": 1},
            {"input": 8, "cached": float("inf"), "output": 1},
            {"input": 8, "cached": 0, "output": -1},
            {"input": 8, "cached": 0, "output": 1, "total": 100},
        ):
            with self.subTest(tokens=tokens):
                _, failures = grade.token_summary(tokens)
                self.assertTrue(failures)
        summary, failures = grade.token_summary({"input": 8, "cached": 0, "output": 1})
        self.assertEqual([], failures)
        self.assertEqual(8, summary["uncached"])
        self.assertIsNone(summary["reasoning"])

    def test_impossible_cache_totals_are_not_clamped_into_a_pass(self):
        usage = CacheMetricValidationTest().usage()
        usage.update(
            input_tokens=8,
            input_cache_read_tokens=9,
            prompt_cache_reused_tokens=0,
            cache_read_share_percent=100,
            reusable_prefix_coverage_percent=0,
        )
        self.assertTrue(
            cache_metric_failures(usage, {"input": 8, "cached": 9}, 4, False)
        )


class EvaluationSummaryTest(unittest.TestCase):
    def test_discovery_looks_through_literal_loop_aliases(self):
        code = 'import inspect as i\nfor name in ("cards", "monitor"):\n    fn = getattr(probe, name)\n    print(i.signature(fn))'
        metrics, failures = grade.discovery_evidence(
            [code], [], {"signatures": ["probe.cards", "probe.monitor"]}
        )
        self.assertEqual([], failures)
        self.assertEqual(2, metrics["signature_calls"])

    def test_known_and_duplicate_docs_fail_even_without_read_activity(self):
        for code in (
            'print(doc("probe.read"))',
            'print(apropos(pattern="probe"))',
            'print(getattr(probe.read, "contract"))',
        ):
            with self.subTest(code=code):
                _, failures = grade.discovery_evidence([code], [], {"known": True})
                self.assertTrue(failures)
        metrics, failures = grade.discovery_evidence(
            ['print(doc("probe.read"))', 'print(doc("probe.read"))'], [], {}
        )
        self.assertEqual(2, metrics["doc_calls"])
        self.assertEqual(1, metrics["redundant_discovery"])
        self.assertTrue(failures)

    def test_read_activity_does_not_double_count_syntax(self):
        metrics, failures = grade.discovery_evidence(
            ['doc("probe.read")'],
            [{"id": "1", "operation": "doc", "argument-key": "a"}],
            {},
        )
        self.assertEqual([], failures)
        self.assertEqual(1, metrics["doc_calls"])

    def test_doc_settles_the_contract_without_a_signature_call(self):
        # `doc(name)` is the registered contract itself: signature, defaults and
        # schema. Demanding inspect.signature on top of it would be the redundant
        # lookup this same audit counts against a run.
        metrics, failures = grade.discovery_evidence(
            ['print(doc("probe.read"))'], [], {"signatures": ["probe.read"]}
        )
        self.assertEqual([], failures)
        self.assertEqual(0, metrics["signature_calls"])
        self.assertEqual(1, metrics["doc_calls"])
        _, failures = grade.discovery_evidence(
            ['print(doc("probe.other"))'], [], {"signatures": ["probe.read"]}
        )
        self.assertEqual(["no signature or doc call found for probe.read"], failures)

    def test_availability_probe_is_not_rediscovery_of_a_known_tool(self):
        # A NameError guard answers "is the extension installed?", not "what is its
        # contract?" — scanning a namespace or a result shape reads no contract.
        guard = (
            "try:\n"
            "    probe\n"
            "except NameError:\n"
            '    print([n for n in dir(builtins) if "probe" in n])\n'
            "else:\n"
            "    print(probe.read())"
        )
        metrics, failures = grade.discovery_evidence(
            [guard], [], {"known": True, "symbols": ["probe.read"]}
        )
        self.assertEqual([], failures)
        self.assertEqual(0, metrics["other_inspection_calls"])
        _, failures = grade.discovery_evidence(
            ["print(dir(probe))"], [], {"known": True, "symbols": ["probe.read"]}
        )
        self.assertEqual(["known contracts were unnecessarily rediscovered"], failures)

    def test_nonobject_token_payload_fails_without_crashing(self):
        for payload in (None, [], "100"):
            with self.subTest(payload=payload):
                _, failures = grade.token_summary(payload)
                self.assertTrue(failures)

    def test_json_duplicate_keys_are_not_silently_overwritten(self):
        failures = grade.structured_failures(
            {"want_answer_json": {"count": 2}}, ".", '{"count":7,"count":2}', []
        )
        self.assertTrue(failures)

    def test_reuse_seeds_one_canonical_fixture(self):
        scenario = load_scenarios(["extension-known-contract"])[0]
        with tempfile.TemporaryDirectory() as work:
            seed_files(scenario, work)
            self.assertTrue(
                (Path(work) / ".vis/extensions/contract_probe.py").is_file()
            )


class DiscoveryEvidenceEdgeTest(unittest.TestCase):
    def test_invoked_helpers_cannot_hide_discovery_or_source_access(self):
        for code in (
            'def refresh():\n    return doc("probe.read")\nrefresh()',
            'refresh = lambda: open("source.py").read()\nrefresh()',
            'class Refresh:\n    def __init__(self):\n        doc("probe.read")\nRefresh()',
        ):
            with self.subTest(code=code):
                _, failures = grade.discovery_evidence([code], [], {"known": True})
                self.assertTrue(failures)
        _, failures = grade.discovery_evidence(
            ["def count(rows):\n    return len(rows)\nprint(count([]))"],
            [],
            {"known": True},
        )
        self.assertEqual([], failures)

    def test_literal_comprehension_matches_direct_inspection(self):
        metrics, failures = grade.discovery_evidence(
            [
                "import inspect\nprint([inspect.signature(fn) for fn in (probe.cards, probe.monitor)])"
            ],
            [],
            {"signatures": ["probe.cards", "probe.monitor"]},
        )
        self.assertEqual([], failures)
        self.assertEqual(2, metrics["signature_calls"])

    def test_missing_scope_is_unknown_not_misclassified_as_caught(self):
        helper = DiscoveryEvaluationTest()
        block = helper.activity("failed", ref=None, error="failure")
        result = helper.run_blocks(block, answer="done")
        self.assertFalse(result["correct"])
        self.assertEqual(0, result["caught_activity_failures"])
        self.assertEqual(1, result["unscoped_activity_failures"])


class EvaluationIntegrationEdgeTest(unittest.TestCase):
    def test_forbidden_fast_read_does_not_need_an_activity_row(self):
        helper = DiscoveryEvaluationTest()
        result = helper.run_blocks(
            atif.block("t1/i1/f1", 'cat("source.py")'),
            answer="done",
            discovery={"known": True},
            forbid_tools=["cat"],
        )
        self.assertFalse(result["correct"])

    def test_malformed_run_metrics_are_a_failure_not_a_crash(self):
        helper = DiscoveryEvaluationTest()
        malformed = {
            "total_prompt_tokens": "bad",
            "total_cached_tokens": 1,
            "total_completion_tokens": 2,
        }
        for metrics in ([1], "tokens", malformed):
            with self.subTest(metrics=metrics):
                document = atif.trajectory(answer="done")
                document["final_metrics"] = metrics
                result = helper.run_document(document, want_cache_read=True)
                self.assertFalse(result["correct"])

    def test_partial_read_activities_do_not_hide_duplicate_syntax(self):
        metrics, failures = grade.discovery_evidence(
            ['doc("probe.read")', 'doc("probe.read")'],
            [{"id": "1", "operation": "doc", "argument-key": "a"}],
            {},
        )
        self.assertEqual(2, metrics["doc_calls"])
        self.assertTrue(failures)


class CouncilRecoveryTest(unittest.TestCase):
    def test_seeded_handoff_matches_exact_evidence(self):
        scenario = load_scenarios(["council-thread-recovery"])[0]
        with tempfile.TemporaryDirectory() as work:
            seed_files(scenario, work)
            self.assertEqual([], grade.structured_failures(scenario, work, "", []))

    def parts(self):
        """Return the rows and the error of a run that repairs one reply-ID read."""
        expected = load_scenarios(["council-thread-recovery"])[0][
            "want_council_recovery"
        ]

        def row(row_id, operation, state, entries, body=None, sequence=1):
            resources = [{"type": "council-thread", "id": "42"}] + [
                {"type": "council-entry", "id": str(entry)} for entry in entries
            ]
            presentation = {
                "headline": operation,
                "summary": state,
                "content": [{"type": "markdown", "text": body}] if body else [],
            }
            if operation == "council.read" and state == "succeeded":
                presentation["sections"] = [
                    {
                        "headline": name,
                        "summary": "Message",
                        "content": [{"type": "markdown", "text": text}],
                    }
                    for name, text in expected.items()
                ]
            return atif.row(
                row_id,
                operation,
                state,
                sequence=sequence,
                resources=resources,
                presentation=presentation,
            )

        return {
            "root": row(
                "root", "council.publish", "succeeded", [42], expected["root_content"]
            ),
            "reply": row(
                "reply",
                "council.publish",
                "succeeded",
                [43],
                expected["reply_content"],
                sequence=2,
            ),
            "mistake": row("mistake", "council.read", "failed", []),
            "error": {
                "type": "error",
                "message": "Council entry_id=43 is a reply in thread_id=42. "
                "Use thread_id=42 to read or continue this thread.",
                "data": {"error": "invalid-thread", "entry_id": 43, "thread_id": 42},
            },
            "repair": row("repair", "council.read", "succeeded", [42, 43]),
        }

    def blocks(self, parts):
        """Return the publish, mistake and repair blocks of the run, in that order."""
        return [
            atif.block(
                "t1/i1/f1", activity=atif.receipt(parts["root"], parts["reply"])
            ),
            atif.block(
                "t1/i2/f1",
                error=parts["error"],
                activity=atif.receipt(parts["mistake"]),
            ),
            atif.block("t1/i3/f1", activity=atif.receipt(parts["repair"])),
        ]

    def evaluate(self, blocks, recovery=True):
        checks = {}
        if recovery:
            checks["want_council_recovery"] = load_scenarios(
                ["council-thread-recovery"]
            )[0]["want_council_recovery"]
        return DiscoveryEvaluationTest().run_blocks(
            *blocks, answer="council-thread-recovery-ok", **checks
        )

    def test_only_a_verified_repair_accepts_the_expected_error(self):
        result = self.evaluate(self.blocks(self.parts()))
        self.assertTrue(result["correct"], result["detail"])
        self.assertEqual(0, result["errors"])
        self.assertEqual(1, result["surfaced_errors"])
        self.assertEqual(1, result["activity_failures"])
        self.assertEqual(1, result["expected_errors"])
        self.assertEqual(1, result["expected_activity_failures"])

    def test_error_data_must_match_the_message_ids(self):
        for data in (
            {},
            {"entry_id": 43},
            {"thread_id": 42},
            {"entry_id": 44, "thread_id": 42},
            {"entry_id": 43, "thread_id": 99},
            {"entry_id": "43", "thread_id": 42},
            {"entry_id": 43.0, "thread_id": 42},
            {"entry_id": 43, "thread_id": 42.0},
        ):
            with self.subTest(data=data):
                parts = self.parts()
                parts["error"]["data"] = {"error": "invalid-thread", **data}
                result = self.evaluate(self.blocks(parts))
                self.assertFalse(result["correct"], result["detail"])

    def test_normal_scenarios_still_reject_the_same_failure(self):
        result = self.evaluate(self.blocks(self.parts()), recovery=False)
        self.assertFalse(result["correct"])
        self.assertGreater(result["errors"], 0)

    def test_incomplete_repeated_or_unrelated_repairs_cannot_pass(self):
        publish, mistake, repair = self.blocks(self.parts())
        cases = {
            "no mistake": [publish, repair],
            "no repair": [publish, mistake],
            "repair before mistake": [publish, repair, mistake],
        }
        repeated = self.parts()
        repeated["repair"].update(id="mistake-again", state="failed")
        cases["repeated mistake"] = [
            publish,
            mistake,
            atif.block(
                "t1/i3/f1",
                error=repeated["error"],
                activity=atif.receipt(repeated["repair"]),
            ),
        ]
        for name in ("wrong thread", "missing reply", "wrong body", "other failure"):
            parts = self.parts()
            row = parts["repair"]
            if name == "wrong thread":
                row["resources"][0]["id"] = "99"
            elif name == "missing reply":
                row["resources"].pop()
            elif name == "wrong body":
                row["presentation"]["sections"][1]["content"][0]["text"] = "Other"
            else:
                parts["mistake"]["operation"] = "probe.read"
            cases[name] = self.blocks(parts)
        for name, blocks in cases.items():
            with self.subTest(case=name):
                result = self.evaluate(blocks)
                self.assertFalse(result["correct"], result["detail"])

    def test_an_extra_failure_is_not_hidden_by_a_valid_repair(self):
        extra = DiscoveryEvaluationTest().activity(
            "failed", row_id="other-failure", ref="t1/i4/f1"
        )
        result = self.evaluate([*self.blocks(self.parts()), extra])
        self.assertFalse(result["correct"])
        self.assertGreater(result["errors"], 0)


if __name__ == "__main__":
    unittest.main()
