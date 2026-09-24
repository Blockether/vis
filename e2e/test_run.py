import collections
import csv
import hashlib
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import mock_open, patch

from e2e import run
from e2e.run import cache_metric_failures, decode_usage_body, usage_percent


class ScenarioFilesTest(unittest.TestCase):
    def test_registers_fixture_directories_with_absolute_paths(self):
        scenario = run.load_scenarios(["py-project-paths"])[0]
        with tempfile.TemporaryDirectory() as work:
            run.seed_files(scenario, work)
            config = json.loads((Path(work) / "vis.yml").read_text())
            self.assertEqual(
                [{"id": "library", "path": str((Path(work) / "library").resolve())}],
                config["workspace"]["filesystem"],
            )
            self.assertEqual(["library"], config["jail"]["filesystem"]["allow"])
            self.assertIn("return 0", (Path(work) / "library/value.py").read_text())

    def test_helper_reuse_scenario_repeats_one_audit_across_blocks(self):
        scenario = run.load_scenarios(["session-helper-reuse"])[0]
        self.assertEqual(2, scenario["want_helper_reuse"])
        self.assertIs(True, scenario["measurement"])
        self.assertIn("one log per python_execution block", scenario["prompt"])
        self.assertEqual(["187", "23", "charlie", "18.5"], scenario["want_answer"])
        with tempfile.TemporaryDirectory() as work:
            run.seed_files(scenario, work)
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

    def test_generated_fixtures_are_deterministic_and_scoped_to_workspace(self):
        with (
            tempfile.TemporaryDirectory() as source,
            tempfile.TemporaryDirectory() as first,
            tempfile.TemporaryDirectory() as second,
        ):
            fixture = Path(source)
            (fixture / "files").mkdir()
            (fixture / "files" / "marker.txt").write_text("fixture\n")
            (fixture / "generate.py").write_text(
                "from pathlib import Path\nimport sys\n"
                "Path(sys.argv[1], 'generated.txt').write_text('large input')\n"
            )
            scenario = {"_dir": source, "fixture_generator": "generate.py"}
            run.seed_files(scenario, first)
            run.seed_files(scenario, second)
            self.assertEqual("large input", (Path(first) / "generated.txt").read_text())
            self.assertEqual(
                (Path(first) / "generated.txt").read_bytes(),
                (Path(second) / "generated.txt").read_bytes(),
            )
            self.assertFalse((fixture / "generated.txt").exists())
            with self.assertRaisesRegex(ValueError, "fixture_generator"):
                run.seed_files(
                    {**scenario, "fixture_generator": "../generate.py"}, first
                )

    def test_namespace_rename_uses_anchored_edits_and_checks_the_moved_source(self):
        scenario = run.load_scenarios(["clj-ns-rename"])[0]
        self.assertEqual(["patch"], scenario["want_tools"])
        self.assertNotIn("symbol_rename", scenario["prompt"])
        self.assertIn("(ns foo.core)", scenario["want"]["src/foo/core.clj"])

    def test_extension_probe_scenario_requires_the_registered_extension_tools(self):
        scenario = run.load_scenarios(["extension-contract-discovery"])[0]
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
        scenarios = {sc["id"]: sc for sc in run.load_scenarios(set(cases))}
        self.assertEqual(set(cases), set(scenarios))
        for sid, modes in cases.items():
            with self.subTest(scenario=sid), tempfile.TemporaryDirectory() as work:
                sc = scenarios[sid]
                run.seed_files(sc, work)
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
                        run.stdout_recovery_failures(
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
                self.assertTrue(run.structured_failures(sc, work, "{}", []))
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
                        run.structured_failures(
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
            sc["id"]: sc for sc in run.load_scenarios(set(cases.values()) | set(cases))
        }
        for sid, source in cases.items():
            with self.subTest(scenario=sid), tempfile.TemporaryDirectory() as work:
                sc = scenarios[sid]
                self.assertEqual(
                    scenarios[source].get("files_from", source), sc["files_from"]
                )
                self.assertEqual(
                    scenarios[source]["want_answer_json"], sc["want_answer_json"]
                )
                self.assertNotIn("read_session", sc["prompt"])
                if sid == "stdout-natural-ledger":
                    self.assertIn("No code fence, prose, or explanation", sc["prompt"])
                    answer = json.dumps(sc["want_answer_json"])
                    self.assertIn(
                        "answer is not a single JSON value",
                        run.structured_failures(
                            sc, work, f"```json\n{answer}\n```\n\nExplanation", []
                        ),
                    )
                self.assertNotIn("want_stdout_recovery", sc)
                self.assertTrue(sc["want_requested_route"])
                run.seed_files(sc, work)
                self.assertEqual(
                    (Path(work) / "emit.py").read_bytes(),
                    (
                        Path(scenarios[sc["files_from"]]["_dir"]) / "files/emit.py"
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
        scenarios = {sc["id"]: sc for sc in run.load_scenarios(set(inputs))}
        self.assertEqual(set(inputs), set(scenarios))
        for sid, names in inputs.items():
            with self.subTest(scenario=sid), tempfile.TemporaryDirectory() as work:
                sc = scenarios[sid]
                run.seed_files(sc, work)
                data = Path(work) / "data"
                self.assertFalse((Path(work) / "generate.py").exists())
                self.assertEqual("generate.py", sc["fixture_generator"])
                for name in names:
                    self.assertGreater((data / name).stat().st_size, 32768)
                self.assertLess(sc["max_form_output_chars"], 1200)
                self.assertLess(sc["max_total_output_chars"], 2300)
                self.assertTrue(sc["want_requested_route"])
                self.assertTrue(run.structured_failures(sc, work, "{}", []))

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
        self.assertEqual([], run.fold_count_failures({"fold_count": 1}))
        for count in (None, False, True, 0, 2):
            with self.subTest(count=count):
                self.assertTrue(
                    run.fold_count_failures(
                        {"fold_count": count, "stdout": "folded through t1/i2"}
                    )
                )

    def test_percentage_rounds_half_up_like_gateway(self):
        self.assertEqual(13, usage_percent(1, 8))
        self.assertEqual(0, usage_percent(0, 0))
        self.assertEqual(100, usage_percent(9, 8))

    def test_usage_query_timeout_does_not_expose_the_command(self):
        with patch.object(
            run,
            "gateway_eval",
            side_effect=run.subprocess.TimeoutExpired(
                ["clojure", "-Scp", "/deps.jar"], 60
            ),
        ):
            with self.assertRaisesRegex(
                RuntimeError, "^usage query timed out after 60s$"
            ):
                run.fetch_session_usage(
                    {}, "00000000-0000-0000-0000-000000000000", 12344
                )

    def test_decodes_public_usage_envelope(self):
        self.assertEqual(
            {"input_tokens": 12}, decode_usage_body('{"usage":{"input_tokens":12}}')
        )
        with self.assertRaisesRegex(ValueError, "no usage object"):
            decode_usage_body('{"usage":null}')


class StdoutGoalRecoveryTest(unittest.TestCase):
    def test_scenario_requires_goal_cache_and_exact_read_back(self):
        scenario = run.load_scenarios(["stdout-goal-cache"])[0]
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
        self.assertEqual([], run.stdout_recovery_failures(outputs, readers, expected))
        proof_later = {**outputs[1], "scope": "t1/i3/f1", "iteration": 3}
        staged = [
            outputs[0],
            {**outputs[1], "stdout": "saved for next block"},
            proof_later,
        ]
        self.assertEqual([], run.stdout_recovery_failures(staged, readers, expected))
        self.assertTrue(
            run.stdout_recovery_failures(
                staged, [{**readers[0], "iteration": 4}], expected
            )
        )
        self.assertTrue(run.stdout_recovery_failures(outputs[:1], readers, expected))
        self.assertTrue(run.stdout_recovery_failures(outputs, [], expected))
        self.assertTrue(run.stdout_recovery_failures(outputs[1:], readers, expected))
        self.assertTrue(
            run.stdout_recovery_failures(
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
            run.stdout_recovery_failures(
                outputs,
                [{**readers[0], "code": readers[0]["code"].replace("t1/i1", "t1/i9")}],
                expected,
            )
        )
        self.assertTrue(
            run.stdout_recovery_failures(
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
            [], run.stdout_recovery_failures(originals + readbacks, readers, expected)
        )
        self.assertTrue(
            run.stdout_recovery_failures(originals + readbacks[:1], readers, expected)
        )
        self.assertTrue(
            run.stdout_recovery_failures(
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
            run.stdout_recovery_failures(
                originals
                + [readbacks[0], {**readbacks[1], "stdout": readbacks[0]["stdout"]}],
                readers,
                expected,
            )
        )
        self.assertTrue(
            run.stdout_recovery_failures(originals + readbacks, readers, expected * 2)
        )

    def test_goal_status_is_read_from_canonical_gateway_client(self):
        sid = "00000000-0000-0000-0000-000000000000"
        fake = type(
            "Response",
            (),
            {
                "returncode": 0,
                "stdout": 'VIS_E2E_SOUL\t200\t{"goal":{"status":"complete"}}\n',
            },
        )()
        with patch.object(run, "gateway_eval", return_value=fake) as query:
            self.assertEqual(
                (200, {"status": "complete"}), run.fetch_session_goal({}, sid, 12344)
            )
        command = query.call_args.args[1]
        self.assertIn("gateway-client/request!", command)
        self.assertIn("/v1/sessions/" + sid, command)


class SourceClasspathTest(unittest.TestCase):
    def test_source_classpath_remains_absolute_when_user_dir_changes(self):
        run.source_classpath.cache_clear()
        try:
            with patch.object(
                run.subprocess,
                "check_output",
                return_value="src" + os.pathsep + "/tmp/dependency.jar\n",
            ) as resolve:
                expected = (
                    os.path.join(run.REPO, "src") + os.pathsep + "/tmp/dependency.jar"
                )
                self.assertEqual(expected, run.source_classpath())
                self.assertEqual(expected, run.source_classpath())
                self.assertEqual(1, resolve.call_count)
        finally:
            run.source_classpath.cache_clear()


class NativeEngineTest(unittest.TestCase):
    def test_native_agent_uses_the_requested_binary_and_workspace(self):
        for magic in (
            b"\x7fELF",
            b"\xcf\xfa\xed\xfe",
            b"\xfe\xed\xfa\xcf",
            b"\xca\xfe\xba\xbe",
        ):
            with (
                self.subTest(magic=magic),
                patch.object(run, "NATIVE_BIN", "/release/vis"),
                patch("builtins.open", mock_open(read_data=magic)),
            ):
                self.assertEqual(
                    ["/release/vis", "-Duser.dir=/tmp/fixture"],
                    run.agent_prefix("/tmp/fixture"),
                )

    def test_native_gateway_still_uses_the_canonical_client(self):
        with (
            patch.object(run, "NATIVE_BIN", "/release/vis"),
            patch("builtins.open", mock_open(read_data=b"\x7fELF")),
            patch.object(run, "source_classpath", return_value="/checkout/src"),
            patch.object(run.subprocess, "run") as invoke,
        ):
            run.gateway_eval({}, "(gateway-client/status)", 20, cwd="/tmp/fixture")
        form = invoke.call_args.args[0][-1]
        self.assertIn("gateway.discovery", form)
        self.assertIn("with-redefs [gateway-discovery/base-argv", form)
        self.assertIn('["/release/vis" "-Duser.dir=/tmp/fixture"]', form)
        self.assertIn("(gateway-client/status)", form)

    def test_native_mode_rejects_a_launcher_that_could_select_the_jvm(self):
        with tempfile.TemporaryDirectory() as work:
            launcher = Path(work) / "vis-agent"
            launcher.write_text('#!/bin/sh\nexec clojure -M:vis "$@"\n')
            with (
                patch.object(run, "NATIVE_BIN", str(launcher)),
                self.assertRaisesRegex(RuntimeError, "raw native executable"),
            ):
                run.agent_prefix(work)

    def test_jvm_agent_keeps_the_source_classpath(self):
        with (
            patch.object(run, "NATIVE_BIN", None, create=True),
            patch.object(run, "source_classpath", return_value="/checkout/src"),
        ):
            self.assertEqual(
                [
                    run.CLOJURE,
                    "-Scp",
                    "/checkout/src",
                    "-J-Duser.dir=/tmp/fixture",
                    "-M:vis",
                ],
                run.agent_prefix("/tmp/fixture"),
            )


class ExtensionGatewayTest(unittest.TestCase):
    # Issue #187: a gateway started before fixture seeding has no local extension.
    def test_gateway_eval_uses_source_classpath_in_fixture_workspace(self):
        with (
            patch.object(
                run, "source_classpath", return_value="/checkout/src:/deps.jar"
            ),
            patch.object(run.subprocess, "run") as invoke,
        ):
            run.gateway_eval(
                {"VIS_DB_PATH": "/tmp/isolated.mdb"}, "(+ 1 2)", 20, cwd="/tmp/fixture"
            )
        args, kwargs = invoke.call_args
        self.assertEqual(run.CLOJURE, args[0][0])
        self.assertEqual(["-Scp", "/checkout/src:/deps.jar"], args[0][1:3])
        self.assertEqual("/tmp/fixture", kwargs["cwd"])
        self.assertEqual(20, kwargs["timeout"])

    def test_source_gateway_starts_in_the_seeded_workspace(self):
        with patch.object(run, "gateway_eval") as evaluate:
            evaluate.return_value.returncode = 0
            gateway = run.start_source_gateway(cwd="/tmp/fixture")
        try:
            self.assertEqual("/tmp/fixture", evaluate.call_args.kwargs["cwd"])
            self.assertTrue(
                gateway["env"]["VIS_DB_PATH"].startswith(gateway["runtime"])
            )
            self.assertNotIn("VIS_GATEWAY_URL", gateway["env"])
        finally:
            run.shutil.rmtree(gateway["runtime"])

    def test_local_extension_gateway_is_seeded_isolated_and_always_stopped(self):
        scenario = run.load_scenarios(["extension-watchdog"])[0]
        shared_env = {"VIS_DB_PATH": "/tmp/shared.mdb"}
        for fail in (False, True):
            with self.subTest(fail=fail), tempfile.TemporaryDirectory() as traces:
                local = {"env": {"VIS_DB_PATH": "/tmp/local.mdb"}, "port": 12345}
                workspaces = []

                def start(*, cwd, workspaces=workspaces, local=local):
                    workspaces.append(cwd)
                    self.assertTrue(
                        (Path(cwd) / ".vis/extensions/watchdog_probe.py").is_file()
                    )
                    return local

                with (
                    patch.object(run, "TRACES", traces),
                    patch.dict(os.environ, {"VIS_E2E_KEEP": ""}),
                    patch.object(run, "source_classpath", return_value="/checkout/src"),
                    patch.object(run, "start_source_gateway", side_effect=start),
                    patch.object(run, "stop_source_gateway", return_value=0) as stop,
                    patch.object(run.subprocess, "run") as invoke,
                ):
                    invoke.return_value.returncode = 0
                    invoke.return_value.stdout = ""
                    if fail:

                        def execute(command, **kwargs):
                            if "--full-trace-json-stream" in command:
                                raise RuntimeError("model process failed")
                            return invoke.return_value

                        invoke.side_effect = execute
                        with self.assertRaisesRegex(
                            RuntimeError, "model process failed"
                        ):
                            run.run_one((scenario, "test-model", shared_env, 12344))
                    else:
                        run.run_one((scenario, "test-model", shared_env, 12344))
                    stop.assert_called_once_with(local)
                    model_call = invoke.call_args_list[-1]
                    self.assertEqual(local["env"], model_call.kwargs["env"])
                self.assertEqual(1, len(workspaces))
                self.assertFalse(Path(workspaces[0]).exists())
                self.assertEqual({"VIS_DB_PATH": "/tmp/shared.mdb"}, shared_env)


class ScenarioTimeoutTest(unittest.TestCase):
    # Issue #187: a >300s watchdog probe needs a separate whole-scenario budget.
    def run_scenario(self, timeout_s=None, override=None, expire=False):
        scenario = run.load_scenarios(["extension-watchdog"])[0].copy()
        scenario.pop("timeout_s", None)
        if timeout_s is not None:
            scenario["timeout_s"] = timeout_s
        env = {} if override is None else {"VIS_E2E_TIMEOUT": str(override)}
        with (
            tempfile.TemporaryDirectory() as traces,
            patch.dict(os.environ, env, clear=True),
            patch.object(run, "TIMEOUT", 300),
            patch.object(run, "TRACES", traces),
            patch.object(run, "source_classpath", return_value="/checkout/src"),
            patch.object(
                run, "start_source_gateway", return_value={"env": {}, "port": 12345}
            ),
            patch.object(run, "stop_source_gateway", return_value=0),
            patch.object(run.subprocess, "run") as invoke,
        ):
            invoke.return_value.returncode = 0
            invoke.return_value.stdout = ""
            if expire:

                def execute(command, **kwargs):
                    if "--full-trace-json-stream" in command:
                        raise run.subprocess.TimeoutExpired(command, kwargs["timeout"])
                    return invoke.return_value

                invoke.side_effect = execute
            result = run.run_one((scenario, "test-model", {}, 12344))
            return invoke.call_args_list[-1].kwargs["timeout"], result

    def test_ordinary_scenarios_keep_the_default(self):
        timeout, _ = self.run_scenario()
        self.assertEqual(300, timeout)

    def test_long_scenario_uses_its_own_budget(self):
        timeout, _ = self.run_scenario(timeout_s=900)
        self.assertEqual(900, timeout)

    def test_explicit_environment_budget_wins(self):
        timeout, _ = self.run_scenario(timeout_s=900, override=720)
        self.assertEqual(720, timeout)

    def test_timeout_reports_the_effective_budget(self):
        _, result = self.run_scenario(timeout_s=900, expire=True)
        self.assertIn("vis-agent timed out after 900s", result["err_msgs"])


class ActivityToolEvidenceTest(unittest.TestCase):
    def test_ls_is_detected_from_end_only_activity_snapshots(self):
        # ls emits form-activity, not the obsolete tool-start trace event.
        scenario = run.load_scenarios(["ls-source-root-discovery"])[0].copy()
        scenario.update(want_answer=[], want_forms=[])
        activity = {
            "event": "trace-chunk",
            "payload": {
                "phase": "form-activity",
                "activity": {
                    "rows": [{"id": "ls-1", "operation": "ls", "state": "succeeded"}]
                },
            },
        }
        result_event = {"event": "result", "payload": {"answer": "Listed directory"}}
        for snapshots, expected in [([], False), ([activity, activity], True)]:
            with (
                self.subTest(snapshots=len(snapshots)),
                tempfile.TemporaryDirectory() as traces,
                patch.object(run, "TRACES", traces),
                patch.object(run, "source_classpath", return_value="/checkout/src"),
                patch.object(run.subprocess, "run") as invoke,
            ):
                invoke.return_value.returncode = 0
                invoke.return_value.stdout = "\n".join(
                    json.dumps(event) for event in [*snapshots, result_event]
                )
                result = run.run_one((scenario, "test-model", {}, 12344))
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
            (6000, 6001, {"message": "recorded failure"}, False),
            (-1, 0, None, False),
            ("6000", 0, None, False),
            (True, 0, None, False),
        ]
        for limit, size, error, expected in cases:
            scenario = run.load_scenarios(["ls-source-root-discovery"])[0].copy()
            scenario.update(
                want={}, wantnot={}, want_answer=[], want_forms=[], want_tools=[]
            )
            if limit is not None:
                scenario["max_form_output_chars"] = limit
            output = "x" * size
            events = [
                {
                    "event": "trace-chunk",
                    "payload": {
                        "phase": "form-result",
                        "stdout": output,
                        "error": error,
                    },
                },
                {
                    "event": "trace-chunk",
                    "payload": {"phase": "form-result", "stdout": ""},
                },
                {"event": "result", "payload": {"answer": "Ready"}},
            ]
            with (
                self.subTest(limit=limit, size=size, error=error),
                tempfile.TemporaryDirectory() as traces,
                patch.object(run, "TRACES", traces),
                patch.object(run, "source_classpath", return_value="/checkout/src"),
                patch.object(run.subprocess, "run") as invoke,
            ):
                invoke.return_value.returncode = 0
                invoke.return_value.stdout = "\n".join(
                    json.dumps(event) for event in events
                )
                result = run.run_one((scenario, "test-model", {}, 12344))
                self.assertEqual(expected, result["correct"])
                if not expected:
                    self.assertIn("max_form_output_chars", " ".join(result["detail"]))
                if type(limit) is int and limit >= 0:
                    self.assertIn(
                        f"max-form-output={size}/{limit} chars", result["evidence"]
                    )
                if error:
                    self.assertIn("recorded failure", result["err_msgs"])
                trace = (Path(traces) / f"{scenario['id']}.jsonl").read_text()
                self.assertIn(output, trace)


class ReasoningEffortTest(unittest.TestCase):
    def run_scenario(
        self,
        effort=None,
        evaluation=None,
        scenario_id="py-fix-body",
        *,
        usage_error=None,
        provider=None,
        content=None,
    ):
        scenario = run.load_scenarios([scenario_id])[0].copy()
        scenario.update(want={}, wantnot={})
        route = {"provider": provider or run.PROVIDER, "model": "test-model"}
        events = [
            {"event": "trace-chunk", "payload": {"phase": "provider-call", **route}},
            {
                "event": "result",
                "payload": {
                    "answer": "done",
                    "content": content,
                    "session-id": "00000000-0000-0000-0000-000000000000",
                    "cost": route,
                    "eval": evaluation,
                },
            },
        ]
        with (
            tempfile.TemporaryDirectory() as traces,
            patch.object(run, "TRACES", traces),
            patch.object(run, "REASONING_EFFORT", effort),
            patch.object(run, "source_classpath", return_value="/checkout/src"),
            patch.object(run, "fetch_session_usage", side_effect=usage_error),
            patch.object(run.subprocess, "run") as invoke,
        ):
            invoke.return_value.returncode = 0
            invoke.return_value.stdout = "\n".join(
                json.dumps(event) for event in events
            )
            result = run.run_one((scenario, "test-model", {}, 12344))
            return invoke.call_args.args[0], result

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
                        "provider": run.PROVIDER,
                        "model": "test-model",
                        "selected": {"provider": run.PROVIDER, "model": "test-model"},
                        "is_fallback": False,
                    }
                ],
            },
        }

    def test_ordinary_editing_scenarios_use_persistent_gateway_sessions(self):
        for scenario_id in ("py-fix-body", "js-rename-var"):
            with self.subTest(scenario_id=scenario_id):
                command, _ = self.run_scenario(scenario_id=scenario_id)
                self.assertEqual(1, command.count("--persist"))

    def test_unset_effort_preserves_default_command_and_validation(self):
        command, result = self.run_scenario()
        self.assertNotIn("--reasoning-effort", command)
        self.assertTrue(result["correct"])

    def test_low_is_forwarded_exactly_and_reported_with_route(self):
        command, result = self.run_scenario("low", self.evaluation())
        index = command.index("--reasoning-effort")
        self.assertEqual("low", command[index + 1])
        self.assertTrue(result["correct"], result["detail"])
        self.assertIn("reasoning-effort=low", result["evidence"])
        self.assertIn(
            f"requested-route={run.PROVIDER}/test-model calls=1", result["evidence"]
        )

    def test_missing_or_invalid_evaluation_fails_even_with_zero_exit(self):
        for evaluation in (None, {"is_valid": False, "invalid_reasons": []}):
            with self.subTest(evaluation=evaluation):
                _, result = self.run_scenario("low", evaluation)
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
                _, result = self.run_scenario("low", evaluation)
                self.assertFalse(result["correct"])

    def test_requested_route_is_not_presented_as_an_observed_route(self):
        _, result = self.run_scenario(
            "low", self.evaluation(), provider="other-provider"
        )
        self.assertFalse(result["correct"])
        self.assertIn(
            f"requested-route={run.PROVIDER}/test-model calls=1", result["evidence"]
        )
        self.assertTrue(any("other-provider" in item for item in result["detail"]))

    def test_usage_failure_preserves_the_scenario_result(self):
        _, result = self.run_scenario(
            "low",
            self.evaluation(),
            scenario_id="context-folding",
            usage_error=RuntimeError("usage query timed out after 60s"),
        )
        self.assertFalse(result["correct"])
        self.assertIn(
            "could not read persisted usage metrics: usage query timed out after 60s",
            result["detail"],
        )

    def test_provider_error_content_is_counted_and_not_converged(self):
        _, result = self.run_scenario(
            "low",
            self.evaluation(),
            content=[{"type": "error", "message": "Provider model unavailable"}],
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
        metrics, failures = run.helper_reuse_evidence(forms, 2)
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
        metrics, failures = run.helper_reuse_evidence(forms, 2)
        self.assertEqual([], failures)
        self.assertEqual(1, metrics["helpers_defined"])
        self.assertEqual(1, metrics["lambda_helpers"])
        self.assertEqual(2, metrics["reuse_forms"])

    def test_retyped_lambda_helper_fails_like_a_retyped_def(self):
        body = "audit = lambda name: name.strip()\n"
        forms = [body + "print(audit('a'))", body + "print(audit('b'))"]
        metrics, failures = run.helper_reuse_evidence(forms, 1)
        self.assertEqual(1, metrics["retyped_helpers"])
        self.assertEqual(
            ["helper 'audit' was retyped in a later form instead of called"],
            failures,
        )

    def test_defining_and_calling_inside_one_form_is_not_reuse(self):
        forms = ["def summarize(path):\n    return path\nprint(summarize('a'))"]
        metrics, failures = run.helper_reuse_evidence(forms, True)
        self.assertEqual(0, metrics["reuse_forms"])
        self.assertIn("reused in 0 later form(s)", failures[0])

    def test_retyped_definition_fails_even_when_a_later_form_calls_it(self):
        body = "def summarize(path):\n    return path.strip()\n"
        forms = [body + "print(summarize('a'))", body + "print(summarize('b'))"]
        metrics, failures = run.helper_reuse_evidence(forms, 1)
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
        metrics, failures = run.helper_reuse_evidence(forms, True)
        self.assertEqual(0, metrics["reused_helpers"])
        self.assertTrue(failures)

    def test_unparsable_form_is_skipped(self):
        forms = [
            "def summarize(path):\n    return path\n",
            "print(summarize(",
            "print(summarize('b'))",
        ]
        _, failures = run.helper_reuse_evidence(forms, 1)
        self.assertEqual([], failures)

    def test_missing_helper_and_invalid_requirements_are_reported(self):
        _, failures = run.helper_reuse_evidence(["print(1)"], True)
        self.assertEqual(["no sandbox form defined a helper to reuse"], failures)
        for want in (0, -1, "2", False, None):
            with self.subTest(want=want):
                metrics, failures = run.helper_reuse_evidence([], want)
                self.assertEqual({}, metrics)
                self.assertEqual(
                    ["want_helper_reuse must be true or a positive integer"], failures
                )


class DiscoveryEvaluationTest(unittest.TestCase):
    # #232/#234: syntactic markers and tool presence hid incorrect workflows.
    def run_events(self, events, **checks):
        scenario = {"id": "evaluation", "lang": "python", "prompt": "test", **checks}
        with (
            tempfile.TemporaryDirectory() as traces,
            patch.object(run, "TRACES", traces),
            patch.object(run, "seed_files"),
            patch.object(run, "source_classpath", return_value="/checkout/src"),
            patch.object(run.subprocess, "run") as invoke,
        ):
            invoke.return_value.returncode = 0
            invoke.return_value.stdout = "\n".join(
                json.dumps(event) for event in events
            )
            return run.run_one((scenario, "test-model", {}, 12344))

    def activity(self, state, operation="probe.read", row_id="call-1"):
        return {
            "event": "trace-chunk",
            "payload": {
                "phase": "form-activity",
                "scope": "t1/i1/f1",
                "activity": {
                    "rows": [{"id": row_id, "operation": operation, "state": state}]
                },
            },
        }

    def test_caught_activity_failure_cannot_pass(self):
        result = self.run_events(
            [
                self.activity("running"),
                self.activity("failed"),
                self.activity("failed"),
                {"event": "result", "payload": {"answer": "ready"}},
            ],
            want_tools=["probe.read"],
        )
        self.assertEqual(1, result["errors"])
        self.assertEqual(1, result["activity_failures"])
        self.assertEqual(1, result["caught_activity_failures"])
        self.assertEqual(0, result["surfaced_errors"])
        self.assertFalse(result["correct"])

    def test_group_row_supersedes_the_rows_it_collapsed(self):
        # A finished form collapses its per-call rows into ONE group row, so the
        # last snapshot no longer lists the row it replaced. That row finished with
        # the group; only a row the final snapshot still shows as running is work
        # the session abandoned.
        result = self.run_events(
            [
                self.activity("running", operation="shell"),
                self.activity("succeeded", operation="shell", row_id="group-call-1"),
                {"event": "result", "payload": {"answer": "ready"}},
            ],
            want_tools=["shell"],
        )
        self.assertEqual(0, result["incomplete_activities"])
        self.assertEqual(0, result["errors"])
        self.assertTrue(result["correct"])

    def test_collapsing_a_scope_keeps_its_failed_row_and_other_scopes(self):
        stale = self.activity("running", operation="shell")
        stale["payload"]["scope"] = "t1/i2/f1"
        failed = self.activity("failed", operation="shell", row_id="call-2")
        group = self.activity("succeeded", operation="shell", row_id="group-call-2")
        result = self.run_events(
            [
                stale,
                failed,
                group,
                {"event": "result", "payload": {"answer": "ready"}},
            ],
            want_tools=["shell"],
        )
        self.assertEqual(1, result["activity_failures"])
        self.assertEqual(1, result["incomplete_activities"])
        self.assertFalse(result["correct"])

    def test_running_or_cancelled_activity_is_not_success(self):
        for state in ("running", "cancelled"):
            with self.subTest(state=state):
                result = self.run_events(
                    [
                        self.activity(state),
                        {"event": "result", "payload": {"answer": "ready"}},
                    ],
                    want_tools=["probe.read"],
                )
                self.assertFalse(result["correct"])
                self.assertGreater(result["errors"], 0)

    def test_success_updates_running_once_and_duplicate_snapshots_are_ignored(self):
        result = self.run_events(
            [
                self.activity("running"),
                self.activity("succeeded"),
                self.activity("succeeded"),
                {"event": "result", "payload": {"answer": "ready"}},
            ],
            want_tools=["probe.read"],
            want_activity_sequence=["probe.read"],
        )
        self.assertTrue(result["correct"], result["detail"])
        self.assertEqual(0, result["errors"])
        self.assertEqual(1, result["activity_successes"])

    def test_surfaced_failure_is_not_labelled_caught(self):
        result = self.run_events(
            [
                self.activity("failed"),
                {
                    "event": "trace-chunk",
                    "payload": {
                        "phase": "form-result",
                        "scope": "t1/i1/f1",
                        "error": {"message": "failed"},
                    },
                },
                {"event": "result", "payload": {"answer": "recovered"}},
            ]
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
                result = self.run_events(
                    [{"event": "result", "payload": {"answer": answer}}],
                    want_answer_json=expected,
                )
                self.assertFalse(result["correct"])
        result = self.run_events(
            [{"event": "result", "payload": {"answer": json.dumps(expected)}}],
            want_answer_json=expected,
        )
        self.assertTrue(result["correct"], result["detail"])
        self.assertEqual(
            [],
            run.structured_failures(
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
            run.structured_failures(
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
                    run.structured_failures(
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
                events = [
                    self.activity("succeeded", op, str(i))
                    for i, op in enumerate(operations)
                ]
                result = self.run_events(
                    [*events, {"event": "result", "payload": {"answer": "done"}}],
                    want_activity_sequence=expected,
                )
                self.assertFalse(result["correct"])

    def test_total_output_is_separate_from_peak_and_tokens_are_reported(self):
        events = [
            {
                "event": "trace-chunk",
                "payload": {"phase": "form-result", "stdout": "x" * 6},
            },
            {
                "event": "trace-chunk",
                "payload": {"phase": "form-result", "stdout": "y" * 5},
            },
            {
                "event": "result",
                "payload": {
                    "answer": "done",
                    "tokens": {"input": 100, "cached": 75, "output": 20, "total": 120},
                },
            },
        ]
        result = self.run_events(
            events, max_form_output_chars=6, max_total_output_chars=10
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
            self.assertTrue(run.structured_failures(scenario, work, "", []))
            for records in (
                [{"key": "wrong"}, {"key": None}],
                [{"key": "atlas"}],
                [{"key": "atlas"}, {"key": None}, {"key": None}],
            ):
                path.write_text("\n".join(json.dumps(row) for row in records))
                self.assertTrue(run.structured_failures(scenario, work, "", []))
            path.write_text('{"key":"atlas"}\n{"key":null}\n')
            self.assertEqual([], run.structured_failures(scenario, work, "", []))

    def test_aliases_count_but_comments_strings_and_uninvoked_functions_do_not(self):
        rules = {"signatures": ["probe.read"]}
        for code in (
            "# inspect.signature(probe.read)",
            'print("inspect.signature(probe.read)")',
            "def unused():\n    return inspect.signature(probe.read)",
        ):
            with self.subTest(code=code):
                _, failures = run.discovery_evidence([code], [], rules)
                self.assertTrue(failures)
        metrics, failures = run.discovery_evidence(
            ["from inspect import signature as sig\nf = probe.read\nprint(sig(f))"],
            [],
            rules,
        )
        self.assertEqual([], failures)
        self.assertEqual(1, metrics["signature_calls"])

    def test_duplicate_and_known_contract_discovery_are_rejected(self):
        code = "import inspect\nprint(inspect.signature(probe.read))"
        metrics, failures = run.discovery_evidence(
            [code, code], [], {"signatures": ["probe.read"]}
        )
        self.assertEqual(1, metrics["redundant_discovery"])
        self.assertTrue(failures)
        _, failures = run.discovery_evidence([code], [], {"known": True})
        self.assertTrue(failures)
        _, failures = run.discovery_evidence(
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
                _, failures = run.discovery_evidence([code], [], {"known": True})
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
                _, failures = run.token_summary(tokens)
                self.assertTrue(failures)
        summary, failures = run.token_summary({"input": 8, "cached": 0, "output": 1})
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
        metrics, failures = run.discovery_evidence(
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
                _, failures = run.discovery_evidence([code], [], {"known": True})
                self.assertTrue(failures)
        metrics, failures = run.discovery_evidence(
            ['print(doc("probe.read"))', 'print(doc("probe.read"))'], [], {}
        )
        self.assertEqual(2, metrics["doc_calls"])
        self.assertEqual(1, metrics["redundant_discovery"])
        self.assertTrue(failures)

    def test_read_activity_does_not_double_count_syntax(self):
        metrics, failures = run.discovery_evidence(
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
        metrics, failures = run.discovery_evidence(
            ['print(doc("probe.read"))'], [], {"signatures": ["probe.read"]}
        )
        self.assertEqual([], failures)
        self.assertEqual(0, metrics["signature_calls"])
        self.assertEqual(1, metrics["doc_calls"])
        _, failures = run.discovery_evidence(
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
        metrics, failures = run.discovery_evidence(
            [guard], [], {"known": True, "symbols": ["probe.read"]}
        )
        self.assertEqual([], failures)
        self.assertEqual(0, metrics["other_inspection_calls"])
        _, failures = run.discovery_evidence(
            ["print(dir(probe))"], [], {"known": True, "symbols": ["probe.read"]}
        )
        self.assertEqual(["known contracts were unnecessarily rediscovered"], failures)

    def test_nonobject_token_payload_fails_without_crashing(self):
        for payload in (None, [], "100"):
            with self.subTest(payload=payload):
                _, failures = run.token_summary(payload)
                self.assertTrue(failures)

    def test_json_duplicate_keys_are_not_silently_overwritten(self):
        failures = run.structured_failures(
            {"want_answer_json": {"count": 2}}, ".", '{"count":7,"count":2}', []
        )
        self.assertTrue(failures)

    def test_repeats_report_all_runs_and_weighted_cache_share(self):
        rows = []
        for index, (input_tokens, cached, output) in enumerate(
            ((100, 75, 20), (300, 125, 40)), 1
        ):
            tokens, _ = run.token_summary(
                {"input": input_tokens, "cached": cached, "output": output}
            )
            rows.append(
                {
                    "id": "probe",
                    "provider": "test",
                    "model": "model",
                    "repeat": index,
                    "converged": True,
                    "correct": index == 1,
                    "errors": 0,
                    "tokens": tokens,
                    "token_errors": [],
                    "wall": index * 10,
                    "forms": index * 2,
                    "provider_calls": index * 3,
                    "max_form_output_chars": index * 4,
                    "total_output_chars": index * 5,
                }
            )
        summary = run.summarize_results(rows)[0]
        self.assertEqual(2, summary["runs"])
        self.assertEqual(1, summary["passed"])
        self.assertEqual(400, summary["token_totals"]["input"])
        self.assertEqual(200, summary["token_totals"]["cached"])
        self.assertEqual(200, summary["token_totals"]["uncached"])
        self.assertEqual(50.0, summary["cached_input_percent"])
        self.assertEqual({"min": 100, "median": 200.0, "max": 300}, summary["input"])
        self.assertEqual({"min": 10, "median": 15.0, "max": 20}, summary["wall"])

    def test_measurement_runs_report_a_behavior_rate_instead_of_gating(self):
        rows = [
            {
                "id": "probe",
                "provider": "test",
                "model": "model",
                "repeat": index,
                "converged": True,
                "correct": True,
                "errors": 0,
                "measurement": True,
                "behavior": [] if index == 1 else ["no helper was reused"],
                "tokens": {},
                "token_errors": [],
                "wall": 10,
                "forms": 5,
                "provider_calls": 3,
                "max_form_output_chars": 4,
                "total_output_chars": 5,
            }
            for index in (1, 2)
        ]
        summary = run.summarize_results(rows)[0]
        self.assertTrue(summary["measurement"])
        self.assertEqual(2, summary["passed"])
        self.assertEqual(1, summary["behavior_passed"])

    def test_reuse_seeds_one_canonical_fixture_and_rejects_path_escape(self):
        scenario = run.load_scenarios(["extension-known-contract"])[0]
        with tempfile.TemporaryDirectory() as work:
            run.seed_files(scenario, work)
            self.assertTrue(
                (Path(work) / ".vis/extensions/contract_probe.py").is_file()
            )
            for name in ("../extension-contract-discovery", "not-a-scenario"):
                with self.subTest(name=name), self.assertRaises(ValueError):
                    run.seed_files({**scenario, "files_from": name}, work)


class DiscoveryEvidenceEdgeTest(unittest.TestCase):
    def test_invoked_helpers_cannot_hide_discovery_or_source_access(self):
        for code in (
            'def refresh():\n    return doc("probe.read")\nrefresh()',
            'refresh = lambda: open("source.py").read()\nrefresh()',
            'class Refresh:\n    def __init__(self):\n        doc("probe.read")\nRefresh()',
        ):
            with self.subTest(code=code):
                _, failures = run.discovery_evidence([code], [], {"known": True})
                self.assertTrue(failures)
        _, failures = run.discovery_evidence(
            ["def count(rows):\n    return len(rows)\nprint(count([]))"],
            [],
            {"known": True},
        )
        self.assertEqual([], failures)

    def test_literal_comprehension_matches_direct_inspection(self):
        metrics, failures = run.discovery_evidence(
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
        activity = helper.activity("failed")
        activity["payload"].pop("scope")
        result = helper.run_events(
            [
                activity,
                {
                    "event": "trace-chunk",
                    "payload": {"phase": "form-result", "error": "failure"},
                },
                {"event": "result", "payload": {"answer": "done"}},
            ]
        )
        self.assertFalse(result["correct"])
        self.assertEqual(0, result["caught_activity_failures"])
        self.assertEqual(1, result["unscoped_activity_failures"])


class EvaluationIntegrationEdgeTest(unittest.TestCase):
    def test_forbidden_fast_read_does_not_need_an_activity_row(self):
        helper = DiscoveryEvaluationTest()
        result = helper.run_events(
            [
                {
                    "event": "trace-chunk",
                    "payload": {"phase": "form-start", "code": 'cat("source.py")'},
                },
                {"event": "result", "payload": {"answer": "done"}},
            ],
            discovery={"known": True},
            forbid_tools=["cat"],
        )
        self.assertFalse(result["correct"])

    def test_malformed_provider_tokens_are_a_failure_not_a_crash(self):
        helper = DiscoveryEvaluationTest()
        for tokens in ([1], "tokens", {"input": "bad", "cached": 1, "output": 2}):
            with self.subTest(tokens=tokens):
                result = helper.run_events(
                    [
                        {
                            "event": "result",
                            "payload": {"answer": "done", "tokens": tokens},
                        }
                    ],
                    want_cache_read=True,
                )
                self.assertFalse(result["correct"])

    def test_partial_read_activities_do_not_hide_duplicate_syntax(self):
        metrics, failures = run.discovery_evidence(
            ['doc("probe.read")', 'doc("probe.read")'],
            [{"id": "1", "operation": "doc", "argument-key": "a"}],
            {},
        )
        self.assertEqual(2, metrics["doc_calls"])
        self.assertTrue(failures)


if __name__ == "__main__":
    unittest.main()
