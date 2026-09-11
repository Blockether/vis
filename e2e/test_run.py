import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

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

    def test_decodes_public_usage_envelope(self):
        self.assertEqual(
            {"input_tokens": 12}, decode_usage_body('{"usage":{"input_tokens":12}}')
        )
        with self.assertRaisesRegex(ValueError, "no usage object"):
            decode_usage_body('{"usage":null}')


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


if __name__ == "__main__":
    unittest.main()
