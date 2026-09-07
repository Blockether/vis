"""Reusable SDK verification owns its processes, artifacts and installed-test boundary."""

import importlib.util
import sys
import zipfile
from dataclasses import FrozenInstanceError
from pathlib import Path

import blockether.vis.extension as vis
import pytest


@pytest.fixture
def checks(monkeypatch):
    monkeypatch.setattr(vis, "_registration", {"spec": None})
    spec = importlib.util.spec_from_file_location(
        "sdk_checks", Path(__file__).with_name("sdk_checks.py")
    )
    module = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, "sdk_checks", module)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def checkout(tmp_path):
    source = tmp_path / "packages/vis-agent/src/blockether/vis"
    canonical = tmp_path / "packages/vis-contract/resources/vis-contract"
    source.mkdir(parents=True)
    canonical.mkdir(parents=True)
    (source / "extension.py").write_text("VALUE = 1\n")
    (source / "py.typed").touch()
    (source.parents[2] / "pyproject.toml").write_text(
        '[project]\nname = "vis-agent"\nversion = "0.0.1"\n'
    )
    (canonical / "example.json").write_text('{"example": true}\n')
    return tmp_path


def wheels(checkout, tmp_path):
    work = tmp_path / "artifacts"
    entries = {
        "blockether/vis/extension.py": b"VALUE = 1\n",
        "blockether/vis/py.typed": b"",
        "blockether/vis/_data/example.json": b'{"example": true}\n',
    }
    for directory in ("rebuilt", "direct"):
        target = work / directory / "vis_agent-0.0.1-py3-none-any.whl"
        target.parent.mkdir(parents=True)
        with zipfile.ZipFile(target, "w") as archive:
            for name, body in entries.items():
                archive.writestr(name, body)
    return work


def test_registration_is_a_typed_sdk_namespace(checks):
    declaration = vis._registration["spec"]
    assert declaration["name"] == "sdk"
    assert declaration["alias"] == "sdk"
    namespace = declaration["symbols"][0]
    assert namespace["name"] == "sdk"
    assert namespace["methods"][0]["name"] == "check"
    assert namespace["methods"][0]["tag"] == "mutation"


def test_results_are_frozen_and_empty_is_not_a_pass(checks):
    result = checks.CheckResult("test", 0, 1, "ok")
    with pytest.raises(FrozenInstanceError):
        result.exit_code = 1
    assert not checks.CheckReport((), False).is_pass
    assert checks.CheckReport((result,), False).is_pass


class Process:
    def __init__(self, result):
        self.result = result
        self.stops = 0

    def wait(self, seconds):
        if isinstance(self.result, BaseException):
            raise self.result
        return self.result

    def stop(self):
        self.stops += 1
        return {"status": "exited", "exit": -15, "out": ""}


@pytest.mark.parametrize(
    "result",
    [{"status": "running", "exit": None, "out": "pending"}, KeyboardInterrupt()],
)
def test_unsettled_process_is_always_stopped(checks, monkeypatch, tmp_path, result):
    process = Process(result)
    monkeypatch.setattr(vis, "shell", lambda _: process)
    if isinstance(result, BaseException):
        with pytest.raises(KeyboardInterrupt):
            checks._run("test", ["python", "-V"], tmp_path, {}, 1)
    else:
        outcome = checks._run("test", ["python", "-V"], tmp_path, {}, 1)
        assert outcome.exit_code is None
        assert outcome.is_timed_out
    assert process.stops == 1


def test_real_shell_result_is_bounded_and_finished(checks, tmp_path):
    result = checks._run(
        "test", [sys.executable, "-c", "print('ok')"], tmp_path, {}, 10
    )
    assert result.exit_code == 0
    assert result.output_tail.strip() == "ok"
    assert not result.is_timed_out


def test_invalid_checkout_fails_before_spawning(checks, tmp_path, monkeypatch):
    monkeypatch.setattr(vis, "shell", lambda _: pytest.fail("unexpected process"))
    with pytest.raises(ValueError, match="Vis checkout"):
        checks.sdk.check(root=str(tmp_path))


def test_checks_stop_at_first_failure(checks, checkout, monkeypatch):
    calls = []

    def run(name, argv, cwd, env, timeout_s):
        calls.append(name)
        return checks.CheckResult(name, 1, 0, "failed")

    monkeypatch.setattr(checks, "_run", run)
    result = checks.sdk.check(root=str(checkout))
    assert not result.is_pass
    assert len(calls) == 1
    assert not result.is_engine_checked


@pytest.mark.parametrize("engine", [None, "/path with spaces/vis-agent"])
def test_installed_checks_use_disposable_python_outside_checkout(
    checks, checkout, monkeypatch, engine
):
    calls = []
    monkeypatch.setenv("VIS_TEST_LOCAL_COMMAND", "must-not-inherit")

    def run(name, argv, cwd, env, timeout_s):
        calls.append((name, argv, Path(cwd), env))
        return checks.CheckResult(name, 0, 0, "310 passed")

    def artifacts(root, work):
        wheel = work / "rebuilt/vis_agent.whl"
        wheel.parent.mkdir()
        wheel.touch()
        return wheel

    monkeypatch.setattr(checks, "_run", run)
    monkeypatch.setattr(checks, "_verify_artifacts", artifacts)
    monkeypatch.setattr(checks, "_check_docs", lambda root: "2 examples, 1 link")
    result = checks.sdk.check(
        root=str(checkout), python="/python with spaces", engine_command=engine
    )
    assert result.is_pass
    assert result.is_engine_checked == (engine is not None)
    installed = next(call for call in calls if call[0] == "installed tests")
    assert not installed[2].is_relative_to(checkout)
    assert installed[1][0] != "/python with spaces"
    assert installed[3]["VIS_TEST_INSTALLED"] == "1"
    assert installed[3]["VIS_TEST_LOCAL_COMMAND"] == engine
    assert installed[3]["PYTHONPATH"] is None
    assert installed[3]["VIS_JVM"] == "0"
    assert installed[3]["VIS_PYTHON_NATIVE_PATH"] is None
    assert not installed[2].exists(), "the disposable environment must be cleaned up"
    source = next(call for call in calls if call[0] == "source tests")
    assert source[3]["VIS_TEST_INSTALLED"] is None
    assert source[3]["VIS_TEST_LOCAL_COMMAND"] is None


def test_wheels_agree_with_each_other_and_all_source(checks, checkout, tmp_path):
    work = wheels(checkout, tmp_path)
    assert checks._verify_artifacts(checkout, work).suffix == ".whl"


@pytest.mark.parametrize("drift", ["source", "contract", "wheel", "extra", "missing"])
def test_artifact_drift_is_not_a_pass(checks, checkout, tmp_path, drift):
    work = wheels(checkout, tmp_path)
    if drift == "source":
        (checkout / "packages/vis-agent/src/blockether/vis/extension.py").write_text(
            "VALUE = 2\n"
        )
    elif drift == "contract":
        (
            checkout / "packages/vis-contract/resources/vis-contract/example.json"
        ).write_text("{}")
    elif drift == "missing":
        next((work / "rebuilt").glob("*.whl")).unlink()
    else:
        targets = ["direct"] if drift == "wheel" else ["rebuilt", "direct"]
        for target in targets:
            with zipfile.ZipFile(next((work / target).glob("*.whl")), "a") as archive:
                archive.writestr("blockether/vis/obsolete.py", "pass")
    with pytest.raises(ValueError):
        checks._verify_artifacts(checkout, work)


@pytest.mark.parametrize(
    "body", ["```python\nthis is invalid python !\n```\n", "[missing](absent.py)\n"]
)
def test_documentation_errors_fail_checks(checks, checkout, body):
    for relative in ("packages/vis-agent/README.md", "resources/vis-docs/extending.md"):
        path = checkout / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(body)
    with pytest.raises(ValueError):
        checks._check_docs(checkout)


def test_activity_keeps_gate_order_and_never_copies_process_output(checks, monkeypatch):
    presentations = []
    monkeypatch.setattr(vis, "publish_activity", presentations.append)
    steps = [
        checks.CheckResult("lint", 0, 1000, "process-only output"),
        checks.CheckResult("tests", 1, 2500, "failure output"),
    ]
    checks._publish(steps, "building")
    presentation = presentations[0]
    assert isinstance(presentation, vis.ActivityPresentation)
    assert presentation.summary == "building"
    assert presentation.content[0].rows == (
        ("lint", "passed", "1.0"),
        ("tests", "failed", "2.5"),
    )
    assert presentation.content[1].label == "building"
    assert "process-only output" not in str(presentation.to_wire())


def test_process_environment_can_remove_inherited_values(checks, tmp_path, monkeypatch):
    monkeypatch.setenv("SDK_CHECK_REMOVE", "must-not-inherit")
    result = checks._run(
        "environment",
        [
            sys.executable,
            "-c",
            "import os; print(os.environ.get('SDK_CHECK_REMOVE', 'absent')); print(os.environ['SDK_CHECK_KEEP'])",
        ],
        tmp_path,
        {"SDK_CHECK_REMOVE": None, "SDK_CHECK_KEEP": "kept"},
        10,
    )
    assert result.exit_code == 0
    assert result.output_tail.strip().splitlines() == ["absent", "kept"]
