"""Tests for the task steps of the Evaluations guide: write, run, summarize and score.

The flow test runs Harbor with a stub engine, so it needs no network and no model.
"""

import json
import os
import re
import stat
import sys
from pathlib import Path

import blockether.vis.evals as evals
import pytest
from blockether.vis.evals import (
    bench_report,
    case_results,
    compare_cases,
    diagnose,
    read_tasks,
    read_trials,
    round_summary,
    run_tasks,
    write_task,
)
from blockether.vis.evals._gateway import handle

E2E = Path(__file__).parents[3] / "e2e"
MODEL = "stub-provider/stub-model"
DOCS = Path(__file__).parents[3] / "resources/vis-docs"
DOC_MODEL = "zai-coding-plan/glm-5.3-flash"
# The stub engine adds the function only when the prompt holds @NEEDLE@, so one task
# passes and one task fails.
STUB_ENGINE = """\
import json
import sys
from pathlib import Path

arguments = sys.argv[1:]
prompt = arguments[-1]
if "@NEEDLE@" in prompt:
    with open("m.py", "a") as stream:
        stream.write("\\n\\ndef c():\\n    return 3\\n")
trajectory = {
    "schema_version": "ATIF-v1.6",
    "session_id": "stub-session",
    "agent": {"name": "vis", "version": "dev", "model_name": "@MODEL@"},
    "steps": [
        {"step_id": 1, "source": "user", "message": prompt},
        {
            "step_id": 2,
            "source": "agent",
            "message": "Done.",
            "model_name": "@MODEL@",
            "metrics": {
                "prompt_tokens": 120,
                "completion_tokens": 30,
                "cached_tokens": 100,
            },
        },
    ],
    "final_metrics": {
        "total_prompt_tokens": 120,
        "total_completion_tokens": 30,
        "total_cached_tokens": 100,
        "total_steps": 2,
    },
}
path = Path(arguments[arguments.index("--trajectory") + 1])
path.write_text(json.dumps(trajectory))
"""


def stub_engine(path: Path, model: str = MODEL, needle: str = "function c") -> str:
    """Write the stub engine to `path` and give the `command` agent option."""
    path.write_text(STUB_ENGINE.replace("@MODEL@", model).replace("@NEEDLE@", needle))
    return f"{sys.executable} {path}"


SEED = "def a():\n    return 1\n\n\ndef b():\n    return 2\n"


def write_add_c(dataset: Path, name: str, instruction: str):
    return write_task(
        dataset,
        name,
        instruction,
        {"want": {"m.py": ["def c(", "return 3", "def a(", "def b("]}},
        files={"m.py": SEED},
        agent_timeout_sec=120,
        verifier_timeout_sec=120,
        metadata={"lang": "python"},
    )


def test_write_task_writes_a_vis_harbor_task(tmp_path):
    task = write_add_c(tmp_path, "add-c", "In m.py add a function c that returns 3.")
    folder = tmp_path / "add-c"
    assert task.path == folder
    assert (task.name, task.agent_timeout_sec, task.verifier_timeout_sec) == (
        "add-c",
        120.0,
        120.0,
    )
    assert dict(task.metadata) == {"lang": "python"}
    assert (folder / "instruction.md").read_text() == (
        "In m.py add a function c that returns 3.\n"
    )
    assert (folder / "environment/workspace/m.py").read_text() == SEED
    checks = json.loads((folder / "tests/checks.json").read_text())
    assert checks["want"]["m.py"][0] == "def c("
    script = folder / "tests/test.sh"
    assert script.stat().st_mode & stat.S_IXUSR
    # The e2e tasks of the repository use the same grader script.
    assert script.read_text() == (E2E / "py-add-func/tests/test.sh").read_text()
    assert read_tasks(tmp_path) == (task,)


@pytest.mark.parametrize(
    ("name", "files", "checks", "message"),
    [
        ("Add_C", None, {"want": {}}, "lowercase words"),
        ("add-c", {"../m.py": ""}, {"want": {}}, "inside the workspace"),
        ("add-c", {"/tmp/m.py": ""}, {"want": {}}, "inside the workspace"),
        ("add-c", None, {}, "nonempty object"),
    ],
)
def test_write_task_refuses_an_invalid_task(tmp_path, name, files, checks, message):
    with pytest.raises(ValueError, match=message):
        write_task(tmp_path, name, "Do it.", checks, files=files)
    assert not any(tmp_path.iterdir())


def test_write_task_refuses_an_artifact_outside_the_workspace(tmp_path):
    with pytest.raises(ValueError, match="inside the workspace"):
        write_task(tmp_path, "add-c", "Do it.", {"want": {}}, artifacts=["/etc/hosts"])
    assert not any(tmp_path.iterdir())


def test_artifacts_keep_the_call_record_of_a_mock(tmp_path, monkeypatch):
    # A mock extension writes one line for each call into the workspace. The trial
    # workspace is temporary, so only `artifacts` keeps the record in the job.
    for name in ("VIS_PROVIDER", "VIS_MODEL", "VIS_REASONING_EFFORT"):
        monkeypatch.delenv(name, raising=False)
    dataset = tmp_path / "tasks"
    record = '{"target": "north"}\n{"target": "south"}\n'
    task = write_task(
        dataset,
        "send-reports",
        "Send each report.",
        {"want": {"payload-calls.jsonl": ['"target": "north"', '"target": "south"']}},
        files={".vis/extensions/mylib.py": "# The mock extension.\n"},
        artifacts=["payload-calls.jsonl", "missing.jsonl"],
    )
    assert (
        (task.path / "task.toml")
        .read_text()
        .startswith(
            'artifacts = ["payload-calls.jsonl", "missing.jsonl"]\n\n[metadata]\n'
        )
    )
    engine = tmp_path / "engine.py"
    stub_engine(engine)
    engine.write_text(
        f"from pathlib import Path\nPath('payload-calls.jsonl').write_text({record!r})\n"
        + engine.read_text()
    )
    job = run_tasks(
        dataset,
        MODEL,
        jobs_dir=tmp_path / "jobs",
        job_name="mock",
        agent_options={"command": f"{sys.executable} {engine}"},
    )
    (trial,) = read_trials(job)
    assert trial.reward == 1.0
    folder = next(path for path in job.iterdir() if path.is_dir())
    assert (folder / "artifacts/payload-calls.jsonl").read_text() == record
    manifest = json.loads((folder / "artifacts/manifest.json").read_text())
    status = {row["source"]: row["status"] for row in manifest}
    assert (status["payload-calls.jsonl"], status["missing.jsonl"]) == ("ok", "failed")


def test_write_task_keeps_an_existing_task(tmp_path):
    write_add_c(tmp_path, "add-c", "First.")
    with pytest.raises(FileExistsError):
        write_add_c(tmp_path, "add-c", "Second.")
    assert (tmp_path / "add-c/instruction.md").read_text() == "First.\n"


def test_run_tasks_refuses_a_folder_without_tasks(tmp_path):
    with pytest.raises(ValueError, match="no Harbor tasks"):
        run_tasks(tmp_path, MODEL)


def test_the_guide_steps_run_end_to_end(tmp_path, monkeypatch):
    monkeypatch.delenv("HARBOR_TELEMETRY", raising=False)
    for name in ("VIS_PROVIDER", "VIS_MODEL", "VIS_REASONING_EFFORT"):
        monkeypatch.delenv(name, raising=False)
    dataset = tmp_path / "tasks"
    write_add_c(dataset, "add-c", "In m.py add a new function c that returns 3.")
    write_add_c(dataset, "add-c-later", "Read m.py and wait.")
    command = stub_engine(tmp_path / "engine.py")
    job = run_tasks(
        dataset,
        MODEL,
        jobs_dir=tmp_path / "jobs",
        job_name="first",
        agent_options={"command": command},
    )
    assert job == tmp_path / "jobs" / "first"
    assert os.environ["HARBOR_TELEMETRY"] == "off"
    rewards = {trial.task: trial.reward for trial in read_trials(job)}
    assert rewards == {"add-c": 1.0, "add-c-later": 0.0}

    summary = round_summary(job, dataset, model=MODEL)
    assert (summary["total_dataset_tasks"], summary["scored_tasks"]) == (2, 2)
    assert summary["solved_tasks"] == ["add-c"]
    report = bench_report(summary, "first")
    assert (report.solved_tasks, report.pass_rate_percent) == (1, 50.0)
    diagnosis = diagnose(summary)
    assert [(finding.kind, finding.task_count) for finding in diagnosis.findings] == [
        ("early_stops", 1)
    ]

    cases = case_results(read_trials(job))
    comparison = compare_cases(cases, cases)
    assert comparison.verdict == "no measurable difference"


def variant_blocks(page: str, variant: str, language: str) -> list[str]:
    """The fenced `language` blocks inside the `variant` blocks of a docs page."""
    blocks, fence, inside = [], None, False
    for line in (DOCS / page).read_text().splitlines():
        if fence is not None:
            if line.startswith("```"):
                blocks.append("\n".join(fence))
                fence = None
            else:
                fence.append(line)
        elif line.strip() == f'<div data-variant="{variant}">':
            inside = True
        elif line.strip() == "</div>":
            inside = False
        elif inside and line.startswith(f"```{language}"):
            fence = []
    return blocks


def section_blocks(page: str, heading: str) -> dict[str, list[str]]:
    """The fenced blocks of one docs page section, keyed by their language."""
    lines = (DOCS / page).read_text().split(f"{heading}\n", 1)[1].splitlines()
    blocks: dict[str, list[str]] = {}
    fence, language = None, ""
    for line in lines:
        if fence is None and line.startswith("#"):
            break
        if fence is not None and line.startswith("```"):
            blocks.setdefault(language, []).append("\n".join(fence) + "\n")
            fence = None
        elif fence is not None:
            fence.append(line)
        elif line.startswith("```"):
            fence, language = [], line[3:]
    return blocks


def test_the_mock_extension_of_the_guide_records_each_call(tmp_path, monkeypatch):
    # The guide mock was also checked with a real model run: the project mock replaced
    # a global extension of the same name, and the real function did not run.
    import blockether.vis.extension as sdk

    blocks = section_blocks("evals.md", "### Replace an extension with a mock")
    registered = []
    monkeypatch.setattr(sdk, "register_extension", registered.append)
    extensions = tmp_path / "workspace/.vis/extensions"
    extensions.mkdir(parents=True)
    mock = extensions / "mylib.py"
    mock.write_text(blocks["python"][0])
    code = compile(mock.read_text(), str(mock), "exec")
    exec(code, {"__file__": str(mock), "__name__": "mylib"})
    (extension,) = registered
    assert (extension.name, extension.alias) == ("mylib", "mylib")
    payloads = extension.symbols[0].fn
    answers = [
        payloads.send_some_huge_payload(target, "region,sales\n" * 100)
        for target in ("north", "south")
    ]
    assert answers == [
        {"status": "accepted", "id": "job-1"},
        {"status": "accepted", "id": "job-2"},
    ]
    record = (tmp_path / "workspace/payload-calls.jsonl").read_text()
    checks = json.loads(blocks["json"][0])
    assert all(text in record for text in checks["want"]["payload-calls.jsonl"])
    assert all(json.loads(line)["size"] == 1300 for line in record.splitlines())
    assert blocks["toml"][0].startswith('artifacts = ["payload-calls.jsonl"]\n')


def test_the_python_examples_of_the_api_page_run(tmp_path, monkeypatch):
    # Each Python example runs as written, in page order. Only the engine is a stub
    # that never solves the task, and the leaderboard example needs the network.
    for name in ("VIS_PROVIDER", "VIS_MODEL", "VIS_REASONING_EFFORT"):
        monkeypatch.delenv(name, raising=False)
    command = stub_engine(tmp_path / "engine.py", DOC_MODEL, needle="never")
    run = evals.run_tasks

    def run_with_stub(path, model, **options):
        options["agent_options"] = {
            **(options.get("agent_options") or {}),
            "command": command,
        }
        return run(path, model, **options)

    monkeypatch.setattr(evals, "run_tasks", run_with_stub)
    monkeypatch.chdir(tmp_path)
    blocks = variant_blocks("evals-api.md", "python", "python")
    assert len(blocks) == 10
    namespace: dict = {}
    for block in blocks:
        if "read_leaderboard(" not in block:
            exec(compile(block, "evals-api.md", "exec"), namespace)
    assert namespace["task"].name == "add-function"
    assert namespace["job"] == Path("jobs/first")
    assert namespace["summary"]["task_outcomes"] == {"failed_tests": ["add-function"]}
    assert json.loads(Path("summary.json").read_text()) == namespace["summary"]
    assert namespace["report"].pass_rate_percent == 0.0
    assert [finding.kind for finding in namespace["diagnosis"].findings] == [
        "early_stops"
    ]
    assert namespace["labels"].keys() == {
        trial.trial for trial in read_trials(namespace["job"])
    }
    assert namespace["comparison"].verdict == "no measurable difference"


def test_the_http_bodies_of_the_api_page_are_valid():
    # The gateway runs `handle` for each route, so each JSON body of the page must pass.
    bodies = 0
    for block in variant_blocks("evals-api.md", "http", "bash"):
        route = re.search(r"/v1/evals/([a-z]+)", block)
        body = re.search(r"<<'EOF'\n(.*?)\nEOF", block, re.S)
        if route and body and block.startswith("cat >"):
            answer = handle(route[1], json.loads(body[1]))
            assert "result" in answer, answer
            bodies += 1
        elif route and "--data '{}'" in block:
            assert handle(route[1], {}) == {"error": "`summary` must be an object."}
    assert bodies == 2
