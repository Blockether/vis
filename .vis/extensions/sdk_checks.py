"""Reproducible SDK gates: source, distributions and an isolated installed consumer."""

from __future__ import annotations

import ast
import os
import re
import shlex
import tempfile
import time
import zipfile
from dataclasses import dataclass, field
from pathlib import Path
from urllib.parse import unquote, urlsplit

import blockether.vis.extension as vis


@dataclass(frozen=True, slots=True)
class CheckResult:
    """One completed gate, with a bounded process tail rather than an unbounded transcript.

    `exit_code` is None only when the process exceeded its deadline and was stopped.
    `duration_ms` includes waiting; `output_tail` holds at most 4000 characters.
    In-process artifact/documentation gates use exit code 1 for a validation failure.
    """

    name: str
    exit_code: int | None
    duration_ms: int
    output_tail: str
    is_timed_out: bool = False


@dataclass(frozen=True, slots=True)
class CheckReport:
    """Ordered, fail-fast SDK gates; no artifacts or processes outlive this report.

    `is_engine_checked` means installed tests ran successfully against the explicitly
    supplied engine. A green local-only report is not evidence of HTTP/stdio or native
    integration. Tests may report opt-in skips when no engine command was supplied.
    """

    steps: tuple[CheckResult, ...]
    is_engine_checked: bool

    is_pass: bool = field(init=False)

    def __post_init__(self):
        object.__setattr__(self, "steps", tuple(self.steps))
        object.__setattr__(
            self,
            "is_pass",
            bool(self.steps) and all(step.exit_code == 0 for step in self.steps),
        )


def _run(
    name: str, argv: list[str], cwd: Path, env: dict[str, str | None], timeout_s: int
) -> CheckResult:
    started = time.monotonic()
    # The extension shell contract accepts string values; env -u removes inherited
    # variables on both supported host platforms without sending null across the host.
    command = [
        "env",
        *[part for key, value in env.items() if value is None for part in ("-u", key)],
        *argv,
    ]
    process = vis.shell(
        {
            "op": "background",
            "command": shlex.join(command),
            "cwd": str(cwd),
            "env": {key: value for key, value in env.items() if value is not None},
        }
    )
    settled = False
    try:
        result = process.wait(timeout_s)
        settled = result.get("exit") is not None or result.get("status") == "exited"
        return CheckResult(
            name,
            result.get("exit"),
            round((time.monotonic() - started) * 1000),
            str(result.get("out", ""))[-4000:],
            is_timed_out=not settled,
        )
    finally:
        if not settled:
            process.stop()


def _verify_artifacts(root: Path, work: Path) -> Path:
    wheels = [list((work / kind).glob("*.whl")) for kind in ("rebuilt", "direct")]
    if any(len(group) != 1 for group in wheels):
        raise ValueError(
            "Expected one direct wheel and one wheel rebuilt from the sdist"
        )
    with (
        zipfile.ZipFile(wheels[0][0]) as rebuilt,
        zipfile.ZipFile(wheels[1][0]) as direct,
    ):
        names = set(rebuilt.namelist())
        if names != set(direct.namelist()) or any(
            rebuilt.read(name) != direct.read(name) for name in names
        ):
            raise ValueError("Direct and sdist-rebuilt wheel contents differ")
        source = root / "packages/vis-agent/src"
        expected = {
            path.relative_to(source).as_posix(): path.read_bytes()
            for path in (source / "blockether/vis").rglob("*")
            if path.is_file() and (path.suffix == ".py" or path.name == "py.typed")
        }
        canonical = root / "packages/vis-contract/resources/vis-contract"
        contracts = list(canonical.rglob("*.json"))
        if not contracts:
            raise ValueError("No canonical SDK contracts found")
        expected.update(
            {
                "blockether/vis/_data/"
                + path.relative_to(canonical).as_posix(): path.read_bytes()
                for path in contracts
            }
        )
        packaged = {
            name
            for name in names
            if name.startswith("blockether/vis/") and not name.endswith("/")
        }
        if packaged != set(expected):
            raise ValueError(
                "Packaged SDK files differ from source and canonical contracts"
            )
        for name, body in expected.items():
            if rebuilt.read(name) != body:
                raise ValueError(f"Packaged SDK file drifted: {name}")
    return wheels[0][0]


def _check_docs(root: Path) -> str:
    examples = links = 0
    for relative in ("packages/vis-agent/README.md", "resources/vis-docs/extending.md"):
        path = root / relative
        text = path.read_text()
        for match in re.finditer(
            r"^```(?:python|py)\s*\n(.*?)^```", text, re.MULTILINE | re.DOTALL
        ):
            try:
                ast.parse(match.group(1))
            except SyntaxError as error:
                raise ValueError(
                    f"Invalid Python example in {relative}: {error.msg}"
                ) from error
            examples += 1
        for target in re.findall(r'\[[^\]\n]*\]\(([^\s)]+)(?:\s+"[^"]*")?\)', text):
            uri = urlsplit(target)
            if uri.scheme:
                if uri.netloc.lower() != "github.com" or not uri.path.startswith(
                    "/Blockether/vis/blob/main/"
                ):
                    continue
                linked = root / unquote(
                    uri.path.removeprefix("/Blockether/vis/blob/main/")
                )
            elif uri.path:
                linked = path.parent / unquote(uri.path)
            else:
                continue
            links += 1
            if not linked.exists():
                raise ValueError(f"Missing file link in {relative}: {target}")
    return f"{examples} Python examples; {links} local file links"


def _publish(steps: list[CheckResult], running: str | None = None) -> None:
    blocks: list[vis.ActivityBlock] = []
    if steps:
        blocks.append(
            vis.ActivityTable(
                columns=["Gate", "Result", "Seconds"],
                rows=[
                    [
                        step.name,
                        "passed" if step.exit_code == 0 else "failed",
                        f"{step.duration_ms / 1000:.1f}",
                    ]
                    for step in steps
                ],
            )
        )
    if running:
        blocks.append(vis.ActivityProgress(running))
    vis.publish_activity(
        vis.ActivityPresentation(
            headline="SDK verification",
            summary=running or f"{len(steps)} gates completed",
            content=blocks,
        )
    )


class SDK:
    """Repository-owned SDK verification, not a second build system or a publisher."""

    @vis.method(
        tag="mutation", activity=vis.Activity(presenter="tests", label="Check SDK")
    )
    def check(
        self,
        root: str = ".",
        python: str = "python3",
        engine_command: str | None = None,
    ) -> CheckReport:
        """Verify the Vis SDK checkout with isolated source and installed-package gates.

        `root` must be a Vis checkout. `python` is one executable, not a shell command,
        with pytest, build, ruff and twine installed. Requires network access for build
        isolation and a disposable venv's pytest/wheel installation. No live environment
        is pip-installed into. All temporary files and started processes are cleaned up.

        Checks formatting/lint, source tests, direct/sdist wheel parity, exact SDK and
        canonical contract contents, strict distribution metadata and documented examples.
        Installed tests run outside the checkout with PYTHONPATH removed. Pass explicit
        `engine_command` (shell-quoted argv, e.g. a staged vis-agent wrapper) to also test
        real HTTP/stdio; ambient VIS_TEST_LOCAL_COMMAND is deliberately ignored. Tests
        own isolated engines and a deterministic local model, never the live gateway.
        Each command has a 15-minute deadline; failure stops subsequent gates. Returns
        bounded per-gate evidence and publishes ordered Activity progress. Does not build
        native images, spend model tokens, commit, tag, publish or restart user services.
        """
        repository = Path(root).expanduser().resolve()
        package = repository / "packages/vis-agent"
        if (
            not (package / "src/blockether/vis/extension.py").is_file()
            or not (package / "pyproject.toml").is_file()
        ):
            raise ValueError("root must name a Vis checkout containing the SDK source")
        if not python.strip() or (
            engine_command is not None and not engine_command.strip()
        ):
            raise ValueError("python and an explicit engine_command must not be empty")
        steps: list[CheckResult] = []
        env = {
            "PYTHONPATH": None,
            "VIS_TEST_INSTALLED": None,
            "VIS_TEST_LOCAL_COMMAND": None,
        }
        extension = repository / ".vis/extensions/sdk_checks.py"
        extension_tests = repository / ".vis/extensions/test_sdk_checks.py"
        tests = [str(package / "tests")]
        style = [str(package)]
        if extension.is_file():
            style.append(str(extension))
        if extension_tests.is_file():
            tests.append(str(extension_tests))
            style.append(str(extension_tests))

        def command(
            name: str,
            argv: list[str],
            cwd: Path = repository,
            environment: dict | None = None,
        ) -> bool:
            _publish(steps, name)
            result = _run(
                name, argv, cwd, env if environment is None else environment, 900
            )
            steps.append(result)
            _publish(steps)
            return result.exit_code == 0

        with tempfile.TemporaryDirectory(prefix="vis-sdk-checks-") as directory:
            work = Path(directory)
            if work.is_relative_to(repository):
                raise ValueError(
                    "TMPDIR must be outside the checkout for installed SDK verification"
                )
            initial = [
                ("lint", [python, "-m", "ruff", "check", *style]),
                ("format", [python, "-m", "ruff", "format", "--check", *style]),
                ("source tests", [python, "-m", "pytest", *tests, "-q", "--tb=short"]),
                (
                    "sdist and rebuilt wheel",
                    [
                        python,
                        "-m",
                        "build",
                        str(package),
                        "--outdir",
                        str(work / "rebuilt"),
                    ],
                ),
                (
                    "direct wheel",
                    [
                        python,
                        "-m",
                        "build",
                        str(package),
                        "--wheel",
                        "--outdir",
                        str(work / "direct"),
                    ],
                ),
            ]
            for name, argv in initial:
                if not command(name, argv):
                    return CheckReport(tuple(steps), False)
            for name, verify in (
                ("artifact parity", lambda: _verify_artifacts(repository, work)),
                ("documentation", lambda: _check_docs(repository)),
            ):
                started = time.monotonic()
                try:
                    detail = verify()
                    if name == "artifact parity":
                        wheel = detail
                    result = CheckResult(
                        name,
                        0,
                        round((time.monotonic() - started) * 1000),
                        "Source, contracts and both wheels agree"
                        if name == "artifact parity"
                        else detail,
                    )
                except (OSError, ValueError, zipfile.BadZipFile) as error:
                    result = CheckResult(
                        name,
                        1,
                        round((time.monotonic() - started) * 1000),
                        str(error)[-4000:],
                    )
                steps.append(result)
                _publish(steps)
                if result.exit_code != 0:
                    return CheckReport(tuple(steps), False)
            installed_python = (
                work
                / "venv"
                / ("Scripts/python.exe" if os.name == "nt" else "bin/python")
            )
            final = [
                (
                    "distribution metadata",
                    [
                        python,
                        "-m",
                        "twine",
                        "check",
                        "--strict",
                        *[str(path) for path in sorted((work / "rebuilt").iterdir())],
                    ],
                ),
                ("isolated environment", [python, "-m", "venv", str(work / "venv")]),
                (
                    "install wheel",
                    [
                        str(installed_python),
                        "-m",
                        "pip",
                        "install",
                        "--disable-pip-version-check",
                        "pytest",
                        str(wheel),
                    ],
                ),
                (
                    "installed tests",
                    [str(installed_python), "-m", "pytest", *tests, "-q", "--tb=short"],
                ),
            ]
            for name, argv in final:
                installed_env = {
                    **env,
                    "VIS_TEST_INSTALLED": "1",
                    "VIS_TEST_LOCAL_COMMAND": engine_command,
                    "VIS_JVM": "0",
                    "VIS_PYTHON_NATIVE_PATH": None,
                }
                if not command(
                    name,
                    argv,
                    work,
                    installed_env if name == "installed tests" else env,
                ):
                    return CheckReport(tuple(steps), False)
            return CheckReport(tuple(steps), engine_command is not None)


sdk = SDK()
vis.register(
    vis.Extension(
        name="sdk",
        description="Verify SDK source, distributions and installed HTTP/stdio consumers.",
        alias="sdk",
        symbols=[vis.Symbol(sdk, name="sdk")],
        prompt="sdk.check(root='.', python='python3', engine_command=None) verifies the SDK; never publishes. "
        "Use an explicit staged native wrapper to include HTTP/stdio integration.",
    )
)
