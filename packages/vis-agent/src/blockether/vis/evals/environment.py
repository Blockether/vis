"""Run Harbor trials on this host, without containers.

Each trial gets a new git repository with the seed files of its task. Harbor's
container paths map to host directories, and the trial log directories are the
``/logs`` directories. Select this class with the Harbor environment import path
``blockether.vis.evals.environment:HostEnvironment``.

A task gives its seed files in ``environment/workspace``. An optional
``environment/workspace_filesystem.json`` maps filesystem root ids to workspace
paths; the environment registers them in ``vis.yml``. The verifier of a task runs
the SDK grader with ``"$VIS_HARBOR_PYTHON" -m blockether.vis.evals.grade``.
"""

from __future__ import annotations

import asyncio
import contextlib
import json
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path, PurePosixPath

from harbor.environments.base import BaseEnvironment, ExecResult
from harbor.environments.capabilities import EnvironmentCapabilities
from harbor.models.trial.paths import EnvironmentPaths

from .grade import WORKSPACE_FILESYSTEM_FILE

WORKSPACE_DIRECTORY = "workspace"
GIT_SETUP = (
    ("git", "init", "-q", "."),
    ("git", "config", "user.email", "t@t.co"),
    ("git", "config", "user.name", "t"),
    ("git", "add", "-A"),
    ("git", "commit", "-qm", "init"),
)
_SAFE_PATH = re.compile(r"[\w./-]+")
_BEFORE_PATH = r"(?<![\w./-])"
_AFTER_PATH = r"(?=$|[/\s'\";:|&<>()])"


class HostEnvironment(BaseEnvironment):
    """A Harbor environment that runs each command with bash on this host."""

    _root: Path | None = None

    @staticmethod
    def type() -> str:
        return "vis-host"

    @property
    def capabilities(self) -> EnvironmentCapabilities:
        """Report mounted logs: Harbor then reads the trial log directories directly."""
        return EnvironmentCapabilities(mounted=True)

    def _validate_definition(self) -> None:
        if not (self.environment_dir / WORKSPACE_DIRECTORY).is_dir():
            raise FileNotFoundError(
                f"The task environment has no {WORKSPACE_DIRECTORY} directory: {self.environment_dir}"
            )

    @property
    def root(self) -> Path:
        """The temporary directory of this trial."""
        if self._root is None:
            raise RuntimeError("The host environment is not started")
        return self._root

    @property
    def workspace(self) -> Path:
        """The git repository of this trial."""
        return self.root / WORKSPACE_DIRECTORY

    def path_table(self) -> dict[str, Path]:
        """Map each container path that Harbor uses to its host directory."""
        paths = EnvironmentPaths.for_os(self.os)
        table = {
            paths.logs_dir: self.root / "logs",
            paths.agent_dir: self.trial_paths.agent_dir.resolve(),
            paths.user_agent_dir: self.trial_paths.user_agent_dir.resolve(),
            paths.verifier_dir: self.trial_paths.verifier_dir.resolve(),
            paths.artifacts_dir: self.trial_paths.artifacts_dir.resolve(),
            paths.tests_dir: self.root / "tests",
            paths.solution_dir: self.root / "solution",
            paths.default_skills_dir: self.root / "skills",
        }
        workdir = self.task_env_config.workdir
        if workdir and workdir != "/":
            table[PurePosixPath(workdir)] = self.workspace
        return {str(container): host for container, host in table.items()}

    def translate(self, command: str) -> str:
        """Replace each container path in ``command`` with its host directory."""
        table = self.path_table()
        prefixes = sorted(table, key=len, reverse=True)
        pattern = re.compile(
            _BEFORE_PATH + "(" + "|".join(map(re.escape, prefixes)) + ")" + _AFTER_PATH
        )
        return pattern.sub(lambda match: str(table[match.group(1)]), command)

    def host_path(self, path: str | PurePosixPath) -> Path:
        """Return the host path of a container path. A relative path is in the workspace."""
        text = str(path)
        for container, host in sorted(
            self.path_table().items(), key=lambda item: -len(item[0])
        ):
            if text == container or text.startswith(container + "/"):
                return host / text[len(container) :].lstrip("/")
        candidate = Path(text)
        return candidate if candidate.is_absolute() else self.workspace / candidate

    def variables(self) -> dict[str, str]:
        """Return the VIS_HARBOR_* values that each command gets."""
        paths = EnvironmentPaths.for_os(self.os)
        return {
            "VIS_HARBOR_TASK": self.environment_dir.parent.name,
            "VIS_HARBOR_WORKSPACE": str(self.workspace),
            "VIS_HARBOR_TESTS": str(self.host_path(paths.tests_dir)),
            "VIS_HARBOR_AGENT_LOGS": str(self.trial_paths.agent_dir.resolve()),
            "VIS_HARBOR_VERIFIER_LOGS": str(self.trial_paths.verifier_dir.resolve()),
            "VIS_HARBOR_PYTHON": sys.executable,
        }

    async def start(self, force_build: bool) -> None:
        """Make the trial directories and a new git repository with the seed files."""
        name = re.sub(r"[^\w.-]", "_", self.session_id)[:60]
        self._root = Path(tempfile.mkdtemp(prefix=f"vis_harbor_{name}_")).resolve()
        await asyncio.to_thread(self._prepare)

    def _prepare(self) -> None:
        shutil.copytree(self.environment_dir / WORKSPACE_DIRECTORY, self.workspace)
        hosts = self.path_table().values()
        unsafe = sorted(
            str(host) for host in hosts if not _SAFE_PATH.fullmatch(str(host))
        )
        if unsafe:
            raise ValueError(
                f"A host path must not need shell quoting: {', '.join(unsafe)}"
            )
        for host in hosts:
            host.mkdir(parents=True, exist_ok=True)
        registrations_file = self.environment_dir / WORKSPACE_FILESYSTEM_FILE
        if registrations_file.is_file():
            registrations = json.loads(registrations_file.read_text())
            entries = [
                {"id": name, "path": os.path.realpath(self.workspace / relative)}
                for name, relative in registrations.items()
            ]
            config = {
                "workspace": {"filesystem": entries},
                "jail": {"filesystem": {"allow": list(registrations)}},
            }
            (self.workspace / "vis.yml").write_text(json.dumps(config, indent=2) + "\n")
        for command in GIT_SETUP:
            subprocess.run(
                command, cwd=self.workspace, check=True, capture_output=True, text=True
            )

    async def stop(self, delete: bool) -> None:
        """Remove the trial directories when Harbor asks for deletion."""
        if delete and self._root is not None:
            root, self._root = self._root, None
            await asyncio.to_thread(shutil.rmtree, root, ignore_errors=True)

    async def exec(
        self,
        command: str,
        cwd: str | None = None,
        env: dict[str, str] | None = None,
        timeout_sec: int | None = None,
        user: str | int | None = None,
    ) -> ExecResult:
        """Run ``command`` with bash on this host, in the workspace by default.

        The command runs as the host user, so ``user`` has no effect. A timeout or
        a cancellation stops the whole process group of the command.
        """
        variables = {**os.environ, **(self._merge_env(env) or {}), **self.variables()}
        process = await asyncio.create_subprocess_exec(
            "bash",
            "-c",
            self.translate(command),
            cwd=self.host_path(cwd) if cwd else self.workspace,
            env=variables,
            stdin=asyncio.subprocess.DEVNULL,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            start_new_session=True,
        )
        try:
            if timeout_sec:
                communication = asyncio.wait_for(
                    process.communicate(), timeout=timeout_sec
                )
                stdout, stderr = await communication
            else:
                stdout, stderr = await process.communicate()
        except TimeoutError:
            await _stop_group(process)
            raise RuntimeError(
                f"Command timed out after {timeout_sec} seconds"
            ) from None
        except BaseException:
            await _stop_group(process)
            raise
        return ExecResult(
            stdout=stdout.decode(errors="replace"),
            stderr=stderr.decode(errors="replace"),
            return_code=await process.wait(),
        )

    async def upload_file(self, source_path: Path | str, target_path: str) -> None:
        await asyncio.to_thread(
            _copy_file, Path(source_path), self.host_path(target_path)
        )

    async def upload_dir(self, source_dir: Path | str, target_dir: str) -> None:
        await asyncio.to_thread(
            _copy_tree, Path(source_dir), self.host_path(target_dir)
        )

    async def download_file(self, source_path: str, target_path: Path | str) -> None:
        await asyncio.to_thread(
            _copy_file, self.host_path(source_path), Path(target_path)
        )

    async def download_dir(self, source_dir: str, target_dir: Path | str) -> None:
        await asyncio.to_thread(
            _copy_tree, self.host_path(source_dir), Path(target_dir)
        )


async def _stop_group(process: asyncio.subprocess.Process) -> None:
    with contextlib.suppress(ProcessLookupError):
        os.killpg(process.pid, signal.SIGKILL)
    with contextlib.suppress(Exception):
        await asyncio.wait_for(process.wait(), timeout=10)


def _copy_file(source: Path, target: Path) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source, target)


def _copy_tree(source: Path, target: Path) -> None:
    if target.exists() and source.resolve() == target.resolve():
        return
    shutil.copytree(source, target, dirs_exist_ok=True)
