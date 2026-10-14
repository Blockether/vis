"""Run the Vis CLI as a Harbor agent on the host.

Select it with the Harbor agent import path ``blockether.vis.evals.agent:VisAgent``.
It runs only in ``blockether.vis.evals.environment:HostEnvironment``. Each trial
starts one CLI process with its own database. With ``--persist`` the
CLI runs the gateway in the same process, so no shared gateway and no operator
extension changes the measurement. The agent writes these files to its logs
directory:

- ``trace.jsonl``: the ``--full-trace-json-stream`` output.
- ``stderr.txt``: the error output of the CLI.
- ``trajectory.json``: the ATIF trajectory of the session.
- ``run.json``: the provider, the model, the exit code and the wall time.
"""

from __future__ import annotations

import asyncio
import functools
import json
import os
import shlex
import shutil
import subprocess
import tempfile
import time
from pathlib import Path

from harbor.agents.base import BaseAgent
from harbor.agents.capabilities import AgentCapabilities
from harbor.agents.options import AgentOptions
from harbor.environments.base import BaseEnvironment
from harbor.models.agent.context import AgentContext
from pydantic import Field

from ..engine._install import ensure_vis_agent
from .environment import HostEnvironment
from .grade import RUN_FILE, STDERR_FILE, TRACE_FILE, TRAJECTORY_FILE
from .trajectory import load_trajectory as load_vis_trajectory

DEFAULT_COMMAND = "vis-agent"
CLASSPATH_TIMEOUT_SEC = 120
_NATIVE_MAGIC = (
    b"\x7fELF",
    b"\xcf\xfa\xed\xfe",
    b"\xfe\xed\xfa\xcf",
    b"\xca\xfe\xba\xbe",
)
_SOURCE_ENGINE = (
    'cd "$VIS_HARBOR_SOURCE" && "$VIS_HARBOR_ENGINE" -Scp "$VIS_HARBOR_CLASSPATH" '
    '-J-Duser.dir="$VIS_HARBOR_WORKSPACE" -M:vis'
)
_NATIVE_ENGINE = '"$VIS_HARBOR_ENGINE" -Duser.dir="$VIS_HARBOR_WORKSPACE"'
_INSTALLED_ENGINE = '"$VIS_HARBOR_ENGINE"'


class VisOptions(AgentOptions):
    """The options that ``--ak key=value`` sets."""

    provider: str | None = Field(
        default=None, description="Vis provider. Wins over the provider in --model."
    )
    model: str | None = Field(
        default=None, description="Vis model. Wins over the model in --model."
    )
    reasoning_effort: str | None = Field(
        default=None, description="Value of the CLI option --reasoning-effort."
    )
    source: str | None = Field(
        default=None, description="Vis checkout to run from source with Clojure."
    )
    clojure: str | None = Field(default=None, description="Clojure CLI executable.")
    native_bin: str | None = Field(
        default=None, description="Raw native Vis executable to run."
    )
    command: str | None = Field(
        default=None, description="Shell prefix that replaces the engine, for tests."
    )


@functools.cache
def source_classpath(clojure: str, source: Path) -> str:
    """Resolve the :vis classpath once, with absolute entries for any user.dir."""
    classpath = subprocess.check_output(
        [clojure, "-Spath", "-A:vis"],
        cwd=source,
        text=True,
        timeout=CLASSPATH_TIMEOUT_SEC,
    ).strip()
    return os.pathsep.join(
        entry if os.path.isabs(entry) else str(source / entry)
        for entry in classpath.split(os.pathsep)
    )


@functools.cache
def source_commit(source: Path) -> str | None:
    """Return the commit of the checkout ``source``, or None when git cannot tell."""
    try:
        completed = subprocess.run(
            ["git", "rev-parse", "--short=12", "HEAD"],
            cwd=source,
            capture_output=True,
            text=True,
            timeout=30,
            check=True,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    return completed.stdout.strip() or None


class ProviderUnavailableError(RuntimeError):
    """The provider refused the turn, so the trial measures nothing about Vis."""


def provider_failure(trace: Path) -> str | None:
    """Return the provider error that ended the turn in ``trace``, or None.

    A provider quota or outage ends the turn with an error block whose code
    starts with ``provider_``. Such a trial is an infrastructure error, not a
    wrong answer.
    """
    try:
        lines = trace.read_text(encoding="utf-8").splitlines()
    except OSError:
        return None
    for line in reversed(lines):
        try:
            frame = json.loads(line)
        except ValueError:
            continue
        if not isinstance(frame, dict) or frame.get("event") != "result":
            continue
        payload = frame.get("payload")
        content = payload.get("content") if isinstance(payload, dict) else None
        for block in content if isinstance(content, list) else ():
            if not isinstance(block, dict) or block.get("type") != "error":
                continue
            code = str(block.get("code"))
            if code.startswith("provider_"):
                return f"{code}: {block.get('title') or 'provider error'}"
        return None
    return None


def check_native_binary(path: str) -> None:
    """Refuse a launcher script: the measurement must use the native image."""
    with open(path, "rb") as executable:
        magic = executable.read(4)
    if magic not in _NATIVE_MAGIC:
        raise ValueError("native_bin must be a raw native executable, not a launcher")


class VisAgent(BaseAgent):
    """Harbor agent that runs one Vis CLI session in the host workspace.

    The provider and the model come from the first source that sets them:

    1. The ``provider`` and ``model`` options (``--ak``).
    2. ``--model provider/model``.
    3. ``VIS_PROVIDER`` and ``VIS_MODEL`` in ``--ae`` or in the host environment.

    The engine comes from the first option that is set: ``command``, ``native_bin``
    (``VIS_HARBOR_NATIVE_BIN``), ``source`` (``VIS_HARBOR_SOURCE``). Without them, the
    agent runs the installed ``vis-agent`` CLI. When it is missing, setup downloads the
    matching Vis release once into the SDK cache.
    """

    capabilities = AgentCapabilities(atif=True)
    options_model = VisOptions

    def __init__(self, logs_dir: Path, model_name: str | None = None, **kwargs) -> None:
        super().__init__(logs_dir=logs_dir, model_name=model_name, **kwargs)
        options = self.options
        if not isinstance(options, VisOptions):
            raise TypeError("VisAgent needs VisOptions")
        provider = (
            options.provider
            or self._parsed_model_provider
            or self._setting("VIS_PROVIDER")
        )
        model = options.model or self._parsed_model_name or self._setting("VIS_MODEL")
        if not provider or not model:
            raise ValueError(
                "VisAgent needs a provider and a model: use --model provider/model"
            )
        self.provider, self.model = provider, model
        self.reasoning_effort = options.reasoning_effort or self._setting(
            "VIS_REASONING_EFFORT"
        )
        self.clojure = (
            options.clojure or self._setting("VIS_HARBOR_CLOJURE") or "clojure"
        )
        native_bin = options.native_bin or self._setting("VIS_HARBOR_NATIVE_BIN")
        self.native_bin = os.path.abspath(native_bin) if native_bin else None
        source = options.source or self._setting("VIS_HARBOR_SOURCE")
        self.source = Path(source).resolve() if source else None
        self.engine_override = options.command
        self.classpath: str | None = None
        self.installed_command: str | None = None
        self.model_name = f"{self.provider}/{self.model}"
        self._init_model_info()

    @staticmethod
    def name() -> str:
        return "vis"

    def version(self) -> str | None:
        if self.engine_override or self.native_bin or self.source is None:
            return None
        return source_commit(self.source)

    def _setting(self, name: str) -> str | None:
        value = self._extra_env.get(name, os.environ.get(name, ""))
        return value.strip() or None

    def vis_arguments(self) -> list[str]:
        """Return the CLI options before ``--trajectory`` and the prompt."""
        arguments = [
            "--full-trace-json-stream",
            "--provider",
            self.provider,
            "--model",
            self.model,
        ]
        if self.reasoning_effort:
            arguments += ["--reasoning-effort", self.reasoning_effort]
        return [*arguments, "--persist"]

    def shell_command(self) -> str:
        """Return the bash command of one trial.

        Paths and the prompt stay in environment variables. Then the path
        translation of the host environment cannot change them.
        """
        if self.engine_override:
            engine = self.engine_override
        elif self.native_bin:
            engine = _NATIVE_ENGINE
        elif self.source is not None:
            engine = _SOURCE_ENGINE
        else:
            engine = _INSTALLED_ENGINE
        arguments = " ".join(shlex.quote(item) for item in self.vis_arguments())
        return (
            "unset VIS_GATEWAY_URL; "
            f"{engine} {arguments} "
            f'--trajectory "$VIS_HARBOR_AGENT_LOGS/{TRAJECTORY_FILE}" '
            '"$VIS_HARBOR_PROMPT" '
            f'> "$VIS_HARBOR_AGENT_LOGS/{TRACE_FILE}" '
            f'2> "$VIS_HARBOR_AGENT_LOGS/{STDERR_FILE}"'
        )

    def process_env(self, instruction: str, runtime: Path) -> dict[str, str]:
        """Return the variables of the CLI process, with a database for this trial."""
        env = dict(self._extra_env)
        env["VIS_DB_PATH"] = str(runtime / "vis.mdb")
        env["VIS_EXTENSIONS_DIR"] = self._setting("VIS_EXTENSIONS_DIR") or str(
            runtime / "extensions"
        )
        env["VIS_HARBOR_PROMPT"] = instruction
        if self.native_bin:
            env["VIS_HARBOR_ENGINE"] = self.native_bin
        elif self.source is not None:
            env["VIS_HARBOR_ENGINE"] = self.clojure
            env["VIS_HARBOR_SOURCE"] = str(self.source)
        else:
            env["VIS_HARBOR_ENGINE"] = self.installed_command or DEFAULT_COMMAND
        if self.classpath is not None:
            env["VIS_HARBOR_CLASSPATH"] = self.classpath
        return env

    async def setup(self, environment: BaseEnvironment) -> None:
        if not isinstance(environment, HostEnvironment):
            raise TypeError(
                "VisAgent runs only in blockether.vis.evals.environment:HostEnvironment"
            )
        if self.engine_override:
            return
        if self.native_bin:
            check_native_binary(self.native_bin)
            return
        if self.source is not None:
            self.classpath = await asyncio.to_thread(
                source_classpath, self.clojure, self.source
            )
            return
        self.installed_command = await asyncio.to_thread(ensure_vis_agent)

    async def run(
        self, instruction: str, environment: BaseEnvironment, context: AgentContext
    ) -> None:
        runtime = Path(tempfile.mkdtemp(prefix="vis_harbor_runtime_"))
        record = {
            "provider": self.provider,
            "model": self.model,
            "reasoning_effort": self.reasoning_effort,
            "exit_code": None,
            "wall_s": None,
            "is_cancelled": False,
        }
        started = time.monotonic()
        try:
            result = await environment.exec(
                self.shell_command(), env=self.process_env(instruction, runtime)
            )
            record["exit_code"] = result.return_code
        except asyncio.CancelledError:
            record["is_cancelled"] = True
            raise
        finally:
            record["wall_s"] = round(time.monotonic() - started, 3)
            self.logs_dir.mkdir(parents=True, exist_ok=True)
            (self.logs_dir / RUN_FILE).write_text(json.dumps(record, indent=2) + "\n")
            shutil.rmtree(runtime, ignore_errors=True)
            self.populate_context_post_run(context)
        if failure := provider_failure(self.logs_dir / TRACE_FILE):
            raise ProviderUnavailableError(failure)

    def populate_context_post_run(self, context: AgentContext) -> None:
        """Copy the token and cost totals of the trajectory into ``context``."""
        path = self.logs_dir / TRAJECTORY_FILE
        if not path.is_file():
            return
        # Harbor calls this hook outside the error handling of a trial. An exception
        # here stops the whole job, so a trajectory that cannot be read only loses
        # its totals.
        try:
            trajectory = load_vis_trajectory(path, strict=False)
        except Exception as error:
            self.logger.warning("Cannot read %s: %s", path, error)
            return
        metrics = trajectory.final_metrics
        if metrics is None:
            return
        context.n_input_tokens = metrics.total_prompt_tokens
        context.n_cache_tokens = metrics.total_cached_tokens
        context.n_output_tokens = metrics.total_completion_tokens
        context.cost_usd = metrics.total_cost_usd
