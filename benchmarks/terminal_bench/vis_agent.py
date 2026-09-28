"""Harbor installed-agent integration for an isolated native Vis trial."""

import gzip
import json
import os
import shlex
from pathlib import Path

from capture_trace import redact_value
from harbor.agents.installed.base import BaseInstalledAgent, with_prompt_template
from harbor.environments.base import BaseEnvironment
from harbor.models.agent.context import AgentContext

MODEL = "zai-coding-plan/glm-5.3-flash"
PROVIDER, MODEL_ID = MODEL.split("/", 1)
BUNDLE = Path(__file__).resolve().parent / "artifacts" / "vis-agent-linux-amd64.tar.gz"
REMOTE = "/installed-agent/vis-agent"
REMOTE_PYTHON = "/installed-agent/vis-agent-python/python/bin/python3"
TRACE = "/logs/agent/vis-trace.jsonl.gz"
HOME = "/tmp/vis-benchmark-home"


def result_frame(path: Path) -> dict | None:
    """Read the final result, retaining the complete stream as a separate artifact."""
    if not path.is_file():
        return None
    result = None
    try:
        with gzip.open(path, "rt", encoding="utf-8") as stream:
            for line in stream:
                try:
                    frame = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if frame.get("event") == "result" and isinstance(
                    frame.get("payload"), dict
                ):
                    result = frame["payload"]
    except (EOFError, OSError):
        return None
    return result


def _count(value: object) -> int | None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    if value < 0 or int(value) != value:
        return None
    return int(value)


def populate_metrics(context: AgentContext, result: dict) -> None:
    """Keep plan billing separate from Vis's estimated metered-API equivalent."""
    tokens = result.get("tokens") or {}
    context.n_input_tokens = _count(tokens.get("input"))
    context.n_cache_tokens = _count(tokens.get("cached"))
    context.n_output_tokens = _count(tokens.get("output"))
    cost = result.get("cost") or {}
    estimate = cost.get("total_cost")
    if isinstance(estimate, bool) or not isinstance(estimate, (int, float)):
        estimate = None
    context.metadata = redact_value(
        {
            "vis": {
                "model": MODEL,
                "duration_ms": result.get("duration-ms", result.get("duration_ms")),
                "iteration_count": result.get(
                    "iteration-count", result.get("iteration_count")
                ),
                "estimated_metered_api_cost_usd": estimate,
                "billing": "Z.ai Coding Plan subscription; per-trial billed USD unknown",
                "eval": result.get("eval"),
                "status": result.get("status"),
            }
        }
    )
    # A subscription's marginal billed cost is not the metered price estimate.
    context.cost_usd = None


class VisAgent(BaseInstalledAgent):
    """Install a pinned native Linux bundle into each disposable task sandbox."""

    @staticmethod
    def name() -> str:
        return "vis-native"

    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        if self.model_name != MODEL:
            raise ValueError(f"This benchmark requires --model {MODEL}")

    def get_version_command(self) -> str:
        return f"{REMOTE} --version"

    async def install(self, environment: BaseEnvironment) -> None:
        if not BUNDLE.is_file():
            raise FileNotFoundError(f"Missing native Linux bundle: {BUNDLE}")
        await environment.upload_file(BUNDLE, "/tmp/vis-benchmark.tar.gz")
        await environment.upload_file(
            Path(__file__).with_name("capture_trace.py"),
            "/installed-agent/capture_trace.py",
        )
        await self.exec_as_root(
            environment,
            command=(
                "tar -xzf /tmp/vis-benchmark.tar.gz -C /installed-agent && "
                f"chmod 755 {REMOTE} /installed-agent/vis-agent-native && "
                f"{REMOTE_PYTHON} --version && "
                f"{REMOTE} --version"
            ),
        )

    @staticmethod
    def command(instruction: str) -> str:
        """Use a fresh home, in-memory DB, fixed model and disabled team/drafts."""
        return (
            f"mkdir -p {HOME}/.vis /logs/agent && chmod 700 {HOME} && "
            f"printf '%s\n' 'providers:' '  - id: {PROVIDER}' "
            f"'default_provider: {PROVIDER}' 'default_model: {MODEL_ID}' "
            f"> {HOME}/.vis/config.yml && chmod 600 {HOME}/.vis/config.yml && "
            f"export HOME={HOME} VIS_HOME={HOME}/.vis; "
            f"set -o pipefail; {REMOTE_PYTHON} /installed-agent/capture_trace.py "
            f"--stdout {TRACE} --stderr /logs/agent/vis-stderr.log -- "
            f"{REMOTE} --db :memory --model {MODEL} "
            "--toggles council=false,draft_backend=off "
            "--full-trace-json-stream -- "
            f"{shlex.quote(instruction)}"
        )

    @with_prompt_template
    async def run(
        self, instruction: str, environment: BaseEnvironment, context: AgentContext
    ) -> None:
        key = os.environ.get("ZAI_CODING_API_KEY")
        if not key:
            raise RuntimeError("ZAI_CODING_API_KEY is required in the Harbor host")
        if redact_value(instruction) != instruction:
            raise ValueError("Benchmark instructions must not contain credentials")
        # Credentials stay in the process environment, never command arguments.
        # The capture wrapper redacts known credentials before storing either stream.
        await self.exec_as_agent(
            environment,
            command=self.command(instruction),
            env={"ZAI_CODING_API_KEY": key},
        )

    def populate_context_post_run(self, context: AgentContext) -> None:
        result = result_frame(self.logs_dir / "vis-trace.jsonl.gz")
        if result is not None:
            populate_metrics(context, result)
