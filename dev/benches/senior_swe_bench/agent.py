"""Harbor installed-agent wrapper for Vis on Senior SWE-Bench."""
from __future__ import annotations

import asyncio
import json
import os
import shlex
import subprocess
from pathlib import Path
from typing import Any

try:  # Harbor 0.17 public import path for installed agents.
    from harbor.agents.installed.base import BaseInstalledAgent, with_prompt_template  # type: ignore
except Exception:  # pragma: no cover - exercised only when Harbor is absent locally.
    try:  # Older docs exposed these from harbor.agents.
        from harbor.agents import BaseInstalledAgent, with_prompt_template  # type: ignore
    except Exception:

        class BaseInstalledAgent:  # type: ignore[no-redef]
            pass

        def with_prompt_template(fn):  # type: ignore[no-redef]
            return fn

try:
    from harbor.agents.installed.pi import Pi as HarborPi  # type: ignore
except Exception:  # pragma: no cover - keeps host-side unit tests Harbor-free.
    HarborPi = BaseInstalledAgent  # type: ignore[misc,assignment]


HERE = Path(__file__).resolve().parent
ARTIFACT_ENV = "VIS_BENCH_ARTIFACT"
DEFAULT_INSTALL = "/tmp/vis-agent"
DEFAULT_ARTIFACTS = "/logs/artifacts"
DEFAULT_REMOTE_HOME = "/tmp/vis-home"
PI_MODELS_ENV = "VIS_BENCH_PI_MODELS_JSON"
PI_MODELS_REMOTE = "/tmp/vis-bench-pi-models.json"



def _artifact_path() -> Path:
    raw = os.environ.get(ARTIFACT_ENV)
    if not raw:
        raise FileNotFoundError(
            f"{ARTIFACT_ENV} is not set. Run dev/benches/senior_swe_bench/package_vis.sh "
            "and export VIS_BENCH_ARTIFACT=target/bench/vis-agent.tar.gz."
        )
    path = Path(raw).expanduser().resolve()
    if not path.exists():
        raise FileNotFoundError(f"{ARTIFACT_ENV} points to missing file: {path}")
    return path


def _workspace_from_context(context: Any) -> str:
    for key in ("workspace", "workdir", "repo_path", "repository_path"):
        value = getattr(context, key, None) if not isinstance(context, dict) else context.get(key)
        if value:
            return str(value)
    return os.environ.get("HARBOR_WORKSPACE", "/repo")


def _env_value(owner: Any, key: str, default: str | None = None) -> str | None:
    getter = getattr(owner, "_get_env", None)
    if callable(getter):
        value = getter(key)
        if value is not None:
            return value
    return os.environ.get(key, default)


def _task_id_from_context(context: Any, explicit_task_id: str | None = None) -> str:
    if explicit_task_id:
        return explicit_task_id
    for key in ("task_name", "task_id", "id", "name"):
        value = getattr(context, key, None) if not isinstance(context, dict) else context.get(key)
        if value:
            if isinstance(value, dict) and value.get("path"):
                return Path(str(value["path"])).name
            return str(value)
    return "task"


def _run_local(cmd: list[str], cwd: str | None = None, env: dict[str, str] | None = None, timeout: float | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, cwd=cwd, env=env, text=True, capture_output=True, timeout=timeout, check=False)


async def _maybe_env_run(environment: Any, command: str, cwd: str | None = None, timeout: float | None = None) -> Any:
    """Run through Harbor environment when it exposes a command API; else local."""
    timeout_sec = int(timeout) if timeout is not None else None
    exec_meth = getattr(environment, "exec", None)
    if exec_meth is not None:
        result = exec_meth(command=command, cwd=cwd, timeout_sec=timeout_sec)
        return await result if asyncio.iscoroutine(result) else result

    for name in ("run", "execute", "shell"):
        meth = getattr(environment, name, None)
        if meth is None:
            continue
        try:
            result = meth(command, cwd=cwd, timeout=timeout)
        except TypeError:
            try:
                result = meth(command)
            except TypeError:
                continue
        return await result if asyncio.iscoroutine(result) else result
    return await asyncio.to_thread(_run_local, ["bash", "-lc", command], cwd, None, timeout)



def _result_text(result: Any, attr: str) -> str:
    value = getattr(result, attr, None)
    if value is None and isinstance(result, dict):
        value = result.get(attr)
    return "" if value is None else str(value)


def _returncode(result: Any) -> int:
    value = getattr(result, "returncode", None)
    if value is None:
        value = getattr(result, "return_code", None)
    if value is None:
        value = getattr(result, "exit_code", None)

    if value is None and isinstance(result, dict):
        value = result.get("returncode", result.get("exit_code", result.get("exit", 0)))
    return int(value or 0)


def _cost_usd(cost: dict[str, Any]) -> Any:
    for key in ("total-cost", "total_cost", "cost_usd", "usd"):
        if key in cost:
            return cost[key]
    return None


def _merge_trace_metrics(stats: dict[str, Any], source: dict[str, Any], max_iteration: int) -> int:
    if isinstance(source.get("tokens"), dict):
        stats["tokens"] = source["tokens"]
    if isinstance(source.get("cost"), dict):
        cost = _cost_usd(source["cost"])
        if cost is not None:
            stats["cost_usd"] = cost
    iteration_count = source.get("iteration-count", source.get("iteration_count"))
    if isinstance(iteration_count, int):
        max_iteration = max(max_iteration, iteration_count)
    return max_iteration


def _summarize_trace(trace: Path) -> dict[str, Any]:
    stats: dict[str, Any] = {
        "trace": str(trace),
        "source": "vis_trace",
        "available": trace.exists(),
        "telemetry_complete": False,
        "iterations": 0,
        "tool_calls": 0,
        "shell_calls": 0,
        "file_reads": 0,
        "tokens": {},
        "cost_usd": None,
        "reasoning_reporting": "unreported",
    }
    max_iteration = 0
    tool_ids: set[str] = set()
    file_read_tools = {"cat", "rg", "find_files", "ls", "outline"}
    if not trace.exists():
        return stats
    for line in trace.read_text(errors="replace").splitlines():
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        payload = obj.get("payload") if isinstance(obj.get("payload"), dict) else {}
        phase = obj.get("phase") or obj.get("type") or payload.get("phase")
        if phase in {"turn-complete", "iteration"}:
            stats["iterations"] += 1
        iteration = obj.get("iteration", payload.get("iteration"))
        if isinstance(iteration, int):
            max_iteration = max(max_iteration, iteration)
        tool_event = payload.get("tool-event") if isinstance(payload.get("tool-event"), dict) else {}
        tool_name = str(
            obj.get("tool")
            or obj.get("name")
            or payload.get("tool-name")
            or payload.get("tool")
            or payload.get("name")
            or tool_event.get("op")
            or ""
        )
        scope = payload.get("scope")
        if phase in {"form-start", "tool-start"} and (scope or tool_name):
            tool_id = str(scope or f"{phase}:{iteration}:{payload.get('position')}:{tool_name}")
            if tool_id not in tool_ids:
                tool_ids.add(tool_id)
                stats["tool_calls"] += 1
                if "shell" in tool_name:
                    stats["shell_calls"] += 1
                if tool_name in file_read_tools:
                    stats["file_reads"] += 1
        elif obj.get("type") == "tool_call" and tool_name:
            stats["tool_calls"] += 1
        max_iteration = _merge_trace_metrics(stats, obj, max_iteration)
        max_iteration = _merge_trace_metrics(stats, payload, max_iteration)
        final_payload = payload.get("final")
        if isinstance(final_payload, dict):
            max_iteration = _merge_trace_metrics(stats, final_payload, max_iteration)
        eval_evidence = payload.get("eval")
        if isinstance(eval_evidence, dict):
            stats["eval"] = eval_evidence
            stats["eval_valid"] = eval_evidence.get("valid?") is True
            effort_evidence = eval_evidence.get("reasoning-effort")
            if isinstance(effort_evidence, dict) and isinstance(effort_evidence.get("requested"), str):
                stats["reasoning_effort"] = effort_evidence["requested"]
                stats["reasoning_effort_explicit"] = True
    if max_iteration:
        stats["iterations"] = max(stats["iterations"], max_iteration)
    stats["telemetry_complete"] = stats["available"] and stats["iterations"] > 0
    if _token_value(stats["tokens"], "reasoning", "reasoning_tokens") is not None:
        stats["reasoning_reporting"] = "reported"
    return stats


def _token_value(tokens: dict[str, Any], *keys: str) -> int | float | None:
    for key in keys:
        value = tokens.get(key)
        if isinstance(value, (int, float)) and not isinstance(value, bool):
            return value
    return None


def _populate_context(context: Any, stats: dict[str, Any]) -> Any:
    """Copy trace accounting into Harbor before it serializes AgentContext."""
    tokens = stats.get("tokens") if isinstance(stats.get("tokens"), dict) else {}
    values = {
        "n_input_tokens": _token_value(tokens, "input", "input_tokens", "prompt"),
        "n_cache_tokens": _token_value(tokens, "cached", "cache-read", "cache_read", "cache_tokens"),
        "n_output_tokens": _token_value(tokens, "output", "output_tokens", "completion"),
        "cost_usd": stats.get("cost_usd") if isinstance(stats.get("cost_usd"), (int, float)) else None,
    }
    metadata = {"vis": stats}
    if isinstance(context, dict):
        context.update({key: value for key, value in values.items() if value is not None})
        context.setdefault("metadata", {}).update(metadata)
        return context
    for key, value in values.items():
        if value is not None and hasattr(context, key):
            setattr(context, key, value)
    current = getattr(context, "metadata", None)
    if isinstance(current, dict):
        current.update(metadata)
    elif hasattr(context, "metadata"):
        setattr(context, "metadata", metadata)
    return context


class VisInstalledAgent(BaseInstalledAgent):
    """Install a packaged Vis artifact and run it in Harbor's task workspace."""

    @staticmethod
    def name() -> str:
        return "vis-installed"

    def get_version_command(self) -> str:
        install_prefix = _env_value(self, "VIS_BENCH_INSTALL_PREFIX", DEFAULT_INSTALL) or DEFAULT_INSTALL
        remote_home = _env_value(self, "VIS_BENCH_REMOTE_HOME", DEFAULT_REMOTE_HOME) or DEFAULT_REMOTE_HOME
        return (
            f"HOME={shlex.quote(remote_home)} {shlex.quote(f'{install_prefix}/vis')} "
            f"-Duser.home={shlex.quote(remote_home)} --version"
        )

    def parse_version(self, stdout: str) -> str:
        return stdout.strip().splitlines()[-1].strip()

    async def install(self, environment: Any) -> None:

        artifact = _artifact_path()
        install_prefix = _env_value(self, "VIS_BENCH_INSTALL_PREFIX", DEFAULT_INSTALL) or DEFAULT_INSTALL
        remote_home = _env_value(self, "VIS_BENCH_REMOTE_HOME", DEFAULT_REMOTE_HOME) or DEFAULT_REMOTE_HOME
        vis_bin = f"{install_prefix}/vis"
        remote_artifact = "/tmp/vis-agent.tar.gz"
        remote_stage = "/tmp/vis-agent-install"

        await environment.upload_file(artifact, remote_artifact)
        script = " && ".join(
            [
                "set -euo pipefail",
                f"rm -rf {shlex.quote(remote_stage)}",
                f"mkdir -p {shlex.quote(remote_stage)}",
                f"tar -xzf {shlex.quote(remote_artifact)} -C {shlex.quote(remote_stage)}",
                f"{shlex.quote(remote_stage)}/vis-agent/install.sh {shlex.quote(install_prefix)}",
            ]
        )
        install_cmd = "bash -lc " + shlex.quote(script)
        proc = await environment.exec(command=install_cmd, timeout_sec=300, user="root")

        if _returncode(proc) != 0:
            raise RuntimeError(
                "failed to install Vis artifact "
                f"(exit {_returncode(proc)}): stdout={_result_text(proc, 'stdout')[-1000:]} "
                f"stderr={_result_text(proc, 'stderr')[-1000:]}"
            )

        config_path = _env_value(self, "VIS_BENCH_CONFIG")
        if config_path:
            local_config = Path(config_path).expanduser().resolve()
            if not local_config.exists():
                raise FileNotFoundError(f"VIS_BENCH_CONFIG points to missing file: {local_config}")
            remote_config = "/tmp/vis-config.edn"
            await environment.upload_file(local_config, remote_config)
            config_cmd = "bash -lc " + shlex.quote(
                " && ".join(
                    [
                        "set -euo pipefail",
                        f"mkdir -p {shlex.quote(remote_home)}/.vis",
                        f"cp {shlex.quote(remote_config)} {shlex.quote(remote_home)}/.vis/config.edn",
                        f"chmod 600 {shlex.quote(remote_home)}/.vis/config.edn",
                    ]
                )
            )
            cfg_proc = await environment.exec(command=config_cmd, timeout_sec=60)
            if _returncode(cfg_proc) != 0:
                raise RuntimeError(
                    "failed to install Vis config "
                    f"(exit {_returncode(cfg_proc)}): stdout={_result_text(cfg_proc, 'stdout')[-1000:]} "
                    f"stderr={_result_text(cfg_proc, 'stderr')[-1000:]}"
                )


        version = await _maybe_env_run(
            environment,
            f"HOME={shlex.quote(remote_home)} {shlex.quote(vis_bin)} "
            f"-Duser.home={shlex.quote(remote_home)} --version",
            timeout=30,
        )
        if _returncode(version) != 0:
            raise RuntimeError(f"vis --version failed after install: {_result_text(version, 'stderr')[-1000:]}")

        meta = {
            "artifact": str(artifact),
            "install_prefix": install_prefix,
            "remote_home": remote_home,
            "config_uploaded": bool(config_path),
            "vis_bin": vis_bin,
            "vis_version": (_result_text(version, "stdout") or _result_text(version, "stderr")).strip(),
        }
        Path(_env_value(self, "VIS_BENCH_INSTALL_METADATA", "/tmp/vis-bench-install.json") or "/tmp/vis-bench-install.json").write_text(json.dumps(meta, indent=2))

    @with_prompt_template
    async def run(self, instruction: str, environment: Any, context: Any) -> Any:
        task_id = _task_id_from_context(context, _env_value(self, "HARBOR_TASK_ID"))
        cwd = _workspace_from_context(context)
        artifacts = Path(_env_value(self, "HARBOR_ARTIFACTS_DIR", DEFAULT_ARTIFACTS) or DEFAULT_ARTIFACTS)
        trace_dir = artifacts / "vis-traces" / task_id
        trace = trace_dir / "vis.trace.jsonl"
        stderr = trace_dir / "vis.stderr.txt"

        install_prefix = _env_value(self, "VIS_BENCH_INSTALL_PREFIX", DEFAULT_INSTALL) or DEFAULT_INSTALL
        vis_bin = _env_value(self, "VIS_BENCH_VIS_BIN", f"{install_prefix}/vis") or f"{install_prefix}/vis"
        remote_home = _env_value(self, "VIS_BENCH_REMOTE_HOME", DEFAULT_REMOTE_HOME) or DEFAULT_REMOTE_HOME
        provider = _env_value(self, "VIS_PROVIDER", "zai-coding-plan") or "zai-coding-plan"
        model = _env_value(self, "VIS_MODEL", "glm-5.2") or "glm-5.2"
        reasoning_effort = _env_value(self, "VIS_BENCH_REASONING_EFFORT", "high") or "high"
        if reasoning_effort not in {"high", "max"}:
            raise ValueError(
                "VIS_BENCH_REASONING_EFFORT must be high or max; "
                f"got {reasoning_effort!r}"
            )
        timeout = float(_env_value(self, "VIS_BENCH_TIMEOUT", "7200") or "7200")
        cmd = [
            vis_bin,
            f"-Duser.home={remote_home}",
            "--db", ":memory",
            "--full-trace-json-stream",
            "--provider", provider,
            "--model", model,
            "--reasoning-effort", reasoning_effort,
            instruction,
        ]
        project_config = f"{cwd}/.vis/config.edn"
        home_config = f"{remote_home}/.vis/config.edn"
        shell = (
            f"mkdir -p {shlex.quote(str(trace_dir))} {shlex.quote(f'{cwd}/.vis')} && "
            f"if [ -f {shlex.quote(home_config)} ]; then cp {shlex.quote(home_config)} {shlex.quote(project_config)}; fi && "
            f"HOME={shlex.quote(remote_home)} "
            + " ".join(shlex.quote(x) for x in cmd)
            + f" > {shlex.quote(str(trace))} 2> {shlex.quote(str(stderr))}"
        )
        result = await _maybe_env_run(environment, shell, cwd=cwd, timeout=timeout)
        local_trace = Path(self.logs_dir) / f"vis-{task_id}.trace.jsonl"
        download_file = getattr(environment, "download_file", None)
        if callable(download_file):
            try:
                downloaded = download_file(str(trace), local_trace)
                if asyncio.iscoroutine(downloaded):
                    await downloaded
                stats = _summarize_trace(local_trace)
                stats.setdefault("reasoning_effort", reasoning_effort)
                stats["reasoning_effort_explicit"] = True
                _populate_context(context, stats)
            except Exception:
                # Harbor still retains the remote artifact; metrics will mark the
                # rollout trace unavailable instead of manufacturing zeroes.
                pass
        if _returncode(result) != 0:
            raise RuntimeError(f"vis exited {_returncode(result)}; stderr artifact: {stderr}")
        return result

    def populate_context_post_run(self, context: Any) -> Any:
        task_id = _task_id_from_context(context, _env_value(self, "HARBOR_TASK_ID"))
        local_trace = Path(self.logs_dir) / f"vis-{task_id}.trace.jsonl"
        trace = local_trace if local_trace.exists() else Path(_env_value(self, "HARBOR_ARTIFACTS_DIR", DEFAULT_ARTIFACTS) or DEFAULT_ARTIFACTS) / "vis-traces" / task_id / "vis.trace.jsonl"
        stats = _summarize_trace(trace)
        stats.setdefault(
            "reasoning_effort",
            _env_value(self, "VIS_BENCH_REASONING_EFFORT", "high") or "high",
        )
        stats["reasoning_effort_explicit"] = True
        return _populate_context(context, stats)


class PiGlm52Agent(HarborPi):
    """Pinned Pi harness that installs the benchmark's explicit GLM-5.2 model."""

    async def install(self, environment: Any) -> None:
        await super().install(environment)
        config_value = _env_value(self, PI_MODELS_ENV)
        if not config_value:
            raise FileNotFoundError(f"{PI_MODELS_ENV} is not set")
        config = Path(config_value).expanduser().resolve()
        if not config.is_file():
            raise FileNotFoundError(f"{PI_MODELS_ENV} points to missing file: {config}")

        await environment.upload_file(config, PI_MODELS_REMOTE)
        proc = await self.exec_as_agent(
            environment,
            command=(
                "set -euo pipefail; "
                "mkdir -p \"$HOME/.pi/agent\"; "
                f"cp {shlex.quote(PI_MODELS_REMOTE)} \"$HOME/.pi/agent/models.json\"; "
                "chmod 600 \"$HOME/.pi/agent/models.json\""
            ),
        )
        if proc is not None and _returncode(proc) != 0:
            raise RuntimeError(
                "failed to install Pi GLM-5.2 models.json "
                f"(exit {_returncode(proc)}): {_result_text(proc, 'stderr')[-1000:]}"
            )
