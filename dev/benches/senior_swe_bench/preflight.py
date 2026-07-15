#!/usr/bin/env python3
"""Host-side preflight checks for Senior SWE-Bench Vis runs."""
from __future__ import annotations

import argparse
import hashlib
import json
import tarfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any


class PreflightError(Exception):
    pass


PYTHON_DEV_REQUIRED_TASKS = {
    "paperless-ngx-perf-document-counts": "verifier setup may build zxing-cpp, which needs Python3_INCLUDE_DIRS/Development.Module",
}
PI_GLM52_CONTEXT_WINDOW = 1_000_000
PI_GLM52_MAX_TOKENS = 32_768
PI_ZAI_BASE_URL = "https://api.z.ai/api/coding/paas/v4"


def pi_thinking_level(reasoning_effort: str) -> str:
    try:
        return {"high": "high", "max": "xhigh"}[reasoning_effort]
    except KeyError as exc:
        raise PreflightError(
            "VIS_BENCH_REASONING_EFFORT must be high or max; "
            f"got {reasoning_effort!r}"
        ) from exc


def pi_glm52_models_config() -> dict[str, Any]:
    """Return the pinned Pi 0.73.1 custom-model definition used by the eval."""
    return {
        "providers": {
            "zai": {
                "name": "Z.ai Coding Plan",
                "baseUrl": PI_ZAI_BASE_URL,
                "api": "openai-completions",
                "apiKey": "$ZAI_API_KEY",
                "models": [
                    {
                        "id": "glm-5.2",
                        "name": "GLM-5.2",
                        "reasoning": True,
                        "thinkingLevelMap": {
                            "off": None,
                            "minimal": None,
                            "low": None,
                            "medium": None,
                            "high": "high",
                            "xhigh": "max",
                        },
                        "input": ["text"],
                        "cost": {
                            "input": 0,
                            "output": 0,
                            "cacheRead": 0,
                            "cacheWrite": 0,
                        },
                        "contextWindow": PI_GLM52_CONTEXT_WINDOW,
                        "maxTokens": PI_GLM52_MAX_TOKENS,
                        "compat": {
                            "supportsDeveloperRole": False,
                            "supportsReasoningEffort": True,
                            "thinkingFormat": "deepseek",
                            "zaiToolStream": True,
                        },
                    }
                ],
            }
        }
    }


def write_pi_glm52_models_config(path: Path) -> dict[str, Any]:
    config = pi_glm52_models_config()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(config, indent=2, sort_keys=True) + "\n")
    return config


def sha256_and_head_stream(stream: Any, head_size: int = 64) -> tuple[str, bytes]:
    digest = hashlib.sha256()
    head = bytearray()
    for chunk in iter(lambda: stream.read(1024 * 1024), b""):
        if len(head) < head_size:
            head.extend(chunk[: head_size - len(head)])
        digest.update(chunk)
    return digest.hexdigest(), bytes(head)


def inspect_elf_header(head: bytes) -> dict[str, Any]:
    if not head.startswith(b"\x7fELF"):
        raise PreflightError("artifact native/vis is not an ELF binary")
    if len(head) < 20:
        raise PreflightError("artifact native/vis is too short to contain an ELF header")

    elf_class = head[4]
    data_encoding = head[5]
    if elf_class != 2:
        raise PreflightError(f"artifact native/vis must be ELF64, got class byte {elf_class}")
    if data_encoding != 1:
        raise PreflightError(f"artifact native/vis must be little-endian ELF, got data byte {data_encoding}")

    machine_id = int.from_bytes(head[18:20], "little")
    machine_names = {
        62: "x86_64",
        183: "aarch64",
    }
    machine = machine_names.get(machine_id, f"unknown-{machine_id}")
    if machine_id != 183:
        raise PreflightError(f"artifact native/vis must be Linux ARM64/aarch64 ELF, got {machine}")
    return {"class": "ELF64", "data": "little", "machine": machine, "machine_id": machine_id}


def resolve_python_dev_image_mode(task_id: str, requested: str, task_image: str, install_only: bool) -> str:
    mode = requested or "auto"
    if mode not in {"auto", "0", "1"}:
        raise PreflightError(f"VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE must be auto, 0, or 1; got {mode!r}")

    requires_python_dev = task_id in PYTHON_DEV_REQUIRED_TASKS
    if mode == "auto":
        return "1" if requires_python_dev and not install_only and not task_image else "0"
    if mode == "0" and requires_python_dev and not install_only and not task_image:
        reason = PYTHON_DEV_REQUIRED_TASKS[task_id]
        raise PreflightError(
            f"{task_id} requires a python-dev verifier image ({reason}). "
            "Set VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE=1 or VIS_BENCH_TASK_IMAGE=<prebuilt-image>."
        )
    return mode


def resolve_config_delivery(config_path: str | Path | None, remote_home_mount: str | Path | None) -> str:
    if config_path:
        return "upload"
    if remote_home_mount and (Path(remote_home_mount) / ".vis" / "config.edn").is_file():
        return "mounted-home"
    return "none"


def validate_config_mount_combination(
    config_path: str | Path | None,
    remote_home_mount: str | Path | None,
    *,
    allow_config_with_mount: bool = False,
) -> None:
    if config_path and remote_home_mount and not allow_config_with_mount:
        raise PreflightError(
            "VIS_BENCH_CONFIG and VIS_BENCH_REMOTE_HOME_MOUNT are both set. "
            "Use VIS_BENCH_CONFIG by itself for config upload, or unset VIS_BENCH_CONFIG to bind-mount a remote home. "
            "Set VIS_BENCH_ALLOW_CONFIG_UPLOAD_WITH_HOME_MOUNT=1 only if the extra host-home bind mount is intentional."
        )


def build_remote_home_mounts(source: str | Path | None, target: str, mode: str) -> list[dict[str, Any]]:
    if not source:
        return []
    if mode not in {"ro", "rw"}:
        raise PreflightError(f"VIS_BENCH_REMOTE_HOME_MOUNT_MODE must be 'ro' or 'rw', got {mode!r}")
    if mode == "ro":
        raise PreflightError(
            "VIS_BENCH_REMOTE_HOME_MOUNT_MODE=ro is not supported when the mount target is VIS_BENCH_REMOTE_HOME; "
            "Vis writes ~/.vis/vis.log during startup. Use rw."
        )

    source_path = Path(source)
    if not source_path.is_dir():
        raise PreflightError(f"VIS_BENCH_REMOTE_HOME_MOUNT must be a directory, got {source_path}")
    return [{"type": "bind", "source": str(source_path), "target": target}]


def validate_credential_source(config_path: Path | None, *, required: bool, allow_missing: bool = False) -> dict[str, Any]:
    if config_path:
        return {"required": required, "config_path": str(config_path), "allow_missing": allow_missing}
    if not required:
        return {"required": False, "config_path": None, "allow_missing": allow_missing}
    if allow_missing:
        return {
            "required": True,
            "config_path": None,
            "allow_missing": True,
            "warning": "full eval has no host-side provider config source; assuming the task container already has credentials",
        }
    raise PreflightError(
        "full eval requires provider credentials. Set VIS_BENCH_CONFIG=<config.edn> "
        "or VIS_BENCH_REMOTE_HOME_MOUNT=<dir-containing-.vis/config.edn>. "
        "Set VIS_BENCH_ALLOW_NO_CONFIG=1 only if the task container is already provisioned."
    )


def validate_artifact(path: Path) -> dict[str, Any]:
    if not path.exists():
        raise PreflightError(f"missing artifact: {path}")
    try:
        with tarfile.open(path, "r:gz") as tf:
            meta_member = tf.getmember("vis-agent/metadata.json")
            meta = json.loads(tf.extractfile(meta_member).read())
            if meta.get("mode") != "native":
                raise PreflightError(f"artifact mode must be native, got {meta.get('mode')!r}")
            native_file = str(meta.get("native_file") or "")
            if "ELF" not in native_file or "Linux" not in native_file:
                raise PreflightError("artifact metadata does not describe a Linux ELF native binary")
            install_member = tf.getmember("vis-agent/install.sh")
            if not install_member.isfile():
                raise PreflightError("artifact vis-agent/install.sh must be a file")
            if not (install_member.mode & 0o111):
                raise PreflightError("artifact vis-agent/install.sh must be executable")
            native_member = tf.getmember("vis-agent/native/vis")
            if not native_member.isfile():
                raise PreflightError("artifact vis-agent/native/vis must be a file")
            if not (native_member.mode & 0o111):
                raise PreflightError("artifact vis-agent/native/vis must be executable")
            with tf.extractfile(native_member) as native_stream:
                native_sha256, native_head = sha256_and_head_stream(native_stream)
            native_elf = inspect_elf_header(native_head)
    except (tarfile.TarError, KeyError, json.JSONDecodeError, AttributeError) as exc:
        raise PreflightError(f"invalid vis artifact {path}: {exc}") from exc

    expected_sha = meta.get("native_sha256")
    if expected_sha and expected_sha != native_sha256:
        raise PreflightError("artifact native_sha256 does not match vis-agent/native/vis")
    return {
        "path": str(path),
        "mode": meta.get("mode"),
        "vis_revision": meta.get("vis_revision"),
        "source_dirty": meta.get("source_dirty"),
        "source_status_count": meta.get("source_status_count"),
        "native_sha256": native_sha256,
        "native_elf": native_elf,
        "install_mode": oct(install_member.mode & 0o777),
        "native_mode": oct(native_member.mode & 0o777),
    }


@dataclass
class Token:
    kind: str
    value: str
    pos: int


class EdnParser:
    def __init__(self, text: str):
        self.tokens = self._tokenize(text)
        self.pos = 0

    @staticmethod
    def _tokenize(text: str) -> list[Token]:
        tokens: list[Token] = []
        i = 0
        while i < len(text):
            ch = text[i]
            if ch.isspace() or ch == ",":
                i += 1
                continue
            if ch == ";":
                while i < len(text) and text[i] != "\n":
                    i += 1
                continue
            if ch in "{}[]":
                tokens.append(Token(ch, ch, i))
                i += 1
                continue
            if ch == '"':
                start = i
                i += 1
                out = []
                while i < len(text):
                    c = text[i]
                    if c == "\\":
                        if i + 1 >= len(text):
                            raise PreflightError(f"unterminated string escape at byte {i}")
                        out.append(text[i + 1])
                        i += 2
                        continue
                    if c == '"':
                        i += 1
                        tokens.append(Token("string", "".join(out), start))
                        break
                    out.append(c)
                    i += 1
                else:
                    raise PreflightError(f"unterminated string at byte {start}")
                continue
            start = i
            while i < len(text) and (not text[i].isspace()) and text[i] not in '{}[]",;':
                i += 1
            tokens.append(Token("atom", text[start:i], start))
        return tokens

    def parse(self) -> Any:
        value = self._parse_value()
        if self.pos != len(self.tokens):
            token = self.tokens[self.pos]
            raise PreflightError(f"unexpected token {token.value!r} at byte {token.pos}")
        return value

    def _pop(self) -> Token:
        if self.pos >= len(self.tokens):
            raise PreflightError("unexpected end of EDN")
        token = self.tokens[self.pos]
        self.pos += 1
        return token

    def _parse_value(self) -> Any:
        token = self._pop()
        if token.kind == "string":
            return token.value
        if token.kind == "atom":
            if token.value == "#" and self.pos < len(self.tokens) and self.tokens[self.pos].kind == "{":
                self._pop()
                return self._parse_set(token)
            return self._parse_atom(token)
        if token.kind == "[":
            return self._parse_vector(token)
        if token.kind == "{":
            return self._parse_map(token)
        raise PreflightError(f"unexpected token {token.value!r} at byte {token.pos}")

    def _parse_atom(self, token: Token) -> Any:
        value = token.value
        if value in {"nil", "null"}:
            return None
        if value == "true":
            return True
        if value == "false":
            return False
        if value.startswith("#"):
            # The preflight parser only needs enough EDN to validate the
            # top-level :providers vector. Preserve common tagged literals by
            # consuming their following value instead of rejecting otherwise
            # valid config files that contain e.g. sets or extension tags in
            # unrelated keys.
            return self._parse_value()
        if value.startswith(":"):
            return value
        try:
            return int(value)
        except ValueError:
            pass
        try:
            return float(value)
        except ValueError:
            return value

    def _parse_vector(self, start: Token) -> list[Any]:
        out = []
        while True:
            if self.pos >= len(self.tokens):
                raise PreflightError(f"unterminated vector starting at byte {start.pos}")
            if self.tokens[self.pos].kind == "]":
                self.pos += 1
                return out
            out.append(self._parse_value())

    def _parse_set(self, start: Token) -> list[Any]:
        out = []
        while True:
            if self.pos >= len(self.tokens):
                raise PreflightError(f"unterminated set starting at byte {start.pos}")
            if self.tokens[self.pos].kind == "}":
                self.pos += 1
                return out
            out.append(self._parse_value())

    def _parse_map(self, start: Token) -> dict[Any, Any]:
        out: dict[Any, Any] = {}
        while True:
            if self.pos >= len(self.tokens):
                raise PreflightError(f"unterminated map starting at byte {start.pos}")
            if self.tokens[self.pos].kind == "}":
                self.pos += 1
                return out
            key = self._parse_value()
            if self.pos >= len(self.tokens) or self.tokens[self.pos].kind == "}":
                raise PreflightError(f"map has odd number of forms near byte {start.pos}")
            out[key] = self._parse_value()


def parse_edn(text: str) -> Any:
    return EdnParser(text).parse()


def _get(mapping: dict[Any, Any], key: str) -> Any:
    return mapping.get(key)


def validate_config(path: Path) -> dict[str, Any]:
    if not path.exists():
        raise PreflightError(f"missing config: {path}")
    try:
        parsed = parse_edn(path.read_text())
    except OSError as exc:
        raise PreflightError(f"cannot read config {path}: {exc}") from exc
    if not isinstance(parsed, dict):
        raise PreflightError("config.edn must be a top-level map")
    providers = _get(parsed, ":providers")
    if providers is None:
        return {
            "path": str(path),
            "providers": 0,
            "provider_ids": [],
            "provider_configs": [],
            "warning": "no :providers key",
        }
    if not isinstance(providers, list):
        raise PreflightError(":providers must be a vector of provider maps")
    provider_ids: list[str] = []
    provider_configs: list[dict[str, Any]] = []
    for idx, provider in enumerate(providers):
        if not isinstance(provider, dict):
            raise PreflightError(f":providers entry {idx} must be a map, got {type(provider).__name__}")
        provider_id = _get(provider, ":id")
        if not isinstance(provider_id, str) or not provider_id:
            raise PreflightError(f":providers entry {idx} is missing keyword/string :id")
        normalized_id = provider_id[1:] if provider_id.startswith(":") else provider_id
        provider_ids.append(normalized_id)
        provider_configs.append({"id": normalized_id, "base_url": _get(provider, ":base-url")})
    return {
        "path": str(path),
        "providers": len(providers),
        "provider_ids": provider_ids,
        "provider_configs": provider_configs,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--artifact", type=Path, required=True)
    parser.add_argument("--config", type=Path)
    parser.add_argument("--require-config", action="store_true")
    parser.add_argument("--allow-no-config", action="store_true")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    try:
        result: dict[str, Any] = {"artifact": validate_artifact(args.artifact)}
        result["credentials"] = validate_credential_source(
            args.config,
            required=args.require_config,
            allow_missing=args.allow_no_config,
        )
        if args.config:
            result["config"] = validate_config(args.config)
        if args.json:
            print(json.dumps(result, indent=2, sort_keys=True))
        else:
            print("preflight ok")
        return 0
    except PreflightError as exc:
        if args.json:
            print(json.dumps({"error": str(exc)}, indent=2, sort_keys=True))
        else:
            print(f"preflight failed: {exc}")
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
