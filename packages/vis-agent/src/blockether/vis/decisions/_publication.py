"""Inference-only decision archives for an explicit, authenticated upload."""

from __future__ import annotations

import hashlib
import json
import re
import zipfile
from collections.abc import Callable
from pathlib import Path
from typing import BinaryIO

from ._models import ARCHITECTURES

_REQUIRED_LAYA = {
    "model.onnx",
    "rl_agent_config.json",
    "tokenizer/tokenizer.json",
    "tokenizer/tokenizer_config.json",
}
_REQUIRED_GLINER = {
    "model.onnx",
    "config.json",
    "encoder_config/config.json",
    "tokenizer/tokenizer.json",
    "tokenizer/tokenizer_config.json",
}
_MAX_EXPANDED = 3_000_000_000
_MAX_ARCHIVE = 2_400_000_000
_CHUNK = 1024 * 1024


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(_CHUNK), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _source(bundle: Path) -> list[Path]:
    if not bundle.is_dir() or bundle.is_symlink():
        raise FileNotFoundError("Decision inference bundle is missing")
    source = bundle / "PROVENANCE.json"
    license_file = bundle / "LICENSE.txt"
    if (
        not source.is_file()
        or source.is_symlink()
        or source.stat().st_size > 1_048_576
        or not license_file.is_file()
        or license_file.is_symlink()
    ):
        raise ValueError("Decision inference provenance or license is missing")
    provenance = json.loads(source.read_text(encoding="utf-8"))
    model_id = provenance.get("model")
    gliner = isinstance(model_id, str) and model_id in ARCHITECTURES
    if (
        not isinstance(model_id, str)
        or model_id not in {"laya-typed-decisions", *ARCHITECTURES}
        or provenance.get("kind") != "inference"
        or provenance.get("format") != "onnx"
        or provenance.get("precision") != "fp32"
        or provenance.get("license") != "Apache-2.0"
        or not isinstance(provenance.get("revision"), str)
        or not re.fullmatch(r"(?:[0-9a-f]{40}|[0-9a-f]{64})", provenance["revision"])
        or (not gliner and provenance.get("family") is not None)
    ):
        raise ValueError(
            "Only a complete supported FP32 inference bundle can be uploaded"
        )
    if gliner and (
        provenance.get("family") != "gliner2.5"
        or provenance.get("architecture") != ARCHITECTURES[model_id]
    ):
        raise ValueError("GLiNER bundle provenance has the wrong architecture")
    entries = provenance.get("files")
    required = _REQUIRED_GLINER if gliner else _REQUIRED_LAYA
    if not isinstance(entries, dict) or not required <= entries.keys():
        raise ValueError("Decision bundle is missing its inference inventory")
    if not all(
        name
        in {
            "model.onnx",
            "model.onnx.data",
            "config.json" if gliner else "rl_agent_config.json",
        }
        or (gliner and name == "encoder_config/config.json")
        or re.fullmatch(r"tokenizer/[A-Za-z0-9_.-]+\.(?:json|txt)", name)
        for name in entries
    ):
        raise ValueError("Decision bundle contains non-inference files")
    if gliner:
        config = json.loads((bundle / "config.json").read_text(encoding="utf-8"))
        if config.get("architecture") != ARCHITECTURES[model_id]:
            raise ValueError("GLiNER bundle config has the wrong architecture")
    paths = []
    expanded = 0
    for path in bundle.rglob("*"):
        if path.is_symlink():
            raise ValueError("Decision bundle contains a symlink")
        if path.is_file():
            paths.append(path)
    actual = {path.relative_to(bundle).as_posix() for path in paths}
    if actual - {".vis-verified"} != set(entries) | {"PROVENANCE.json", "LICENSE.txt"}:
        raise ValueError("Decision bundle contains untracked or missing files")
    for name, detail in entries.items():
        path = bundle / name
        if not isinstance(detail, dict) or not path.is_file() or path.is_symlink():
            raise ValueError("Decision bundle file inventory is invalid")
        expanded += path.stat().st_size
        if (
            expanded > _MAX_EXPANDED
            or detail.get("bytes") != path.stat().st_size
            or detail.get("sha256") != _sha256(path)
        ):
            raise ValueError("Decision bundle file checksum failed")
    return sorted(path for path in paths if path.name != ".vis-verified")


def package(bundle: Path, archive: Path) -> tuple[str, int]:
    """Write only inventoried FP32 inference files; never include checkpoints/data."""
    paths = _source(bundle)
    with zipfile.ZipFile(archive, "w", allowZip64=True) as zipped:
        for path in paths:
            name = path.relative_to(bundle).as_posix()
            entry = zipfile.ZipInfo(name, date_time=(1980, 1, 1, 0, 0, 0))
            entry.compress_type = zipfile.ZIP_DEFLATED
            entry.external_attr = 0o600 << 16
            with (
                path.open("rb") as source,
                zipped.open(entry, "w", force_zip64=True) as target,
            ):
                for chunk in iter(lambda: source.read(_CHUNK), b""):
                    target.write(chunk)
    size = archive.stat().st_size
    if not 0 < size <= _MAX_ARCHIVE:
        raise ValueError("Decision inference archive exceeds the gateway upload limit")
    return _sha256(archive), size


class ProgressReader:
    """Bounded HTTP read() wrapper; report actual bytes written, not guessed progress."""

    def __init__(
        self, source: BinaryIO, size: int, callback: Callable[[int, int], None] | None
    ) -> None:
        self.source = source
        self.size = size
        self.callback = callback
        self.sent = 0

    def read(self, length: int = -1) -> bytes:
        if length == 0:
            return b""
        content = self.source.read(min(length, 65536) if length > 0 else 65536)
        self.sent += len(content)
        if self.callback:
            self.callback(self.sent, self.size)
        return content
