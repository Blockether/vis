"""Offline Laya ModernBERT training bundles and explicit FP32 preparation.

This optional module remains lightweight until a trainer is constructed. Install
``vis-agent[decisions-training]`` and a platform-compatible offline wheelhouse
before calling the export/training operations.
"""

from __future__ import annotations

import hashlib
import json
import shutil
import tempfile
import zipfile
from dataclasses import dataclass
from importlib.resources import files
from pathlib import Path, PurePosixPath
from urllib.request import urlopen

from ._trainer import ModernBertTrainer, TrainingResult

_MAX_EXPANDED_BYTES = 3_000_000_000
_MAX_ARCHIVE_ENTRIES = 1024
_CHUNK = 1024 * 1024
_REQUIRED = {
    "model.safetensors",
    "rl_agent_config.json",
    "encoder/config.json",
    "tokenizer/tokenizer.json",
    "tokenizer/tokenizer_config.json",
}


def _manifest() -> list[dict]:
    bundled = files("blockether.vis.decisions").joinpath("data/models.json")
    if bundled.is_file():
        return json.loads(bundled.read_text(encoding="utf-8"))
    # The editable checkout uses the same source the wheel build includes.
    checkout = (
        Path(__file__).resolve().parents[6] / "resources/vis-models/decisions.json"
    )
    if checkout.is_file():
        return json.loads(checkout.read_text(encoding="utf-8"))
    raise FileNotFoundError("Pinned decision model catalog is missing from the SDK")


def _safe_name(name: str) -> PurePosixPath:
    if (
        not isinstance(name, str)
        or not name
        or "\\" in name
        or "\x00" in name
        or name.startswith("/")
        or any(part in {"", ".", ".."} for part in name.split("/"))
    ):
        raise ValueError(f"Unsafe training bundle path: {name!r}")
    return PurePosixPath(name)


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(_CHUNK), b""):
            digest.update(chunk)
    return digest.hexdigest()


@dataclass(frozen=True)
class TrainingBundle:
    """Complete local checkpoint with both decision heads, not an ONNX graph."""

    path: Path

    @classmethod
    def open(cls, path: str | Path) -> TrainingBundle:
        """Validate provenance, file digests, tokenizer and ModernBERT architecture."""
        root = Path(path).expanduser().resolve()
        if not root.is_dir():
            raise FileNotFoundError(f"Training checkpoint does not exist: {root}")
        source = root / "PROVENANCE.json"
        if not source.is_file() or source.is_symlink():
            raise FileNotFoundError(f"Training provenance is missing: {source}")
        provenance = json.loads(source.read_text(encoding="utf-8"))
        if (
            provenance.get("kind") != "training"
            or provenance.get("format") != "safetensors"
        ):
            raise ValueError(
                "Only a complete safetensors training checkpoint is supported"
            )
        declared = provenance.get("files")
        if not isinstance(declared, dict) or not _REQUIRED <= declared.keys():
            raise ValueError(
                "Training checkpoint is missing required weights or configuration"
            )
        actual = set()
        for path_ in root.rglob("*"):
            if path_.is_symlink():
                raise ValueError("Training checkpoint contains a symlink")
            if path_.is_file():
                actual.add(path_.relative_to(root).as_posix())
        if (
            actual - {"PROVENANCE.json", "LICENSE.txt", ".vis-verified"}
            != declared.keys()
        ):
            raise ValueError("Training checkpoint has untracked or missing files")
        for name, metadata in declared.items():
            target = root.joinpath(*_safe_name(name).parts)
            if not target.is_file() or target.is_symlink():
                raise FileNotFoundError(f"Training checkpoint file is missing: {name}")
            if (
                not isinstance(metadata, dict)
                or metadata.get("bytes") != target.stat().st_size
                or metadata.get("sha256") != _sha256(target)
            ):
                raise ValueError(f"Training checkpoint checksum failed: {name}")
        encoder = json.loads((root / "encoder/config.json").read_text(encoding="utf-8"))
        cfg = json.loads((root / "rl_agent_config.json").read_text(encoding="utf-8"))
        if (
            encoder.get("model_type") != "modernbert"
            or not isinstance(cfg.get("head_layers"), int)
            or cfg["head_layers"] < 1
        ):
            raise ValueError(
                "Only the complete Laya ModernBERT decision heads are supported"
            )
        return cls(root)

    @classmethod
    def fetch(cls, *, model_ref: str, cache_dir: str | Path) -> TrainingBundle:
        """Download exactly a catalog-pinned checkpoint once; reuse it offline.

        A failed checksum or unsafe archive never becomes an installed checkpoint.
        Network access occurs only on this explicit call when the bundle is absent.
        """
        catalog = next(
            (
                item
                for item in _manifest()
                if model_ref == f"{item['id']}@{item['revision']}"
            ),
            None,
        )
        if catalog is None:
            raise ValueError("A pinned catalog model_ref (<id>@<revision>) is required")
        artifact = catalog["artifacts"]["training"]
        target = (
            Path(cache_dir).expanduser().resolve()
            / catalog["id"]
            / catalog["revision"]
            / "training"
        )
        if target.exists():
            if (target / ".vis-verified").read_text().strip() != artifact["sha256"]:
                raise ValueError(
                    "Existing training checkpoint is not verified; choose a clean cache"
                )
            return cls.open(target)
        target.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(
            prefix=".training-fetch-", dir=target.parent
        ) as temporary:
            workspace = Path(temporary)
            archive_path = workspace / artifact["file"]
            size = 0
            digest = hashlib.sha256()
            with (
                urlopen(artifact["url"], timeout=60) as response,
                archive_path.open("wb") as output,
            ):
                for chunk in iter(lambda: response.read(_CHUNK), b""):
                    size += len(chunk)
                    if size > artifact["bytes"]:
                        raise ValueError(
                            "Training archive size exceeds the pinned catalog"
                        )
                    digest.update(chunk)
                    output.write(chunk)
            if size != artifact["bytes"] or digest.hexdigest() != artifact["sha256"]:
                raise ValueError(
                    "Training archive size or checksum disagrees with the pinned catalog"
                )
            staging = workspace / "bundle"
            staging.mkdir()
            with zipfile.ZipFile(archive_path) as archive:
                entries = archive.infolist()
                if (
                    len(entries) > _MAX_ARCHIVE_ENTRIES
                    or sum(info.file_size for info in entries) > _MAX_EXPANDED_BYTES
                ):
                    raise ValueError("Training archive exceeds the extraction limits")
                seen: set[str] = set()
                for info in entries:
                    name = info.filename
                    relative = _safe_name(name)
                    if (
                        name in seen
                        or info.is_dir()
                        or (info.external_attr >> 28) == 0o12
                    ):
                        raise ValueError("Unsafe training archive entry")
                    seen.add(name)
                    destination = staging.joinpath(*relative.parts)
                    destination.parent.mkdir(parents=True, exist_ok=True)
                    with archive.open(info) as source, destination.open("wb") as output:
                        shutil.copyfileobj(source, output, _CHUNK)
            if not set(artifact["requires"]) <= seen:
                raise ValueError("Training archive is missing required files")
            provenance = json.loads(
                (staging / "PROVENANCE.json").read_text(encoding="utf-8")
            )
            if (
                provenance.get("model") != catalog["id"]
                or provenance.get("revision") != catalog["revision"]
            ):
                raise ValueError(
                    "Training checkpoint identity disagrees with the pinned catalog"
                )
            cls.open(staging)
            (staging / ".vis-verified").write_text(artifact["sha256"] + "\n")
            staging.rename(target)
        return cls.open(target)


__all__ = ["ModernBertTrainer", "TrainingBundle", "TrainingResult"]
