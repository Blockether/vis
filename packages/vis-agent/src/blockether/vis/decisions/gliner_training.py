"""Explicit offline GLiNER2.5 checkpoints and two-head CPU training.

Importing this module is lightweight. Use a separately installed
``vis-agent[decisions-gliner-training]`` environment to construct a trainer;
Laya's Transformers 5 environment is not compatible with GLiNER's version 4.
"""

from __future__ import annotations

import json
import re
import shutil
import tempfile
from dataclasses import dataclass
from pathlib import Path
from urllib.request import urlopen

from ._models import ARCHITECTURES
from ._trainer import TrainingResult
from .training import TrainingBundle, _manifest, _safe_name, _sha256

_REQUIRED = {
    "config.json",
    "encoder_config/config.json",
    "model.safetensors",
    "tokenizer.json",
    "tokenizer_config.json",
}
_OPTIONAL = {"special_tokens_map.json"}
_MAX_EXPANDED_BYTES = 3_000_000_000


@dataclass(frozen=True)
class GlinerTrainingBundle(TrainingBundle):
    """A verified full FP32 checkpoint, never the encoder-only ONNX graph."""

    @property
    def model_id(self) -> str:
        return json.loads((self.path / "PROVENANCE.json").read_text(encoding="utf-8"))[
            "model"
        ]

    @classmethod
    def _catalog(cls) -> list[dict]:
        return _manifest()

    @classmethod
    def _open_url(cls, url: str):
        return urlopen(url, timeout=60)

    @classmethod
    def open(cls, path: str | Path) -> GlinerTrainingBundle:
        """Check every file and its digest without loading Torch or accessing the network."""
        root = Path(path).expanduser().resolve()
        provenance_path = root / "PROVENANCE.json"
        license_path = root / "LICENSE.txt"
        if (
            not root.is_dir()
            or not provenance_path.is_file()
            or provenance_path.is_symlink()
            or provenance_path.stat().st_size > 1_048_576
            or not license_path.is_file()
            or license_path.is_symlink()
        ):
            raise FileNotFoundError(
                "Complete GLiNER checkpoint provenance and license required"
            )
        metadata = json.loads(provenance_path.read_text(encoding="utf-8"))
        model_id = metadata.get("model")
        revision = metadata.get("revision")
        if (
            metadata.get("schema_version") != 1
            or metadata.get("kind") != "training"
            or metadata.get("format") != "safetensors"
            or metadata.get("family") != "gliner2.5"
            or metadata.get("license") != "Apache-2.0"
            or model_id not in ARCHITECTURES
            or metadata.get("architecture") != ARCHITECTURES[model_id]
            or not isinstance(revision, str)
            or not re.fullmatch(r"(?:[0-9a-f]{40}|[0-9a-f]{64})", revision)
            or (
                "parent_revision" in metadata
                and (
                    not isinstance(metadata["parent_revision"], str)
                    or not re.fullmatch(
                        r"(?:[0-9a-f]{40}|[0-9a-f]{64})", metadata["parent_revision"]
                    )
                )
            )
        ):
            raise ValueError("Unsupported GLiNER checkpoint identity or architecture")
        declared = metadata.get("files")
        if not isinstance(declared, dict) or not _REQUIRED <= declared.keys():
            raise ValueError(
                "GLiNER checkpoint is missing complete weights or tokenizer"
            )
        if set(declared) - (_REQUIRED | _OPTIONAL):
            raise ValueError("GLiNER checkpoint contains unsupported training files")
        actual: set[str] = set()
        for item in root.rglob("*"):
            if item.is_symlink():
                raise ValueError("GLiNER checkpoint contains a symlink")
            if item.is_file():
                actual.add(item.relative_to(root).as_posix())
        if actual - {"PROVENANCE.json", "LICENSE.txt", ".vis-verified"} != set(
            declared
        ):
            raise ValueError("GLiNER checkpoint has untracked or missing files")
        size = 0
        for name, detail in declared.items():
            target = root.joinpath(*_safe_name(name).parts)
            if (
                not target.is_file()
                or target.is_symlink()
                or not isinstance(detail, dict)
            ):
                raise ValueError("GLiNER checkpoint inventory is invalid")
            size += target.stat().st_size
            if (
                size > _MAX_EXPANDED_BYTES
                or detail.get("bytes") != target.stat().st_size
                or detail.get("sha256") != _sha256(target)
            ):
                raise ValueError(f"GLiNER checkpoint checksum failed: {name}")
        config = json.loads((root / "config.json").read_text(encoding="utf-8"))
        encoder = json.loads(
            (root / "encoder_config/config.json").read_text(encoding="utf-8")
        )
        if (
            config.get("architecture") != ARCHITECTURES[model_id]
            or encoder.get("model_type") != "deberta-v2"
        ):
            raise ValueError(
                "GLiNER checkpoint config has an incompatible architecture"
            )
        return cls(root)

    @classmethod
    def from_local(
        cls,
        source: str | Path,
        destination: str | Path,
        *,
        model_id: str,
        revision: str,
        license_file: str | Path,
        parent_revision: str | None = None,
    ) -> GlinerTrainingBundle:
        """Atomically inventory an explicit local full checkpoint without downloading."""
        if model_id not in ARCHITECTURES:
            raise ValueError("Unsupported GLiNER checkpoint model")
        if not isinstance(revision, str) or not re.fullmatch(
            r"(?:[0-9a-f]{40}|[0-9a-f]{64})", revision
        ):
            raise ValueError("A pinned checkpoint revision is required")
        source = Path(source).expanduser()
        destination = Path(destination).expanduser().resolve()
        license_file = Path(license_file).expanduser().resolve()
        if destination.exists():
            raise FileExistsError(destination)
        if source.is_symlink() or not source.is_dir() or not license_file.is_file():
            raise FileNotFoundError(
                "Local full checkpoint and Apache-2.0 license required"
            )
        if any(
            not (source / name).is_file() or (source / name).is_symlink()
            for name in _REQUIRED
        ):
            raise FileNotFoundError("Complete GLiNER weights and tokenizer required")
        if (
            json.loads((source / "config.json").read_text(encoding="utf-8")).get(
                "architecture"
            )
            != ARCHITECTURES[model_id]
        ):
            raise ValueError("GLiNER checkpoint architecture does not match model")
        if parent_revision is not None and (
            not isinstance(parent_revision, str)
            or not re.fullmatch(r"(?:[0-9a-f]{40}|[0-9a-f]{64})", parent_revision)
        ):
            raise ValueError("Invalid parent checkpoint revision")
        destination.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(
            prefix=".gliner-checkpoint-", dir=destination.parent
        ) as temporary:
            staging = Path(temporary) / "checkpoint"
            staging.mkdir()
            for name in sorted(_REQUIRED | _OPTIONAL):
                original = source / name
                if name in _OPTIONAL and not original.exists():
                    continue
                if original.is_symlink() or not original.is_file():
                    raise ValueError("GLiNER checkpoint contains an invalid tokenizer")
                copied = staging / name
                copied.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(original, copied)
            shutil.copyfile(license_file, staging / "LICENSE.txt")
            files = {
                item.relative_to(staging).as_posix(): {
                    "bytes": item.stat().st_size,
                    "sha256": _sha256(item),
                }
                for item in sorted(staging.rglob("*"))
                if item.is_file() and item.name != "LICENSE.txt"
            }
            metadata = {
                "schema_version": 1,
                "kind": "training",
                "format": "safetensors",
                "family": "gliner2.5",
                "architecture": ARCHITECTURES[model_id],
                "model": model_id,
                "revision": revision,
                "license": "Apache-2.0",
                "files": files,
            }
            if parent_revision is not None:
                metadata["parent_revision"] = parent_revision
            (staging / "PROVENANCE.json").write_text(
                json.dumps(metadata, indent=2) + "\n"
            )
            cls.open(staging)
            staging.rename(destination)
        return cls.open(destination)


from ._gliner_trainer import GlinerTrainer  # noqa: E402

__all__ = ["GlinerTrainer", "GlinerTrainingBundle", "TrainingResult"]
