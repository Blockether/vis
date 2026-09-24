"""Pinned training bundles can be used offline without importing tensor libraries."""

import hashlib
import io
import json
import sys
import zipfile
from pathlib import Path

import pytest
from blockether.vis.decisions.training import TrainingBundle


def checkpoint(root: Path) -> Path:
    root.mkdir(parents=True)
    files = {
        "model.safetensors": b"placeholder weights",
        "encoder/config.json": json.dumps({"model_type": "modernbert"}).encode(),
        "rl_agent_config.json": json.dumps({"head_layers": 2}).encode(),
        "tokenizer/tokenizer.json": b"{}",
        "tokenizer/tokenizer_config.json": b"{}",
    }
    for name, content in files.items():
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
    (root / "LICENSE.txt").write_text("Apache-2.0")
    provenance = {
        "schema_version": 1,
        "model": "laya-typed-decisions",
        "revision": "pinned",
        "kind": "training",
        "format": "safetensors",
        "files": {
            name: {"bytes": len(content), "sha256": hashlib.sha256(content).hexdigest()}
            for name, content in files.items()
        },
    }
    (root / "PROVENANCE.json").write_text(json.dumps(provenance))
    return root


def archive_bytes(root: Path) -> bytes:
    output = io.BytesIO()
    with zipfile.ZipFile(output, "w") as archive:
        for path in sorted(root.rglob("*")):
            if path.is_file():
                archive.write(path, path.relative_to(root).as_posix())
    return output.getvalue()


def test_open_rejects_missing_incompatible_and_tampered_checkpoints(tmp_path):
    root = checkpoint(tmp_path / "checkpoint")
    assert TrainingBundle.open(root).path == root.resolve()
    (root / "encoder/config.json").write_text('{"model_type":"bert"}')
    with pytest.raises(ValueError, match="(checksum|ModernBERT)"):
        TrainingBundle.open(root)
    (root / "encoder/config.json").unlink()
    with pytest.raises((FileNotFoundError, ValueError)):
        TrainingBundle.open(root)
    with pytest.raises((FileNotFoundError, ValueError)):
        TrainingBundle.open(tmp_path / "model.onnx")


def test_fetch_is_pinned_streamed_verified_atomic_and_cached(tmp_path, monkeypatch):
    import blockether.vis.decisions.training as training

    source = checkpoint(tmp_path / "source")
    data = archive_bytes(source)
    entry = {
        "id": "laya-typed-decisions",
        "revision": "pinned",
        "artifacts": {
            "training": {
                "file": "training.zip",
                "url": "https://github.com/Blockether/vis/releases/download/assets-pack/training.zip",
                "bytes": len(data),
                "sha256": hashlib.sha256(data).hexdigest(),
                "requires": [
                    "model.safetensors",
                    "encoder/config.json",
                    "rl_agent_config.json",
                    "tokenizer/tokenizer.json",
                    "tokenizer/tokenizer_config.json",
                    "PROVENANCE.json",
                    "LICENSE.txt",
                ],
            }
        },
    }
    monkeypatch.setattr(training, "_manifest", lambda: [entry])
    calls = []

    def open_url(url, *, timeout):
        calls.append(url)
        return io.BytesIO(data)

    monkeypatch.setattr(training, "urlopen", open_url)
    with pytest.raises(ValueError, match="pinned"):
        TrainingBundle.fetch(
            model_ref="laya-typed-decisions", cache_dir=tmp_path / "cache"
        )
    bundle = TrainingBundle.fetch(
        model_ref="laya-typed-decisions@pinned", cache_dir=tmp_path / "cache"
    )
    assert bundle.path.name == "training"
    assert len(calls) == 1
    assert (
        TrainingBundle.fetch(
            model_ref="laya-typed-decisions@pinned", cache_dir=tmp_path / "cache"
        ).path
        == bundle.path
    )
    assert len(calls) == 1
    assert "torch" not in sys.modules


def test_fetch_rejects_corrupt_archive_and_unsafe_paths(tmp_path, monkeypatch):
    import blockether.vis.decisions.training as training

    source = checkpoint(tmp_path / "source")
    data = archive_bytes(source)
    entry = {
        "id": "laya-typed-decisions",
        "revision": "pinned",
        "artifacts": {
            "training": {
                "file": "training.zip",
                "url": "https://github.com/Blockether/vis/releases/download/assets-pack/training.zip",
                "bytes": len(data),
                "sha256": hashlib.sha256(data).hexdigest(),
                "requires": [],
            }
        },
    }
    monkeypatch.setattr(training, "_manifest", lambda: [entry])
    monkeypatch.setattr(
        training, "urlopen", lambda url, *, timeout: io.BytesIO(b"invalid")
    )
    with pytest.raises(ValueError, match="(size|checksum)"):
        TrainingBundle.fetch(
            model_ref="laya-typed-decisions@pinned", cache_dir=tmp_path / "cache"
        )
    assert not (tmp_path / "cache/laya-typed-decisions/pinned/training").exists()

    malicious = io.BytesIO()
    with zipfile.ZipFile(malicious, "w") as archive:
        archive.writestr("../outside", b"unsafe")
    data = malicious.getvalue()
    entry["artifacts"]["training"].update(
        bytes=len(data), sha256=hashlib.sha256(data).hexdigest()
    )
    monkeypatch.setattr(training, "urlopen", lambda url, *, timeout: io.BytesIO(data))
    with pytest.raises(ValueError, match="Unsafe"):
        TrainingBundle.fetch(
            model_ref="laya-typed-decisions@pinned", cache_dir=tmp_path / "cache"
        )
    assert not (tmp_path / "outside").exists()


def test_training_requires_both_labels_and_an_explicit_quality_gate(tmp_path):
    from blockether.vis.decisions._trainer import _config, _examples

    data = tmp_path / "examples.jsonl"
    data.write_text(
        json.dumps({"state": "request", "question": {"type": "choice"}, "target": 0})
        + "\n"
    )
    with pytest.raises(ValueError, match="Both decision and action labels"):
        _examples(data)
    data.write_text(
        json.dumps(
            {
                "state": "request",
                "question": {"type": "choice"},
                "target": 0,
                "action": 1,
            }
        )
        + "\n"
    )
    assert len(_examples(data)) == 1
    policy = tmp_path / "policy.json"
    policy.write_text("{}")
    with pytest.raises(ValueError, match="min_decision_accuracy"):
        _config(policy, kind="quality policy")
    policy.write_text(
        json.dumps({"min_decision_accuracy": 0.75, "min_action_accuracy": 0.9})
    )
    assert _config(policy, kind="quality policy")["min_action_accuracy"] == 0.9
