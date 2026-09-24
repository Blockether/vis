"""The public asset builder must reject untracked bytes and reproduce archives."""

import hashlib
import json
import zipfile

import pytest
from build_release import inventory, pack


def prepared_bundle(root):
    root.mkdir()
    payloads = {
        "model.onnx": b"complete FP32 model graph",
        "rl_agent_config.json": b"{}",
        "tokenizer/tokenizer.json": b"{}",
    }
    for name, data in payloads.items():
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)
    (root / "LICENSE.txt").write_text("Apache-2.0 test notice")
    provenance = {
        "kind": "inference",
        "format": "onnx",
        "precision": "fp32",
        "model": "test-model",
        "revision": "test-revision",
        "files": {
            name: {"sha256": hashlib.sha256(data).hexdigest(), "bytes": len(data)}
            for name, data in payloads.items()
        },
    }
    (root / "PROVENANCE.json").write_text(json.dumps(provenance))
    return root


def test_archive_is_reproducible_and_contains_exactly_declared_files(tmp_path):
    source = prepared_bundle(tmp_path / "source")
    (source / ".vis-verified").write_text("installer marker, not release content")
    first = tmp_path / "first.zip"
    second = tmp_path / "second.zip"
    assert pack(source, first) == pack(source, second)
    assert first.read_bytes() == second.read_bytes()
    with zipfile.ZipFile(first) as archive:
        assert sorted(archive.namelist()) == [name for name, _ in inventory(source)]
        assert "model.onnx" in archive.namelist()
        assert ".vis-verified" not in archive.namelist()


def test_corruption_or_untracked_file_aborts_before_publication(tmp_path):
    source = prepared_bundle(tmp_path / "source")
    output = tmp_path / "output.zip"
    (source / "model.onnx").write_bytes(b"corrupt")
    with pytest.raises(ValueError, match="digest"):
        pack(source, output)
    assert not output.exists()
    (source / "model.onnx").write_bytes(b"complete FP32 model graph")
    (source / "private-data.txt").write_text("must not publish")
    with pytest.raises(ValueError, match="untracked"):
        pack(source, output)
    assert not output.exists()


def test_refuses_symlinks_and_non_fp32_graphs(tmp_path):
    source = prepared_bundle(tmp_path / "source")
    (source / "link").symlink_to(source / "model.onnx")
    with pytest.raises(ValueError, match="symlink"):
        pack(source, tmp_path / "bad.zip")
    (source / "link").unlink()
    path = source / "PROVENANCE.json"
    provenance = json.loads(path.read_text())
    provenance["precision"] = "int8"
    path.write_text(json.dumps(provenance))
    with pytest.raises(ValueError, match="FP32"):
        pack(source, tmp_path / "bad.zip")


def test_training_wheelhouse_keeps_precompressed_wheels_unchanged(tmp_path):
    source = tmp_path / "wheels"
    source.mkdir()
    wheel = source / "wheels" / "sample-1.0-py3-none-any.whl"
    wheel.parent.mkdir()
    wheel.write_bytes(b"already compressed wheel contents")
    for name in ("install.sh", "requirements.txt", "README.md"):
        (source / name).write_text("sample")
    metadata = {
        "kind": "training-dependencies",
        "python": "CPython 3.12",
        "wheels": [
            {
                "wheel": wheel.name,
                "bytes": wheel.stat().st_size,
                "sha256": hashlib.sha256(wheel.read_bytes()).hexdigest(),
                "license": "Apache-2.0",
                "source_url": "https://example.com/wheel",
            }
        ],
    }
    (source / "PROVENANCE.json").write_text(json.dumps(metadata))
    output = tmp_path / "wheelhouse.zip"
    pack(source, output)
    with zipfile.ZipFile(output) as archive:
        assert all(
            item.compress_type == zipfile.ZIP_STORED for item in archive.infolist()
        )
        assert archive.read(f"wheels/{wheel.name}") == wheel.read_bytes()
