"""Public decision import and alias operations use the authenticated SDK transport."""

import hashlib
import io
import json
import zipfile
from pathlib import Path

import pytest
from blockether.vis.decisions import Decisions
from blockether.vis.engine import GatewayClient, GatewayError
from test_client import compatible, endpoint


def inference_bundle(root: Path) -> Path:
    root.mkdir()
    files = {
        "model.onnx": b"inference weights",
        "rl_agent_config.json": b"{}",
        "tokenizer/tokenizer.json": b"{}",
        "tokenizer/tokenizer_config.json": b"{}",
    }
    for name, data in files.items():
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)
    (root / "LICENSE.txt").write_text("Apache-2.0")
    (root / "PROVENANCE.json").write_text(
        json.dumps(
            {
                "model": "laya-typed-decisions",
                "revision": "1" * 64,
                "kind": "inference",
                "format": "onnx",
                "precision": "fp32",
                "license": "Apache-2.0",
                "files": {
                    name: {
                        "bytes": len(data),
                        "sha256": hashlib.sha256(data).hexdigest(),
                    }
                    for name, data in files.items()
                },
            }
        )
    )
    return root


def test_upload_uses_one_streamed_authenticated_call_and_explicit_alias(tmp_path):
    root = inference_bundle(tmp_path / "inference")
    (tmp_path / "checkpoint").mkdir()
    (tmp_path / "checkpoint" / "model.safetensors").write_bytes(b"private")
    model_ref = None

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if method == "POST" and path == "/v1/decisions/models":
            with zipfile.ZipFile(io.BytesIO(body)) as archive:
                assert set(archive.namelist()) == {
                    "model.onnx",
                    "rl_agent_config.json",
                    "tokenizer/tokenizer.json",
                    "tokenizer/tokenizer_config.json",
                    "PROVENANCE.json",
                    "LICENSE.txt",
                }
            return 201, {"model_ref": "sha256-" + hashlib.sha256(body).hexdigest()}
        if path == "/v1/decisions/aliases/sales":
            if method == "PUT":
                assert json.loads(body) == {
                    "model_ref": model_ref,
                    "expected_current": None,
                }
                return 200, {"alias": "sales", "model_ref": model_ref}
            return 200, {"alias": "sales", "model_ref": model_ref}
        if method == "GET" and path.startswith("/v1/decisions/models/"):
            return 200, {"model_ref": model_ref, "installed": True}
        raise AssertionError((method, path))

    with (
        endpoint(respond) as (url, calls),
        GatewayClient(url, token="secret") as gateway,
    ):
        decisions = Decisions(gateway)
        progress = []
        uploaded = decisions.upload_model(
            root, progress=lambda current, total: progress.append((current, total))
        )
        model_ref = uploaded["model_ref"]
        assert decisions.get_model(model_ref)["installed"]
        assert (
            decisions.activate_model("sales", model_ref, expected_current=None)[
                "model_ref"
            ]
            == model_ref
        )
        assert decisions.get_alias("sales")["model_ref"] == model_ref
        uploads = [
            row
            for row in calls
            if row[0] == "POST" and row[1] == "/v1/decisions/models"
        ]
        assert len(uploads) == 1
        headers = {key.lower(): value for key, value in uploads[0][2].items()}
        assert headers["authorization"] == "Bearer secret"
        assert headers["x-content-sha256"] == hashlib.sha256(uploads[0][3]).hexdigest()
        assert int(headers["content-length"]) == len(uploads[0][3])
        assert progress[-1][0] == progress[-1][1] == len(uploads[0][3])


def test_upload_rejects_incomplete_and_alias_conflict_is_visible(tmp_path):
    root = inference_bundle(tmp_path / "inference")
    (root / "tokenizer/tokenizer.json").unlink()
    gateway = GatewayClient("http://127.0.0.1:1")
    try:
        with pytest.raises((ValueError, FileNotFoundError)):
            Decisions(gateway).upload_model(root)
    finally:
        gateway.close()

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        return 409, {"error": {"type": "decisions/error", "reason": "alias-conflict"}}

    with endpoint(respond) as (url, _), GatewayClient(url) as gateway:
        with pytest.raises(GatewayError) as error:
            Decisions(gateway).activate_model("sales", "sha256-" + "1" * 64)
        assert error.value.status == 409


def test_same_inference_files_produce_one_immutable_ref_regardless_of_timestamps(
    tmp_path,
):
    import os

    from blockether.vis.decisions._publication import package

    root = inference_bundle(tmp_path / "inference")
    first, _ = package(root, tmp_path / "first.zip")
    for path in root.rglob("*"):
        if path.is_file():
            os.utime(path, (1_800_000_000, 1_800_000_000))
    second, _ = package(root, tmp_path / "second.zip")
    assert first == second


def test_ambiguous_upload_reads_known_digest_without_resubmitting(
    tmp_path, monkeypatch
):
    from blockether.vis.decisions._publication import package
    from blockether.vis.engine import VisTimeout

    root = inference_bundle(tmp_path / "inference")
    digest, _ = package(root, tmp_path / "known.zip")
    expected = "sha256-" + digest
    gateway = GatewayClient("http://127.0.0.1:1")
    sent = []

    def timeout(**kwargs):
        sent.append(kwargs["sha256"])
        raise VisTimeout("ambiguous")

    monkeypatch.setattr(gateway, "post_decision_model", timeout)
    monkeypatch.setattr(
        gateway,
        "get_decision_model",
        lambda ref, **_: {"model_ref": ref, "installed": True},
    )
    assert Decisions(gateway).upload_model(root)["model_ref"] == expected
    assert sent == [digest]
    gateway.close()


def gliner_bundle(root: Path, model_id: str) -> Path:
    root.mkdir()
    architecture = {"gliner2.5-base": "boundary", "gliner2.5-decide": "span"}[model_id]
    files = {
        "model.onnx": b"graph",
        "config.json": json.dumps({"architecture": architecture}).encode(),
        "encoder_config/config.json": b"{}",
        "tokenizer/tokenizer.json": b"{}",
        "tokenizer/tokenizer_config.json": b"{}",
    }
    for name, content in files.items():
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
    (root / "LICENSE.txt").write_text("Apache-2.0")
    (root / "PROVENANCE.json").write_text(
        json.dumps(
            {
                "model": model_id,
                "family": "gliner2.5",
                "architecture": architecture,
                "revision": "a" * 64,
                "kind": "inference",
                "format": "onnx",
                "precision": "fp32",
                "license": "Apache-2.0",
                "files": {
                    name: {
                        "bytes": len(content),
                        "sha256": hashlib.sha256(content).hexdigest(),
                    }
                    for name, content in files.items()
                },
            }
        )
    )
    return root


@pytest.mark.parametrize("model_id", ["gliner2.5-base", "gliner2.5-decide"])
def test_gliner_sdk_upload_alias_and_typed_inference(tmp_path, model_id):
    root = gliner_bundle(tmp_path / "inference", model_id)
    calls = []

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        calls.append((method, path))
        if path == "/v1/decisions/models" and method == "POST":
            with zipfile.ZipFile(io.BytesIO(body)) as archive:
                assert set(archive.namelist()) == {
                    "model.onnx",
                    "config.json",
                    "encoder_config/config.json",
                    "tokenizer/tokenizer.json",
                    "tokenizer/tokenizer_config.json",
                    "PROVENANCE.json",
                    "LICENSE.txt",
                }
            return 201, {"model_ref": "sha256-" + hashlib.sha256(body).hexdigest()}
        if path == "/v1/decisions/aliases/review" and method == "PUT":
            ref = json.loads(body)["model_ref"]
            return 200, {"alias": "review", "model_ref": ref}
        if path == "/v1/systemone" and method == "POST":
            assert json.loads(body)["model"] == "review"
            return 200, {
                "model": model_id,
                "answers": {
                    "intent": {"choice": "refund", "action": {"act_probability": 0.2}}
                },
            }
        raise AssertionError((method, path))

    with endpoint(respond) as (url, _), GatewayClient(url, token="secret") as gateway:
        decisions = Decisions(gateway)
        ref = decisions.upload_model(root)["model_ref"]
        assert calls == [("POST", "/v1/decisions/models")]
        assert decisions.activate_model("review", ref)["model_ref"] == ref
        assert (
            decisions.infer(
                model="review",
                state="Refund requested",
                questions={
                    "intent": {
                        "type": "choice",
                        "instructions": "Intent",
                        "criteria": ["refund", "repair"],
                    }
                },
            )["answers"]["intent"]["action"]["act_probability"]
            == 0.2
        )


@pytest.mark.parametrize("model_id", ["gliner2.5-base", "gliner2.5-decide"])
def test_gliner_sdk_rejects_mismatched_architecture_before_upload(tmp_path, model_id):
    root = gliner_bundle(tmp_path / "inference", model_id)
    metadata = json.loads((root / "PROVENANCE.json").read_text())
    metadata["architecture"] = "span" if model_id.endswith("base") else "boundary"
    (root / "PROVENANCE.json").write_text(json.dumps(metadata))
    gateway = GatewayClient("http://127.0.0.1:1")
    try:
        with pytest.raises(ValueError, match="(architecture|GLiNER)"):
            Decisions(gateway).upload_model(root)
    finally:
        gateway.close()
