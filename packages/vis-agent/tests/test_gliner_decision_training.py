"""GLiNER training checkpoints stay offline, complete and separate from inference."""

import hashlib
import io
import json
import sys
import zipfile
from pathlib import Path

import pytest
from blockether.vis.decisions import gliner_training
from blockether.vis.decisions.gliner_training import GlinerTrainingBundle

MODELS = {"gliner2.5-base": "boundary", "gliner2.5-decide": "span"}


def source_checkpoint(path: Path, model_id: str) -> Path:
    path.mkdir(parents=True)
    files = {
        "config.json": json.dumps({"architecture": MODELS[model_id]}).encode(),
        "encoder_config/config.json": b'{"model_type": "deberta-v2"}',
        "model.safetensors": b"placeholder full weights",
        "tokenizer.json": b"{}",
        "tokenizer_config.json": b"{}",
    }
    for name, content in files.items():
        target = path / name
        target.parent.mkdir(exist_ok=True)
        target.write_bytes(content)
    return path


@pytest.mark.parametrize("model_id", MODELS)
def test_local_checkpoint_is_inventoried_offline_and_fails_closed(tmp_path, model_id):
    source = source_checkpoint(tmp_path / "source", model_id)
    license_file = tmp_path / "LICENSE.txt"
    license_file.write_text("Apache-2.0")
    destination = tmp_path / "checked"
    with pytest.raises(FileNotFoundError):
        GlinerTrainingBundle.open(source)
    bundle = GlinerTrainingBundle.from_local(
        source,
        destination,
        model_id=model_id,
        revision="a" * 40,
        license_file=license_file,
    )
    assert bundle.path == destination.resolve()
    assert GlinerTrainingBundle.open(destination).model_id == model_id
    assert "torch" not in sys.modules
    assert not (destination / "model.onnx").exists()
    assert not (destination / "train.jsonl").exists()
    with pytest.raises(ValueError, match="architecture"):
        GlinerTrainingBundle.from_local(
            source,
            tmp_path / "wrong",
            model_id=next(x for x in MODELS if x != model_id),
            revision="a" * 40,
            license_file=license_file,
        )
    assert not (tmp_path / "wrong").exists()
    (destination / "model.safetensors").write_bytes(b"changed")
    with pytest.raises(ValueError, match="checksum"):
        GlinerTrainingBundle.open(destination)


def test_fetch_streams_only_pinned_training_artifact_and_reuses_verified_cache(
    tmp_path, monkeypatch
):
    source = source_checkpoint(tmp_path / "source", "gliner2.5-base")
    license_file = tmp_path / "LICENSE.txt"
    license_file.write_text("Apache-2.0")
    checked = GlinerTrainingBundle.from_local(
        source,
        tmp_path / "checked",
        model_id="gliner2.5-base",
        revision="b" * 40,
        license_file=license_file,
    )
    output = io.BytesIO()
    with zipfile.ZipFile(output, "w") as zipped:
        for path in sorted(checked.path.rglob("*")):
            if path.is_file():
                zipped.write(path, path.relative_to(checked.path).as_posix())
    data = output.getvalue()
    artifact = {
        "file": "training.zip",
        "url": "https://gateway.example.com/training.zip",
        "bytes": len(data),
        "sha256": hashlib.sha256(data).hexdigest(),
        "requires": ["PROVENANCE.json", "LICENSE.txt", "model.safetensors"],
    }
    entry = {
        "id": "gliner2.5-base",
        "revision": "b" * 40,
        "artifacts": {"training": artifact},
    }
    monkeypatch.setattr(gliner_training, "_manifest", lambda: [entry])
    calls = []

    def download(url, *, timeout):
        calls.append(url)
        return io.BytesIO(data)

    monkeypatch.setattr(gliner_training, "urlopen", download)
    with pytest.raises(ValueError, match="pinned"):
        GlinerTrainingBundle.fetch(
            model_ref="gliner2.5-base", cache_dir=tmp_path / "cache"
        )
    first = GlinerTrainingBundle.fetch(
        model_ref="gliner2.5-base@" + "b" * 40, cache_dir=tmp_path / "cache"
    )
    assert first.model_id == "gliner2.5-base"
    assert (
        GlinerTrainingBundle.fetch(
            model_ref="gliner2.5-base@" + "b" * 40, cache_dir=tmp_path / "cache"
        ).path
        == first.path
    )
    assert len(calls) == 1
    artifact["sha256"] = "0" * 64
    (first.path / ".vis-verified").unlink()
    with pytest.raises((ValueError, FileNotFoundError)):
        GlinerTrainingBundle.fetch(
            model_ref="gliner2.5-base@" + "b" * 40, cache_dir=tmp_path / "cache"
        )
    assert "torch" not in sys.modules


def test_labeled_examples_match_gateway_question_and_both_heads(tmp_path):
    from blockether.vis.decisions._gliner_trainer import _examples, _training_config

    rows = [
        {
            "state": {"message": "Refund"},
            "question": {
                "type": "choice",
                "instructions": "Choose intent",
                "criteria": {"refund": "request for money", "other": "not a refund"},
            },
            "target": 0,
            "action": 1,
        },
        {
            "state": "Yes",
            "question": {"type": "noul", "instructions": "Is this true?"},
            "target": 1,
            "action": 0,
        },
        {
            "state": "Okay",
            "question": {
                "type": "score",
                "instructions": "Rate",
                "criteria": ["low", "medium", "high"],
            },
            "target": 2,
            "action": 1,
        },
    ]
    data = tmp_path / "examples.jsonl"
    data.write_text("\n".join(json.dumps(row) for row in rows) + "\n")
    parsed = _examples(data)
    assert parsed[0].text == '{"message": "Refund"}.'
    assert parsed[0].tasks == {
        "choice: Choose intent": ["refund: request for money", "other: not a refund"],
        "action": ["act", "escalate"],
    }
    assert parsed[1].tasks["noul: Is this true?"] == [
        "false: no, the statement does not hold",
        "true: yes, the statement holds",
    ]
    assert parsed[2].tasks["score: Rate"] == [
        "level 0: low",
        "level 1: medium",
        "level 2: high",
    ]
    rows[0].pop("action")
    data.write_text(json.dumps(rows[0]))
    with pytest.raises(ValueError, match="Both decision and action labels"):
        _examples(data)
    config = tmp_path / "config.json"
    config.write_text(
        '{"epochs": 1, "max_steps": 1, "encoder_lr": 1e-5, "task_lr": 5e-4}'
    )
    assert _training_config(config)["max_steps"] == 1
    config.write_text(
        '{"epochs": 1, "max_steps": 0, "encoder_lr": 1e-5, "task_lr": 5e-4}'
    )
    with pytest.raises(ValueError, match="max_steps"):
        _training_config(config)


@pytest.mark.parametrize("model_id", MODELS)
def test_offline_full_checkpoint_training_export_sdk_publish_and_resume(model_id):
    """Opt-in real weights: two labels train, validate, stream, activate, infer and reopen."""
    import os
    import subprocess
    import tempfile

    from blockether.vis.decisions import Decisions
    from blockether.vis.decisions.gliner_training import GlinerTrainer

    env_name = (
        "VIS_GLINER_BASE_CHECKPOINT"
        if model_id.endswith("base")
        else "VIS_GLINER_DECIDE_CHECKPOINT"
    )
    checkpoint_source = os.environ.get(env_name)
    license_name = os.environ.get("VIS_GLINER_LICENSE")
    if not checkpoint_source or not license_name:
        pytest.skip(
            f"Set {env_name} and VIS_GLINER_LICENSE to opt in to local real weights"
        )
    source = Path(checkpoint_source)
    license_file = Path(license_name)
    with tempfile.TemporaryDirectory(prefix="gliner-sdk-integration-") as directory:
        root = Path(directory)
        checkpoint = GlinerTrainingBundle.from_local(
            source,
            root / "checkpoint",
            model_id=model_id,
            revision="a" * 40,
            license_file=license_file,
        )
        train = root / "train.jsonl"
        evaluation = root / "eval.jsonl"
        config = root / "config.json"
        policy = root / "policy.json"
        question = {
            "type": "choice",
            "instructions": "Choose intent",
            "criteria": ["refund_request", "order_status", "other"],
        }
        train.write_text(
            json.dumps(
                {
                    "state": "Please refund my order",
                    "question": question,
                    "target": 0,
                    "action": 1,
                }
            )
            + "\n"
        )
        evaluation.write_text(
            json.dumps(
                {
                    "state": "Please refund this purchase",
                    "question": question,
                    "target": 0,
                    "action": 1,
                }
            )
            + "\n"
        )
        config.write_text(
            json.dumps(
                {
                    "epochs": 1,
                    "max_steps": 1,
                    "encoder_lr": 1e-5,
                    "task_lr": 5e-4,
                }
            )
        )
        policy.write_text(
            json.dumps(
                {
                    "min_decision_accuracy": 0.0,
                    "min_action_accuracy": 0.0,
                }
            )
        )
        overlap = root / "overlap.jsonl"
        overlap.write_text(
            json.dumps(
                {
                    "state": "Please refund my order",
                    "question": question,
                    "target": 1,
                    "action": 0,
                }
            )
            + "\n"
        )
        with GlinerTrainer(checkpoint) as trainer:
            with pytest.raises(ValueError, match="disjoint"):
                trainer.finetune(
                    train_data=train,
                    eval_data=overlap,
                    training_config=config,
                    validation_policy=policy,
                    output_dir=root / "invalid",
                )
            assert not (root / "invalid").exists()
            result = trainer.finetune(
                train_data=train,
                eval_data=evaluation,
                training_config=config,
                validation_policy=policy,
                output_dir=root / "trained",
            )
        saved = GlinerTrainingBundle.open(result.checkpoint_dir)
        assert saved.model_id == model_id
        assert (
            json.loads((saved.path / "PROVENANCE.json").read_text())["parent_revision"]
            == "a" * 40
        )
        report = json.loads(result.validation_report.read_text())
        assert report["examples"] == 1
        assert report["max_abs_logit_error"] < 1e-3
        assert report["status"] == "evaluated_not_approved_for_autonomous_actions"
        assert not (result.inference_bundle / "model.safetensors").exists()

        class Gateway:
            alias = None
            uploads = 0

            def post_decision_model(self, *, content, sha256, length, timeout):
                digest = hashlib.sha256()
                count = 0
                for chunk in iter(lambda: content.read(65536), b""):
                    count += len(chunk)
                    digest.update(chunk)
                assert count == length and digest.hexdigest() == sha256
                self.uploads += 1
                self.ref = "sha256-" + sha256
                return {"model_ref": self.ref, "installed": True}

            def get_decision_model(self, model_ref, *, timeout):
                assert model_ref == self.ref
                return {"model_ref": self.ref, "installed": True}

            def put_decision_alias(
                self, alias, *, model_ref, expected_current, timeout
            ):
                assert (
                    alias == "review"
                    and expected_current is None
                    and self.alias is None
                )
                assert model_ref == self.ref
                self.alias = model_ref
                return {"alias": alias, "model_ref": model_ref}

            def post_systemone(self, *, body, timeout):
                assert body["model"] == "review" and self.alias == self.ref
                assert body["questions"]["intent"] == question
                return {
                    "model": model_id,
                    "answers": {
                        "intent": {
                            "choice": "refund_request",
                            "action": {"act_probability": 0.4},
                        }
                    },
                }

        gateway = Gateway()
        client = Decisions(gateway)
        ref = client.upload_model(result)["model_ref"]
        assert gateway.uploads == 1 and client.get_model(ref)["installed"]
        assert (
            client.activate_model("review", ref, expected_current=None)["model_ref"]
            == ref
        )
        assert (
            client.infer(
                model="review",
                state="Please refund this purchase",
                questions={"intent": question},
            )["answers"]["intent"]["action"]["act_probability"]
            == 0.4
        )

        env = os.environ.copy()
        env.update(HF_HUB_OFFLINE="1", TRANSFORMERS_OFFLINE="1")
        subprocess.run(
            [
                sys.executable,
                "-c",
                "import sys; from blockether.vis.decisions.gliner_training import "
                "GlinerTrainingBundle, GlinerTrainer; "
                "checkpoint=GlinerTrainingBundle.open(sys.argv[1]); "
                "trainer=GlinerTrainer(checkpoint); "
                "result=trainer.prepare_fp32(eval_data=sys.argv[2], "
                "validation_policy=sys.argv[3], output_dir=sys.argv[4]); "
                "assert result.inference_bundle.is_dir(); trainer.close()",
                str(saved.path),
                str(evaluation),
                str(policy),
                str(root / "resumed"),
            ],
            check=True,
            env=env,
            timeout=1800,
        )
        assert (
            json.loads((root / "resumed/validation_report.json").read_text())[
                "examples"
            ]
            == 1
        )


def test_incomplete_export_never_exposes_inference_or_downloads_dependencies(tmp_path):
    from blockether.vis.decisions._gliner_trainer import GlinerTrainer

    with pytest.raises(TypeError, match="GlinerTrainingBundle"):
        GlinerTrainer("inference.onnx")
    source = source_checkpoint(tmp_path / "source", "gliner2.5-base")
    license_file = tmp_path / "LICENSE.txt"
    license_file.write_text("Apache-2.0")
    checkpoint = GlinerTrainingBundle.from_local(
        source,
        tmp_path / "checkpoint",
        model_id="gliner2.5-base",
        revision="a" * 40,
        license_file=license_file,
    )
    data = tmp_path / "eval.jsonl"
    data.write_text(
        json.dumps(
            {
                "state": "Refund this",
                "question": {
                    "type": "choice",
                    "instructions": "Intent",
                    "criteria": ["refund", "other"],
                },
                "target": 0,
                "action": 1,
            }
        )
        + "\n"
    )
    policy = tmp_path / "policy.json"
    policy.write_text('{"min_decision_accuracy": 0.0, "min_action_accuracy": 0.0}')
    trainer = object.__new__(GlinerTrainer)
    trainer._closed = False
    trainer.checkpoint = checkpoint

    def interrupted(*_):
        raise RuntimeError("Export interrupted")

    trainer._prepare = interrupted
    with pytest.raises(RuntimeError, match="interrupted"):
        trainer.prepare_fp32(
            eval_data=data, validation_policy=policy, output_dir=tmp_path / "incomplete"
        )
    assert not (tmp_path / "incomplete").exists()
    assert "torch" not in sys.modules


def test_training_extra_is_explicit_and_separate_from_light_client():
    import tomllib

    project = tomllib.loads(
        (Path(__file__).resolve().parents[1] / "pyproject.toml").read_text()
    )["project"]
    assert not any(
        "torch" in item or "gliner2" in item for item in project["dependencies"]
    )
    extra = project["optional-dependencies"]["decisions-gliner-training"]
    assert "gliner2[train]==2.0.0" in extra
    assert "transformers==4.57.6" in extra
    assert (
        "transformers==5.0.0" in project["optional-dependencies"]["decisions-training"]
    )
