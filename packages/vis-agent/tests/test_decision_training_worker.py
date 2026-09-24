"""Remote trainer uses the same SDK pipeline and only packages inference files."""

import json
from pathlib import Path

import pytest
from blockether.vis.decisions import _worker


def test_worker_reports_progress_and_exports_inference_only(
    tmp_path, monkeypatch, capsys
):
    output = tmp_path / "output"
    inference = output / "inference"
    checkpoint = output / "checkpoint"
    spec = tmp_path / "job.json"
    spec.write_text(
        json.dumps(
            {
                "model_id": "laya-typed-decisions",
                "checkpoint": str(tmp_path / "baseline"),
                "train_data": str(tmp_path / "training.jsonl"),
                "eval_data": str(tmp_path / "evaluation.jsonl"),
                "training_config": str(tmp_path / "config.json"),
                "validation_policy": str(tmp_path / "policy.json"),
                "output_dir": str(output),
                "archive": str(tmp_path / "inference.zip"),
                "result": str(tmp_path / "result.json"),
            }
        )
    )
    seen = []

    class FakeBundle:
        @classmethod
        def open(cls, path):
            seen.append(("open", str(path)))
            return cls()

    class FakeTrainer:
        def __init__(self, bundle):
            assert isinstance(bundle, FakeBundle)

        def __enter__(self):
            return self

        def __exit__(self, *_):
            pass

        def finetune(self, *, progress, **kwargs):
            seen.append(("finetune", kwargs))
            progress({"stage": "training", "step": 1, "max_steps": 2})
            inference.mkdir(parents=True)
            (inference / "PROVENANCE.json").write_text(
                json.dumps({"model": "laya-typed-decisions"})
            )
            checkpoint.mkdir(parents=True)
            (checkpoint / "model.safetensors").write_bytes(b"private")
            report = output / "validation_report.json"
            report.write_text(
                json.dumps({"decision_accuracy": 1.0, "action_accuracy": 0.5})
            )
            return type(
                "Result",
                (),
                {
                    "inference_bundle": inference,
                    "checkpoint_dir": checkpoint,
                    "validation_report": report,
                },
            )()

    def fake_package(bundle, archive):
        assert bundle == inference
        assert not (bundle / "model.safetensors").exists()
        Path(archive).write_bytes(b"inference archive")
        return "a" * 64, len(b"inference archive")

    monkeypatch.setattr(_worker, "TrainingBundle", FakeBundle)
    monkeypatch.setattr(_worker, "ModernBertTrainer", FakeTrainer)
    monkeypatch.setattr(_worker, "package", fake_package)
    _worker.run(spec)
    result = json.loads((tmp_path / "result.json").read_text())
    assert result == {
        "sha256": "a" * 64,
        "bytes": len(b"inference archive"),
        "decision_accuracy": 1.0,
        "action_accuracy": 0.5,
    }
    assert [
        json.loads(line)["stage"] for line in capsys.readouterr().out.splitlines()
    ] == ["loading", "training", "training", "publishing", "completed"]
    assert seen[0] == ("open", str(tmp_path / "baseline"))
    assert seen[1][0] == "finetune"


def test_worker_failure_leaves_no_success_descriptor(tmp_path, monkeypatch):
    spec = tmp_path / "job.json"
    spec.write_text(json.dumps({"checkpoint": "missing"}))
    monkeypatch.setattr(
        _worker.TrainingBundle,
        "open",
        lambda _path: (_ for _ in ()).throw(ValueError("bad")),
    )
    with pytest.raises(ValueError):
        _worker.run(spec)
    assert not (tmp_path / "result.json").exists()


@pytest.mark.parametrize("model_id", ["gliner2.5-base", "gliner2.5-decide"])
def test_worker_selects_and_preserves_gliner_checkpoint_identity(
    tmp_path, monkeypatch, capsys, model_id
):
    from blockether.vis.decisions import gliner_training

    spec = tmp_path / "job.json"
    spec.write_text(
        json.dumps(
            {
                "model_id": model_id,
                "checkpoint": str(tmp_path / "approved-checkpoint"),
                "train_data": str(tmp_path / "train.jsonl"),
                "eval_data": str(tmp_path / "eval.jsonl"),
                "training_config": str(tmp_path / "config.json"),
                "validation_policy": str(tmp_path / "policy.json"),
                "output_dir": str(tmp_path / "output"),
                "archive": str(tmp_path / "inference.zip"),
                "result": str(tmp_path / "result.json"),
            }
        )
    )
    opened = []

    class FakeBundle:
        @classmethod
        def open(cls, path):
            opened.append(Path(path))
            return cls()

    FakeBundle.model_id = model_id

    class FakeTrainer:
        def __init__(self, bundle):
            assert bundle.model_id == model_id

        def __enter__(self):
            return self

        def __exit__(self, *_):
            pass

        def finetune(self, *, output_dir, progress, **_):
            progress({"stage": "training", "step": 1, "max_steps": 1})
            output = Path(output_dir)
            (output / "checkpoint").mkdir(parents=True)
            (output / "inference").mkdir()
            (output / "inference" / "PROVENANCE.json").write_text(
                json.dumps({"model": model_id})
            )
            (output / "checkpoint" / "model.safetensors").write_bytes(b"private")
            report = output / "validation_report.json"
            report.write_text(
                json.dumps({"decision_accuracy": 0.9, "action_accuracy": 0.8})
            )
            return type(
                "Result",
                (),
                {"inference_bundle": output / "inference", "validation_report": report},
            )()

    def fake_package(bundle, archive):
        assert bundle == tmp_path / "output" / "inference"
        assert not (bundle / "model.safetensors").exists()
        archive.write_bytes(b"fp32")
        return "a" * 64, 4

    monkeypatch.setattr(gliner_training, "GlinerTrainingBundle", FakeBundle)
    monkeypatch.setattr(gliner_training, "GlinerTrainer", FakeTrainer)
    monkeypatch.setattr(_worker, "package", fake_package)
    _worker.run(spec)
    assert opened == [tmp_path / "approved-checkpoint"]
    assert json.loads((tmp_path / "result.json").read_text())["sha256"] == "a" * 64
    assert [
        json.loads(line)["stage"] for line in capsys.readouterr().out.splitlines()
    ] == ["loading", "training", "training", "publishing", "completed"]


def test_worker_rejects_cross_family_resume_before_training(tmp_path, monkeypatch):
    from blockether.vis.decisions import gliner_training

    spec = tmp_path / "job.json"
    spec.write_text(
        json.dumps(
            {
                "model_id": "gliner2.5-decide",
                "checkpoint": str(tmp_path / "base-checkpoint"),
                "train_data": "private.jsonl",
                "eval_data": "eval.jsonl",
                "training_config": "config.json",
                "validation_policy": "policy.json",
                "output_dir": str(tmp_path / "output"),
                "archive": str(tmp_path / "inference.zip"),
                "result": str(tmp_path / "result.json"),
            }
        )
    )

    class WrongBundle:
        model_id = "gliner2.5-base"

        @classmethod
        def open(cls, _):
            return cls()

    monkeypatch.setattr(gliner_training, "GlinerTrainingBundle", WrongBundle)
    with pytest.raises(ValueError, match="identity"):
        _worker.run(spec)
    assert not (tmp_path / "result.json").exists()
