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
