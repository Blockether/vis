"""Pinned training bundles can be used offline without importing tensor libraries."""

import hashlib
import io
import json
import math
import sys
import zipfile
from pathlib import Path
from types import SimpleNamespace

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


def test_fetch_joins_parts_in_order_and_verifies_each_part(tmp_path, monkeypatch):
    import blockether.vis.decisions.training as training

    source = checkpoint(tmp_path / "source")
    data = archive_bytes(source)
    middle = len(data) // 2
    release = "https://github.com/Blockether/vis/releases/download/assets-pack/"
    pieces = {"training.zip.001": data[:middle], "training.zip.002": data[middle:]}
    parts = [
        {
            "file": name,
            "url": release + name,
            "bytes": len(piece),
            "sha256": hashlib.sha256(piece).hexdigest(),
        }
        for name, piece in pieces.items()
    ]
    entry = {
        "id": "laya-typed-decisions",
        "revision": "pinned",
        "artifacts": {
            "training": {
                "file": "training.zip",
                "bytes": len(data),
                "sha256": hashlib.sha256(data).hexdigest(),
                "requires": ["model.safetensors", "PROVENANCE.json", "LICENSE.txt"],
                "parts": parts,
            }
        },
    }
    monkeypatch.setattr(training, "_manifest", lambda: [entry])
    calls = []

    def open_url(url, *, timeout):
        calls.append(url)
        return io.BytesIO(pieces[url.removeprefix(release)])

    monkeypatch.setattr(training, "urlopen", open_url)
    bundle = TrainingBundle.fetch(
        model_ref="laya-typed-decisions@pinned", cache_dir=tmp_path / "cache"
    )
    assert calls == [part["url"] for part in parts]
    assert (bundle.path / "model.safetensors").read_bytes() == b"placeholder weights"

    parts[0]["bytes"] -= 1
    with pytest.raises(ValueError, match="size"):
        TrainingBundle.fetch(
            model_ref="laya-typed-decisions@pinned", cache_dir=tmp_path / "short"
        )
    parts[0]["bytes"] += 1
    parts[1]["sha256"] = "0" * 64
    with pytest.raises(ValueError, match="checksum"):
        TrainingBundle.fetch(
            model_ref="laya-typed-decisions@pinned", cache_dir=tmp_path / "corrupt"
        )
    for cache in ("short", "corrupt"):
        assert not (tmp_path / cache / "laya-typed-decisions/pinned/training").exists()


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


def test_inventory_hashes_bundle_files_in_order_without_the_license(tmp_path):
    from blockether.vis.decisions.training import _inventory

    (tmp_path / "tokenizer").mkdir()
    (tmp_path / "tokenizer" / "tokenizer.json").write_bytes(b"{}")
    (tmp_path / "model.onnx").write_bytes(b"graph")
    (tmp_path / "LICENSE.txt").write_text("Apache-2.0")
    inventory = _inventory(tmp_path)
    assert list(inventory) == ["model.onnx", "tokenizer/tokenizer.json"]
    assert inventory["model.onnx"] == {
        "bytes": 5,
        "sha256": hashlib.sha256(b"graph").hexdigest(),
    }


def test_quality_report_gates_both_heads_before_writing(tmp_path):
    from blockether.vis.decisions._trainer import _quality_report

    policy = {"min_decision_accuracy": 0.75, "min_action_accuracy": 0.5}

    def evaluate(decisions, actions):
        return _quality_report(
            tmp_path,
            policy,
            revision="pinned",
            examples=4,
            decisions=decisions,
            actions=actions,
            largest_error=0.01,
        )

    for decisions, actions in ((2, 2), (3, 1)):
        with pytest.raises(ValueError, match="failed the quality policy"):
            evaluate(decisions, actions)
    assert not (tmp_path / "validation_report.json").exists()
    report = evaluate(3, 2)
    assert json.loads(report.read_text()) == {
        "examples": 4,
        "decision_accuracy": 0.75,
        "action_accuracy": 0.5,
        "max_abs_logit_error": 0.01,
        "quality_policy": policy,
        "status": "evaluated_not_approved_for_autonomous_actions",
        "checkpoint_revision": "pinned",
    }


def test_fp32_export_publishes_only_a_completed_preparation(tmp_path):
    from blockether.vis.decisions._trainer import TrainingResult, _export_fp32

    exports = tmp_path.resolve() / "exports"
    checkpoint = tmp_path / "checkpoint"
    stages = []

    def prepare(prepared):
        (prepared / "inference").mkdir()
        (prepared / "validation_report.json").write_text("{}")

    def fail(prepared):
        (prepared / "partial").write_text("incomplete")
        raise ValueError("FP32 export disagrees with the training checkpoint")

    def export(name, step, progress=None):
        return _export_fp32(
            exports / name,
            checkpoint=checkpoint,
            prefix=".test-export-",
            prepare=step,
            progress=progress,
        )

    assert export("fp32", prepare, stages.append) == TrainingResult(
        checkpoint,
        exports / "fp32" / "inference",
        exports / "fp32" / "validation_report.json",
    )
    assert stages == [{"stage": "exporting"}, {"stage": "validated"}]
    assert (exports / "fp32" / "inference").is_dir()
    with pytest.raises(FileExistsError):
        export("fp32", prepare)
    with pytest.raises(ValueError, match="disagrees"):
        export("failed", fail)
    assert [path.name for path in exports.iterdir()] == ["fp32"]


def trained_states(path: Path) -> list[str]:
    """Fake Laya weights are the ordered example states that trained them."""
    weights = (path / "model.safetensors").read_bytes()
    return [] if weights == b"placeholder weights" else json.loads(weights)


class FakeLoss(float):
    def __add__(self, other: float) -> "FakeLoss":
        return FakeLoss(float(self) + float(other))

    def backward(self) -> None:
        pass

    def detach(self) -> "FakeLoss":
        return self


class FakeParameter:
    requires_grad = True

    def requires_grad_(self, value: bool) -> None:
        self.requires_grad = value


class FakeLaya:
    """Laya stand-in whose loss falls with every state that it trained."""

    def __init__(self, trained: list[str]) -> None:
        self.trained = trained
        self.current = None
        self.head, self.encoder_weight = FakeParameter(), FakeParameter()
        self.encoder = SimpleNamespace(
            parameters=lambda: [self.encoder_weight],
            config=SimpleNamespace(save_pretrained=self.save_encoder),
        )

    @staticmethod
    def save_encoder(directory: Path) -> None:
        directory.mkdir()
        (directory / "config.json").write_text('{"model_type": "modernbert"}')

    def parameters(self) -> list[FakeParameter]:
        return [self.head, self.encoder_weight]

    def train(self) -> None:
        pass

    def eval(self) -> None:
        pass

    def state_dict(self) -> dict:
        return {"trained": list(self.trained)}

    def __call__(self, state: str, *_) -> tuple[FakeLoss, FakeLoss]:
        self.current = state
        loss = math.nan if state == "poisoned" else 1 / (len(self.trained) + 1)
        return FakeLoss(loss / 2), FakeLoss(loss / 2)


def laya_trainer(bundle: TrainingBundle, prepare=None):
    """Run the real ModernBertTrainer loop over Torch, Laya and export stand-ins."""
    from blockether.vis.decisions._trainer import ModernBertTrainer, TrainingResult

    model = FakeLaya(trained_states(bundle.path))

    def validated(*, checkpoint, destination, **_):
        (destination / "inference").mkdir()
        (destination / "validation_report.json").write_text("{}")
        return TrainingResult(
            checkpoint,
            destination / "inference",
            destination / "validation_report.json",
        )

    trainer = object.__new__(ModernBertTrainer)
    trainer.checkpoint = bundle
    trainer.agent = SimpleNamespace(
        model=model, cfg=json.loads((bundle.path / "rl_agent_config.json").read_text())
    )
    trainer._exporter = SimpleNamespace(
        make_batch=lambda agent, state, questions: (
            state,
            None,
            None,
            [SimpleNamespace(sum=lambda: 2)],
        ),
        save_file=lambda weights, path: Path(path).write_text(
            json.dumps(weights["trained"])
        ),
    )
    optimizer = SimpleNamespace(
        zero_grad=lambda set_to_none: None,
        step=lambda: model.trained.append(model.current),
    )
    trainer._torch = SimpleNamespace(
        manual_seed=lambda seed: None,
        tensor=lambda value: value,
        isfinite=math.isfinite,
        optim=SimpleNamespace(AdamW=lambda parameters, lr: optimizer),
        nn=SimpleNamespace(
            functional=SimpleNamespace(cross_entropy=lambda logits, target: logits),
            utils=SimpleNamespace(clip_grad_norm_=lambda *_, **__: None),
        ),
    )
    trainer._prepare = prepare or validated
    trainer._closed = False
    return trainer


def write_rows(path: Path, states: list[str]) -> Path:
    question = {"type": "choice", "instructions": "Choose", "criteria": ["a", "b"]}
    path.write_text(
        "".join(
            json.dumps(
                {"state": state, "question": question, "target": index % 2, "action": 1}
            )
            + "\n"
            for index, state in enumerate(states)
        )
    )
    return path


def laya_training(tmp_path: Path):
    """Train a fake Laya checkpoint into ``tmp_path / name`` with one quality gate."""
    evaluation = write_rows(tmp_path / "eval.jsonl", ["held out"])
    policy = tmp_path / "policy.json"
    policy.write_text('{"min_decision_accuracy": 0.5, "min_action_accuracy": 0.5}')

    def train(bundle, name, data, config, *, progress=None, prepare=None):
        with laya_trainer(bundle, prepare) as trainer:
            return trainer.finetune(
                train_data=data,
                eval_data=evaluation,
                training_config=config,
                validation_policy=policy,
                output_dir=tmp_path / name,
                progress=progress,
            )

    return train


def test_laya_training_resumes_a_partial_checkpoint_or_continues_on_new_rows(tmp_path):
    """#297: Laya reports steps, keeps partial checkpoints and resumes like GLiNER."""
    train = laya_training(tmp_path)
    base = TrainingBundle.open(checkpoint(tmp_path / "base"))
    rows = write_rows(tmp_path / "train.jsonl", [f"s{index}" for index in range(5)])
    settings = {"epochs": 3, "learning_rate": 0.0001, "max_steps": 12}
    config = tmp_path / "config.json"
    config.write_text(json.dumps(settings))
    saving = tmp_path / "saving.json"
    saving.write_text(json.dumps({**settings, "checkpoint_steps": 5}))
    expected = [f"s{step % 5}" for step in range(12)]

    events = []
    reference = train(base, "reference", rows, config, progress=events.append)
    assert trained_states(reference.checkpoint_dir) == expected
    assert [event["step"] for event in events if event["stage"] == "training"] == list(
        range(13)
    )
    assert events[1] == {
        "stage": "training",
        "step": 1,
        "max_steps": 12,
        "epoch": 0.2,
        "loss": 1.0,
    }
    assert events[-3:] == [
        {"stage": "checkpoint_saved", "step": 12, "max_steps": 12},
        {"stage": "exporting"},
        {"stage": "validated"},
    ]
    first = json.loads((reference.checkpoint_dir / "PROVENANCE.json").read_text())
    assert "partial" not in first and first["parent_revision"] == "pinned"

    def stop(event):
        if event["stage"] == "training" and event["step"] == 8:
            raise RuntimeError("Training stopped")

    with pytest.raises(RuntimeError, match="stopped"):
        train(base, "stopped", rows, saving, progress=stop)
    stopped = tmp_path / "stopped"
    assert sorted(path.name for path in stopped.iterdir()) == [
        "checkpoint",
        "training_report.json",
    ]
    assert not list(tmp_path.glob(".decision-train-*"))
    assert trained_states(stopped / "checkpoint") == expected[:5]
    report = json.loads((stopped / "training_report.json").read_text())
    assert (report["steps"], report["max_steps"], report["status"]) == (
        5,
        12,
        "partial",
    )
    partial = json.loads((stopped / "checkpoint/PROVENANCE.json").read_text())
    assert partial["partial"]["step"] == 5 and partial["parent_revision"] == "pinned"

    events = []
    resumed = train(
        TrainingBundle.open(stopped / "checkpoint"),
        "resumed",
        rows,
        config,
        progress=events.append,
    )
    assert events[:2] == [
        {"stage": "training", "step": 5, "max_steps": 12},
        {
            "stage": "training",
            "step": 6,
            "max_steps": 12,
            "epoch": 1.2,
            "loss": pytest.approx(1 / 6),
        },
    ]
    assert trained_states(resumed.checkpoint_dir) == expected
    final = json.loads((resumed.checkpoint_dir / "PROVENANCE.json").read_text())
    assert "partial" not in final
    assert final["parent_revision"] == partial["revision"]
    assert final["revision"] == first["revision"]

    events = []
    continued = train(
        TrainingBundle.open(stopped / "checkpoint"),
        "continued",
        write_rows(tmp_path / "other.jsonl", ["n0", "n1"]),
        config,
        progress=events.append,
    )
    assert events[0] == {"stage": "training", "step": 0, "max_steps": 6}
    assert trained_states(continued.checkpoint_dir) == expected[:5] + ["n0", "n1"] * 3


def test_laya_training_failure_keeps_only_a_verified_checkpoint(tmp_path):
    train = laya_training(tmp_path)
    base = TrainingBundle.open(checkpoint(tmp_path / "base"))
    config = tmp_path / "config.json"
    config.write_text('{"epochs": 1, "learning_rate": 0.0001}')

    def reject(*, destination, **_):
        (destination / "inference").mkdir()
        (destination / "validation_report.json").write_text("{}")
        raise ValueError("Decision accuracy is below the quality policy")

    rows = write_rows(tmp_path / "train.jsonl", ["s0", "s1"])
    with pytest.raises(ValueError, match="below the quality policy"):
        train(base, "rejected", rows, config, prepare=reject)
    rejected = tmp_path / "rejected"
    assert sorted(path.name for path in rejected.iterdir()) == [
        "checkpoint",
        "training_report.json",
    ]
    report = json.loads((rejected / "training_report.json").read_text())
    assert report["status"] == "checkpoint_saved"
    assert trained_states(TrainingBundle.open(rejected / "checkpoint").path) == [
        "s0",
        "s1",
    ]
    poisoned = write_rows(tmp_path / "poisoned.jsonl", ["poisoned"])
    with pytest.raises(ValueError, match="non-finite"):
        train(base, "poisoned", poisoned, config)
    assert not (tmp_path / "poisoned").exists()
    assert not list(tmp_path.glob(".decision-train-*"))


def test_laya_checkpoint_steps_and_partial_provenance_are_validated(tmp_path):
    from blockether.vis.decisions._trainer import _config

    config = tmp_path / "config.json"
    for value in (1, 100_000, 0, 100_001, True, 2.0):
        config.write_text(
            json.dumps(
                {"epochs": 1, "learning_rate": 0.0001, "checkpoint_steps": value}
            )
        )
        if value in (1, 100_000) and type(value) is int:
            assert (
                _config(config, kind="training configuration")["checkpoint_steps"]
                == value
            )
        else:
            with pytest.raises(ValueError, match="checkpoint_steps"):
                _config(config, kind="training configuration")
    root = checkpoint(tmp_path / "checkpoint")
    provenance = json.loads((root / "PROVENANCE.json").read_text())
    valid = {"step": 1, "max_steps": 3, "fingerprint": "a" * 64}
    (root / "PROVENANCE.json").write_text(json.dumps({**provenance, "partial": valid}))
    assert TrainingBundle.open(root).path == root.resolve()
    for partial in (
        {**valid, "step": 3},
        {"step": 1, "max_steps": 3},
        {**valid, "fingerprint": "A" * 64},
    ):
        (root / "PROVENANCE.json").write_text(
            json.dumps({**provenance, "partial": partial})
        )
        with pytest.raises(ValueError, match="partial"):
            TrainingBundle.open(root)
