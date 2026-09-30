"""GLiNER training checkpoints stay offline, complete and separate from inference."""

import hashlib
import io
import json
import math
import sys
import zipfile
from pathlib import Path
from types import ModuleType, SimpleNamespace

import pytest
from blockether.vis.decisions import gliner_training
from blockether.vis.decisions.gliner_training import GlinerTrainingBundle

MODELS = {
    "gliner2.5-base": "boundary",
    "gliner2.5-small": "boundary",
    "gliner2.5-multi": "boundary",
    "gliner2.5-decide": "span",
    "gliner2.5-multi-decide": "boundary",
}


@pytest.mark.parametrize(
    ("model_id", "revision"),
    [
        ("gliner2.5-base", "7f1ae80f150e9d3e262ec1684d0d78208e2595d0"),
        ("gliner2.5-small", "7132dc4561c3f94563c6147e75ffa8ef34c4964a"),
        ("gliner2.5-multi", "2ca71aafb3446d9014e1c55c7ff51c9bc7209c47"),
        ("gliner2.5-decide", "bbe10ff77ebb238777c17d3a8ac9260e30929057"),
        ("gliner2.5-multi-decide", "a35a0cd3b7a0f00f2effc576f454cd48fa98aa5f"),
    ],
)
def test_catalog_pins_complete_gliner_downloads(model_id, revision):
    catalog = gliner_training._manifest()
    assert {entry["id"] for entry in catalog} == {"laya-typed-decisions", *MODELS}
    model = next(entry for entry in catalog if entry["id"] == model_id)
    assert model["revision"] == revision
    assert model["license"] == "Apache-2.0"
    assert model["source-url"].endswith(revision)
    artifacts = model["artifacts"]
    assert "model.safetensors" in artifacts["training"]["requires"]
    for artifact in (
        artifacts["inference"],
        artifacts["training"],
        *artifacts["wheels"].values(),
    ):
        assert artifact["bytes"] > 0
        assert len(artifact["sha256"]) == 64
        assert artifact["url"] == (
            "https://github.com/Blockether/vis/releases/download/assets-pack/"
            + artifact["file"]
        )


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
            model_id=next(x for x in MODELS if MODELS[x] != MODELS[model_id]),
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
    for value in (0, 100_001, True):
        config.write_text(
            json.dumps({"encoder_lr": 1e-5, "task_lr": 5e-4, "checkpoint_steps": value})
        )
        with pytest.raises(ValueError, match="checkpoint_steps"):
            _training_config(config)


@pytest.mark.parametrize("model_id", MODELS)
def test_offline_full_checkpoint_training_export_sdk_publish_and_resume(model_id):
    """Opt-in real weights: train, stop, resume, validate, stream, activate and reopen."""
    import os
    import subprocess
    import tempfile

    from blockether.vis.decisions import Decisions
    from blockether.vis.decisions.gliner_training import GlinerTrainer

    name = model_id.removeprefix("gliner2.5-").replace("-", "_").upper()
    env_name = f"VIS_GLINER_{name}_CHECKPOINT"
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
                    "max_steps": 2,
                    "encoder_lr": 1e-5,
                    "task_lr": 5e-4,
                    "checkpoint_steps": 1,
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
            stopped = []
            with pytest.raises(RuntimeError, match="stopped"):
                trainer.finetune(
                    train_data=train,
                    eval_data=evaluation,
                    training_config=config,
                    validation_policy=policy,
                    output_dir=root / "stopped",
                    progress=stop_at_checkpoint(stopped),
                )
        assert [event["stage"] for event in stopped] == [
            "training",
            "training",
            "checkpoint_saved",
        ]
        assert stopped[1]["step"] == 1 and math.isfinite(stopped[1]["loss"])
        partial = GlinerTrainingBundle.open(root / "stopped/checkpoint")
        events = []
        with GlinerTrainer(partial) as trainer:
            result = trainer.finetune(
                train_data=train,
                eval_data=evaluation,
                training_config=config,
                validation_policy=policy,
                output_dir=root / "trained",
                progress=events.append,
            )
        assert [(event["stage"], event.get("step")) for event in events] == [
            ("training", 1),
            ("training", 2),
            ("checkpoint_saved", 2),
            ("exporting", None),
            ("validated", None),
        ]
        saved = GlinerTrainingBundle.open(result.checkpoint_dir)
        assert saved.model_id == model_id
        lineage = json.loads((saved.path / "PROVENANCE.json").read_text())
        assert "partial" not in lineage
        assert lineage["parent_revision"] == provenance(partial.path)["revision"]
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


class FakeModel:
    """Its weights are a digest of every batch that the checkpoint has trained."""

    def __init__(self, checkpoint: Path, model_id: str, batches: list, loss: float):
        self.weights = (checkpoint / "model.safetensors").read_bytes()
        self.model_id = model_id
        self.batches = batches
        self.loss = loss

    def fit(self, batch: list) -> float:
        texts = [example.text for example in batch]
        self.batches.append(texts)
        self.weights = hashlib.sha256(
            self.weights + json.dumps(texts).encode()
        ).digest()
        return self.loss


class FakeDataset:
    """Like gliner2 validation, silently drop an example that it cannot train."""

    def __init__(self, data: list, shuffle: bool = True, validate: bool = False):
        assert not shuffle and validate
        self.data = [example for example in data if "[invalid]" not in example.text]

    def __len__(self) -> int:
        return len(self.data)

    def __getitem__(self, index: int):
        return self.data[index]


class FakeLoader:
    """The torch DataLoader arguments that the Vis trainer uses."""

    def __init__(self, dataset, batch_size=1, sampler=None, collate_fn=None, **_):
        self.dataset = dataset
        self.batch_size = batch_size
        self.sampler = range(len(dataset)) if sampler is None else sampler
        self.collate_fn = collate_fn

    def __len__(self) -> int:
        return math.ceil(len(self.sampler) / self.batch_size)

    def __iter__(self):
        indices = list(self.sampler)
        for start in range(0, len(indices), self.batch_size):
            batch = indices[start : start + self.batch_size]
            yield self.collate_fn([self.dataset[index] for index in batch])


class FakeExtractorTrainer:
    """The gliner2 2.0.0 training loop and hooks that the Vis trainer adapts."""

    def __init__(self, model: FakeModel, config: SimpleNamespace):
        self.model = model
        self.config = config
        self.output_dir = Path(config.output_dir)
        self.global_step = 0
        self.history = []

    def _create_dataloader(self, dataset, batch_size, shuffle=True, is_training=True):
        return FakeLoader(dataset, batch_size=batch_size, collate_fn=list)

    def _log_metrics(self, metrics: dict, prefix: str = "") -> None:
        if prefix == "train":
            self.history.append(metrics)

    def _save_checkpoint(self, name: str) -> None:
        saved = source_checkpoint(self.output_dir / name, self.model.model_id)
        (saved / "model.safetensors").write_bytes(self.model.weights)

    def train(self, dataset: FakeDataset) -> dict:
        loader = self._create_dataloader(
            dataset, self.config.batch_size, shuffle=True, is_training=True
        )
        for _ in range(math.ceil(self.config.max_steps / len(loader))):
            for batch in loader:
                loss = self.model.fit(batch)
                self.global_step += 1
                self._log_metrics(
                    {"loss": loss, "classification_loss": loss}, prefix="train"
                )
                if self.global_step >= self.config.max_steps:
                    break
        self._save_checkpoint("final")
        return {"total_steps": self.global_step, "train_metrics_history": self.history}


def install_fake_gliner2(monkeypatch) -> None:
    """Replace the optional gliner2 training modules, so that Torch never loads."""
    data = ModuleType("gliner2.training.data")
    data.InputExample = data.Classification = SimpleNamespace
    trainer = ModuleType("gliner2.training.trainer")
    trainer.ExtractorDataset = FakeDataset
    trainer.ExtractorTrainer = FakeExtractorTrainer
    trainer.TrainingConfig = SimpleNamespace
    for module in (
        ModuleType("gliner2"),
        ModuleType("gliner2.training"),
        data,
        trainer,
    ):
        monkeypatch.setitem(sys.modules, module.__name__, module)


def fake_trainer(checkpoint, batches: list, *, prepare=None, loss: float = 0.5):
    """A GlinerTrainer whose export writes placeholder inference files."""
    from blockether.vis.decisions._gliner_trainer import GlinerTrainer

    def exported(saved: Path, destination: Path, rows: list, policy: dict) -> None:
        GlinerTrainingBundle.open(saved)
        (destination / "inference").mkdir()
        (destination / "validation_report.json").write_text(json.dumps(len(rows)))

    trainer = object.__new__(GlinerTrainer)
    trainer._closed = False
    trainer.checkpoint = checkpoint
    trainer.model = None
    trainer._exporter = SimpleNamespace(
        load_checkpoint=lambda path, model_id: FakeModel(path, model_id, batches, loss)
    )
    trainer._prepare = prepare or exported
    return trainer


def base_checkpoint(root: Path) -> GlinerTrainingBundle:
    license_file = root / "LICENSE.txt"
    license_file.write_text("Apache-2.0")
    return GlinerTrainingBundle.from_local(
        source_checkpoint(root / "source", "gliner2.5-base"),
        root / "base",
        model_id="gliner2.5-base",
        revision="a" * 40,
        license_file=license_file,
    )


def training_inputs(root: Path, states: list[str], **settings) -> dict:
    """Write labeled rows, one disjoint held-out row, the settings and a policy."""
    question = {
        "type": "choice",
        "instructions": "Choose intent",
        "criteria": ["refund", "other"],
    }
    for name, rows in (("train", states), ("eval", ["Where is my parcel"])):
        (root / f"{name}.jsonl").write_text(
            "".join(
                json.dumps(
                    {
                        "state": state,
                        "question": question,
                        "target": index % 2,
                        "action": (index + 1) % 2,
                    }
                )
                + "\n"
                for index, state in enumerate(rows)
            )
        )
    (root / "config.json").write_text(
        json.dumps({"encoder_lr": 1e-5, "task_lr": 5e-4, **settings})
    )
    (root / "policy.json").write_text(
        '{"min_decision_accuracy": 0.0, "min_action_accuracy": 0.0}'
    )
    return {
        "train_data": root / "train.jsonl",
        "eval_data": root / "eval.jsonl",
        "training_config": root / "config.json",
        "validation_policy": root / "policy.json",
    }


def provenance(checkpoint: Path) -> dict:
    path = GlinerTrainingBundle.open(checkpoint).path / "PROVENANCE.json"
    return json.loads(path.read_text())


def stop_at_checkpoint(events: list):
    """Progress that stops the job, like the gateway, after the first saved step."""

    def progress(event: dict) -> None:
        events.append(event)
        if event["stage"] == "checkpoint_saved":
            raise RuntimeError("Gateway stopped the job")

    return progress


def steps(events: list, stage: str) -> list:
    return [event["step"] for event in events if event["stage"] == stage]


def test_gliner_training_reports_steps_and_resumes_a_partial_checkpoint(
    tmp_path, monkeypatch
):
    """#297: bounded step and loss progress, a partial checkpoint and an exact resume."""
    from blockether.vis.decisions._gliner_trainer import _examples

    install_fake_gliner2(monkeypatch)
    base = base_checkpoint(tmp_path)
    inputs = training_inputs(
        tmp_path,
        [f"Refund order {index}" for index in range(10)],
        max_steps=250,
        batch_size=2,
        seed=7,
        checkpoint_steps=100,
    )
    events, batches = [], []
    reference = fake_trainer(base, batches).finetune(
        **inputs, output_dir=tmp_path / "reference", progress=events.append
    )
    assert events[:2] == [
        {"stage": "training", "step": 0, "max_steps": 250},
        {"stage": "training", "step": 1, "max_steps": 250, "epoch": 0.2, "loss": 0.5},
    ]
    assert steps(events, "training") == [0, 1, *range(2, 251, 2)]
    assert steps(events, "checkpoint_saved") == [100, 200, 250]
    assert events[-2:] == [{"stage": "exporting"}, {"stage": "validated"}]
    assert all(event["max_steps"] == 250 for event in events if "step" in event)
    texts = sorted(row.text for row in _examples(inputs["train_data"]))
    assert len(batches) == 250
    for start in range(0, 250, 5):
        # Each pass trains every row once, in its own seeded order.
        passed = batches[start : start + 5]
        assert sorted(text for batch in passed for text in batch) == texts
    assert batches[:5] != batches[5:10]
    final = provenance(reference.checkpoint_dir)
    assert "partial" not in final
    assert final["parent_revision"] == "a" * 40
    assert json.loads((tmp_path / "reference/training_report.json").read_text()) == {
        "steps": 250,
        "max_steps": 250,
        "status": "checkpoint_saved",
    }
    assert reference.inference_bundle.is_dir()

    trained = []
    with pytest.raises(RuntimeError, match="stopped"):
        fake_trainer(base, trained).finetune(
            **inputs, output_dir=tmp_path / "stopped", progress=stop_at_checkpoint([])
        )
    assert trained == batches[:100]
    stopped = tmp_path / "stopped"
    assert sorted(path.name for path in stopped.iterdir()) == [
        "checkpoint",
        "training_report.json",
    ]
    assert json.loads((stopped / "training_report.json").read_text()) == {
        "steps": 100,
        "max_steps": 250,
        "status": "partial",
    }
    partial = provenance(stopped / "checkpoint")
    assert partial["partial"]["step"] == 100
    assert partial["partial"]["max_steps"] == 250
    assert partial["parent_revision"] == "a" * 40

    events, resumed_batches = [], []
    resumed = fake_trainer(
        GlinerTrainingBundle.open(stopped / "checkpoint"), resumed_batches
    ).finetune(**inputs, output_dir=tmp_path / "resumed", progress=events.append)
    assert resumed_batches == batches[100:]
    assert steps(events, "training") == [100, 101, *range(102, 251, 2)]
    assert steps(events, "checkpoint_saved") == [200, 250]
    continued = provenance(resumed.checkpoint_dir)
    assert "partial" not in continued
    # The resumed weights equal the uninterrupted run; the parent is the partial step.
    assert continued["revision"] == final["revision"]
    assert continued["parent_revision"] == partial["revision"]
    assert not list(tmp_path.glob(".gliner-train-*"))


def test_gliner_training_continues_a_checkpoint_on_new_data_from_step_zero(
    tmp_path, monkeypatch
):
    """Other rows or settings start a new run from the saved weights."""
    install_fake_gliner2(monkeypatch)
    refunds = [f"Refund order {index}" for index in range(4)]
    inputs = training_inputs(tmp_path, refunds, max_steps=8, checkpoint_steps=3)
    with pytest.raises(RuntimeError, match="stopped"):
        fake_trainer(base_checkpoint(tmp_path), []).finetune(
            **inputs, output_dir=tmp_path / "stopped", progress=stop_at_checkpoint([])
        )
    saved = GlinerTrainingBundle.open(tmp_path / "stopped/checkpoint")
    cancellations = [f"Cancel order {index}" for index in range(4)]
    inputs = training_inputs(tmp_path, cancellations, max_steps=8, checkpoint_steps=3)
    events, batches = [], []
    result = fake_trainer(saved, batches).finetune(
        **inputs, output_dir=tmp_path / "continued", progress=events.append
    )
    assert events[0] == {"stage": "training", "step": 0, "max_steps": 8}
    assert len(batches) == 8
    assert all(text.startswith("Cancel") for batch in batches for text in batch)
    continued = provenance(result.checkpoint_dir)
    assert continued["parent_revision"] == provenance(saved.path)["revision"]
    assert "partial" not in continued
    for name, settings, first in (
        ("reseeded", {"seed": 1, "checkpoint_steps": 3}, 0),
        ("saved-less-often", {"checkpoint_steps": 5}, 3),
    ):
        inputs = training_inputs(tmp_path, refunds, max_steps=8, **settings)
        events = []
        fake_trainer(saved, []).finetune(
            **inputs, output_dir=tmp_path / name, progress=events.append
        )
        assert events[0] == {"stage": "training", "step": first, "max_steps": 8}


def test_failed_gliner_training_keeps_only_published_checkpoints(tmp_path, monkeypatch):
    """No inference without validation and no output before the first saved step."""
    install_fake_gliner2(monkeypatch)
    base = base_checkpoint(tmp_path)
    inputs = training_inputs(tmp_path, ["Refund order 0", "Refund order 1"])

    def rejected(*_):
        raise ValueError("Decision accuracy is below the quality policy")

    events = []
    with pytest.raises(ValueError, match="quality policy"):
        fake_trainer(base, [], prepare=rejected).finetune(
            **inputs, output_dir=tmp_path / "rejected", progress=events.append
        )
    rejected_output = tmp_path / "rejected"
    assert sorted(path.name for path in rejected_output.iterdir()) == [
        "checkpoint",
        "training_report.json",
    ]
    assert "partial" not in provenance(rejected_output / "checkpoint")
    assert events[-2:] == [
        {"stage": "checkpoint_saved", "step": 2, "max_steps": 2},
        {"stage": "exporting"},
    ]
    with pytest.raises(ValueError, match="finite loss"):
        fake_trainer(base, [], loss=math.nan).finetune(
            **inputs, output_dir=tmp_path / "diverged"
        )
    inputs = training_inputs(tmp_path, ["Refund order 0", "[invalid] Refund order 1"])
    with pytest.raises(ValueError, match="rejected a training example"):
        fake_trainer(base, []).finetune(**inputs, output_dir=tmp_path / "dropped")
    many = [f"Refund order {index}" for index in range(1001)]
    inputs = training_inputs(tmp_path, many, epochs=100)
    with pytest.raises(ValueError, match="at most 100000 steps"):
        fake_trainer(base, []).finetune(**inputs, output_dir=tmp_path / "unbounded")
    names = {path.name for path in tmp_path.iterdir()}
    assert not names & {"diverged", "dropped", "unbounded"}
    assert not list(tmp_path.glob(".gliner-train-*"))


def test_partial_training_progress_is_checked_in_provenance(tmp_path):
    """Only a step before max_steps and a run digest mark a resumable checkpoint."""
    license_file = tmp_path / "LICENSE.txt"
    license_file.write_text("Apache-2.0")
    source = source_checkpoint(tmp_path / "source", "gliner2.5-base")
    progress = {"step": 4, "max_steps": 10, "fingerprint": "f" * 64}

    def inventory(name: str, partial: dict) -> GlinerTrainingBundle:
        return GlinerTrainingBundle.from_local(
            source,
            tmp_path / name,
            model_id="gliner2.5-base",
            revision="a" * 40,
            license_file=license_file,
            partial=partial,
        )

    for invalid in (
        {**progress, "step": 10},
        {**progress, "step": True},
        {**progress, "fingerprint": "F" * 64},
        {"step": 4, "max_steps": 10},
    ):
        with pytest.raises(ValueError, match="Invalid partial"):
            inventory("invalid", invalid)
        assert not (tmp_path / "invalid").exists()
    saved = inventory("partial", progress)
    metadata = json.loads((saved.path / "PROVENANCE.json").read_text())
    assert metadata["partial"] == progress
    metadata["partial"]["step"] = 0
    (saved.path / "PROVENANCE.json").write_text(json.dumps(metadata))
    with pytest.raises(ValueError, match="identity"):
        GlinerTrainingBundle.open(saved.path)
