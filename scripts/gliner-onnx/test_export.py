"""Offline regression for both official GLiNER2.5 architectures and trained weights.

Supply VIS_GLINER_BASE_CHECKPOINT and VIS_GLINER_DECIDE_CHECKPOINT explicitly.
No test downloads checkpoints, accesses Hugging Face, or approves model actions.
"""

import gc
import json
import math
import os
from pathlib import Path
from tempfile import TemporaryDirectory

import numpy as np
import pytest
import torch
from blockether.vis.decisions._gliner import (
    ARCHITECTURES,
    INPUT_NAMES,
    PROBES,
    load_checkpoint,
    make_batch,
    prepare_fp32,
    validate_graph,
)
from gliner2.training.data import Classification, InputExample
from gliner2.training.trainer import ExtractorCollator, ExtractorTrainer, TrainingConfig

MODEL_IDS = ("gliner2.5-base", "gliner2.5-decide")
LICENSE = Path(__file__).resolve().parents[2] / "LICENSE"


def _checkpoint(model_id):
    name = (
        "VIS_GLINER_BASE_CHECKPOINT"
        if model_id == MODEL_IDS[0]
        else "VIS_GLINER_DECIDE_CHECKPOINT"
    )
    value = os.environ.get(name)
    if not value:
        pytest.skip(f"Set {name} to run the offline real-checkpoint test")
    return Path(value)


def test_rejects_missing_or_cross_architecture_checkpoint(tmp_path):
    with pytest.raises(FileNotFoundError, match="complete local"):
        load_checkpoint(tmp_path, model_id=MODEL_IDS[0])
    (tmp_path / "config.json").write_text(json.dumps({"architecture": "span"}))
    (tmp_path / "encoder_config").mkdir()
    (tmp_path / "encoder_config" / "config.json").write_text("not an encoder")
    for name in ("model.safetensors", "tokenizer.json", "tokenizer_config.json"):
        (tmp_path / name).write_text("not a checkpoint")
    with pytest.raises(ValueError, match="boundary architecture"):
        load_checkpoint(tmp_path, model_id=MODEL_IDS[0])
    with pytest.raises(ValueError, match="Unsupported"):
        load_checkpoint(tmp_path, model_id="gliner2.5-other")


@pytest.mark.parametrize("model_id", MODEL_IDS)
def test_export_official_checkpoint_matches_upstream(model_id):
    torch.set_num_threads(4)
    source = _checkpoint(model_id)
    with TemporaryDirectory(prefix="gliner-offline-") as directory:
        destination = Path(directory) / "inference"
        report = prepare_fp32(
            source, destination, model_id=model_id, license_file=LICENSE
        )
        provenance = json.loads((destination / "PROVENANCE.json").read_text())
        assert report["examples"] == 3
        assert report["max_abs_logit_error"] < 1e-3
        assert provenance["architecture"] == ARCHITECTURES[model_id]
        assert provenance["precision"] == "fp32"
        assert {
            "config.json",
            "encoder_config/config.json",
            "model.onnx",
            "tokenizer/tokenizer.json",
        } <= provenance["files"].keys()
        assert (destination / "LICENSE.txt").read_bytes() == LICENSE.read_bytes()
        assert (destination / "model.onnx").stat().st_size > 700_000_000
        with pytest.raises(FileExistsError):
            prepare_fp32(source, destination, model_id=model_id, license_file=LICENSE)


@pytest.mark.parametrize("model_id", MODEL_IDS)
def test_strict_batch_matches_official_collator(model_id):
    torch.set_num_threads(4)
    model = load_checkpoint(_checkpoint(model_id), model_id=model_id)
    text, tasks = PROBES[1]
    actual = make_batch(model, text, tasks)
    schema = model._classification_schema(tasks)
    schemas, _ = model._build_schema_dicts_and_metadata([schema])
    reference = ExtractorCollator(
        model.processor, is_training=False, architecture=model.architecture
    )([(text, schemas[0])])
    assert len(INPUT_NAMES) == 3
    for selected, expected in zip(
        actual,
        (reference.input_ids, reference.attention_mask, reference.cls_marker_indices),
        strict=True,
    ):
        assert torch.equal(selected, expected)
    with pytest.raises(ValueError, match="nonempty"):
        make_batch(model, text, {"question": []})
    with pytest.raises(ValueError, match="encoder limit"):
        make_batch(model, "word " * 1200, tasks)


@pytest.mark.parametrize("model_id", MODEL_IDS)
def test_one_training_step_saves_reloadable_fp32_export(model_id):
    torch.set_num_threads(4)
    source = _checkpoint(model_id)
    with TemporaryDirectory(prefix="gliner-trained-") as directory:
        root = Path(directory)
        model = load_checkpoint(source, model_id=model_id)
        example = InputExample(
            text="Please refund my order",
            classifications=[
                Classification(
                    task="intent",
                    labels=["refund_request", "order_status", "other"],
                    true_label="refund_request",
                ),
                Classification(
                    task="action", labels=["act", "escalate"], true_label="escalate"
                ),
            ],
        )
        config = TrainingConfig(
            output_dir=str(root / "training"),
            max_steps=1,
            batch_size=1,
            num_workers=0,
            eval_strategy="no",
            scheduler_type="constant",
            fp16=False,
            bf16=False,
        )
        summary = ExtractorTrainer(model, config).train([example])
        assert summary["total_steps"] == 1
        assert math.isfinite(summary["train_metrics_history"][0]["classification_loss"])
        del model
        gc.collect()
        checkpoint = root / "training" / "final"
        assert (checkpoint / "model.safetensors").is_file()
        report = prepare_fp32(
            checkpoint, root / "inference", model_id=model_id, license_file=LICENSE
        )
        assert report["max_abs_logit_error"] < 1e-3
        reloaded = load_checkpoint(checkpoint, model_id=model_id)
        assert (
            validate_graph(reloaded, root / "inference" / "model.onnx")["examples"] == 3
        )
        assert np.isfinite(make_batch(reloaded, *PROBES[0])[0].numpy()).all()
