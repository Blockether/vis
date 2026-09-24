"""Offline GLiNER2.5 decision-classification export for the two pinned architectures.

This module is optional: importing the gateway client never loads Torch or GLiNER.
The ONNX graph contains the encoder and contextual classification head, not the
extractive entity/JSON heads. No checkpoint or dependency is downloaded here.
"""

from __future__ import annotations

import hashlib
import json
import math
import shutil
import tempfile
from collections.abc import Mapping
from pathlib import Path

import numpy as np
import onnx
import onnxruntime as ort
import torch
from gliner2 import AutoExtractor

from ._models import ARCHITECTURES

INPUT_NAMES = ("input_ids", "attention_mask", "label_indices")
PROBES = (
    (
        "Please refund my order",
        {
            "intent": ["refund_request", "order_status", "other"],
            "action": ["act", "escalate"],
        },
    ),
    (
        "The transfer is pending. Please stop it before it is sent.",
        {
            "decision": ["transfer_pending", "transfer_cancel", "fraud", "other"],
            "action": ["act", "escalate"],
        },
    ),
    ("Yes", {"truth": ["true", "false"], "action": ["act", "escalate"]}),
)


class DecisionGraph(torch.nn.Module):
    """Select classification-marker embeddings from the complete encoder output."""

    def __init__(self, model: torch.nn.Module) -> None:
        super().__init__()
        self.encoder = model.encoder
        self.classifier = model.classifier

    def forward(
        self,
        input_ids: torch.Tensor,
        attention_mask: torch.Tensor,
        label_indices: torch.Tensor,
    ) -> torch.Tensor:
        states = self.encoder(
            input_ids=input_ids, attention_mask=attention_mask
        ).last_hidden_state
        labels = states.gather(
            1, label_indices.unsqueeze(-1).expand(-1, -1, states.shape[-1])
        )
        return self.classifier(labels).squeeze(-1)


def _digest(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_checkpoint(source: str | Path, *, model_id: str):
    """Load a local full checkpoint, rejecting a different or incomplete architecture."""
    if model_id not in ARCHITECTURES:
        raise ValueError(f"Unsupported GLiNER decision model: {model_id}")
    root = Path(source).expanduser().resolve()
    required = (
        "config.json",
        "encoder_config/config.json",
        "model.safetensors",
        "tokenizer.json",
        "tokenizer_config.json",
    )
    if not root.is_dir() or any(not (root / name).is_file() for name in required):
        raise FileNotFoundError("A complete local GLiNER2.5 checkpoint is required")
    config = json.loads((root / "config.json").read_text(encoding="utf-8"))
    architecture = ARCHITECTURES[model_id]
    if config.get("architecture") != architecture:
        raise ValueError(f"{model_id} requires the {architecture} architecture")
    model = (
        AutoExtractor.from_pretrained(
            str(root),
            architecture=architecture,
            local_files_only=True,
            map_location="cpu",
        )
        .float()
        .eval()
    )
    if model.architecture != architecture or any(
        parameter.dtype != torch.float32
        for parameter in model.parameters()
        if parameter.is_floating_point()
    ):
        raise ValueError(
            "Only the complete FP32 GLiNER decision checkpoint is supported"
        )
    return model


def make_batch(model, text: str, tasks: Mapping[str, list[str]]):
    """Use the official GLiNER2.0 processor, with strict errors instead of its fallback."""
    if (
        not isinstance(text, str)
        or not text.strip()
        or not tasks
        or any(
            not isinstance(task, str)
            or not task.strip()
            or not isinstance(labels, list)
            or not 1 <= len(labels) <= 64
            or any(not isinstance(label, str) or not label.strip() for label in labels)
            for task, labels in tasks.items()
        )
    ):
        raise ValueError(
            "GLiNER decisions require text and nonempty classification labels"
        )
    schema = model._classification_schema(dict(tasks))
    schemas, _ = model._build_schema_dicts_and_metadata([schema])
    batch = model.processor.collate_fn_inference(
        [(text, schemas[0])], architecture=model.architecture, error_policy="raise"
    )
    expected = sum(len(labels) for labels in tasks.values())
    if batch.cls_marker_indices.shape != (1, expected):
        raise ValueError(
            "GLiNER classification markers do not match the requested labels"
        )
    limit = model.encoder.config.max_position_embeddings
    if batch.input_ids.shape[1] > limit:
        raise ValueError(f"GLiNER input exceeds the encoder limit of {limit} tokens")
    return batch.input_ids, batch.attention_mask, batch.cls_marker_indices


def export_graph(model, output: str | Path) -> Path:
    """Export a dynamic FP32 graph of the encoder and classification head."""
    graph = DecisionGraph(model).eval()
    arguments = make_batch(model, *PROBES[0])
    output = Path(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    with torch.no_grad():
        torch.onnx.export(
            graph,
            arguments,
            str(output),
            input_names=list(INPUT_NAMES),
            output_names=["logits"],
            dynamic_axes={
                "input_ids": {0: "batch", 1: "sequence"},
                "attention_mask": {0: "batch", 1: "sequence"},
                "label_indices": {0: "batch", 1: "labels"},
                "logits": {0: "batch", 1: "labels"},
            },
            opset_version=17,
            dynamo=False,
            external_data=True,
        )
    onnx.checker.check_model(str(output))
    return output


def validate_graph(model, output: str | Path) -> dict[str, float | int]:
    """Compare dynamic ONNX logits and both classification heads with upstream."""
    options = ort.SessionOptions()
    options.intra_op_num_threads = 4
    runtime = ort.InferenceSession(
        str(output), sess_options=options, providers=["CPUExecutionProvider"]
    )
    if (
        {entry.name for entry in runtime.get_inputs()} != set(INPUT_NAMES)
        or [entry.name for entry in runtime.get_outputs()] != ["logits"]
        or runtime.get_outputs()[0].type != "tensor(float)"
    ):
        raise ValueError("GLiNER graph inputs or FP32 classifier output do not match")
    graph = DecisionGraph(model).eval()
    largest_error = 0.0
    for text, tasks in PROBES:
        arguments = make_batch(model, text, tasks)
        with torch.inference_mode():
            expected = graph(*arguments).cpu().numpy()[0]
        actual = runtime.run(
            None,
            {
                name: value.numpy()
                for name, value in zip(INPUT_NAMES, arguments, strict=True)
            },
        )[0][0]
        if not np.isfinite(actual).all() or not np.allclose(
            expected, actual, rtol=1e-4, atol=1e-3
        ):
            raise ValueError("GLiNER FP32 export disagrees with the checkpoint")
        largest_error = max(largest_error, float(np.max(np.abs(expected - actual))))
        official = model.classify_text(text, tasks, include_confidence=True)
        offset = 0
        for name, labels in tasks.items():
            chunk = actual[offset : offset + len(labels)]
            scores = np.exp(chunk - max(chunk))
            scores /= scores.sum()
            winner = int(scores.argmax())
            if official[name]["label"] != labels[winner] or not math.isclose(
                official[name]["confidence"], float(scores[winner]), abs_tol=2e-5
            ):
                raise ValueError("GLiNER classifier differs from upstream inference")
            offset += len(labels)
    return {"examples": len(PROBES), "max_abs_logit_error": largest_error}


def prepare_fp32(
    source: str | Path,
    destination: str | Path,
    *,
    model_id: str,
    license_file: str | Path,
    revision: str | None = None,
) -> dict[str, float | int]:
    """Atomically prepare an offline, inventoried bundle from a local checkpoint."""
    source = Path(source).expanduser().resolve()
    destination = Path(destination).expanduser().resolve()
    if destination.exists():
        raise FileExistsError(destination)
    license_file = Path(license_file).expanduser().resolve()
    if not license_file.is_file():
        raise FileNotFoundError("An Apache-2.0 license file is required")
    model = load_checkpoint(source, model_id=model_id)
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(
        prefix=".gliner-export-", dir=destination.parent
    ) as temporary:
        staging = Path(temporary) / "inference"
        staging.mkdir()
        export_graph(model, staging / "model.onnx")
        report = validate_graph(model, staging / "model.onnx")
        shutil.copyfile(source / "config.json", staging / "config.json")
        (staging / "encoder_config").mkdir()
        shutil.copyfile(
            source / "encoder_config" / "config.json",
            staging / "encoder_config" / "config.json",
        )
        (staging / "tokenizer").mkdir()
        for name in (
            "tokenizer.json",
            "tokenizer_config.json",
            "special_tokens_map.json",
        ):
            if (source / name).is_file():
                shutil.copyfile(source / name, staging / "tokenizer" / name)
        shutil.copyfile(license_file, staging / "LICENSE.txt")
        inventory = {
            item.relative_to(staging).as_posix(): {
                "bytes": item.stat().st_size,
                "sha256": _digest(item),
            }
            for item in sorted(staging.rglob("*"))
            if item.is_file() and item.name != "LICENSE.txt"
        }
        metadata = {
            "schema_version": 1,
            "kind": "inference",
            "format": "onnx",
            "precision": "fp32",
            "family": "gliner2.5",
            "architecture": ARCHITECTURES[model_id],
            "model": model_id,
            "revision": revision or _digest(source / "model.safetensors"),
            "license": "Apache-2.0",
            "files": inventory,
        }
        (staging / "PROVENANCE.json").write_text(json.dumps(metadata, indent=2) + "\n")
        staging.rename(destination)
    return report
