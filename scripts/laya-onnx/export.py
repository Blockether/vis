"""Checkout entrypoint for the SDK's canonical Laya exporter and parity tests."""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "packages/vis-agent/src"))

from blockether.vis.decisions._training import (  # noqa: E402
    ExportSelfAttention,
    OnnxGraph,
    export_model,
    load_onnx,
    load_reference,
    make_batch,
    quantize_bundle,
    train_and_export,
)

__all__ = [
    "ExportSelfAttention",
    "OnnxGraph",
    "export_model",
    "load_onnx",
    "load_reference",
    "make_batch",
    "quantize_bundle",
    "train_and_export",
]
