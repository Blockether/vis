"""Laya decision-head fine-tuning and the shared FP32 export/validation path.

Heavy dependencies are imported only after constructing ModernBertTrainer. Raw
training and evaluation examples never enter checkpoint provenance or reports.
"""

from __future__ import annotations

import gc
import json
import math
import shutil
import tempfile
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from typing import Any

    from .training import TrainingBundle


@dataclass(frozen=True)
class TrainingResult:
    """Checkpoint, independently validated FP32 inference bundle and report."""

    checkpoint_dir: Path
    inference_bundle: Path
    validation_report: Path


def _examples(source: str | Path) -> list[dict]:
    path = Path(source)
    if not path.is_file():
        raise FileNotFoundError(f"Labeled examples are missing: {path}")
    rows = []
    with path.open(encoding="utf-8") as stream:
        for number, line in enumerate(stream, 1):
            if not line.strip():
                continue
            row = json.loads(line)
            if (
                not isinstance(row, dict)
                or not {"state", "question", "target", "action"} <= row.keys()
            ):
                raise ValueError(
                    f"Both decision and action labels are required at line {number}"
                )
            if not isinstance(row["question"], dict) or row["question"].get(
                "type"
            ) not in {"choice", "score", "noul"}:
                raise ValueError(f"Invalid decision question at line {number}")
            if (
                type(row["target"]) is not int
                or row["target"] < 0
                or type(row["action"]) is not int
                or row["action"] not in (0, 1)
            ):
                raise ValueError(f"Invalid decision or action target at line {number}")
            rows.append(row)
    if not rows:
        raise ValueError("Labeled examples cannot be empty")
    return rows


def _config(source: str | Path, *, kind: str) -> dict:
    config = json.loads(Path(source).read_text(encoding="utf-8"))
    if not isinstance(config, dict):
        raise ValueError(f"{kind} must be a JSON object")
    if kind == "quality policy":
        for name in ("min_decision_accuracy", "min_action_accuracy"):
            value = config.get(name)
            if (
                type(value) not in (int, float)
                or not math.isfinite(value)
                or not 0 <= value <= 1
            ):
                raise ValueError(f"quality policy requires {name} in [0,1]")
    else:
        if set(config) - {
            "epochs",
            "learning_rate",
            "train_encoder",
            "seed",
            "max_steps",
        }:
            raise ValueError("Unknown training configuration option")
        epochs = config.get("epochs")
        rate = config.get("learning_rate")
        if type(epochs) is not int or not 1 <= epochs <= 100:
            raise ValueError("Training epochs must be in [1,100]")
        if (
            type(rate) not in (int, float)
            or not math.isfinite(rate)
            or not 0 < rate <= 0.01
        ):
            raise ValueError("Training learning_rate must be in (0,0.01]")
        if type(config.get("train_encoder", False)) is not bool:
            raise ValueError("train_encoder must be boolean")
        if type(config.get("seed", 42)) is not int:
            raise ValueError("Training seed must be an integer")
        if (
            type(config.get("max_steps", 1000)) is not int
            or not 1 <= config.get("max_steps", 1000) <= 100_000
        ):
            raise ValueError("Training max_steps must be in [1,100000]")
    return config


class ModernBertTrainer:
    """Explicit CPU training; an ONNX inference bundle is never a checkpoint.

    Each JSONL row contains ``state``, one Laya ``question``, an integer
    ``target`` option index and ``action`` (0=act, 1=escalate). Evaluation rows
    must be disjoint from training rows. A quality policy supplies independent
    minimum decision and action accuracies; passing it does not approve
    autonomous actions or establish domain calibration.
    """

    def __init__(self, checkpoint: TrainingBundle) -> None:
        from . import _training as exporter
        from .training import TrainingBundle

        if not isinstance(checkpoint, TrainingBundle):
            raise TypeError("TrainingBundle.open/fetch is required")
        self.checkpoint = TrainingBundle.open(checkpoint.path)

        self._exporter = exporter
        self._torch = exporter.torch
        self._torch.set_num_threads(min(self._torch.get_num_threads(), 4))
        self.agent = exporter.Agent(str(self.checkpoint.path), device="cpu")
        self._closed = False

    def __enter__(self) -> ModernBertTrainer:
        return self

    def __exit__(self, *_: Any) -> None:
        self.close()

    def close(self) -> None:
        """Release the CPU model and its native tensor storage."""
        if not self._closed:
            self.agent = None
            gc.collect()
            self._closed = True

    def _batch(self, row: dict):
        if self._closed:
            raise RuntimeError("Trainer is closed")
        batch = self._exporter.make_batch(
            self.agent, row["state"], {"sample": row["question"]}
        )
        options = int(batch[3][0].sum())
        if row["target"] >= options:
            raise ValueError("Decision target exceeds the number of options")
        return batch

    def _checkpoint(self, directory: Path, *, parent: Path) -> None:
        directory.mkdir(parents=True)
        exporter = self._exporter
        exporter.save_file(
            self.agent.model.state_dict(), str(directory / "model.safetensors")
        )
        (directory / "rl_agent_config.json").write_text(
            json.dumps(self.agent.cfg, indent=2)
        )
        self.agent.model.encoder.config.save_pretrained(directory / "encoder")
        # The tokenizer is frozen while training. Saving it with Transformers can
        # rewrite tokenizer_config.json to a form Laya mutates on the next load;
        # retain the verified parent files so the new checkpoint stays immutable.
        shutil.copytree(parent / "tokenizer", directory / "tokenizer")
        shutil.copyfile(parent / "LICENSE.txt", directory / "LICENSE.txt")
        from .training import TrainingBundle, _sha256

        files = {
            item.relative_to(directory).as_posix(): {
                "bytes": item.stat().st_size,
                "sha256": _sha256(item),
            }
            for item in sorted(directory.rglob("*"))
            if item.is_file() and item.name != "LICENSE.txt"
        }
        source = json.loads((parent / "PROVENANCE.json").read_text())
        metadata = {
            "schema_version": 1,
            "kind": "training",
            "format": "safetensors",
            "model": source["model"],
            "revision": _sha256(directory / "model.safetensors"),
            "parent_revision": source["revision"],
            "license": source.get("license", "Apache-2.0"),
            "files": files,
        }
        (directory / "PROVENANCE.json").write_text(
            json.dumps(metadata, indent=2) + "\n"
        )
        TrainingBundle.open(directory)

    def _prepare(
        self,
        *,
        checkpoint: Path,
        eval_rows: list[dict],
        policy: dict,
        destination: Path,
    ) -> TrainingResult:
        if self._closed:
            raise RuntimeError("Trainer is closed")
        model = self.agent.model
        model.eval()
        torch = self._torch
        sample = None
        for row in eval_rows:
            candidate = self._batch(row)
            if candidate[2].shape[1] >= 2:
                sample = candidate
                break
        if sample is None:
            raise ValueError(
                "FP32 export requires a question with at least two options"
            )
        inference = destination / "inference"
        self._exporter.export_model(self.agent, inference, sample)
        shutil.copyfile(checkpoint / "LICENSE.txt", inference / "LICENSE.txt")
        source = json.loads((checkpoint / "PROVENANCE.json").read_text())
        from .training import _sha256

        files = {
            item.relative_to(inference).as_posix(): {
                "bytes": item.stat().st_size,
                "sha256": _sha256(item),
            }
            for item in sorted(inference.rglob("*"))
            if item.is_file() and item.name != "LICENSE.txt"
        }
        metadata = {
            "schema_version": 1,
            "kind": "inference",
            "format": "onnx",
            "precision": "fp32",
            "model": source["model"],
            "revision": source["revision"],
            "license": source.get("license", "Apache-2.0"),
            "files": files,
        }
        (inference / "PROVENANCE.json").write_text(
            json.dumps(metadata, indent=2) + "\n"
        )
        onnx = self._exporter.onnx
        graph = onnx.load(str(inference / "model.onnx"), load_external_data=False)
        if [output.name for output in graph.graph.output] != [
            "logits",
            "act_logits",
        ] or any(
            output.type.tensor_type.elem_type != onnx.TensorProto.FLOAT
            for output in graph.graph.output
        ):
            raise ValueError("FP32 decision graph has incompatible output heads")
        runtime = self._exporter.OnnxGraph(inference / "model.onnx")
        decision_correct = action_correct = 0
        largest_error = 0.0
        with torch.no_grad():
            for row in eval_rows:
                batch = self._batch(row)
                expected = model(*batch)
                actual = runtime(*batch)
                for native, onnx_value in zip(expected, actual, strict=True):
                    error = (native - onnx_value).abs()
                    largest_error = max(largest_error, float(error.max()))
                    if not torch.allclose(native, onnx_value, rtol=1e-4, atol=1e-2):
                        raise ValueError(
                            "FP32 export disagrees with the training checkpoint"
                        )
                decision_correct += int(actual[0][0].argmax().item() == row["target"])
                action_correct += int(actual[1][0].argmax().item() == row["action"])
        del runtime, graph
        count = len(eval_rows)
        metrics = {
            "examples": count,
            "decision_accuracy": decision_correct / count,
            "action_accuracy": action_correct / count,
            "max_abs_logit_error": largest_error,
            "quality_policy": policy,
            "status": "evaluated_not_approved_for_autonomous_actions",
            "checkpoint_revision": source["revision"],
        }
        if (
            metrics["decision_accuracy"] < policy["min_decision_accuracy"]
            or metrics["action_accuracy"] < policy["min_action_accuracy"]
        ):
            raise ValueError(
                "Held-out decision/action evaluation failed the quality policy"
            )
        report = destination / "validation_report.json"
        report.write_text(json.dumps(metrics, indent=2) + "\n")
        return TrainingResult(checkpoint, inference, report)

    def prepare_fp32(
        self,
        *,
        eval_data: str | Path,
        validation_policy: str | Path,
        output_dir: str | Path,
        progress: Callable[[dict], None] | None = None,
    ) -> TrainingResult:
        """Export, reopen in ORT and gate both heads with separate labeled data."""
        if progress:
            progress({"stage": "loading"})
        rows = _examples(eval_data)
        policy = _config(validation_policy, kind="quality policy")
        target = Path(output_dir).expanduser().resolve()
        if target.exists():
            raise FileExistsError(target)
        target.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(
            prefix=".decision-export-", dir=target.parent
        ) as temporary:
            prepared = Path(temporary) / "prepared"
            prepared.mkdir()
            if progress:
                progress({"stage": "exporting"})
            self._prepare(
                checkpoint=self.checkpoint.path,
                eval_rows=rows,
                policy=policy,
                destination=prepared,
            )
            prepared.rename(target)
            if progress:
                progress({"stage": "validated"})
        return TrainingResult(
            self.checkpoint.path,
            target / "inference",
            target / "validation_report.json",
        )

    def finetune(
        self,
        *,
        train_data: str | Path,
        eval_data: str | Path,
        training_config: str | Path,
        validation_policy: str | Path,
        output_dir: str | Path,
        progress: Callable[[dict], None] | None = None,
    ) -> TrainingResult:
        """Train both heads, persist a resumable checkpoint and prepare FP32."""
        rows = _examples(train_data)
        evaluation = _examples(eval_data)
        if {json.dumps(row, sort_keys=True) for row in rows} & {
            json.dumps(row, sort_keys=True) for row in evaluation
        }:
            raise ValueError("Training and evaluation examples must be disjoint")
        config = _config(training_config, kind="training configuration")
        policy = _config(validation_policy, kind="quality policy")
        target = Path(output_dir).expanduser().resolve()
        if target.exists():
            raise FileExistsError(target)
        target.mkdir(parents=True)
        model = self.agent.model
        torch = self._torch
        torch.manual_seed(config.get("seed", 42))
        train_encoder = config.get("train_encoder", False)
        for parameter in model.encoder.parameters():
            parameter.requires_grad_(train_encoder)
        parameters = [
            parameter for parameter in model.parameters() if parameter.requires_grad
        ]
        optimizer = torch.optim.AdamW(parameters, lr=config["learning_rate"])
        losses = []
        model.train()
        try:
            for _ in range(config["epochs"]):
                for row in rows:
                    if len(losses) >= config.get("max_steps", 1000):
                        break
                    batch = self._batch(row)
                    optimizer.zero_grad(set_to_none=True)
                    logits, action_logits = model(*batch)
                    loss = torch.nn.functional.cross_entropy(
                        logits, torch.tensor([row["target"]])
                    ) + torch.nn.functional.cross_entropy(
                        action_logits, torch.tensor([row["action"]])
                    )
                    if not torch.isfinite(loss):
                        raise ValueError("Training produced a non-finite loss")
                    loss.backward()
                    torch.nn.utils.clip_grad_norm_(
                        parameters, 1.0, error_if_nonfinite=True
                    )
                    optimizer.step()
                    losses.append(float(loss.detach()))
                    if progress and (
                        len(losses) == 1
                        or len(losses) % max(1, config.get("max_steps", 1000) // 100)
                        == 0
                    ):
                        progress(
                            {
                                "stage": "training",
                                "step": len(losses),
                                "max_steps": min(
                                    config.get("max_steps", 1000),
                                    len(rows) * config["epochs"],
                                ),
                            }
                        )
                if len(losses) >= config.get("max_steps", 1000):
                    break
            optimizer.zero_grad(set_to_none=True)
            model.eval()
            checkpoint = target / "checkpoint"
            self._checkpoint(checkpoint, parent=self.checkpoint.path)
            if progress:
                progress({"stage": "checkpoint_saved"})
            (target / "training_report.json").write_text(
                json.dumps(
                    {
                        "steps": len(losses),
                        "initial_loss": losses[0],
                        "final_loss": losses[-1],
                        "status": "checkpoint_saved",
                    },
                    indent=2,
                )
                + "\n"
            )
            if progress:
                progress({"stage": "exporting"})
            result = self._prepare(
                checkpoint=checkpoint,
                eval_rows=evaluation,
                policy=policy,
                destination=target,
            )
            if progress:
                progress({"stage": "validated"})
            return result
        except Exception:
            shutil.rmtree(target / "inference", ignore_errors=True)
            (target / "validation_report.json").unlink(missing_ok=True)
            raise
        finally:
            optimizer.zero_grad(set_to_none=True)
            del optimizer
            gc.collect()


__all__ = ["ModernBertTrainer", "TrainingResult"]
