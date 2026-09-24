"""Offline GLiNER decision/action labels, held-out evaluation and FP32 training.

Only constructing the trainer imports tensor dependencies. JSONL rows and their
contents never enter provenance, reports or upload archives.
"""

from __future__ import annotations

import gc
import json
import math
import tempfile
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

from ._trainer import TrainingResult, _config

if TYPE_CHECKING:
    from .gliner_training import GlinerTrainingBundle


@dataclass(frozen=True)
class _Example:
    text: str
    tasks: dict[str, list[str]]
    target: int
    action: int


def _text(value: Any) -> str:
    return value if isinstance(value, str) else json.dumps(value, ensure_ascii=False)


def _example(row: dict) -> _Example:
    if (
        not isinstance(row, dict)
        or not {"state", "question", "target", "action"} <= row.keys()
    ):
        raise ValueError("Both decision and action labels are required")
    question = row["question"]
    if not isinstance(question, dict):
        raise ValueError("Decision question must be an object")
    qtype = question.get("type")
    instruction = question.get("instructions")
    if (
        qtype not in ("choice", "score", "noul")
        or instruction is None
        or len(_text(instruction)) > 4096
    ):
        raise ValueError("Decision question needs a valid type and instructions")
    criteria = question.get("criteria")
    if qtype == "choice":
        if isinstance(criteria, dict):
            choices = list(criteria.items())
        elif isinstance(criteria, list):
            choices = [(label, None) for label in criteria]
        else:
            raise ValueError("Choice criteria must contain labeled options")
        if not 1 <= len(choices) <= 64 or any(
            not isinstance(label, str) or not label.strip() for label, _ in choices
        ):
            raise ValueError("Choice criteria must contain 1–64 text labels")
        labels = [
            label
            if description is None or description == ""
            else f"{label}: {_text(description)}"
            for label, description in choices
        ]
    elif qtype == "score":
        if not isinstance(criteria, list) or not 1 <= len(criteria) <= 64:
            raise ValueError("Score criteria must contain 1–64 levels")
        labels = [
            f"level {i}: {_text(description)}" for i, description in enumerate(criteria)
        ]
    else:
        if criteria is not None and not isinstance(criteria, dict):
            raise ValueError("Noul criteria must be an object")
        criteria = criteria or {}
        labels = [
            f"{label}: {_text(criteria[label]) if criteria.get(label) is not None and criteria[label] != '' else fallback}"
            for label, fallback in (
                ("false", "no, the statement does not hold"),
                ("true", "yes, the statement holds"),
            )
        ]
    state = row["state"]
    if not isinstance(state, (str, dict, list)):
        raise ValueError("Decision state must be text, an object or a list")
    text = _text(state)
    if not text.endswith((".", "!", "?")):
        text += "."
    if not text.strip() or any(not label.strip() for label in labels):
        raise ValueError("Decision state and labels cannot be blank")
    target, action = row["target"], row["action"]
    if (
        type(target) is not int
        or not 0 <= target < len(labels)
        or type(action) is not int
        or action not in (0, 1)
    ):
        raise ValueError("Decision target exceeds the options or action is invalid")
    task = f"{qtype}: {_text(instruction)}"
    return _Example(text, {task: labels, "action": ["act", "escalate"]}, target, action)


def _examples(source: str | Path) -> list[_Example]:
    path = Path(source)
    if not path.is_file():
        raise FileNotFoundError(f"Labeled examples are missing: {path}")
    rows = []
    with path.open(encoding="utf-8") as stream:
        for number, line in enumerate(stream, 1):
            if not line.strip():
                continue
            try:
                rows.append(_example(json.loads(line)))
            except (ValueError, TypeError) as error:
                raise ValueError(
                    f"Invalid labeled example at line {number}: {error}"
                ) from None
    if not rows:
        raise ValueError("Labeled examples cannot be empty")
    return rows


def _training_config(source: str | Path) -> dict:
    config = json.loads(Path(source).read_text(encoding="utf-8"))
    if not isinstance(config, dict) or set(config) - {
        "epochs",
        "max_steps",
        "batch_size",
        "encoder_lr",
        "task_lr",
        "seed",
    }:
        raise ValueError("Unknown GLiNER training configuration option")
    for name, lower, upper in (
        ("epochs", 1, 100),
        ("max_steps", 1, 100_000),
        ("batch_size", 1, 32),
    ):
        value = config.get(name, 1)
        if type(value) is not int or not lower <= value <= upper:
            raise ValueError(f"Training {name} must be in [{lower},{upper}]")
    for name in ("encoder_lr", "task_lr"):
        value = config.get(name)
        if (
            type(value) not in (int, float)
            or not math.isfinite(value)
            or not 0 < value <= 0.01
        ):
            raise ValueError(f"Training {name} must be in (0,0.01]")
    if type(config.get("seed", 42)) is not int:
        raise ValueError("Training seed must be an integer")
    return config


class GlinerTrainer:
    """Explicit local two-head training; a validated ONNX bundle is never a checkpoint."""

    def __init__(self, checkpoint: GlinerTrainingBundle) -> None:
        from .gliner_training import GlinerTrainingBundle

        if not isinstance(checkpoint, GlinerTrainingBundle):
            raise TypeError("GlinerTrainingBundle.open/fetch/from_local is required")
        self.checkpoint = GlinerTrainingBundle.open(checkpoint.path)

        from . import _gliner as exporter

        self._exporter = exporter
        self._torch = exporter.torch
        self._torch.set_num_threads(min(self._torch.get_num_threads(), 4))
        self._closed = False
        self.model = None

    def __enter__(self) -> GlinerTrainer:
        return self

    def __exit__(self, *_: Any) -> None:
        self.close()

    def close(self) -> None:
        """Release the model and its native tensor storage."""
        self.model = None
        gc.collect()
        self._closed = True

    def _prepare(
        self, checkpoint: Path, destination: Path, rows: list[_Example], policy: dict
    ) -> TrainingResult:
        if self._closed:
            raise RuntimeError("Trainer is closed")
        from .gliner_training import GlinerTrainingBundle

        GlinerTrainingBundle.open(checkpoint)
        metadata = json.loads(
            (checkpoint / "PROVENANCE.json").read_text(encoding="utf-8")
        )
        model_id = metadata["model"]
        inference = destination / "inference"
        parity = self._exporter.prepare_fp32(
            checkpoint,
            inference,
            model_id=model_id,
            license_file=checkpoint / "LICENSE.txt",
            revision=metadata["revision"],
        )
        model = self._exporter.load_checkpoint(checkpoint, model_id=model_id)
        options = self._exporter.ort.SessionOptions()
        options.intra_op_num_threads = 4
        runtime = self._exporter.ort.InferenceSession(
            str(inference / "model.onnx"),
            sess_options=options,
            providers=["CPUExecutionProvider"],
        )
        correct_decisions = correct_actions = 0
        largest_error = parity["max_abs_logit_error"]
        try:
            for row in rows:
                arguments = self._exporter.make_batch(model, row.text, row.tasks)
                with self._torch.inference_mode():
                    expected = (
                        self._exporter.DecisionGraph(model)(*arguments).cpu().numpy()
                    )
                actual = runtime.run(
                    None,
                    {
                        name: value.numpy()
                        for name, value in zip(
                            self._exporter.INPUT_NAMES, arguments, strict=True
                        )
                    },
                )[0]
                if (
                    actual.shape != expected.shape
                    or not self._exporter.np.isfinite(actual).all()
                    or not self._exporter.np.allclose(
                        expected, actual, rtol=1e-4, atol=1e-3
                    )
                ):
                    raise ValueError(
                        "FP32 export disagrees with the held-out checkpoint"
                    )
                largest_error = max(
                    largest_error,
                    float(
                        self._exporter.np.max(self._exporter.np.abs(expected - actual))
                    ),
                )
                choices = len(next(iter(row.tasks.values())))
                correct_decisions += int(actual[0, :choices].argmax() == row.target)
                correct_actions += int(actual[0, choices:].argmax() == row.action)
        finally:
            del runtime, model
            gc.collect()
        count = len(rows)
        metrics = {
            "examples": count,
            "decision_accuracy": correct_decisions / count,
            "action_accuracy": correct_actions / count,
            "max_abs_logit_error": largest_error,
            "quality_policy": policy,
            "checkpoint_revision": metadata["revision"],
            "status": "evaluated_not_approved_for_autonomous_actions",
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
        """Export an existing full checkpoint and gate both heads on held-out rows."""
        if self._closed:
            raise RuntimeError("Trainer is closed")
        rows = _examples(eval_data)
        policy = _config(validation_policy, kind="quality policy")
        target = Path(output_dir).expanduser().resolve()
        if target.exists():
            raise FileExistsError(target)
        target.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory(
            prefix=".gliner-prepare-", dir=target.parent
        ) as temporary:
            prepared = Path(temporary) / "prepared"
            prepared.mkdir()
            if progress:
                progress({"stage": "exporting"})
            self._prepare(self.checkpoint.path, prepared, rows, policy)
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
        """Train both labels, save a complete checkpoint and atomically publish FP32."""
        if self._closed:
            raise RuntimeError("Trainer is closed")
        from gliner2.training.data import Classification, InputExample
        from gliner2.training.trainer import ExtractorTrainer, TrainingConfig

        rows, evaluation = _examples(train_data), _examples(eval_data)

        def identity(row: _Example) -> tuple:
            return (
                row.text,
                tuple((task, tuple(labels)) for task, labels in row.tasks.items()),
            )

        if {identity(row) for row in rows} & {identity(row) for row in evaluation}:
            raise ValueError("Training and evaluation examples must be disjoint")
        config = _training_config(training_config)
        policy = _config(validation_policy, kind="quality policy")
        target = Path(output_dir).expanduser().resolve()
        if target.exists():
            raise FileExistsError(target)
        target.parent.mkdir(parents=True, exist_ok=True)
        from .gliner_training import GlinerTrainingBundle

        with tempfile.TemporaryDirectory(
            prefix=".gliner-train-", dir=target.parent
        ) as temporary:
            staging = Path(temporary)
            GlinerTrainingBundle.open(self.checkpoint.path)
            model = self._exporter.load_checkpoint(
                self.checkpoint.path, model_id=self.checkpoint.model_id
            )
            self.model = model
            examples = [
                InputExample(
                    text=row.text,
                    classifications=[
                        Classification(
                            task=task,
                            labels=labels,
                            true_label=labels[row.target]
                            if name == 0
                            else labels[row.action],
                        )
                        for name, (task, labels) in enumerate(row.tasks.items())
                    ],
                )
                for row in rows
            ]
            training = TrainingConfig(
                output_dir=str(staging / "training"),
                num_epochs=config.get("epochs", 1),
                max_steps=config.get("max_steps", -1),
                batch_size=config.get("batch_size", 1),
                num_workers=0,
                encoder_lr=config["encoder_lr"],
                task_lr=config["task_lr"],
                seed=config.get("seed", 42),
                eval_strategy="no",
                save_best=False,
                scheduler_type="constant",
                fp16=False,
                bf16=False,
                report_to_wandb=False,
            )
            try:
                if progress:
                    progress({"stage": "training"})
                summary = ExtractorTrainer(model, training).train(examples)
                if summary["total_steps"] < 1 or any(
                    not math.isfinite(row["classification_loss"])
                    for row in summary["train_metrics_history"]
                ):
                    raise ValueError(
                        "GLiNER training did not complete with finite loss"
                    )
            finally:
                self.model = None
                del model
                gc.collect()
            original = json.loads(
                (self.checkpoint.path / "PROVENANCE.json").read_text(encoding="utf-8")
            )
            trained = staging / "training" / "final"
            if not (trained / "model.safetensors").is_file():
                raise FileNotFoundError("GLiNER training did not save full weights")
            revision = self._exporter._digest(trained / "model.safetensors")
            prepared = staging / "prepared"
            prepared.mkdir()
            checkpoint = GlinerTrainingBundle.from_local(
                trained,
                prepared / "checkpoint",
                model_id=self.checkpoint.model_id,
                revision=revision,
                license_file=self.checkpoint.path / "LICENSE.txt",
                parent_revision=original["revision"],
            ).path
            if progress:
                progress({"stage": "checkpoint_saved"})
            self._prepare(checkpoint, prepared, evaluation, policy)
            (prepared / "training_report.json").write_text(
                json.dumps(
                    {
                        "steps": summary["total_steps"],
                        "status": "checkpoint_saved",
                    },
                    indent=2,
                )
                + "\n"
            )
            prepared.rename(target)
        if progress:
            progress({"stage": "validated"})
        return TrainingResult(
            target / "checkpoint",
            target / "inference",
            target / "validation_report.json",
        )


__all__ = ["GlinerTrainer"]
