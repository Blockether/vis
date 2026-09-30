"""Offline GLiNER decision/action labels, held-out evaluation and FP32 training.

Only constructing the trainer imports tensor dependencies. JSONL rows and their
contents never enter provenance, reports or upload archives. A partial checkpoint
records only a digest of its rows and settings, so that the same run can resume.
"""

from __future__ import annotations

import gc
import json
import math
import random
import shutil
import tempfile
from collections.abc import Callable, Iterator
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

from ._trainer import (
    TrainingResult,
    _config,
    _export_fp32,
    _fingerprint,
    _publish_checkpoint,
    _quality_report,
)
from .training import _sha256

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
        "checkpoint_steps",
    }:
        raise ValueError("Unknown GLiNER training configuration option")
    for name, lower, upper in (
        ("epochs", 1, 100),
        ("max_steps", 1, 100_000),
        ("batch_size", 1, 32),
        ("checkpoint_steps", 1, 100_000),
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


class _Schedule:
    """Seeded batch order; a resumed run skips the batches that it already trained."""

    def __init__(self, count: int, config: dict) -> None:
        self.count = count
        self.batch = min(config.get("batch_size", 1), count)
        # Like gliner2, drop an incomplete batch at the end of each pass.
        self.per_pass = count // self.batch
        self.total = config.get("max_steps", self.per_pass * config.get("epochs", 1))
        self.seed = config.get("seed", 42)
        self.done = 0

    def __len__(self) -> int:
        return (self.total - self.done) * self.batch

    def __iter__(self) -> Iterator[int]:
        generator = random.Random(self.seed)
        start, stop = self.done * self.batch, self.total * self.batch
        position = 0
        while position < stop:
            order = list(range(self.count))
            generator.shuffle(order)
            for index in order[: self.per_pass * self.batch]:
                if start <= position < stop:
                    yield index
                position += 1


def _extractor(
    base: type, schedule: _Schedule, after_step: Callable[[Any, dict], None]
) -> type:
    """Adapt the pinned gliner2 2.0.0 trainer to the Vis batch order and step hook."""

    class Extractor(base):
        def _create_dataloader(
            self,
            dataset: Any,
            batch_size: int,
            shuffle: bool = True,
            is_training: bool = True,
        ) -> Any:
            loader = super()._create_dataloader(
                dataset, batch_size, shuffle=False, is_training=is_training
            )
            if not is_training:
                return loader
            return type(loader)(
                loader.dataset,
                batch_size=schedule.batch,
                sampler=schedule,
                collate_fn=loader.collate_fn,
            )

        def _log_metrics(self, metrics: Any, prefix: str = "") -> None:
            super()._log_metrics(metrics, prefix)
            if prefix == "train":
                after_step(
                    self, metrics if isinstance(metrics, dict) else metrics.to_dict()
                )

        def train(self, *args: Any, **kwargs: Any) -> Any:
            # gliner2 never removes its per-parameter gradient hooks. Each hook
            # captures the trainer through autograd state that gc cannot see, so
            # the weights, gradients and optimizer state would stay resident.
            try:
                return super().train(*args, **kwargs)
            finally:
                for handle in self._finite_grad_hook_handles:
                    handle.remove()
                self._finite_grad_hook_handles = []

    return Extractor


class GlinerTrainer:
    """Explicit local two-head training. A validated ONNX bundle is never a checkpoint."""

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
        report = _quality_report(
            destination,
            policy,
            revision=metadata["revision"],
            examples=len(rows),
            decisions=correct_decisions,
            actions=correct_actions,
            largest_error=largest_error,
        )
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
        return _export_fp32(
            output_dir,
            checkpoint=self.checkpoint.path,
            prefix=".gliner-prepare-",
            prepare=lambda prepared: self._prepare(
                self.checkpoint.path, prepared, rows, policy
            ),
            progress=progress,
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
        """Train both labels, keep resumable checkpoints and publish validated FP32.

        ``progress`` receives the step, ``max_steps``, epoch and loss about once per
        percent of the run. With ``checkpoint_steps``, ``output_dir/checkpoint`` keeps
        the latest partial checkpoint when training fails or stops. Training that
        checkpoint with the same rows and settings resumes at its saved step.
        """
        if self._closed:
            raise RuntimeError("Trainer is closed")
        from gliner2.training.data import Classification, InputExample
        from gliner2.training.trainer import (
            ExtractorDataset,
            ExtractorTrainer,
            TrainingConfig,
        )

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

        GlinerTrainingBundle.open(self.checkpoint.path)
        original = json.loads(
            (self.checkpoint.path / "PROVENANCE.json").read_text(encoding="utf-8")
        )
        dataset = ExtractorDataset(
            [
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
            ],
            shuffle=False,
            validate=True,
        )
        if len(dataset) != len(rows):
            raise ValueError("GLiNER rejected a training example")
        fingerprint = _fingerprint(
            {
                "epochs": config.get("epochs", 1),
                "max_steps": config.get("max_steps"),
                "batch_size": config.get("batch_size", 1),
                "encoder_lr": config["encoder_lr"],
                "task_lr": config["task_lr"],
                "seed": config.get("seed", 42),
            },
            ([row.text, row.tasks, row.target, row.action] for row in rows),
        )
        schedule = _Schedule(len(dataset), config)
        if schedule.total > 100_000:
            raise ValueError("GLiNER training must plan at most 100000 steps")
        partial = original.get("partial")
        if (
            partial
            and partial["fingerprint"] == fingerprint
            and partial["max_steps"] == schedule.total
        ):
            schedule.done = partial["step"]
        every = config.get("checkpoint_steps")
        interval = max(1, schedule.total // 100)
        with tempfile.TemporaryDirectory(
            prefix=".gliner-train-", dir=target.parent
        ) as temporary:
            staging = Path(temporary)

            def publish(saved: Path, step: int) -> Path:
                pending = staging / "pending"
                GlinerTrainingBundle.from_local(
                    saved,
                    pending,
                    model_id=self.checkpoint.model_id,
                    revision=_sha256(saved / "model.safetensors"),
                    license_file=self.checkpoint.path / "LICENSE.txt",
                    parent_revision=original["revision"],
                    partial=None
                    if step == schedule.total
                    else {
                        "step": step,
                        "max_steps": schedule.total,
                        "fingerprint": fingerprint,
                    },
                )
                shutil.rmtree(saved)
                return _publish_checkpoint(
                    pending,
                    target,
                    {"steps": step, "max_steps": schedule.total},
                    progress,
                )

            def after_step(extractor: Any, metrics: dict) -> None:
                step = schedule.done + extractor.global_step
                loss = float(metrics["loss"])
                if not math.isfinite(loss):
                    raise ValueError(
                        "GLiNER training did not complete with finite loss"
                    )
                if progress and (
                    step == schedule.done + 1
                    or step % interval == 0
                    or step == schedule.total
                ):
                    progress(
                        {
                            "stage": "training",
                            "step": step,
                            "max_steps": schedule.total,
                            "epoch": round(step / schedule.per_pass, 4),
                            "loss": loss,
                        }
                    )
                if every and step % every == 0 and step < schedule.total:
                    extractor._save_checkpoint(f"checkpoint-{step}")
                    publish(extractor.output_dir / f"checkpoint-{step}", step)

            model = self._exporter.load_checkpoint(
                self.checkpoint.path, model_id=self.checkpoint.model_id
            )
            self.model = model
            training = TrainingConfig(
                output_dir=str(staging / "training"),
                num_epochs=1,
                max_steps=schedule.total - schedule.done,
                batch_size=schedule.batch,
                num_workers=0,
                encoder_lr=config["encoder_lr"],
                task_lr=config["task_lr"],
                seed=config.get("seed", 42),
                eval_strategy="no",
                save_best=False,
                scheduler_type="constant",
                logging_steps=1,
                fp16=False,
                bf16=False,
                report_to_wandb=False,
            )
            try:
                if progress:
                    progress(
                        {
                            "stage": "training",
                            "step": schedule.done,
                            "max_steps": schedule.total,
                        }
                    )
                trainer = _extractor(ExtractorTrainer, schedule, after_step)
                summary = trainer(model, training).train(dataset)
                if summary["total_steps"] != schedule.total - schedule.done:
                    raise ValueError("GLiNER training stopped before its planned steps")
                if any(
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
            final = staging / "training" / "final"
            if not (final / "model.safetensors").is_file():
                raise FileNotFoundError("GLiNER training did not save full weights")
            checkpoint = publish(final, schedule.total)
            if progress:
                progress({"stage": "exporting"})
            prepared = staging / "prepared"
            prepared.mkdir()
            self._prepare(checkpoint, prepared, evaluation, policy)
            (prepared / "inference").rename(target / "inference")
            (prepared / "validation_report.json").rename(
                target / "validation_report.json"
            )
        if progress:
            progress({"stage": "validated"})
        return TrainingResult(
            target / "checkpoint",
            target / "inference",
            target / "validation_report.json",
        )


__all__ = ["GlinerTrainer"]
