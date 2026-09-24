"""Private offline process entrypoint for gateway-owned decision training jobs.

The gateway supplies only local, staged paths in a small JSON file. This worker
never downloads a model or publishes a training checkpoint over HTTP.
"""

from __future__ import annotations

import json
import os
import sys
from pathlib import Path

from ._publication import package
from .training import ModernBertTrainer, TrainingBundle


def _progress(event: dict) -> None:
    """Emit bounded, non-sensitive progress on the worker's stdout."""
    safe = {
        name: event[name] for name in ("stage", "step", "max_steps") if name in event
    }
    print(json.dumps(safe, separators=(",", ":")), flush=True)


def run(spec_path: Path) -> None:
    """Train both heads, export FP32, and record a digest only after success."""
    spec = json.loads(spec_path.read_text(encoding="utf-8"))
    if set(spec) != {
        "checkpoint",
        "train_data",
        "eval_data",
        "training_config",
        "validation_policy",
        "output_dir",
        "archive",
        "result",
    }:
        raise ValueError("Invalid decision training job description")
    _progress({"stage": "loading"})
    bundle = TrainingBundle.open(spec["checkpoint"])
    _progress({"stage": "training"})
    with ModernBertTrainer(bundle) as trainer:
        result = trainer.finetune(
            train_data=spec["train_data"],
            eval_data=spec["eval_data"],
            training_config=spec["training_config"],
            validation_policy=spec["validation_policy"],
            output_dir=spec["output_dir"],
            progress=_progress,
        )
    _progress({"stage": "publishing"})
    digest, size = package(result.inference_bundle, Path(spec["archive"]))
    report = json.loads(result.validation_report.read_text(encoding="utf-8"))
    completed = {
        "sha256": digest,
        "bytes": size,
        "decision_accuracy": report["decision_accuracy"],
        "action_accuracy": report["action_accuracy"],
    }
    destination = Path(spec["result"])
    temporary = destination.with_suffix(".pending")
    temporary.write_text(json.dumps(completed) + "\n", encoding="utf-8")
    os.replace(temporary, destination)
    _progress({"stage": "completed"})


def main() -> int:
    if len(sys.argv) != 2:
        return 2
    try:
        run(Path(sys.argv[1]))
    except Exception as error:
        # Do not print private training rows or server-local paths in API-visible logs.
        _progress({"stage": "failed"})
        print(f"Decision training failed ({type(error).__name__})", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
