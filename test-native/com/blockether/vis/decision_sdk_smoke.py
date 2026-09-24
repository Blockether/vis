"""Exercise the installed Python SDK against an isolated native gateway."""

from __future__ import annotations

import json
import sys
from hashlib import sha256
from io import BytesIO
from pathlib import Path
from tempfile import TemporaryDirectory

from blockether.vis.decisions import Decisions
from blockether.vis.engine import GatewayClient, GatewayError


def train(checkpoint: Path, root: Path):
    from blockether.vis.decisions.training import ModernBertTrainer, TrainingBundle

    row = {
        "state": "Please refund my damaged purchase.",
        "question": {
            "type": "choice",
            "instructions": "Choose intent",
            "criteria": ["refund", "repair"],
        },
        "target": 0,
        "action": 0,
    }
    evaluation = [
        {**row, "state": "A different damaged item needs a refund."},
        {
            "state": "Repeated billing errors block the account.",
            "question": {
                "type": "score",
                "instructions": "Rate urgency",
                "criteria": ["low", "medium", "high"],
            },
            "target": 2,
            "action": 1,
        },
        {
            "state": "My delivery is still missing.",
            "question": {"type": "noul", "instructions": "Is it missing?"},
            "target": 1,
            "action": 0,
        },
    ]
    (root / "train.jsonl").write_text(json.dumps(row) + "\n")
    (root / "eval.jsonl").write_text(
        "".join(json.dumps(example) + "\n" for example in evaluation)
    )
    (root / "config.json").write_text(
        json.dumps(
            {"epochs": 1, "learning_rate": 1e-5, "train_encoder": False, "max_steps": 1}
        )
    )
    (root / "policy.json").write_text(
        json.dumps({"min_decision_accuracy": 0.0, "min_action_accuracy": 0.0})
    )
    with ModernBertTrainer(TrainingBundle.open(checkpoint)) as trainer:
        result = trainer.finetune(
            train_data=root / "train.jsonl",
            eval_data=root / "eval.jsonl",
            training_config=root / "config.json",
            validation_policy=root / "policy.json",
            output_dir=root / "result",
        )
    assert result.checkpoint_dir.is_dir()
    return result


def main(url: str, inference: str, checkpoint: str) -> None:
    with TemporaryDirectory(prefix="vis-native-decision-sdk-") as directory:
        source = (
            Path(inference)
            if checkpoint == "-"
            else train(Path(checkpoint), Path(directory))
        )
        with GatewayClient(url, timeout=900) as gateway:
            decisions = Decisions(gateway)
            assert decisions.list_models()[0]["installed"]
            invalid = b"not a decision archive"
            try:
                gateway.post_decision_model(
                    content=BytesIO(invalid),
                    sha256=sha256(invalid).hexdigest(),
                    length=len(invalid),
                    timeout=30,
                )
            except GatewayError as error:
                assert error.status == 400
            else:
                raise AssertionError("invalid model archive was accepted")
            milestone = -1

            def progress(sent: int, total: int) -> None:
                nonlocal milestone
                current = sent // (128 * 1024 * 1024)
                if current != milestone or sent == total:
                    milestone = current
                    print(f"VIS_DECISION_UPLOAD_BYTES={sent}/{total}", flush=True)

            published = decisions.upload_model(source, progress=progress, timeout=900)
            ref = published["model_ref"]
            assert decisions.get_model(ref)["installed"]
            decisions.activate_model("sdk-native", ref)
            assert decisions.get_alias("sdk-native")["model_ref"] == ref
            questions = {
                "intent": {
                    "type": "choice",
                    "instructions": "Choose intent",
                    "criteria": ["refund", "repair"],
                },
                "priority": {
                    "type": "score",
                    "instructions": "Rate urgency",
                    "criteria": ["low", "medium", "high"],
                },
                "policy": {"type": "noul", "instructions": "Is this refundable?"},
            }
            answer = decisions.infer(
                model="sdk-native", state="broken item refund", questions=questions
            )
            baseline = decisions.infer(
                model="laya-typed-decisions",
                state="broken item refund",
                questions=questions,
            )
            print(
                "VIS_DECISION_RESULT="
                + json.dumps(
                    {
                        "ref": ref,
                        "routing": answer["routing"]["model_ref"],
                        "baseline": baseline["routing"]["model"],
                        "trained": checkpoint != "-",
                        "choice": answer["answers"]["intent"]["choice"],
                        "score": answer["answers"]["priority"]["score"],
                        "noul": answer["answers"]["policy"]["noul"],
                        "action": answer["answers"]["intent"]["action"],
                    }
                )
            )


if __name__ == "__main__":
    main(*sys.argv[1:])
