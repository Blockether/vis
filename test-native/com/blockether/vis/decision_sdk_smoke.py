"""Exercise the installed Python SDK against an isolated native gateway."""

from __future__ import annotations

import json
import sys
from hashlib import sha256
from io import BytesIO
from pathlib import Path
from tempfile import TemporaryDirectory

from blockether.vis._contracts import definition
from blockether.vis.engine import GatewayClient, GatewayError
from blockether.vis_decisions import Decisions, Trainer, TrainingBundle


def train(checkpoint: Path, root: Path):

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
    with Trainer(TrainingBundle.open(checkpoint)) as trainer:
        result = trainer.train(
            train_data=root / "train.jsonl",
            eval_data=root / "eval.jsonl",
            training_config=root / "config.json",
            validation_policy=root / "policy.json",
            output_dir=root / "result",
        )
    assert result.checkpoint_dir.is_dir()
    return result


def main(url: str, inference: str, checkpoint: str, model_id: str) -> None:
    with TemporaryDirectory(prefix="vis-native-decision-sdk-") as directory:
        source = (
            Path(inference)
            if checkpoint == "-"
            else train(Path(checkpoint), Path(directory))
        )
        with GatewayClient(url, timeout=900) as gateway:
            decisions = Decisions(gateway)
            if model_id == "laya-typed-decisions":
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
            upload_bytes = 0

            def progress(sent: int, total: int) -> None:
                nonlocal milestone, upload_bytes
                upload_bytes = total
                current = sent // (128 * 1024 * 1024)
                if current != milestone or sent == total:
                    milestone = current
                    print(f"VIS_DECISION_UPLOAD_BYTES={sent}/{total}", flush=True)

            published = decisions.upload_model(source, progress=progress, timeout=900)
            ref = published["model_ref"]
            assert decisions.get_model(ref)["installed"]
            alias = f"sdk-native-{model_id.replace('.', '-')}"
            try:
                decisions.get_alias(alias)
            except GatewayError as error:
                assert error.status == 404
            else:
                raise AssertionError("upload activated an alias")
            if model_id == "gliner2.5-decide":
                # #294: exercise the SDK with the real FP32 bundle above its old cap.
                assert 1_600_000_000 < upload_bytes <= 2_400_000_000
            if model_id == "gliner2.5-decide-1b":
                assert (
                    2**31
                    <= upload_bytes
                    <= definition("gateway", "decision_archive_bytes")["maximum"]
                )
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
            immutable = decisions.infer(
                model=ref, state="broken item refund", questions=questions
            )
            assert immutable["routing"]["model_ref"] == ref
            decisions.activate_model(alias, ref)
            assert decisions.get_alias(alias)["model_ref"] == ref
            answer = decisions.infer(
                model=alias, state="broken item refund", questions=questions
            )
            assert answer["answers"] == immutable["answers"]
            baseline = None
            if model_id == "laya-typed-decisions":
                baseline = decisions.infer(
                    model=model_id, state="broken item refund", questions=questions
                )["routing"]["model"]
            print(
                "VIS_DECISION_RESULT="
                + json.dumps(
                    {
                        "ref": ref,
                        "routing": answer["routing"]["model_ref"],
                        "model": model_id,
                        "upload_bytes": upload_bytes,
                        "baseline": baseline,
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
