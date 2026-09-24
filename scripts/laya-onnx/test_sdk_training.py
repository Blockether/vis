"""Offline SDK acceptance: local checkpoint → train → export → reopen FP32."""

import json
import os
import socket
import subprocess
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "packages/vis-agent/src"))

from blockether.vis.decisions.training import (  # noqa: E402
    ModernBertTrainer,
    TrainingBundle,
)


@pytest.fixture(scope="module")
def base():
    path = os.environ.get("VIS_LAYA_TRAINING_DIR")
    if not path:
        pytest.skip("Set VIS_LAYA_TRAINING_DIR to a verified complete checkpoint")
    return TrainingBundle.open(path)


@pytest.fixture
def labeled(tmp_path):
    data = [
        {
            "state": "Please refund my damaged purchase.",
            "question": {
                "type": "choice",
                "instructions": "Choose a request.",
                "criteria": ["refund", "repair"],
            },
            "target": 0,
            "action": 0,
        },
        {
            "state": "The account is blocked after repeated billing issues.",
            "question": {
                "type": "score",
                "instructions": "Rate urgency.",
                "criteria": ["low", "medium", "high"],
            },
            "target": 2,
            "action": 1,
        },
        {
            "state": "The parcel is still missing.",
            "question": {"type": "noul", "instructions": "Is the parcel missing?"},
            "target": 1,
            "action": 0,
        },
    ]
    eval_file = tmp_path / "eval.jsonl"
    eval_file.write_text("\n".join(json.dumps(row) for row in data) + "\n")
    train_file = tmp_path / "train.jsonl"
    train_file.write_text(
        json.dumps({**data[0], "state": "A second item arrived damaged."}) + "\n"
    )
    policy = tmp_path / "policy.json"
    policy.write_text(
        json.dumps({"min_decision_accuracy": 0.0, "min_action_accuracy": 0.0})
    )
    training_config = tmp_path / "config.json"
    training_config.write_text(
        json.dumps(
            {"epochs": 1, "learning_rate": 1e-5, "train_encoder": False, "max_steps": 1}
        )
    )
    return train_file, eval_file, policy, training_config


def _deny_network(*_args, **_kwargs):
    raise AssertionError("Offline Laya SDK validation attempted network access")


def test_prepare_fp32_without_network(base, labeled, tmp_path, monkeypatch):
    monkeypatch.setenv("HF_HUB_OFFLINE", "1")
    monkeypatch.setenv("TRANSFORMERS_OFFLINE", "1")
    monkeypatch.setattr(socket.socket, "connect", _deny_network)
    _, evaluation, policy, _ = labeled
    progress = []
    with ModernBertTrainer(base) as trainer:
        result = trainer.prepare_fp32(
            eval_data=evaluation,
            validation_policy=policy,
            output_dir=tmp_path / "prepared",
            progress=progress.append,
        )
    assert [event["stage"] for event in progress] == [
        "loading",
        "exporting",
        "validated",
    ]
    report = json.loads(result.validation_report.read_text())
    assert report["examples"] == 3
    assert 0 <= report["decision_accuracy"] <= 1
    assert report["status"] == "evaluated_not_approved_for_autonomous_actions"
    assert (result.inference_bundle / "model.onnx").is_file()
    assert (result.inference_bundle / "tokenizer/tokenizer.json").is_file()
    assert not (result.inference_bundle / "model.safetensors").exists()


def test_finetune_resume_and_reopen_fp32_without_network(
    base, labeled, tmp_path, monkeypatch
):
    monkeypatch.setenv("HF_HUB_OFFLINE", "1")
    monkeypatch.setenv("TRANSFORMERS_OFFLINE", "1")
    monkeypatch.setattr(socket.socket, "connect", _deny_network)
    train, evaluation, policy, config = labeled
    progress = []
    with ModernBertTrainer(base) as trainer:
        result = trainer.finetune(
            train_data=train,
            eval_data=evaluation,
            training_config=config,
            validation_policy=policy,
            output_dir=tmp_path / "finetuned",
            progress=progress.append,
        )
    assert [event["stage"] for event in progress] == [
        "training",
        "checkpoint_saved",
        "exporting",
        "validated",
    ]
    assert progress[0]["step"] == progress[0]["max_steps"] == 1
    assert result.checkpoint_dir != base.path
    resumed = TrainingBundle.open(result.checkpoint_dir)
    with ModernBertTrainer(resumed) as trainer:
        assert trainer.agent.model.encoder.config.model_type == "modernbert"
    TrainingBundle.open(
        resumed.path
    )  # Loading must not rewrite the verified checkpoint.
    assert (result.inference_bundle / "model.onnx").is_file()
    assert not (result.inference_bundle / "model.safetensors").exists()
    report = json.loads(result.validation_report.read_text())
    assert report["max_abs_logit_error"] < 0.5
    # A new interpreter sees only saved local files, not the trainer's tensors.
    process = subprocess.run(
        [
            sys.executable,
            "-c",
            "from blockether.vis.decisions.training import TrainingBundle; "
            "from blockether.vis.decisions._training import load_onnx; "
            "import sys; TrainingBundle.open(sys.argv[1]); "
            "load_onnx(sys.argv[2]).predict('Please refund my order.', "
            "{'request': {'type': 'choice', 'instructions': 'Choose a request.', "
            "'criteria': ['refund', 'repair']}})",
            str(resumed.path),
            str(result.inference_bundle),
        ],
        env={
            **os.environ,
            "PYTHONPATH": str(
                Path(__file__).resolve().parents[2] / "packages/vis-agent/src"
            ),
            "HF_HUB_OFFLINE": "1",
            "TRANSFORMERS_OFFLINE": "1",
        },
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert process.returncode == 0, process.stderr[-1000:]
