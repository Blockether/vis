"""Explicit gateway training jobs use the lightweight authenticated SDK transport."""

import json

import pytest
from blockether.vis.decisions import Decisions
from blockether.vis.engine import GatewayClient, GatewayError
from test_client import compatible, endpoint


def test_remote_training_job_lifecycle_without_uploading_private_rows():
    job_id = "28b15a56-014d-4c3d-9824-dc41edb6569a"
    data = {
        "train_data": "training.jsonl",
        "eval_data": "held-out.jsonl",
        "training_config": "config.json",
        "validation_policy": "policy.json",
    }

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        if method == "POST" and path == "/v1/decisions/training/jobs":
            assert json.loads(body) == data
            return 202, {"job_id": job_id, "status": "running", "stage": "staging"}
        if path == f"/v1/decisions/training/jobs/{job_id}":
            if method == "GET":
                return 200, {
                    "job_id": job_id,
                    "status": "completed",
                    "model_ref": "sha256-" + "a" * 64,
                    "metrics": {"decision_accuracy": 1.0, "action_accuracy": 0.5},
                }
            if method == "DELETE":
                return 200, {"job_id": job_id, "status": "deleted"}
        raise AssertionError((method, path))

    with (
        endpoint(respond) as (url, calls),
        GatewayClient(url, token="secret") as gateway,
    ):
        decisions = Decisions(gateway)
        assert decisions.start_training(**data)["job_id"] == job_id
        result = decisions.get_training_job(job_id)
        assert result["metrics"]["decision_accuracy"] == 1.0
        assert decisions.cancel_training_job(job_id)["status"] == "deleted"
        assert [row[1] for row in calls if "/decisions/training/jobs" in row[1]] == [
            "/v1/decisions/training/jobs",
            f"/v1/decisions/training/jobs/{job_id}",
            f"/v1/decisions/training/jobs/{job_id}",
        ]
        headers = {key.lower(): value for key, value in calls[-3][2].items()}
        assert headers["authorization"] == "Bearer secret"


def test_remote_training_validates_local_filenames_and_preserves_gateway_failures():
    gateway = GatewayClient("http://127.0.0.1:1")
    try:
        with pytest.raises(ValueError):
            Decisions(gateway).start_training(
                train_data="../private.jsonl",
                eval_data="held-out.jsonl",
                training_config="config.json",
                validation_policy="policy.json",
            )
    finally:
        gateway.close()

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        return 503, {
            "error": {"type": "decisions/error", "reason": "training-unavailable"}
        }

    with endpoint(respond) as (url, _), GatewayClient(url) as gateway:
        with pytest.raises(GatewayError) as error:
            Decisions(gateway).start_training(
                train_data="training.jsonl",
                eval_data="held-out.jsonl",
                training_config="config.json",
                validation_policy="policy.json",
            )
        assert error.value.status == 503


@pytest.mark.parametrize("model_id", ["gliner2.5-base", "gliner2.5-decide"])
def test_remote_training_explicit_gliner_family_and_resume(model_id):
    previous = "28b15a56-014d-4c3d-9824-dc41edb6569a"
    current = "93b91cb4-ed9a-421b-a4cd-0d659ea2c510"

    def respond(method, path, body):
        if result := compatible(method, path, body):
            return result
        assert method == "POST" and path == "/v1/decisions/training/jobs"
        assert json.loads(body) == {
            "model_id": model_id,
            "source_job_id": previous,
            "train_data": "training.jsonl",
            "eval_data": "held-out.jsonl",
            "training_config": "config.json",
            "validation_policy": "policy.json",
        }
        return 202, {"job_id": current, "model_id": model_id, "status": "running"}

    with endpoint(respond) as (url, calls), GatewayClient(url) as gateway:
        decisions = Decisions(gateway)
        result = decisions.start_training(
            model_id=model_id,
            source_job_id=previous,
            train_data="training.jsonl",
            eval_data="held-out.jsonl",
            training_config="config.json",
            validation_policy="policy.json",
        )
        assert result["model_id"] == model_id
        assert len([call for call in calls if call[1].endswith("/training/jobs")]) == 1
        with pytest.raises(ValueError, match="model_id"):
            decisions.start_training(
                model_id="unknown",
                train_data="training.jsonl",
                eval_data="held-out.jsonl",
                training_config="config.json",
                validation_policy="policy.json",
            )
