"""Typed decisions through an existing authenticated gateway connection.

For training, publishing and using a decision model, see the
[decision models guide](https://vis.blockether.com/decision-models.html).
"""

from __future__ import annotations

import re
from collections.abc import Callable, Mapping
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any

from blockether.vis.engine import (
    GatewayClient,
    GatewayError,
    ProtocolError,
    TransportError,
)

from ._models import ARCHITECTURES
from ._publication import ProgressReader, package
from ._trainer import TrainingResult


class Decisions:
    """Infer with a named model; the gateway owns weights and execution.

    This lightweight client does not install PyTorch or ONNX Runtime. Training and
    model publication use separate, explicit APIs.
    """

    def __init__(self, gateway: GatewayClient) -> None:
        self._gateway = gateway

    def infer(
        self,
        *,
        model: str,
        state: str | dict[str, Any] | list[Any],
        questions: Mapping[str, Mapping[str, Any]],
        timeout: float | None = None,
    ) -> dict[str, Any]:
        """Answer choice, score and noul questions, including the action head.

        ``model`` is mandatory and must refer to an installed version or alias.
        Results retain the selected model and ``routing.model`` fields.
        Baseline action probabilities are not approved for autonomous execution.
        """
        if not isinstance(model, str) or not model.strip():
            raise ValueError("model is required")
        if not isinstance(questions, Mapping):
            raise ValueError("questions must be a mapping")
        result = self._gateway.post_systemone(
            body={"model": model, "state": state, "questions": dict(questions)},
            timeout=timeout,
        )
        if not isinstance(result, dict):
            raise ProtocolError("decision response is not a JSON object")
        return result

    def list_models(self, *, timeout: float | None = None) -> list[dict[str, Any]]:
        """Read installed and in-memory states without downloading or loading a model."""
        result = self._gateway.get_decision_models(timeout=timeout)
        if not isinstance(result, dict) or not isinstance(result.get("models"), list):
            raise ProtocolError("decision models response lacks a models list")
        models = result["models"]
        if not all(isinstance(model, dict) for model in models):
            raise ProtocolError("decision models response contains an invalid model")
        return models

    def get_model(
        self, model_ref: str, *, timeout: float | None = None
    ) -> dict[str, Any]:
        """Read an immutable version, including after an ambiguous upload timeout."""
        result = self._gateway.get_decision_model(model_ref, timeout=timeout)
        if not isinstance(result, dict) or result.get("model_ref") != model_ref:
            raise ProtocolError(
                "decision model status does not match the requested ref"
            )
        return result

    def get_alias(self, alias: str, *, timeout: float | None = None) -> dict[str, Any]:
        """Read the alias and its current immutable version before changing it."""
        result = self._gateway.get_decision_alias(alias, timeout=timeout)
        if not isinstance(result, dict) or result.get("alias") != alias:
            raise ProtocolError("decision alias response does not match the request")
        return result

    def activate_model(
        self,
        alias: str,
        model_ref: str,
        *,
        expected_current: str | None = None,
        timeout: float | None = None,
    ) -> dict[str, Any]:
        """Create an unused alias, or CAS-update it with its exact previous ref.

        A conflict raises GatewayError(409); neither upload nor training activates
        anything. Existing requests finish on their pinned old version.
        """
        result = self._gateway.put_decision_alias(
            alias,
            model_ref=model_ref,
            expected_current=expected_current,
            timeout=timeout,
        )
        if not isinstance(result, dict) or result.get("model_ref") != model_ref:
            raise ProtocolError("decision alias activation returned an invalid ref")
        return result

    def start_training(
        self,
        *,
        train_data: str,
        eval_data: str,
        training_config: str,
        validation_policy: str,
        model_id: str = "laya-typed-decisions",
        source_job_id: str | None = None,
        timeout: float | None = None,
    ) -> dict[str, Any]:
        """Train offline on approved gateway-local filenames, without uploading rows.

        Select ``gliner2.5-base`` or ``gliner2.5-decide`` explicitly for GLiNER;
        they require a separate ``decisions-gliner-training`` environment. The
        gateway needs a pinned local checkpoint and an approved data directory.
        No model alias changes when the job finishes. Use ``get_training_job``
        for progress, and activate its model_ref separately after review.
        """
        names = {
            "train_data": (train_data, ".jsonl"),
            "eval_data": (eval_data, ".jsonl"),
            "training_config": (training_config, ".json"),
            "validation_policy": (validation_policy, ".json"),
        }
        if any(
            not isinstance(value, str)
            or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", value)
            or not value.endswith(extension)
            for value, extension in names.values()
        ):
            raise ValueError("Training inputs must be approved gateway-local filenames")
        if model_id != "laya-typed-decisions" and model_id not in ARCHITECTURES:
            raise ValueError("Unsupported decision training model_id")
        if source_job_id is not None and not _valid_job_id(source_job_id):
            raise ValueError("source_job_id must be a previously completed job id")
        body = {name: value for name, (value, _) in names.items()}
        if model_id != "laya-typed-decisions":
            body["model_id"] = model_id
        if source_job_id is not None:
            body["source_job_id"] = source_job_id
        result = self._gateway.post_decision_training_job(body=body, timeout=timeout)
        if (
            not isinstance(result, dict)
            or not _valid_job_id(result.get("job_id"))
            or ("model_id" in result and result["model_id"] != model_id)
        ):
            raise ProtocolError(
                "decision training did not return the selected job identity"
            )
        return result

    def get_training_job(
        self, job_id: str, *, timeout: float | None = None
    ) -> dict[str, Any]:
        """Read a durable job's stage, step, errors, metrics and final model_ref."""
        if not _valid_job_id(job_id):
            raise ValueError("Invalid decision training job id")
        result = self._gateway.get_decision_training_job(job_id, timeout=timeout)
        if not isinstance(result, dict) or result.get("job_id") != job_id:
            raise ProtocolError("decision training status does not match the job id")
        return result

    def cancel_training_job(
        self, job_id: str, *, timeout: float | None = None
    ) -> dict[str, Any]:
        """Cancel a running trainer; on terminal jobs, delete its private checkpoint.

        Registered inference versions are immutable and remain available.
        """
        if not _valid_job_id(job_id):
            raise ValueError("Invalid decision training job id")
        result = self._gateway.delete_decision_training_job(job_id, timeout=timeout)
        if not isinstance(result, dict) or result.get("job_id") != job_id:
            raise ProtocolError(
                "decision training cancellation returned an invalid job id"
            )
        return result

    def upload_model(
        self,
        bundle: str | Path | TrainingResult,
        *,
        progress: Callable[[int, int], None] | None = None,
        timeout: float | None = None,
    ) -> dict[str, Any]:
        """Verify and stream only FP32 inference files; never upload training data.

        A transport timeout does not retry the mutation. A single safe GET for the
        known content digest can confirm a completed import; otherwise the original
        failure remains visible and the caller can inspect that ref later.
        """
        source = (
            bundle.inference_bundle if isinstance(bundle, TrainingResult) else bundle
        )
        directory = Path(source).expanduser().resolve()
        with TemporaryDirectory(prefix="vis-decision-upload-") as temporary:
            archive = Path(temporary) / "inference.zip"
            digest, size = package(directory, archive)
            ref = f"sha256-{digest}"
            with archive.open("rb") as stream:
                try:
                    result = self._gateway.post_decision_model(
                        content=ProgressReader(stream, size, progress),
                        sha256=digest,
                        length=size,
                        timeout=timeout,
                    )
                except TransportError as error:
                    try:
                        return self.get_model(ref, timeout=timeout)
                    except (GatewayError, TransportError):
                        raise error from None
            if not isinstance(result, dict) or result.get("model_ref") != ref:
                raise ProtocolError(
                    "decision import response does not match the upload digest"
                )
            return result


def _valid_job_id(value: object) -> bool:
    return isinstance(value, str) and bool(
        re.fullmatch(r"[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}", value)
    )


__all__ = ["Decisions"]
