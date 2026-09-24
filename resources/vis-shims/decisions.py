# First-party Vis Python gateway reads; the full vis-agent SDK remains opt-in.


def __vis_install_decisions__():
    import importlib.machinery as _machinery
    import json as _json
    import re as _re
    import sys as _sys
    from collections.abc import Mapping as _Mapping
    from types import ModuleType as _ModuleType

    class DecisionGatewayError(RuntimeError):
        """A decision request failed; inspect status and code without logging the body."""

        def __init__(self, status, code):
            self.status = status
            self.code = code
            super().__init__(f"Decision gateway HTTP {status} ({code})")

    def _query(operation, *, body=None, model_ref=None, timeout=None):
        bridge = globals().get("__vis_decision_query__")
        if bridge is None:
            raise RuntimeError("vis_decisions: gateway bridge is unavailable")
        if timeout is not None and (
            isinstance(timeout, bool)
            or not isinstance(timeout, (int, float))
            or not 1 <= timeout <= 600
        ):
            raise ValueError("timeout must be between 1 and 600 seconds")
        args = {"operation": operation}
        if body is not None:
            args["body"] = body
        if model_ref is not None:
            args["model_ref"] = model_ref
        if timeout is not None:
            args["timeout_ms"] = int(timeout * 1000)
        result = _json.loads(bridge(_json.dumps(args, allow_nan=False)))
        if not isinstance(result, dict) or not isinstance(result.get("status"), int):
            raise RuntimeError("vis_decisions: invalid gateway response")
        status = result["status"]
        payload = result.get("body")
        if not isinstance(payload, dict):
            raise RuntimeError("vis_decisions: decision response is not a JSON object")
        if status >= 400:
            error = payload.get("error")
            code = (
                (error.get("reason") or error.get("type"))
                if isinstance(error, dict)
                else None
            )
            raise DecisionGatewayError(
                status, code if isinstance(code, str) else "unknown"
            )
        return payload

    def infer(*, model, state, questions, timeout=None):
        """Answer choice, score and noul questions with a named installed model.

        The result includes the gateway's action probability and routing.model.
        No model or dependency is downloaded; an absent model raises HTTP 409.
        Baseline action probabilities are not an autonomous action policy.
        """
        if not isinstance(model, str) or not model.strip():
            raise ValueError("model is required")
        if not isinstance(questions, _Mapping):
            raise ValueError("questions must be a mapping")
        return _query(
            "infer",
            body={"model": model, "state": state, "questions": dict(questions)},
            timeout=timeout,
        )

    def models(*, timeout=None):
        """List installed decision versions and their residency without downloading."""
        result = _query("models", timeout=timeout)
        rows = result.get("models")
        if not isinstance(rows, list) or not all(isinstance(row, dict) for row in rows):
            raise RuntimeError("vis_decisions: models response lacks a models list")
        return rows

    def model(model_ref, *, timeout=None):
        """Read the status of one immutable decision version or named baseline."""
        if not isinstance(model_ref, str) or not _re.fullmatch(
            r"[A-Za-z0-9][A-Za-z0-9._-]{0,127}", model_ref
        ):
            raise ValueError("invalid decision model reference")
        result = _query("model", model_ref=model_ref, timeout=timeout)
        if result.get("model_ref") != model_ref:
            raise RuntimeError("vis_decisions: model status does not match the request")
        return result

    module = _ModuleType("vis_decisions")
    module.__doc__ = (
        "Read decision models through Vis' authenticated gateway without installing "
        "third-party packages. Use the full vis-agent SDK for training, upload, "
        "aliases and remote client connections. Downloads always require an explicit CLI command."
    )
    module.__spec__ = _machinery.ModuleSpec("vis_decisions", None)
    module.DecisionGatewayError = DecisionGatewayError
    module.infer = infer
    module.models = models
    module.model = model
    _sys.modules[module.__name__] = module


__vis_install_decisions__()
del __vis_install_decisions__
