"""Opt-in real-checkpoint ONNX parity tests; download the pinned weights first."""

import gc
import hashlib
import json
import os
import resource
import socket
import statistics
import subprocess
import sys
import textwrap
import time
from pathlib import Path

import numpy as np
import onnx
import pytest
import torch
from export import (
    export_model,
    load_onnx,
    load_reference,
    make_batch,
    quantize_bundle,
    train_and_export,
)
from fastapi.testclient import TestClient
from laya import Agent, Router
from laya.serve import create_app

STATE = {"message": "I was charged twice. Please refund the duplicate payment."}
QUESTIONS = {
    "department": {
        "type": "choice",
        "instructions": "Which department should handle this ticket?",
        "criteria": ["billing", "technical", "sales"],
    },
    "urgency": {
        "type": "score",
        "instructions": "How urgent is this ticket?",
        "criteria": ["low", "medium", "high"],
    },
    "billing": {"type": "noul", "instructions": "Does this ticket concern a payment?"},
}
CASES = [
    ("mixed", STATE, QUESTIONS),
    (
        "one-question",
        "Hello!",
        {"greeting": {"type": "noul", "instructions": "Is this a greeting?"}},
    ),
    (
        "one-option",
        "Only one action is permitted.",
        {
            "next": {
                "type": "choice",
                "instructions": "Select the action.",
                "criteria": ["continue"],
            }
        },
    ),
    (
        "structured-criteria",
        [{"role": "user", "content": "Please cancel my order."}],
        {
            "route": {
                "type": "choice",
                "instructions": "Classify this request.",
                "criteria": {
                    "billing": {"description": "payments", "priority": 1},
                    "support": "orders",
                    "sales": "new purchases",
                    "other": None,
                },
            }
        },
    ),
    (
        "long-context",
        "The duplicate payment requires a refund. " * 200,
        {
            "urgency": QUESTIONS["urgency"],
            "category": {
                "type": "choice",
                "instructions": "Choose a category.",
                "criteria": [
                    "refund",
                    "account",
                    "delivery",
                    "technical",
                    "privacy",
                    "sales",
                    "other",
                ],
            },
        },
    ),
    (
        "many-options",
        STATE,
        {
            "category": {
                "type": "choice",
                "instructions": "Choose a category.",
                "criteria": [f"category {i}" for i in range(12)],
            }
        },
    ),
    (
        "near-boundary-choice",
        "Request about a duplicate payment",
        {
            "decision": {
                "type": "choice",
                "instructions": "Which is best?",
                "criteria": ["accept", "decline"],
            }
        },
    ),
]


def assert_json_close(actual, expected, *, abs_tol=2e-4):
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys()
        for key in expected:
            assert_json_close(actual[key], expected[key], abs_tol=abs_tol)
    elif isinstance(expected, float):
        assert actual == pytest.approx(expected, abs=abs_tol)
    else:
        assert actual == expected


def elapsed_predict(agent, state, questions):
    agent.predict(state, questions)
    times = []
    for _ in range(3):
        started = time.perf_counter()
        result = agent.predict(state, questions)
        times.append((time.perf_counter() - started) * 1000)
    return result, statistics.median(times)


@pytest.fixture(scope="module")
def artifacts(tmp_path_factory):
    torch.set_num_threads(4)
    agent = load_reference()
    expected = {}
    for name, state, questions in CASES:
        batch = make_batch(agent, state, questions)
        with torch.no_grad():
            logits, act = agent.model(*batch)
        result, milliseconds = elapsed_predict(agent, state, questions)
        expected[name] = {
            "batch": batch,
            "logits": logits,
            "act": act,
            "result": result,
            "pytorch_ms": milliseconds,
        }
    destination = tmp_path_factory.mktemp("laya-onnx")
    started = time.monotonic()
    graph = export_model(agent, destination / "base", expected["mixed"]["batch"])
    print(
        json.dumps(
            {
                "graph_bytes": graph.stat().st_size,
                "export_seconds": time.monotonic() - started,
                "bundle": str(graph.parent),
            }
        )
    )
    runtime = load_onnx(graph.parent)
    yield {
        "agent": agent,
        "runtime": runtime,
        "expected": expected,
        "directory": destination,
    }
    del runtime, agent
    gc.collect()


@pytest.mark.parametrize("case", CASES, ids=[case[0] for case in CASES])
def test_full_graph_dynamic_shapes_and_answer_parity(artifacts, case):
    name, state, questions = case
    expected = artifacts["expected"][name]
    runtime = artifacts["runtime"]
    logits, act = runtime.model(*expected["batch"])
    np.testing.assert_allclose(
        logits.numpy(), expected["logits"].numpy(), rtol=1e-4, atol=1e-4
    )
    np.testing.assert_allclose(
        act.numpy(), expected["act"].numpy(), rtol=1e-4, atol=1e-4
    )
    result, milliseconds = elapsed_predict(runtime, state, questions)
    assert_json_close(result, expected["result"])
    print(
        json.dumps(
            {
                "case": name,
                "shape": list(expected["batch"][0].shape),
                "options": expected["batch"][2].shape[1],
                "max_logit_error": float((logits - expected["logits"]).abs().max()),
                "max_action_error": float((act - expected["act"]).abs().max()),
                "pytorch_ms": expected["pytorch_ms"],
                "onnx_ms": milliseconds,
            }
        )
    )


def test_empty_questions_do_not_run_model(artifacts, monkeypatch):
    def fail(*args, **kwargs):
        pytest.fail("Empty questions must not run ONNX")

    monkeypatch.setattr(artifacts["runtime"].model, "forward", fail)
    assert artifacts["runtime"].predict(STATE, {}) == {
        "model": "laya-rl-agent",
        "answers": {},
        "usage": {"input_tokens": 0, "output_tokens": 0},
    }


def test_http_jev_contract_and_validation(artifacts, monkeypatch):
    monkeypatch.delenv("LAYA_API_KEY", raising=False)
    router = Router(device="cpu", max_loaded=2)
    router.attach("english", artifacts["runtime"])
    with TestClient(create_app(router)) as client:
        response = client.post(
            "/v1/systemone",
            json={
                "model": "english",
                "state": STATE,
                "questions": QUESTIONS,
                "unknown_field": True,
            },
        )
        assert response.status_code == 200
        body = response.json()
        assert_json_close(
            body["answers"], artifacts["expected"]["mixed"]["result"]["answers"]
        )
        assert body["usage"] == artifacts["expected"]["mixed"]["result"]["usage"]
        bad = client.post(
            "/v1/systemone",
            json={
                "state": STATE,
                "questions": {
                    "bad": {"type": "choice", "instructions": "Choose.", "criteria": []}
                },
            },
        )
        assert bad.status_code == 422
        assert "bad" in bad.json()["detail"]
        assert client.post("/v1/systemone", json={"state": STATE}).status_code == 400


def test_quantize_bundle_rejects_missing_source_and_existing_destination(tmp_path):
    with pytest.raises(FileNotFoundError, match="model.onnx"):
        quantize_bundle(tmp_path / "missing", tmp_path / "target")
    target = tmp_path / "target"
    target.mkdir()
    with pytest.raises(FileExistsError, match="Bundle already exists"):
        quantize_bundle(tmp_path / "missing", target)
    assert list(target.iterdir()) == []


def test_int8_bundle_dynamic_shapes_and_http(artifacts, monkeypatch):
    started = time.perf_counter()
    graph = quantize_bundle(
        artifacts["directory"] / "base", artifacts["directory"] / "base-int8"
    )
    quantize_seconds = time.perf_counter() - started
    model = onnx.load(str(graph), load_external_data=False)
    ops = [node.op_type for node in model.graph.node]
    assert ops.count("MatMulInteger") >= 100
    for head in ("/scorer/", "/act_head/"):
        assert any(
            node.op_type == "MatMulInteger" and node.name.startswith(head)
            for node in model.graph.node
        ), f"Unquantized decision head: {head}"
    assert ops.count("DynamicQuantizeLinear") >= 100
    assert (
        ops.count("MatMul") == 62
    )  # Dynamic attention products have no fixed weights.
    assert (graph.parent / "model.onnx.data").is_file()
    assert (
        sum(f.stat().st_size for f in graph.parent.rglob("*") if f.is_file())
        < 750_000_000
    )
    started = time.perf_counter()
    runtime = load_onnx(graph.parent)
    cold_load_seconds = time.perf_counter() - started
    peak_rss = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    if sys.platform != "darwin":
        peak_rss *= 1024
    print(
        json.dumps(
            {
                "int8_graph_bytes": graph.stat().st_size,
                "int8_weights_bytes": (graph.parent / "model.onnx.data").stat().st_size,
                "fp32_graph_bytes": (artifacts["directory"] / "base" / "model.onnx")
                .stat()
                .st_size,
                "quantize_seconds": quantize_seconds,
                "cold_load_seconds": cold_load_seconds,
                "process_peak_rss_bytes_including_fp32_reference": peak_rss,
                "quantized_linear_ops": ops.count("MatMulInteger"),
                "floating_activation_products": ops.count("MatMul"),
            }
        )
    )
    for name, state, questions in CASES:
        expected = artifacts["expected"][name]
        logits, act = runtime.model(*expected["batch"])
        np.testing.assert_allclose(
            logits.numpy(), expected["logits"].numpy(), rtol=0.04, atol=0.5
        )
        np.testing.assert_allclose(
            act.numpy(), expected["act"].numpy(), rtol=0.04, atol=8
        )
        result, milliseconds = elapsed_predict(runtime, state, questions)
        assert_json_close(result, expected["result"], abs_tol=0.06)
        print(
            json.dumps(
                {
                    "int8_case": name,
                    "max_logit_error": float((logits - expected["logits"]).abs().max()),
                    "max_action_error": float((act - expected["act"]).abs().max()),
                    "pytorch_ms": expected["pytorch_ms"],
                    "int8_ms": milliseconds,
                }
            )
        )
    monkeypatch.delenv("LAYA_API_KEY", raising=False)
    router = Router(device="cpu", max_loaded=1)
    router.attach("english", runtime)
    with TestClient(create_app(router)) as client:
        response = client.post(
            "/v1/systemone",
            json={"model": "english", "state": STATE, "questions": QUESTIONS},
        )
        assert response.status_code == 200, response.text
        assert (
            response.json()["answers"] == runtime.predict(STATE, QUESTIONS)["answers"]
        )


def test_finetune_auto_export_reload_and_two_models_offline(artifacts, monkeypatch):
    def deny_network(*args, **kwargs):
        pytest.fail("An offline checkpoint must not use the network")

    monkeypatch.setattr(socket.socket, "connect", deny_network)
    monkeypatch.delenv("LAYA_API_KEY", raising=False)
    agent = artifacts["agent"]
    probe = next(
        layer
        for layer in agent.model.encoder.modules()
        if isinstance(layer, torch.nn.Linear)
    ).weight
    before_encoder = probe.detach().clone()
    before_head = agent.model.scorer[-1].weight.detach().clone()
    before_act_head = agent.model.act_head[-1].weight.detach().clone()
    batch = artifacts["expected"]["mixed"]["batch"]
    started = time.monotonic()
    trained = train_and_export(
        agent,
        batch,
        torch.tensor([1, 2, 1]),
        artifacts["directory"] / "trained",
        act_targets=torch.tensor([1, 0, 1]),
    )
    duration = time.monotonic() - started
    encoder_delta = float((probe.detach() - before_encoder).abs().max())
    head_delta = float(
        (agent.model.scorer[-1].weight.detach() - before_head).abs().max()
    )
    act_head_delta = float(
        (agent.model.act_head[-1].weight.detach() - before_act_head).abs().max()
    )
    assert encoder_delta > 0
    assert head_delta > 0
    assert act_head_delta > 0
    assert trained["graph"].is_file()
    assert (trained["checkpoint"] / "model.safetensors").is_file()
    expected = agent.predict(STATE, QUESTIONS)
    reloaded = Agent(str(trained["checkpoint"]), device="cpu")
    assert_json_close(reloaded.predict(STATE, QUESTIONS), expected)
    del reloaded, before_encoder, before_head, before_act_head
    gc.collect()
    with torch.no_grad():
        expected_logits, expected_act = agent.model(*batch)
    fine = load_onnx(trained["graph"].parent)
    logits, act = fine.model(*batch)
    np.testing.assert_allclose(
        logits.numpy(), expected_logits.numpy(), rtol=1e-4, atol=1e-4
    )
    np.testing.assert_allclose(act.numpy(), expected_act.numpy(), rtol=1e-4, atol=1e-4)
    assert not torch.equal(logits, artifacts["expected"]["mixed"]["logits"])
    assert_json_close(fine.predict(STATE, QUESTIONS), expected)
    trained_int8 = quantize_bundle(
        trained["graph"].parent, artifacts["directory"] / "trained-int8"
    )
    fine = load_onnx(trained_int8.parent)
    quant_logits, quant_act = fine.model(*batch)
    np.testing.assert_allclose(
        quant_logits.numpy(), expected_logits.numpy(), rtol=0.04, atol=0.5
    )
    np.testing.assert_allclose(
        quant_act.numpy(), expected_act.numpy(), rtol=0.04, atol=8
    )
    base_graph = artifacts["directory"] / "base-int8" / "model.onnx"
    if not base_graph.is_file():
        base_graph = quantize_bundle(
            artifacts["directory"] / "base", artifacts["directory"] / "base-int8"
        )
    base = load_onnx(base_graph.parent)
    router = Router(device="cpu", max_loaded=2)
    router.attach("english", base)
    router.attach("typed-decisions", fine)
    assert set(router.loaded) == {"english", "typed-decisions"}

    def deny_torch_neural_forward(*args, **kwargs):
        pytest.fail("All neural inference must run in ONNX Runtime")

    monkeypatch.setattr(torch.nn.Linear, "forward", deny_torch_neural_forward)
    with TestClient(create_app(router)) as client:
        for name, reference in [
            ("english", artifacts["expected"]["mixed"]["result"]),
            ("typed-decisions", expected),
            ("english", artifacts["expected"]["mixed"]["result"]),
        ]:
            response = client.post(
                "/v1/systemone",
                json={"model": name, "state": STATE, "questions": QUESTIONS},
            )
            assert response.status_code == 200, response.text
            body = response.json()
            assert body["routing"]["model"] == name
            assert_json_close(
                body["answers"],
                reference["answers"],
                abs_tol=0.06,
            )
    print(
        json.dumps(
            {
                "training_losses": trained["losses"],
                "train_save_export_seconds": duration,
                "encoder_max_weight_delta": encoder_delta,
                "head_max_weight_delta": head_delta,
                "act_head_max_weight_delta": act_head_delta,
                "trained_logit_error": float((logits - expected_logits).abs().max()),
                "trained_graph_bytes": trained["graph"].stat().st_size,
                "checkpoint_bytes": (trained["checkpoint"] / "model.safetensors")
                .stat()
                .st_size,
                "two_models": router.loaded,
                "network": "denied",
                "pytorch_neural_forward": "denied during HTTP checks",
            }
        )
    )


def test_isolated_int8_runtime_memory_and_offline_reload(artifacts, tmp_path):
    graph = artifacts["directory"] / "base-int8" / "model.onnx"
    if not graph.is_file():
        graph = quantize_bundle(
            artifacts["directory"] / "base", artifacts["directory"] / "base-int8"
        )
    batch = artifacts["expected"]["mixed"]["batch"]
    feed = tmp_path / "inputs.json"
    feed.write_text(
        json.dumps(
            {
                name: value.tolist()
                for name, value in zip(
                    (
                        "input_ids",
                        "attention_mask",
                        "marker_pos",
                        "marker_mask",
                        "qtype",
                    ),
                    batch,
                    strict=True,
                )
            }
        )
    )
    script = textwrap.dedent("""
        import json
        import resource
        import socket
        import sys
        import time

        import numpy as np
        import onnxruntime as ort

        def offline(*args, **kwargs):
            raise RuntimeError("INT8 inference must not use the network")

        socket.socket.connect = offline
        with open(sys.argv[1]) as handle:
            inputs = json.load(handle)
        feed = {key: np.asarray(values, dtype=np.bool_ if key == "marker_mask"
                           else np.int64) for key, values in inputs.items()}
        options = ort.SessionOptions()
        options.intra_op_num_threads = 4
        start = time.perf_counter()
        session = ort.InferenceSession(sys.argv[2], sess_options=options,
                                       providers=["CPUExecutionProvider"])
        load_seconds = time.perf_counter() - start
        durations = []
        for _ in range(4):
            start = time.perf_counter()
            logits, act = session.run(None, feed)
            durations.append((time.perf_counter() - start) * 1000)
        peak = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
        if sys.platform != "darwin":
            peak *= 1024
        print(json.dumps({"logits": logits.tolist(), "act": act.tolist(),
                          "peak_rss_bytes": peak, "load_seconds": load_seconds,
                          "inference_ms": durations[1:]}))
    """)
    env = os.environ.copy()
    env.update(HF_HUB_OFFLINE="1", TRANSFORMERS_OFFLINE="1")
    result = subprocess.run(
        [sys.executable, "-c", script, str(feed), str(graph)],
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
        env=env,
    )
    assert result.returncode == 0, (result.stdout, result.stderr)
    measured = json.loads(result.stdout)
    expected = artifacts["expected"]["mixed"]
    np.testing.assert_allclose(
        measured["logits"], expected["logits"].numpy(), rtol=0.04, atol=0.5
    )
    np.testing.assert_allclose(
        measured["act"], expected["act"].numpy(), rtol=0.04, atol=8
    )
    assert measured["peak_rss_bytes"] > 0
    assert measured["load_seconds"] >= 0
    assert all(ms > 0 for ms in measured["inference_ms"])
    print(
        json.dumps(
            {
                key: value
                for key, value in measured.items()
                if key not in {"logits", "act"}
            }
        )
    )


@pytest.mark.skipif(
    not os.environ.get("VIS_LAYA_EVAL_JSONL"),
    reason="Set VIS_LAYA_EVAL_JSONL to the pinned public typed-decisions test split",
)
def test_labeled_public_decisions_int8_parity(artifacts):
    """Report accuracy and calibration on 2,000 independently labeled questions.

    Source: LocalLLaMA/typed-decisions, Apache-2.0, test split of revision
    c76749ec58bd8c3d2ea706b31c333a9059c38f90. Store the JSONL outside the
    checkout; its SHA-256 pins the dataset-server materialization. These
    diagnostics are not a quality gate; act/escalate has no independent label.
    """
    path = Path(os.environ["VIS_LAYA_EVAL_JSONL"])
    assert hashlib.sha256(path.read_bytes()).hexdigest() == (
        "eac0dd0a633c47e599ff5a439abe5c2500867529f919de97926594269d8c3353"
    )
    graph = artifacts["directory"] / "base-int8" / "model.onnx"
    if not graph.is_file():
        graph = quantize_bundle(
            artifacts["directory"] / "base", artifacts["directory"] / "base-int8"
        )
    runtime = load_onnx(graph.parent)
    counters = {
        kind: {
            "questions": 0,
            "fp32_correct": 0,
            "int8_correct": 0,
            "decision_changes": 0,
        }
        for kind in ("choice", "score", "noul")
    }
    calibration = {
        kind: {
            name: {"brier_sum": 0.0, "bins": [[0, 0.0, 0.0] for _ in range(10)]}
            for name in ("fp32", "int8")
        }
        for kind in counters
    }
    action_bounds = {name: [1.0, 0.0] for name in ("fp32", "int8")}
    max_confidence_shift = 0.0
    max_action_shift = 0.0
    max_score_shift = 0.0
    latency = {"fp32": [], "int8": []}

    def answer_label(answer, kind):
        if kind == "noul":
            return "true" if answer["noul"] >= 0.5 else "false"
        return max(answer["probabilities"], key=answer["probabilities"].get)

    case_count = 0
    with path.open() as lines:
        for line in lines:
            case_count += 1
            row = json.loads(line)
            state = json.loads(row["state"])
            questions = json.loads(row["questions"])
            gold = json.loads(row["gold"])
            results = {}
            for name, model in (("fp32", artifacts["agent"]), ("int8", runtime)):
                started = time.perf_counter()
                results[name] = model.predict(state, questions)["answers"]
                latency[name].append((time.perf_counter() - started) * 1000)
                assert results[name].keys() == gold.keys()
            for qid, expected in gold.items():
                kind = questions[qid]["type"]
                counters[kind]["questions"] += 1
                base = results["fp32"][qid]
                quant = results["int8"][qid]

                base_label = answer_label(base, kind)
                quant_label = answer_label(quant, kind)
                counters[kind]["fp32_correct"] += base_label == expected["label"]
                counters[kind]["int8_correct"] += quant_label == expected["label"]
                counters[kind]["decision_changes"] += base_label != quant_label
                for name, answer in (("fp32", base), ("int8", quant)):
                    probabilities = (
                        {"false": 1.0 - answer["noul"], "true": answer["noul"]}
                        if kind == "noul"
                        else answer["probabilities"]
                    )
                    assert expected["label"] in probabilities
                    prediction = max(probabilities, key=probabilities.get)
                    confidence = float(probabilities[prediction])
                    metrics = calibration[kind][name]
                    metrics["brier_sum"] += sum(
                        (float(probability) - (label == expected["label"])) ** 2
                        for label, probability in probabilities.items()
                    )
                    bucket = metrics["bins"][min(9, int(confidence * 10))]
                    bucket[0] += 1
                    bucket[1] += confidence
                    bucket[2] += prediction == expected["label"]
                    probability = answer["action"]["act_probability"]
                    action_bounds[name][0] = min(action_bounds[name][0], probability)
                    action_bounds[name][1] = max(action_bounds[name][1], probability)
                max_confidence_shift = max(
                    max_confidence_shift, abs(base["confidence"] - quant["confidence"])
                )
                max_action_shift = max(
                    max_action_shift,
                    abs(
                        base["action"]["act_probability"]
                        - quant["action"]["act_probability"]
                    ),
                )
                if kind == "score":
                    max_score_shift = max(
                        max_score_shift, abs(base["score"] - quant["score"])
                    )
    assert case_count == 400
    assert sum(stats["questions"] for stats in counters.values()) == 2000
    calibration_report = {
        kind: {
            name: {
                "brier": metrics["brier_sum"] / counters[kind]["questions"],
                "ece_10_bins": sum(
                    abs(total_confidence - correct)
                    for _, total_confidence, correct in metrics["bins"]
                )
                / counters[kind]["questions"],
            }
            for name, metrics in by_model.items()
        }
        for kind, by_model in calibration.items()
    }
    print(
        json.dumps(
            {
                "questions": counters,
                "calibration": calibration_report,
                "action_probability_bounds": action_bounds,
                "max_confidence_shift": max_confidence_shift,
                "max_action_shift": max_action_shift,
                "max_score_shift": max_score_shift,
                "median_ms": {
                    name: statistics.median(ms) for name, ms in latency.items()
                },
            }
        )
    )


@pytest.mark.skipif(
    not os.environ.get("VIS_LAYA_OFFLINE_TRAIN_PYTHON"),
    reason="Set VIS_LAYA_OFFLINE_TRAIN_PYTHON and VIS_LAYA_OFFLINE_CHECKPOINT",
)
def test_minimal_wheelhouse_trains_and_exports_without_network(tmp_path):
    """Run both training heads and INT8 export in the isolated training environment."""
    python = Path(os.environ["VIS_LAYA_OFFLINE_TRAIN_PYTHON"])
    checkpoint = Path(os.environ["VIS_LAYA_OFFLINE_CHECKPOINT"])
    assert python.is_file()
    for name in (
        "model.safetensors",
        "rl_agent_config.json",
        "encoder/config.json",
        "tokenizer/tokenizer.json",
    ):
        assert (checkpoint / name).is_file(), name

    script = textwrap.dedent("""
        import gc
        import json
        import socket
        import sys
        from pathlib import Path

        def offline(*args, **kwargs):
            raise AssertionError("Offline training attempted network access")

        socket.socket.connect = offline

        import torch
        from laya import Agent
        from export import load_onnx, make_batch, quantize_bundle, train_and_export

        torch.set_num_threads(2)
        source, destination = map(Path, sys.argv[1:3])
        state, questions = map(json.loads, sys.argv[3:5])
        agent = Agent(str(source), device="cpu")
        before_choice = agent.model.scorer[-1].weight.detach().clone()
        before_action = agent.model.act_head[-1].weight.detach().clone()
        batch = make_batch(agent, state, questions)
        trained = train_and_export(
            agent, batch, torch.tensor([1, 2, 1]), destination,
            act_targets=torch.tensor([1, 0, 1]),
        )
        choice_delta = float((agent.model.scorer[-1].weight - before_choice).abs().max())
        action_delta = float((agent.model.act_head[-1].weight - before_action).abs().max())
        assert choice_delta > 0 and action_delta > 0
        assert torch.isfinite(torch.tensor(trained["losses"])).all()
        assert (trained["checkpoint"] / "model.safetensors").is_file()
        del agent
        gc.collect()

        graph = quantize_bundle(destination / "onnx", destination / "int8")
        assert graph.is_file() and (graph.parent / "model.onnx.data").is_file()
        runtime = load_onnx(graph.parent)
        assert set(runtime.predict(state, questions)["answers"]) == set(questions)
        del runtime
        gc.collect()
        reloaded = Agent(str(trained["checkpoint"]), device="cpu")
        assert not torch.equal(reloaded.model.act_head[-1].weight, before_action)
        print(json.dumps({"offline-training-ready": True,
                          "choice_delta": choice_delta, "action_delta": action_delta,
                          "int8_graph_bytes": graph.stat().st_size}))
    """)
    working_directory = tmp_path / "unrelated-cwd"
    working_directory.mkdir()
    unrelated_weights = working_directory / "model.onnx.data"
    unrelated_weights.write_bytes(b"unrelated weights")
    env = os.environ.copy()
    env.update(
        HF_HOME=str(tmp_path / "empty-hf-cache"),
        HF_HUB_OFFLINE="1",
        TRANSFORMERS_OFFLINE="1",
        PYTHONDONTWRITEBYTECODE="1",
        PYTHONPATH=os.pathsep.join(
            filter(None, (str(Path(__file__).resolve().parent), env.get("PYTHONPATH")))
        ),
    )
    result = subprocess.run(
        [
            str(python),
            "-c",
            script,
            str(checkpoint),
            str(tmp_path / "trained"),
            json.dumps(STATE),
            json.dumps(QUESTIONS),
        ],
        capture_output=True,
        text=True,
        timeout=3600,
        check=False,
        env=env,
        cwd=working_directory,
    )
    assert unrelated_weights.read_bytes() == b"unrelated weights"
    assert result.returncode == 0, (result.stdout[-2000:], result.stderr[-3000:])
    assert '"offline-training-ready": true' in result.stdout
    print(result.stdout[-400:])


@pytest.mark.skipif(
    not all(
        os.environ.get(name)
        for name in (
            "VIS_LAYA_OFFLINE_TRAIN_PYTHON",
            "VIS_LAYA_OFFLINE_CHECKPOINT",
            "VIS_LAYA_OFFLINE_FP32",
        )
    ),
    reason="Set VIS_LAYA_OFFLINE_TRAIN_PYTHON, VIS_LAYA_OFFLINE_CHECKPOINT and VIS_LAYA_OFFLINE_FP32",
)
def test_release_bundles_train_and_reload_fp32_without_network(tmp_path):
    """An offline wheelhouse and the two independent release bundles complete the FP32 cycle."""
    python = Path(os.environ["VIS_LAYA_OFFLINE_TRAIN_PYTHON"])
    checkpoint = Path(os.environ["VIS_LAYA_OFFLINE_CHECKPOINT"])
    fp32 = Path(os.environ["VIS_LAYA_OFFLINE_FP32"])
    for directory, files in (
        (
            checkpoint,
            ("model.safetensors", "encoder/config.json", "tokenizer/tokenizer.json"),
        ),
        (fp32, ("model.onnx", "rl_agent_config.json", "tokenizer/tokenizer.json")),
    ):
        for name in files:
            assert (directory / name).is_file(), f"{directory}: {name}"

    script = textwrap.dedent("""
        import gc
        import json
        import socket
        import sys
        from pathlib import Path

        def offline(*args, **kwargs):
            raise AssertionError("An offline FP32 cycle attempted network access")

        socket.socket.connect = offline

        import numpy as np
        import torch
        from laya import Agent
        from export import load_onnx, make_batch, train_and_export

        torch.set_num_threads(2)
        checkpoint, fp32, destination = map(Path, sys.argv[1:4])
        state, questions = map(json.loads, sys.argv[4:6])
        original = Agent(str(checkpoint), device="cpu")
        baseline = load_onnx(fp32)
        batch = make_batch(original, state, questions)
        with torch.no_grad():
            expected_choice, expected_act = original.model(*batch)
        baseline_choice, baseline_act = baseline.model(*batch)
        np.testing.assert_allclose(baseline_choice.numpy(), expected_choice.numpy(), rtol=1e-4, atol=1e-4)
        np.testing.assert_allclose(baseline_act.numpy(), expected_act.numpy(), rtol=1e-4, atol=1e-4)
        assert set(baseline.predict(state, questions)["answers"]) == set(questions)
        del baseline
        gc.collect()

        trained = train_and_export(
            original, batch, torch.tensor([1, 2, 1]), destination,
            act_targets=torch.tensor([1, 0, 1]),
        )
        assert trained["graph"].is_file()
        assert (trained["checkpoint"] / "model.safetensors").is_file()
        del original
        gc.collect()

        restored = Agent(str(trained["checkpoint"]), device="cpu")
        exported = load_onnx(trained["graph"].parent)
        with torch.no_grad():
            expected_choice, expected_act = restored.model(*batch)
        actual_choice, actual_act = exported.model(*batch)
        np.testing.assert_allclose(actual_choice.numpy(), expected_choice.numpy(), rtol=1e-4, atol=1e-4)
        np.testing.assert_allclose(actual_act.numpy(), expected_act.numpy(), rtol=1e-4, atol=1e-4)
        assert set(exported.predict(state, questions)["answers"]) == set(questions)
        print("offline-fp32-release-cycle: ready")
    """)
    env = os.environ.copy()
    env.update(
        HF_HOME=str(tmp_path / "empty-hf-cache"),
        HF_HUB_OFFLINE="1",
        TRANSFORMERS_OFFLINE="1",
        PYTHONDONTWRITEBYTECODE="1",
        PYTHONPATH=str(Path(__file__).resolve().parent),
    )
    result = subprocess.run(
        [
            str(python),
            "-c",
            script,
            str(checkpoint),
            str(fp32),
            str(tmp_path / "trained"),
            json.dumps(STATE),
            json.dumps(QUESTIONS),
        ],
        capture_output=True,
        text=True,
        timeout=3600,
        check=False,
        env=env,
        cwd=tmp_path,
    )
    assert result.returncode == 0, (result.stdout[-1500:], result.stderr[-3000:])
    assert "offline-fp32-release-cycle: ready" in result.stdout
