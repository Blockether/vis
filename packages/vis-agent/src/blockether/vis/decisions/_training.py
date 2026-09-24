"""Export the complete Laya ModernBERT decision graph, not just its encoder."""

import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import onnx
import onnxruntime as ort
import torch
from huggingface_hub import snapshot_download
from laya import Agent
from laya.common import QTYPES, build_sequence, clamp_temperature, collate_items
from safetensors.torch import save_file
from transformers import AutoTokenizer

MODEL_ID = "convaiinnovations/laya"
REVISION = "5e7b2b1b8ca2ecdd3f2322d94069c9b6ce7e844b"
INPUT_NAMES = ("input_ids", "attention_mask", "marker_pos", "marker_mask", "qtype")


def load_reference():
    """Load only the pinned ModernBERT checkpoint, never its sibling models."""
    source = snapshot_download(
        MODEL_ID,
        revision=REVISION,
        allow_patterns=[
            "model.safetensors",
            "rl_agent_config.json",
            "encoder/*",
            "tokenizer/*",
        ],
        local_files_only=True,
    )
    return Agent(source, device="cpu")


def make_batch(agent, state, questions):
    """Use Laya's own serialization, option markers and padding as the reference."""
    items = []
    for qid, definition in questions.items():
        agent._check_question(qid, definition)
        question = agent._to_internal(definition)
        ids, markers = build_sequence(
            agent.tok, state, question, agent.cfg["max_len"], agent.cfg["head_max_len"]
        )
        items.append({"ids": ids, "markers": markers, "qtype": QTYPES[question["t"]]})
    batch = collate_items([items], agent.tok.pad_token_id)
    return tuple(batch[name] for name in INPUT_NAMES)


class ExportSelfAttention(torch.nn.Module):
    """Express the decision head's attention with genuinely dynamic ONNX shapes.

    The legacy PyTorch MHA exporter freezes the example's sequence length in a
    Reshape node. Reuse its exact parameters and math without that export path.
    This adapter is installed only during export and never used for training.
    """

    def __init__(self, attention):
        super().__init__()
        if (
            not attention.batch_first
            or attention.bias_k is not None
            or attention.add_zero_attn
        ):
            raise ValueError("Expected standard batch-first self-attention")
        self.in_proj_weight = attention.in_proj_weight
        self.in_proj_bias = attention.in_proj_bias
        self.out_proj = attention.out_proj
        self.num_heads = attention.num_heads
        self.embed_dim = attention.embed_dim
        self.dropout = attention.dropout

    def forward(
        self,
        query,
        key,
        value,
        key_padding_mask=None,
        need_weights=False,
        attn_mask=None,
        average_attn_weights=True,
        is_causal=False,
    ):
        if (
            query is not key
            or query is not value
            or attn_mask is not None
            or is_causal
            or need_weights
        ):
            raise ValueError("Only unmasked self-attention with padding is supported")
        batch, length, _ = query.shape
        width = self.embed_dim // self.num_heads
        projected = torch.nn.functional.linear(
            query, self.in_proj_weight, self.in_proj_bias
        )
        projected = projected.reshape(batch, length, 3, self.num_heads, width)
        q, k, v = projected.permute(2, 0, 3, 1, 4).unbind(0)
        scores = torch.matmul(q * (width**-0.5), k.transpose(-2, -1))
        if key_padding_mask is not None:
            mask = key_padding_mask[:, None, None, :]
            if mask.dtype == torch.bool:
                scores = scores.masked_fill(mask, float("-inf"))
            else:
                scores = scores + mask
        probabilities = torch.softmax(scores, dim=-1)
        probabilities = torch.nn.functional.dropout(
            probabilities, self.dropout, self.training
        )
        context = torch.matmul(probabilities, v).transpose(1, 2)
        context = context.reshape(batch, length, self.embed_dim)
        return self.out_proj(context), None


def export_model(agent, destination, example):
    """Write a local bundle with dynamic question, token and option dimensions."""
    if agent.model.encoder.config.model_type != "modernbert":
        raise ValueError("Only ModernBERT is supported by this exporter")
    if example[2].shape[1] < 2:
        raise ValueError("The export example must contain at least two option slots")
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    graph = destination / "model.onnx"
    fastpath = torch.backends.mha.get_fastpath_enabled()
    originals = []
    try:
        torch.backends.mha.set_fastpath_enabled(False)
        if agent.model.head is not None:
            for layer in agent.model.head.layers:
                originals.append((layer, layer.self_attn))
                layer.self_attn = ExportSelfAttention(layer.self_attn).eval()
        with torch.no_grad():
            torch.onnx.export(
                agent.model.eval(),
                example,
                str(graph),
                input_names=list(INPUT_NAMES),
                output_names=["logits", "act_logits"],
                dynamic_axes={
                    "input_ids": {0: "questions", 1: "tokens"},
                    "attention_mask": {0: "questions", 1: "tokens"},
                    "marker_pos": {0: "questions", 1: "options"},
                    "marker_mask": {0: "questions", 1: "options"},
                    "qtype": {0: "questions"},
                    "logits": {0: "questions", 1: "options"},
                    "act_logits": {0: "questions"},
                },
                opset_version=17,
                dynamo=False,
                do_constant_folding=False,
            )
    finally:
        for layer, attention in originals:
            layer.self_attn = attention
        torch.backends.mha.set_fastpath_enabled(fastpath)
    onnx.checker.check_model(str(graph))
    (destination / "rl_agent_config.json").write_text(json.dumps(agent.cfg, indent=2))
    agent.tok.save_pretrained(destination / "tokenizer")
    return graph


def quantize_bundle(source, destination):
    """Make a deployable weight-only INT8 bundle from a full FP32 export.

    ORT basic optimization folds ModernBERT linear-layer weight transposes before
    quantization. Quantizing the raw export silently leaves those layers FP32.
    Keep source, temporary optimization and destination separate; publish only
    the graph, its external weights, tokenizer and runtime configuration.
    """
    source = Path(source).resolve()
    destination = Path(destination).resolve()
    if destination.exists():
        raise FileExistsError(f"Bundle already exists: {destination}")
    graph = source / "model.onnx"
    if not graph.is_file():
        raise FileNotFoundError(graph)
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=destination.parent) as temporary:
        temporary = Path(temporary)
        optimized = temporary / "optimized.onnx"
        options = ort.SessionOptions()
        options.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_BASIC
        options.optimized_model_filepath = str(optimized)
        options.intra_op_num_threads = 4
        ort.InferenceSession(
            str(graph), sess_options=options, providers=["CPUExecutionProvider"]
        )
        bundle = temporary / "bundle"
        bundle.mkdir()
        output = bundle / "model.onnx"
        # ONNX checks the external-data name against process cwd, not the output
        # directory. Isolate that cwd without changing it for other threads.
        quantization = (
            "import sys\n"
            "from onnxruntime.quantization import QuantType, quantize_dynamic\n"
            "quantize_dynamic(\n"
            "    sys.argv[1], sys.argv[2],\n"
            '    op_types_to_quantize=["MatMul", "Gemm"],\n'
            "    per_channel=True, weight_type=QuantType.QInt8,\n"
            "    use_external_data_format=True,\n"
            ")\n"
        )
        subprocess.run(
            [sys.executable, "-c", quantization, str(optimized), str(output)],
            cwd=temporary,
            check=True,
        )
        model = onnx.load(str(output), load_external_data=False)
        if sum(node.op_type == "MatMulInteger" for node in model.graph.node) < 100:
            raise ValueError("INT8 graph is missing the ModernBERT linear layers")
        if not (bundle / "model.onnx.data").is_file():
            raise ValueError("INT8 external weights are missing")
        shutil.copy2(source / "rl_agent_config.json", bundle)
        shutil.copytree(source / "tokenizer", bundle / "tokenizer")
        onnx.checker.check_model(str(output))
        bundle.rename(destination)
    return destination / "model.onnx"


class OnnxGraph(torch.nn.Module):
    """Run all neural computation in ORT behind Laya's existing tensor interface."""

    def __init__(self, graph):
        super().__init__()
        options = ort.SessionOptions()
        options.intra_op_num_threads = 4
        options.inter_op_num_threads = 1
        self.session = ort.InferenceSession(
            str(graph), sess_options=options, providers=["CPUExecutionProvider"]
        )

    def forward(self, input_ids, attention_mask, marker_pos, marker_mask, qtype):
        original_options = marker_pos.shape[1]
        if original_options == 1:
            marker_pos = torch.nn.functional.pad(marker_pos, (0, 1))
            marker_mask = torch.nn.functional.pad(marker_mask, (0, 1), value=False)
        values = (input_ids, attention_mask, marker_pos, marker_mask, qtype)
        feed = {
            name: value.detach().cpu().numpy()
            for name, value in zip(INPUT_NAMES, values, strict=True)
        }
        logits, act = self.session.run(None, feed)
        return torch.from_numpy(logits[:, :original_options]), torch.from_numpy(act)


def load_onnx(directory):
    """Load only local ONNX/config/tokenizer files, without loading PyTorch weights."""
    directory = Path(directory)
    agent = Agent.__new__(Agent)
    agent.cfg = json.loads((directory / "rl_agent_config.json").read_text())
    agent.tok = AutoTokenizer.from_pretrained(
        directory / "tokenizer", local_files_only=True
    )
    agent.device = torch.device("cpu")
    agent.dtype = torch.float32
    agent.temperature = [clamp_temperature(t) for t in agent.cfg["temperature"]]
    agent.temperature_by_options = {
        key: clamp_temperature(value)
        for key, value in agent.cfg.get("temperature_by_options", {}).items()
    }
    agent.model = OnnxGraph(directory / "model.onnx")
    return agent


def train_and_export(
    agent, example, targets, destination, *, act_targets, steps=1, learning_rate=1e-4
):
    """Smoke-test full-parameter fine-tuning, checkpoint persistence and auto export.

    This supervised SGD exercise trains both decision and action heads and proves
    persistence mechanics, not RLCD quality or calibration. Production training
    requires labeled data and an evaluation policy.
    """
    if steps < 1:
        raise ValueError("Training requires at least one step")
    torch.manual_seed(42)
    optimizer = torch.optim.SGD(agent.model.parameters(), lr=learning_rate)
    losses = []
    agent.model.train()
    for _ in range(steps):
        optimizer.zero_grad(set_to_none=True)
        logits, act_logits = agent.model(*example)
        loss = torch.nn.functional.cross_entropy(
            logits, targets
        ) + torch.nn.functional.cross_entropy(act_logits, act_targets)
        if not torch.isfinite(loss):
            raise ValueError("Non-finite training loss")
        loss.backward()
        torch.nn.utils.clip_grad_norm_(
            agent.model.parameters(), 1.0, error_if_nonfinite=True
        )
        optimizer.step()
        losses.append(float(loss.detach()))
    optimizer.zero_grad(set_to_none=True)
    agent.model.eval()
    destination = Path(destination)
    checkpoint = destination / "checkpoint"
    checkpoint.mkdir(parents=True, exist_ok=True)
    save_file(agent.model.state_dict(), str(checkpoint / "model.safetensors"))
    (checkpoint / "rl_agent_config.json").write_text(json.dumps(agent.cfg, indent=2))
    agent.model.encoder.config.save_pretrained(checkpoint / "encoder")
    agent.tok.save_pretrained(checkpoint / "tokenizer")
    graph = export_model(agent, destination / "onnx", example)
    return {"checkpoint": checkpoint, "graph": graph, "losses": losses}
