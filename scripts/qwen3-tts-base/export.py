"""Export the Qwen3-TTS-12Hz-1.7B-Base graphs that Vis publishes.

The onnx-community export runs the whole code predictor once for each of the 15
codebooks of a frame. These graphs run it one row at a time with a KV cache:

  code_predictor_step.onnx   rows and a KV cache to the logits of one codebook (int8)
  code_predictor_embed.onnx  a code of one codebook to its input row (float16 table)

The int4 talker and text embedding of onnx-community compute with float32
activations. These copies set accuracy_level 4 (int8 activations), which is
several times faster on a CPU:

  talker_cache.onnx  the talker with a KV cache
  text_embed.onnx    text token ids to projected embeddings

The checks compare the logits with the onnx-community graphs.
"""

import argparse
import json
import math
import struct
import urllib.request
from pathlib import Path

import numpy as np
import onnx
import onnxruntime as ort
import torch
import torch.nn as nn
import torch.nn.functional as F
from onnxruntime.quantization.matmul_nbits_quantizer import MatMulNBitsQuantizer
from torch.export import Dim

WEIGHTS = "https://huggingface.co/Qwen/Qwen3-TTS-12Hz-1.7B-Base/resolve/fd4b254389122332181a7c3db7f27e918eec64e3/model.safetensors"
ONNX_COMMUNITY = "https://huggingface.co/onnx-community/Qwen3-TTS-12Hz-1.7B-Base/resolve/16ec6f3335d919e0e007d825a6f47fdd2cd77594"
PREFIX = "talker.code_predictor."

H, LAYERS, NQ, NKV, D, INTER, THETA, EPS, GROUPS, VOCAB = (
    1024,
    5,
    16,
    8,
    128,
    3072,
    1e6,
    1e-6,
    15,
    2048,
)


def fetch(url, start, end):
    request = urllib.request.Request(url, headers={"Range": f"bytes={start}-{end}"})
    with urllib.request.urlopen(request, timeout=600) as response:
        return response.read()


def code_predictor_weights():
    """The code predictor tensors of the Base model, read by range from the checkpoint."""
    size = struct.unpack("<Q", fetch(WEIGHTS, 0, 7))[0]
    header = json.loads(fetch(WEIGHTS, 8, 8 + size - 1))
    weights = {}
    for name, meta in header.items():
        if not name.startswith(PREFIX):
            continue
        start, end = meta["data_offsets"]
        raw = fetch(WEIGHTS, 8 + size + start, 8 + size + end - 1)
        assert meta["dtype"] == "BF16" and len(raw) == end - start, name
        tensor = torch.frombuffer(bytearray(raw), dtype=torch.bfloat16).reshape(
            meta["shape"]
        )
        weights[name[len(PREFIX) :]] = tensor.float()
    return weights


class RMSNorm(nn.Module):
    def __init__(self, n):
        super().__init__()
        self.weight = nn.Parameter(torch.ones(n))

    def forward(self, x):
        return x * torch.rsqrt(x.pow(2).mean(-1, keepdim=True) + EPS) * self.weight


def rotate(x):
    a, b = x.chunk(2, -1)
    return torch.cat((-b, a), -1)


class Layer(nn.Module):
    def __init__(self):
        super().__init__()
        self.q = nn.Linear(H, NQ * D, bias=False)
        self.k = nn.Linear(H, NKV * D, bias=False)
        self.v = nn.Linear(H, NKV * D, bias=False)
        self.o = nn.Linear(NQ * D, H, bias=False)
        self.q_norm, self.k_norm = RMSNorm(D), RMSNorm(D)
        self.ln1, self.ln2 = RMSNorm(H), RMSNorm(H)
        self.gate = nn.Linear(H, INTER, bias=False)
        self.up = nn.Linear(H, INTER, bias=False)
        self.down = nn.Linear(INTER, H, bias=False)

    def forward(self, x, cos, sin, past_k, past_v, mask):
        b, t, _ = x.shape
        y = self.ln1(x)
        q = self.q_norm(self.q(y).view(b, t, NQ, D)).transpose(1, 2)
        k = self.k_norm(self.k(y).view(b, t, NKV, D)).transpose(1, 2)
        v = self.v(y).view(b, t, NKV, D).transpose(1, 2)
        q = q * cos + rotate(q) * sin
        k = torch.cat([past_k, k * cos + rotate(k) * sin], 2)
        v = torch.cat([past_v, v], 2)
        scores = (
            q @ k.repeat_interleave(NQ // NKV, 1).transpose(-1, -2) / math.sqrt(D)
            + mask
        )
        a = torch.softmax(scores, -1) @ v.repeat_interleave(NQ // NKV, 1)
        x = x + self.o(a.transpose(1, 2).reshape(b, t, NQ * D))
        y = self.ln2(x)
        return x + self.down(F.silu(self.gate(y)) * self.up(y)), k, v


class Step(nn.Module):
    """Rows [1, cur, 2048] and the KV cache to the logits of codebook `head` + 1."""

    def __init__(self, w):
        super().__init__()
        self.proj = nn.Linear(2048, H)
        self.proj.weight.data, self.proj.bias.data = (
            w["small_to_mtp_projection.weight"],
            w["small_to_mtp_projection.bias"],
        )
        self.layers = nn.ModuleList(Layer() for _ in range(LAYERS))
        names = {
            "q": "self_attn.q_proj",
            "k": "self_attn.k_proj",
            "v": "self_attn.v_proj",
            "o": "self_attn.o_proj",
            "gate": "mlp.gate_proj",
            "up": "mlp.up_proj",
            "down": "mlp.down_proj",
        }
        for i, layer in enumerate(self.layers):
            p = f"model.layers.{i}."
            for attr, name in names.items():
                getattr(layer, attr).weight.data = w[p + name + ".weight"]
            layer.q_norm.weight.data = w[p + "self_attn.q_norm.weight"]
            layer.k_norm.weight.data = w[p + "self_attn.k_norm.weight"]
            layer.ln1.weight.data = w[p + "input_layernorm.weight"]
            layer.ln2.weight.data = w[p + "post_attention_layernorm.weight"]
        self.norm = RMSNorm(H)
        self.norm.weight.data = w["model.norm.weight"]
        self.heads = nn.Parameter(
            torch.stack(
                [w[f"lm_head.{j}.weight"].T for j in range(GROUPS)]
            ).contiguous()
        )
        self.register_buffer(
            "inv_freq",
            1.0 / (THETA ** (torch.arange(0, D, 2).float() / D)),
            persistent=False,
        )

    def forward(self, inputs_embeds, head, past):
        t, p = inputs_embeds.shape[1], past[0].shape[2]
        positions = torch.arange(t) + p
        freqs = positions[:, None].float() * self.inv_freq[None, :]
        angles = torch.cat([freqs, freqs], -1)
        cos, sin = angles.cos()[None, None], angles.sin()[None, None]
        rows, cols = torch.arange(t)[:, None] + p, torch.arange(p + t)[None, :]
        mask = torch.where(cols <= rows, 0.0, float("-inf"))[None, None]
        x, present = self.proj(inputs_embeds), []
        for i, layer in enumerate(self.layers):
            x, k, v = layer(x, cos, sin, past[2 * i], past[2 * i + 1], mask)
            present += [k, v]
        logits = self.norm(x)[:, -1] @ torch.index_select(self.heads, 0, head).squeeze(
            0
        )
        return (logits, *present)


class Embed(nn.Module):
    """Codes and their codebook index (code j uses table j - 1) to input rows."""

    def __init__(self, w):
        super().__init__()
        table = torch.stack(
            [w[f"model.codec_embedding.{j}.weight"] for j in range(GROUPS)]
        )
        self.table = nn.Parameter(table.reshape(GROUPS * VOCAB, 2048).half())

    def forward(self, codes, group):
        return (
            torch.index_select(self.table, 0, group * VOCAB + codes)
            .float()
            .unsqueeze(0)
        )


PAST = [f"past_{kind}_{i}" for i in range(LAYERS) for kind in ("key", "value")]
PRESENT = [f"present_{kind}_{i}" for i in range(LAYERS) for kind in ("key", "value")]


def export(w, out):
    step, embed = Step(w).eval(), Embed(w).eval()
    rows = torch.randn(1, 2, 2048)
    past = tuple(torch.zeros(1, NKV, 3, D) for _ in PAST)
    cur, prior = Dim("cur", min=1, max=64), Dim("past", min=0, max=64)
    with torch.no_grad():
        program = torch.onnx.export(
            step,
            (rows, torch.tensor([0]), past),
            input_names=["inputs_embeds", "head", *PAST],
            output_names=["logits", *PRESENT],
            opset_version=18,
            dynamo=True,
            optimize=True,
            external_data=False,
            dynamic_shapes=({1: cur}, None, tuple({2: prior} for _ in PAST)),
        )
        program.save(str(out / "code_predictor_step_fp32.onnx"))
        torch.onnx.export(
            embed,
            (torch.tensor([5]), torch.tensor([2])),
            str(out / "code_predictor_embed.onnx"),
            input_names=["codes", "group"],
            output_names=["embeds"],
            opset_version=17,
            dynamo=False,
            dynamic_axes={"codes": {0: "n"}, "group": {0: "n"}, "embeds": {1: "n"}},
        )
    quantizer = MatMulNBitsQuantizer(
        onnx.load(str(out / "code_predictor_step_fp32.onnx")),
        block_size=32,
        is_symmetric=True,
        bits=8,
        accuracy_level=4,
    )
    quantizer.process()
    quantizer.model.save_model_to_file(
        str(out / "code_predictor_step.onnx"), use_external_data_format=False
    )
    (out / "code_predictor_step_fp32.onnx").unlink()


def int8_activations(work, out):
    """The int4 graphs of onnx-community with accuracy_level 4 on each MatMulNBits node."""
    for name in ("talker_cache.onnx", "text_embed.onnx"):
        source = work / f"int4_{name}"
        if not source.exists():
            urllib.request.urlretrieve(f"{ONNX_COMMUNITY}/cpu_int4/{name}", source)
        model = onnx.load(str(source))
        for node in model.graph.node:
            if node.op_type == "MatMulNBits":
                kept = [a for a in node.attribute if a.name != "accuracy_level"]
                del node.attribute[:]
                node.attribute.extend(
                    kept + [onnx.helper.make_attribute("accuracy_level", 4)]
                )
        onnx.save(model, str(out / name))


def check_talker(out, work):
    """Compare a prefill of the int8-activation talker with the onnx-community talker."""
    urllib.request.urlretrieve(
        f"{ONNX_COMMUNITY}/cpu_int4/codec_embed.onnx", work / "int4_codec_embed.onnx"
    )
    text = ort.InferenceSession(str(work / "int4_text_embed.onnx"))
    codec = ort.InferenceSession(str(work / "int4_codec_embed.onnx"))
    rng = np.random.default_rng(0)
    rows = text.run(
        None, {"text_ids": rng.integers(1000, 20000, (1, 12)).astype(np.int64)}
    )[0]
    rows = (
        rows
        + codec.run(
            None, {"codec_ids": rng.integers(0, VOCAB, (1, 12)).astype(np.int64)}
        )[0]
    )
    n = rows.shape[1]
    outputs = []
    for path in (work / "int4_talker_cache.onnx", out / "talker_cache.onnx"):
        session = ort.InferenceSession(str(path))
        feed = {
            "inputs_embeds": rows.astype(np.float32),
            "position_ids": np.tile(np.arange(n), (3, 1, 1)),
            "attention_mask": np.ones((1, n), np.int64),
        }
        feed.update(
            {
                i.name: np.zeros((1, NKV, 0, D), np.float32)
                for i in session.get_inputs()[3:]
            }
        )
        outputs.append(session.run(None, feed)[0].reshape(n, -1))
    want, got = outputs
    cosines = (
        (want * got).sum(-1)
        / np.linalg.norm(want, axis=-1)
        / np.linalg.norm(got, axis=-1)
    )
    agree = int((want.argmax(-1) == got.argmax(-1)).sum())
    print(f"talker logit cosine min {cosines.min():.4f}, same argmax {agree}/{n}")
    assert cosines.min() > 0.99 and agree >= 0.75 * n, (
        "the talker does not match the reference"
    )


def check(out, work):
    """Compare the cached logits with the fp32 onnx-community graph, which reads a whole frame."""
    for name in ("code_predictor.onnx", "codec_embed.onnx"):
        target = work / name
        if not target.exists():
            urllib.request.urlretrieve(f"{ONNX_COMMUNITY}/cpu_fp32/{name}", target)
    reference = ort.InferenceSession(str(work / "code_predictor.onnx"))
    codec = ort.InferenceSession(str(work / "codec_embed.onnx"))
    step = ort.InferenceSession(str(out / "code_predictor_step.onnx"))
    embed = ort.InferenceSession(str(out / "code_predictor_embed.onnx"))
    rng, cosines, agree = np.random.default_rng(0), [], 0
    for _ in range(8):
        hidden = (rng.standard_normal((1, 2048)) * 0.5).astype(np.float32)
        codes = rng.integers(0, VOCAB, (1, 16)).astype(np.int64)
        expected = reference.run(None, {"talker_hidden": hidden, "codec_ids": codes})[
            0
        ][0]
        first = codec.run(None, {codec.get_inputs()[0].name: codes[:, :1]})[0].reshape(
            1, 1, 2048
        )
        feed = {
            "inputs_embeds": np.concatenate([hidden.reshape(1, 1, 2048), first], 1),
            "head": np.array([0]),
        }
        feed.update({name: np.zeros((1, NKV, 0, D), np.float32) for name in PAST})
        for j in range(GROUPS):
            result = step.run(None, feed)
            got, want = result[0][0], expected[j]
            cosines.append(
                float(got @ want / np.linalg.norm(got) / np.linalg.norm(want))
            )
            agree += int(got.argmax() == want.argmax())
            if j + 1 < GROUPS:
                rows = embed.run(
                    None, {"codes": codes[0, j + 1 : j + 2], "group": np.array([j])}
                )[0]
                feed = {
                    "inputs_embeds": rows,
                    "head": np.array([j + 1]),
                    **dict(zip(PAST, result[1:], strict=True)),
                }
    print(
        f"logit cosine min {min(cosines):.4f} mean {np.mean(cosines):.4f}, same argmax {agree}/{len(cosines)}"
    )
    assert min(cosines) > 0.97 and agree >= 0.85 * len(cosines), (
        "the export does not match the reference"
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--work", type=Path, required=True)
    args = parser.parse_args()
    args.out.mkdir(parents=True, exist_ok=True)
    args.work.mkdir(parents=True, exist_ok=True)
    export(code_predictor_weights(), args.out)
    check(args.out, args.work)
    int8_activations(args.work, args.out)
    check_talker(args.out, args.work)


if __name__ == "__main__":
    main()
