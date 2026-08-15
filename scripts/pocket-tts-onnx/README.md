# pocket-tts → ONNX

The export layer behind `bin/export-pocket-tts`: it wraps Kyutai's pocket-tts
modules so `torch.onnx.export` can trace them, and quantizes the result to int8.

| | |
|---|---|
| model code | `pocket-tts==1.0.3` on PyPI — Kyutai's own package, installed by the build |
| weights | [kyutai/pocket-tts](https://huggingface.co/kyutai/pocket-tts) — CC BY 4.0, checkpoint `b6369a24` |
| this layer | MIT, vendored here and pinned by content |
| licence text | `LICENSE` — as received, and it names no copyright holder |

Vendored rather than fetched at build time: the build then pulls no code it has
not pinned, the revision cannot move under a release, and the graphs are ours
to fix when sherpa-onnx changes what it reads.

```
export_mimi_and_conditioner.py   mimi_encoder.onnx, mimi_decoder.onnx, text_conditioner.onnx
export_flow_lm.py                flow_lm_main.onnx, flow_lm_flow.onnx
quantize.py                      *_int8.onnx (dynamic, MatMul only, for broad CPU support)
onnx_export/                     state flattening and the traced module wrappers
```

Each script writes into `--output_dir`; nothing here is run by hand — `bin/export-pocket-tts`
downloads the weights, runs all three, keeps the seven files sherpa-onnx reads, and refuses
to package a bundle that fails its speech round trip.
