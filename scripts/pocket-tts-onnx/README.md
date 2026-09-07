# pocket-tts → ONNX

`bin/export-pocket-tts` uses these wrappers to export Kyutai's pocket-tts
modules with `torch.onnx.export` and quantize the results to int8.

| | |
|---|---|
| model code | `pocket-tts==1.0.3` on PyPI — Kyutai's own package, installed by the build |
| weights | [kyutai/pocket-tts](https://huggingface.co/kyutai/pocket-tts) — CC BY 4.0, checkpoint `b6369a24` |
| this layer | MIT, vendored here and pinned by content |
| licence text | `LICENSE` — as received, and it names no copyright holder |

The export code is vendored so builds use a fixed revision. Update it here
when sherpa-onnx requires a different graph format.

```
export_mimi_and_conditioner.py   mimi_encoder.onnx, mimi_decoder.onnx, text_conditioner.onnx
export_flow_lm.py                flow_lm_main.onnx, flow_lm_flow.onnx
quantize.py                      *_int8.onnx (dynamic, MatMul only, for broad CPU support)
onnx_export/                     state flattening and the traced module wrappers
```

Each script writes to `--output_dir`. Run them through `bin/export-pocket-tts`,
which downloads weights, runs all three scripts and packages the seven files
required by sherpa-onnx after a successful speech round-trip test.
