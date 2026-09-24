# Decision models

Vis can answer typed `choice`, `score` and `noul` questions with the Laya ModernBERT
decision model. Each answer also includes an action-versus-escalation score. The
published baseline is a starting point, **not** a policy for taking actions on your
behalf: collect representative labels for your use case, evaluate both heads and decide
when a human should review the result before relying on it.

The `assets-pack` release keeps the existing voice assets and adds a pinned FP32
inference bundle, a complete training checkpoint and offline training dependencies.
Model downloads are explicit; starting a gateway never downloads weights. Inside Vis Python,
a small first-party client reads and infers without installing the full Python SDK or
any third-party packages. Install the full `vis-agent` SDK separately for training,
upload, aliases and remote clients; its training extra is optional.

## Download the baseline

Install Vis, then download the pinned inference bundle on the machine running your
gateway:

```bash
vis-agent decisions models status
vis-agent decisions models download --model laya-typed-decisions
```

The status command prints the model revision and whether the FP32 bundle is installed.
The download verifies the archive and its file inventory before installation. To train
on that machine, download the additional checkpoint and platform-specific CPython 3.12
wheelhouse:

```bash
vis-agent decisions models download --model laya-typed-decisions --training
```

The command prints the three installation directories and an offline `install.sh`
command for a **new** Python environment. The wheelhouse supports macOS arm64 and Linux
x86-64 with CPython 3.12. Obtain the matching `vis-agent` SDK wheel while online; the
wheelhouse contains its training dependencies, not the SDK wheel. Run the printed
installer, install that SDK wheel into the environment, then disconnect it from the
network if needed. Allow several gigabytes of disk space for the checkpoint, FP32
bundle, training environment and exported versions. Review model licenses and provenance
in
[`THIRD_PARTY_MODELS.md`](https://github.com/Blockether/vis/blob/main/THIRD_PARTY_MODELS.md).

For a client on another machine, use [a secured
gateway](gateway-service.md#connect-from-another-machine). Only the gateway needs the
inference bundle. It will refuse a missing model instead of downloading one during a
request.

## Ask from Vis Python

Once the gateway machine has the inference bundle, you can ask typed questions in a
`python_execution` block without installing a Python package into the sandbox:

```python
import vis_decisions

print(vis_decisions.models())  # Known versions and their installed/resident states.
answer = vis_decisions.infer(
    model="laya-typed-decisions",
    state="A damaged item needs a refund",
    questions={
        "intent": {"type": "choice", "instructions": "Choose a request", "criteria": ["refund", "repair"]},
        "urgency": {"type": "score", "instructions": "Rate urgency", "criteria": ["low", "medium", "high"]},
        "refundable": {"type": "noul", "instructions": "Can the item be refunded?"},
    },
)
print(answer["answers"], answer["routing"]["model"])
```

`vis_decisions.model("laya-typed-decisions")` reads one model's status. These calls use
Vis' existing gateway connection and authentication. They do not download a model or
install a dependency. An uninstalled model raises `vis_decisions.DecisionGatewayError`
with `status == 409`; download the model explicitly with the CLI above. The baseline
`act_probability` in an answer is a diagnostic score, not authorization to act.

The separate `py` extension REPL uses your project's Python interpreter, not the
`python_execution` sandbox. To call Decisions there or from an external Python app,
install the full `vis-agent` SDK in that environment and use the `Decisions` and
`GatewayClient` example below. Only install `[decisions-training]` when you need local
training or export; neither installation downloads model weights automatically.

## Train locally with the Python SDK

Install `vis-agent[decisions-training]` with a supported Python 3.12 environment (or use
the verified offline wheelhouse and SDK wheel). `TrainingBundle.open` reads the complete
checkpoint printed by the CLI;
`TrainingBundle.fetch(model_ref="laya-typed-decisions@<revision>", cache_dir="...")` is
a separate, explicit catalog-pinned download when you have network access. Neither
`open` nor `finetune` fetches weights.

Make separate training and evaluation JSONL files. Every line needs `state`, one Laya
`question`, an integer `target` option index and `action` (`0` for act, `1` for
escalate). For example, a training line can be:

```json
{"state":"A damaged item needs a refund","question":{"type":"choice","instructions":"Choose a request","criteria":["refund","repair"]},"target":0,"action":1}
```

Use *different* labeled examples in `eval.jsonl`. You can also label `score` and `noul`
questions. Put your training settings in `config.json` and minimum held-out accuracies
for **both** heads in `policy.json`:

```json
{"epochs":1,"learning_rate":0.00001,"train_encoder":false,"max_steps":100}
```

```json
{"min_decision_accuracy":0.8,"min_action_accuracy":0.8}
```

Choose thresholds from your own evaluation, not the example values. Then train, export
FP32 and reopen the graph for validation in one call:

```python
from blockether.vis.decisions.training import ModernBertTrainer, TrainingBundle

base = TrainingBundle.open("/path/printed/by/download/training")
with ModernBertTrainer(base) as trainer:
    result = trainer.finetune(
        train_data="train.jsonl",
        eval_data="eval.jsonl",
        training_config="config.json",
        validation_policy="policy.json",
        output_dir="my-new-version",  # must not exist yet
        progress=lambda event: print(event["stage"]),
    )
print(result.checkpoint_dir, result.inference_bundle, result.validation_report)
```

`result.checkpoint_dir` is a complete resumable checkpoint; open it with
`TrainingBundle.open` to train further. The separate `inference_bundle` contains ONNX
FP32, tokenizer, configuration and provenance, not private training rows or checkpoint
weights. To export an existing checkpoint without training, call
`trainer.prepare_fp32(eval_data=..., validation_policy=..., output_dir=...)`. An export
or quality failure does not produce a deployable result, though a successfully saved
checkpoint can remain for diagnosis. Training and export consume substantial local CPU,
RAM and disk; the extra is not needed for inference-only clients.

## Publish explicitly and select a version

Configure `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` as described in the [Python SDK
gateway guide](python-sdk.md#connect-to-a-gateway-and-run-a-task). Do not put the token
in code or logs. The gateway verifies the streamed, inference-only bundle and runs both
heads before registering an immutable `sha256-...` version. Upload never changes a
running alias.

```python
import os

from blockether.vis.decisions import Decisions
from blockether.vis.engine import GatewayClient

with GatewayClient(os.environ["VIS_GATEWAY_URL"], token=os.environ["VIS_GATEWAY_TOKEN"]) as gateway:
    decisions = Decisions(gateway)
    print(decisions.list_models())
    published = decisions.upload_model(result, progress=lambda sent, total: print(sent, total))
    ref = published["model_ref"]
    print(decisions.get_model(ref))
    decisions.activate_model("my-case", ref)  # creates a previously unused alias
    answer = decisions.infer(
        model="my-case",
        state="A damaged item needs a refund",
        questions={
            "intent": {"type": "choice", "instructions": "Choose a request", "criteria": ["refund", "repair"]},
            "urgency": {"type": "score", "instructions": "Rate urgency", "criteria": ["low", "medium", "high"]},
            "refundable": {"type": "noul", "instructions": "Can the item be refunded?"},
        },
    )
    print(answer["answers"], answer["routing"])
```

To switch an existing alias, read `decisions.get_alias("my-case")["model_ref"]` and pass
it as `expected_current=...` to `activate_model`; a concurrent change returns a conflict
instead of silently overwriting it. You can always infer with the immutable `ref` or the
original `laya-typed-decisions` baseline independently. If an upload times out,
`get_model("sha256-" + archive_digest)` lets you check whether the immutable version was
registered before deciding to retry. Do not publish private training checkpoints or row
files as model assets.

## Train on the gateway instead

An operator can set `VIS_DECISION_TRAINING_PYTHON` to a Python 3.12 executable with the
SDK and offline training dependencies, and `VIS_DECISION_TRAINING_DATA_ROOT` to a
directory of approved JSONL/JSON files on the gateway. Install the pinned checkpoint
first with `--training`. The API accepts **filenames in that directory**, not local
laptop paths and not raw data uploads. Each dataset is limited to 16 MiB; configuration
and policy files to 16 KiB. One training job runs at a time, for up to two hours by
default. Other decision inferences are temporarily refused while the trainer owns the
model budget.

```python
job = decisions.start_training(
    train_data="train.jsonl", eval_data="eval.jsonl",
    training_config="config.json", validation_policy="policy.json",
)
print(decisions.get_training_job(job["job_id"]))
# Call get_training_job again to observe stages, steps, metrics and model_ref.
# decisions.cancel_training_job(job["job_id"]) cancels a running job;
# after completion, it deletes the private resumable checkpoint.
```

The gateway stages bounded inputs, launches an isolated offline CPU worker, saves a
private checkpoint and validates a new FP32 inference version. To continue from it, pass
the completed `job_id` as `source_job_id` to `start_training`; do not delete that job
first. Training does not activate an alias. Review the held-out metrics and use
`activate_model` separately. A quality failure, interruption or cancellation leaves
existing versions and aliases unchanged. Metrics on a small sample do **not** establish
domain safety or authorize autonomous actions.

## See also

- [Python SDK](python-sdk.md) — connect to a gateway with your credentials.
- [Running a gateway](gateway-service.md) — secure and operate the service.
