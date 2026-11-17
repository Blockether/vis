# Decision models

Vis answers typed `choice`, `score`, and `noul` questions with decision models that run on your
gateway from a downloaded ONNX bundle. Laya and GLiNER answers also have a score that compares
action with escalation. The published baseline is a start point. It is not a policy for actions on
your behalf. Collect representative labels for your use case and evaluate both heads. Then decide
when a person must review a result.

## When to use

- **Your application classifies requests into a fixed set of labels, such as refund or repair.**
  Ask a typed `choice` question [from Python](#ask-from-python).
- **You also need a rating or a direct answer, such as the urgency of a request.** Ask `score` and
  `noul` questions in the [same call](#ask-from-python).
- **You must decide when a person reviews a result.** Evaluate the action score of
  [Laya and GLiNER2.5 answers](#ask-from-python) on your own labels.
- **Your requests are not in English, your gateway has little memory or CPU, or the smaller models
  classify your English requests badly.** Choose a multilingual, the small, or the large Decide 1B
  [GLiNER2.5 model](#choose-and-train-a-gliner2-5-model).
- **The GLiNER2.5 models do not answer your questions well enough, and you need no action score.**
  Evaluate the [Decision 2.0 models](#choose-and-train-decision-2-0) and the
  [D3 models](#choose-and-train-d3) on your own labels.
- **The baseline does not fit your data.**
  [Train with your own labels](#train-locally-with-the-python-sdk). Then
  [publish a verified version and select it](#publish-explicitly-and-select-a-version).
- **Long training stops before it completes, or you have new labels for a trained version.**
  [Resume or continue training](#resume-or-continue-training).
- **A long input fails or loses late context.** Check the
  [token budget and truncation policy](#handle-long-inputs).
- **You have no local bundle, or you want a second opinion from OpenAI.** Ask an
  [OpenAI decision model](#ask-openai-decision-models) with the same questions.

For open work that needs files or tools, run an agent task with the [Python SDK](python-sdk.md).

## Download the baseline

The `assets-pack` release holds pinned FP32 inference bundles and complete checkpoints, next to
the voice assets. A gateway start never downloads weights. The
[vis-decisions extension](https://github.com/Blockether/vis-decisions) supplies the Python client
and the training runtime. Its one pinned environment, with Transformers 5, supports Laya,
GLiNER2.5, Decision 2.0, and D3.

Install Vis. Then download the pinned inference bundle on the machine that runs your gateway:

```bash
vis-agent decisions models status
vis-agent decisions models download --model laya-typed-decisions
```

The status command prints the model revision and the installation state. A download verifies the
archive and its file inventory. For training, also download the complete checkpoint:

```bash
vis-agent decisions models download --model laya-typed-decisions --training
```

The command prints the inference directory and the training directory. It downloads no Python
dependencies, so install the vis-decisions runtime while you have network access. Keep many
gigabytes free for checkpoints, FP32 bundles, dependencies, and exported versions. Review the
model licenses and provenance in
[`THIRD_PARTY_MODELS.md`](https://github.com/Blockether/vis/blob/main/THIRD_PARTY_MODELS.md).

Only the gateway needs the inference bundle. For a client on a different machine, use
[a secured gateway](gateway-service.md#connect-from-another-machine).

## Ask from Python

Call gateway inference with `Decisions` from the vis-decisions package. Install the package in
your application environment, or in the project interpreter that the `py` extension uses. The
vis-decisions extension also gives local training tools in `python_execution`.

Set `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` as the
[Python SDK gateway guide](python-sdk.md#connect-to-a-gateway-and-run-a-task) describes. Do not
put the token in code or logs. Then ask typed questions:

```python
import os

from blockether.vis_decisions import Decisions
from blockether.vis.engine import GatewayClient

with GatewayClient(os.environ["VIS_GATEWAY_URL"], token=os.environ["VIS_GATEWAY_TOKEN"]) as gateway:
    decisions = Decisions(gateway)
    print(decisions.list_models())  # Installed and in-memory states; nothing is downloaded.
    answer = decisions.infer(
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

These calls never download a model or install a dependency. A model that is not installed raises
`blockether.vis.engine.GatewayError` with `status == 409`. Download it with the CLI. The baseline
`act_probability` in an answer is a diagnostic score, not a permission to act.

### Handle long inputs

Each question has its own token budget. The state, the question type, the instructions, the
option text, and the structural tokens share it. An object or list state counts as its JSON text.

| Model family | Token limit for each question | Above the limit |
| --- | --- | --- |
| GLiNER | `max_position_embeddings` in `encoder_config/config.json` of the installed bundle, and at most 2,048 | The gateway rejects the question |
| Decision 2.0 Eos | 16,384 | The gateway rejects the question |
| Decision 2.0 Kai | 8,192 | The gateway rejects the question |
| D3 | 16,384 | The gateway rejects the question |
| Laya | `max_len` in the installed `config.json`, 512 by default | Laya truncates the input |

For GLiNER, the budget also holds the criterion text and the action labels. Its limit of 2,048
applies also when the encoder accepts more. For
Decision 2.0 and D3, the budget also holds the option keys, the option descriptions, and the
prompt text.

The gateway rejects a longer sequence before model inference, and accepts one exactly at the
limit. It counts tokens with the installed tokenizer, so your application needs no tokenizer. A
character count cannot predict the limit.

For a rejected input, `Decisions.infer` raises `GatewayError` with `status == 400` and
`code == "input-too-long"`. Its `input_tokens` attribute gives the token count of the rejected
sequence, and `max_input_tokens` gives the limit. Both are `None` if the gateway supplies no valid
counts. The error also tells you what to shorten: the state, the instructions, or the criteria.
Check that your results stay correct with less context.

Laya truncates the instructions and the option text to fit the question head. Then it keeps only
the first state tokens that fit the space that remains. Put important context early in the state.

### Ask OpenAI decision models

An OpenAI classifier model, such as GPT-6 Luna, answers the same questions. Each request is a paid
OpenAI call, and the state goes to OpenAI. Images are not supported.

The gateway needs an OpenAI API key. Add the `openai` provider with an API key as
[Configuration](configuration.md) describes, or set `OPENAI_API_KEY` for the gateway. A ChatGPT
(Codex) sign-in does not work with the OpenAI Decisions API.

Name the model as `openai/<id>`. The questions and the answer shapes stay the same:

```python
answer = decisions.infer(
    model="openai/gpt-6-luna",
    state="A damaged item needs a refund",
    questions={"intent": {"type": "choice", "instructions": "Choose a request", "criteria": ["refund", "repair"]}},
)
print(answer["answers"]["intent"]["choice"], answer["routing"]["provider"])  # refund openai
```

`decisions.list_models()` lists OpenAI models with `"residency": "remote"`. Their `available`
field is `true` when the gateway has a key. An OpenAI answer has no `action` score. If OpenAI
declines one question, that answer is `{"type": "refusal"}`. Without a usable key,
`Decisions.infer` raises `GatewayError` with `status == 409`.

## Train locally with the Python SDK

Install the [vis-decisions runtime](https://github.com/Blockether/vis-decisions#development) with
Python 3.12. `TrainingBundle.open` reads the verified checkpoint whose path the CLI printed.
`TrainingBundle.fetch("laya-typed-decisions@<revision>", "cache")` is an explicit, pinned
download. Neither `open` nor `train` fetches weights.

To use the training tools in Vis, add this
[extension declaration](extension-packages.md#declare-packages-in-configuration):

```yaml
extensions:
  vis-decisions:
    source: https://github.com/Blockether/vis-decisions
    version: "0.6.0"
```

Then open a session in that project. For example, ask:

> Train this checkpoint with train.jsonl, eval.jsonl, config.json and policy.json. Save a new version without activating it.

Vis uses the extension tools to verify the checkpoint, train, and report the validation results.

Make separate JSONL files for training and evaluation. Each line needs `state`, one Laya
`question`, an integer `target` option index, and `action`: `0` for act, `1` for escalate. For
example, a training line can be:

```json
{"state":"A damaged item needs a refund","question":{"type":"choice","instructions":"Choose a request","criteria":["refund","repair"]},"target":0,"action":1}
```

Put different labeled examples in `eval.jsonl`. You can also label `score` and `noul` questions.
Put your training settings in `config.json`. Put the minimum held-out accuracy of each of the two
heads in `policy.json`:

```json
{"epochs":1,"learning_rate":0.00001,"train_encoder":false,"max_steps":100}
```

```json
{"min_decision_accuracy":0.8,"min_action_accuracy":0.8}
```

These optional settings go in `config.json`:

- `"device"` selects the training device: `auto`, `cpu`, `mps`, or `cuda`. With MPS, training
  stops before it starts if `PYTORCH_ENABLE_MPS_FALLBACK` is set. That setting runs some work on
  the CPU with no error. If MPS does not support the model, set `"device": "cpu"`.
- `"lora": {"rank": 16}` trains small adapters, not all encoder weights. Keep `train_encoder`
  false. Each checkpoint still stores the full merged weights.

Choose the thresholds from your own evaluation, not from the example values. Then train, export
FP32, and open the graph again for validation in one call:

```python
from blockether.vis_decisions import Trainer, TrainingBundle

base = TrainingBundle.open("/path/printed/by/download/training")
with Trainer(base) as trainer:
    result = trainer.train(
        train_data="train.jsonl",
        eval_data="eval.jsonl",
        training_config="config.json",
        validation_policy="policy.json",
        output_dir="my-new-version",  # must not exist yet
        progress=lambda event: print(event["stage"]),
    )
print(result.checkpoint_dir, result.inference_bundle, result.validation_report)
```

`result.checkpoint_dir` is a complete checkpoint. To train it more, open it with
`TrainingBundle.open`, as [Resume or continue training](#resume-or-continue-training) shows. The
separate `inference_bundle` holds the ONNX FP32 model files, the tokenizer, the configuration, and
the provenance. It holds no private training rows and no checkpoint weights.

To export an existing checkpoint without training, call
`trainer.prepare(eval_data=..., validation_policy=..., output_dir=...)`. If the export or a
quality check fails, you get no deployable result. A saved checkpoint can stay for diagnosis.
Training and export use much local CPU, RAM, and disk. A client that only asks questions does not
load the training runtime.

### Choose and train a GLiNER2.5 model

A GLiNER2.5 model from Fastino can replace Laya. Choose a model for the language of your text and
the memory of your gateway:

| Model ID | Text language | Fastino model | FP32 bundle size | Checkpoint size |
|---|---|---|---|---|
| `gliner2.5-small` | English | Small multi-task model | 293 MB | 263 MB |
| `gliner2.5-base` | English | Default multi-task model | 749 MB | 694 MB |
| `gliner2.5-decide` | English | Classification model | 1.75 GB | 1.80 GB |
| `gliner2.5-decide-1b` | English | Large classification model | 4.14 GB | 4.41 GB |
| `gliner2.5-multi` | Many languages | Multilingual multi-task model | 1.13 GB | 960 MB |
| `gliner2.5-multi-decide` | Many languages | Multilingual classification model | 1.13 GB | 998 MB |

Fastino trained the Decide models for classification. Vis uses each model only for decisions. The small model needs the least memory and
CPU. Evaluate your candidates on your own held-out labels before you choose one.

`gliner2.5-decide-1b` needs these resources:

- While the model is loaded, the gateway reserves about 6 GB of memory. The default
  `VIS_DECISION_MEMORY_BUDGET_MB` of 8,192 MB is enough for this model alone.
- Local training with a batch size of 2 used a peak of 23 GB of RAM. A larger batch needs more.
- The FP32 export used a peak of 10 GB of RAM.
- Keep at least 25 GB of disk free for the checkpoint, a trained version, and its upload archive.
- Each question can use up to 2,048 tokens.
- Each archive downloads in three ordered parts, each below the release asset limit. The
  downloader joins the parts, and checks each part and the joined archive. The weights keep their
  precision.

Download the pinned FP32 bundle of the model that the gateway must use:

```bash
vis-agent decisions models download --model gliner2.5-base
# Or choose another model ID from the table, for example:
vis-agent decisions models download --model gliner2.5-multi
```

For training, also download the complete checkpoint of the same model:

```bash
vis-agent decisions models download --model gliner2.5-base --training
# Or train another model from the table, for example:
vis-agent decisions models download --model gliner2.5-multi --training
```

Each command prints the installed inference path and checkpoint path.

`TrainingBundle.open` selects the model family from the verified provenance.
`TrainingBundle.fetch("<model>@<revision>", "cache")` is the explicit, pinned download, as for
Laya.

The JSONL rows have the same `state`, `question`, `target`, and `action` fields as the Laya
example. Keep the training rows and the held-out rows separate. GLiNER has its own configuration,
for example `{"epochs":1,"max_steps":100,"encoder_lr":0.00001,"task_lr":0.0005}`. Set both minimum
accuracies in `policy.json` from your use case, not from a baseline model. Then run:

```python
from blockether.vis_decisions import Trainer, TrainingBundle

base = TrainingBundle.open("/path/to/gliner-training")
with Trainer(base) as trainer:
    result = trainer.train(
        train_data="train.jsonl",
        eval_data="eval.jsonl",
        training_config="gliner-config.json",
        validation_policy="policy.json",
        output_dir="my-gliner-version",  # must not exist yet
        progress=print,
    )
print(result.checkpoint_dir, result.inference_bundle, result.validation_report)
```

During training, `progress` gets `training` events with `step`, `max_steps`, `epoch`, and `loss`.
Before the first step, one event shows the start step without `epoch` or `loss`. Then an event
arrives at the first step, about one time for each percent of the steps, and at the last step.
`epoch` counts passes over your training rows, so `0.5` is half a pass. The `checkpoint_saved`,
`exporting`, and `validated` events follow.

`TrainingBundle.open` can open `result.checkpoint_dir` in a new process. Then
[train it again](#resume-or-continue-training) or export it, with no network access. To export
without training, call `trainer.prepare` on a complete checkpoint with `eval_data`,
`validation_policy`, and a new `output_dir`. Publish with `Decisions.upload_model(result)`, and
use the version, alias, and inference calls of
[Publish explicitly and select a version](#publish-explicitly-and-select-a-version).

In Vis, GLiNER inference covers decision classification and the choice between
act and escalate. It does not cover entity or JSON extraction. Do not use these weights for
autonomous actions without representative, held-out validation.

### Choose and train Decision 2.0

Decision 2.0 has two decision models from vLLM Semantic Router.

| Model | Model ID | FP32 bundle download | Checkpoint download |
| --- | --- | --- | --- |
| Decision 2.0 Eos 0.8B | `decision2.0-eos-0.8b` | 1.76 GB | 1.52 GB |
| Decision 2.0 Kai 0.6B | `decision2.0-kai-0.6b` | 1.44 GB | 1.23 GB |

Evaluate both models on your own labels, and choose the model that answers them better.

Decision 2.0 answers `choice`, `score`, and `noul` questions. It has no action head, so its
answers have no `action` field. Decide with your own review rule when a person checks a result. A
`choice` question needs 2 to 64 options. A `score` question needs 2 to 10 levels.

For a `score` question with five levels, Kai changes the level probabilities with fixed offsets
from its publisher, as its upstream runtime does. The offsets fit only the published weights. A
version that you train or export from the Kai checkpoint does not use them.

The times are from an Apple M4 Max with 4 CPU threads.

| Resource | Eos | Kai |
| --- | --- | --- |
| Memory that the gateway reserves while the model is loaded | 4,312 MB | 4,096 MB |
| Time for a question of about 2,000 tokens | About 2 seconds | About 2 seconds |
| Time for a question of 8,192 tokens | About 8 seconds | About 17 seconds |
| Time for a question of 16,384 tokens | About 19 seconds | Above the token limit |
| Largest memory use of a question at the token limit | About 4.2 GB | About 3.3 GB |
| Peak RAM of local training with the default batch size of 1, FP32 export included | About 16 GB | About 12 GB |

- The default `VIS_DECISION_MEMORY_BUDGET_MB` of 8,192 MB is enough for one of these models. To
  keep both loaded at the same time, set it to at least 8,408 MB.
- The first question also waits about 1.3 seconds while the model loads.
- A larger training batch needs more memory.
- Keep at least 15 GB of disk free for the checkpoint, a trained version, and its upload archive.

Download the FP32 bundle for the gateway. For training, also download the complete checkpoint.
These commands download Kai. For Eos, use the model ID `decision2.0-eos-0.8b`:

```bash
vis-agent decisions models download --model decision2.0-kai-0.6b
vis-agent decisions models download --model decision2.0-kai-0.6b --training
```

Training uses the same JSONL rows as GLiNER2.5. The `action` field is optional and not used. For a
`noul` question, set `target` to `0` for false or `1` for true. Use the GLiNER2.5 training
settings, for example `{"epochs":1,"max_steps":100,"encoder_lr":0.00001,"task_lr":0.0005}`.

Set only `min_decision_accuracy` in `policy.json`. The trainer rejects a policy with
`min_action_accuracy`. Then call `Trainer.train` as in the GLiNER2.5 example. The validation
report and the gateway job metrics give `action_accuracy` as `null`. To train on the gateway, pass
the model ID as `model_id`, as [Train on the gateway instead](#train-on-the-gateway-instead) shows.

### Choose and train D3

D3 (Decision 3.0) has two text decision models from vLLM Semantic Router. Each model gives each option a probability.

| Model | Model ID | FP32 bundle download | Checkpoint download |
| --- | --- | --- | --- |
| D3 Edge | `d3-edge` | 1.03 GB | 0.80 GB |
| D3 Lite | `d3-lite` | 1.56 GB | 1.20 GB |

Evaluate both models and Decision 2.0 on your own labels, and choose the model that answers them
better.

D3 answers `choice`, `score`, and `noul` questions with the same answer fields as Decision 2.0. It
has no action head, so its answers have no `action` field. A `choice` question needs 1 to 64
options. A `score` question needs 2 to 10 distinct levels. For a `noul` question, `criteria` can
describe `false` and `true`.

The Vis bundles hold only the text model. The upstream models also read images and videos, but Vis
does not send them. Vis runs the model in FP32, and the upstream runtime runs it in BF16. So the
probabilities can differ from the upstream runtime in the third decimal place.

The times are from an Apple M4 Max with 4 CPU threads.

| Resource | Edge | Lite |
| --- | --- | --- |
| Memory that the gateway reserves while the model is loaded | 4,096 MB | 4,308 MB |
| Time for a question of about 2,000 tokens | About 0.9 seconds | About 1.8 seconds |
| Time for a question of 16,384 tokens | About 13 seconds | About 19 seconds |
| Model load before the first question | About 6 seconds | About 10 seconds |
| Largest memory use of a question of 16,384 tokens | About 3.2 GB | About 4.2 GB |
| Peak RAM of local training of all weights, FP32 export included | About 7.6 GB | About 11.4 GB |
| Free disk for the checkpoint, a trained version, and its upload archive | At least 10 GB | At least 14 GB |

- The default `VIS_DECISION_MEMORY_BUDGET_MB` of 8,192 MB is enough for one of these models. To
  keep both loaded at the same time, set it to at least 8,404 MB.
- The memory of a question is in the reserved memory. It grows linearly with the question length.
- The training peak is for short rows and the default batch size of 1. Long rows and a larger
  batch need more memory. LoRA and `max_input_tokens` decrease it.

Download the FP32 bundle for the gateway. For training, also download the checkpoint. These
commands download Edge. For Lite, use the model ID `d3-lite`:

```bash
vis-agent decisions models download --model d3-edge
vis-agent decisions models download --model d3-edge --training
```

Then pass the model ID as `model` when you [ask from Python](#ask-from-python).

The checkpoint holds the text weights and the 255-way answer-code readout of the upstream model,
without the image and video encoder. Training raises the score of the answer code of the `target`
option. With `lora` in `config.json`, training changes only small adapters. Each saved checkpoint
merges them into the full weights, and the trained version uses the same FP32 export.

D3 training needs vis-decisions 0.6.0 or newer. The JSONL rows, the `policy.json` rule, the `null`
`action_accuracy`, and gateway training with `model_id` are the same as for Decision 2.0. Use the
GLiNER2.5 training settings, for example
`{"max_steps":100,"encoder_lr":0.00001,"task_lr":0.0005,"lora":{"rank":16}}`. `max_input_tokens`
can be at most 16,384.

### Resume or continue training

To keep the progress of long training, add `checkpoint_steps` to the training configuration. A
Laya configuration can be:

```json
{"epochs":3,"learning_rate":0.00001,"max_steps":5000,"checkpoint_steps":500}
```

A GLiNER configuration can be:

```json
{"epochs":3,"batch_size":8,"encoder_lr":0.00001,"task_lr":0.0005,"checkpoint_steps":500}
```

With this setting, `train` saves a partial checkpoint every 500 steps. Each save replaces
`output_dir/checkpoint` and sends a `checkpoint_saved` event. It also writes
`output_dir/training_report.json` with the saved `steps`, the planned `max_steps`, and a `status`.
One run can plan at most 100,000 steps.

If training fails or stops, `output_dir/checkpoint` keeps the last saved step. To resume, open
that checkpoint and train it again with the same rows and settings:

```python
stopped = TrainingBundle.open("my-new-version/checkpoint")
with Trainer(stopped) as trainer:
    result = trainer.train(
        train_data="train.jsonl",
        eval_data="eval.jsonl",
        training_config="config.json",
        validation_policy="policy.json",
        output_dir="my-new-version-2",  # must not exist yet
        progress=print,
    )
```

The `TrainingBundle` and `Trainer` calls are the same for each model family.

The first event shows the saved step. Training then uses only the examples that the stopped run
did not train, in the same order. The optimizer state starts again, and GLiNER also starts its
learning rate warmup again. So the result can differ a little from a run that did not stop. You
can change `checkpoint_steps` when you resume.

To continue a trained version on new labels, open its checkpoint and train it with the new rows.
Different rows or settings always start a new run at step 0 from the saved weights. In both cases,
`PROVENANCE.json` records the start checkpoint as `parent_revision`.

A partial checkpoint records a digest of its rows and settings, not the rows. If the export or the
validation fails, `output_dir` keeps the final checkpoint without inference files.

## Publish explicitly and select a version

Connect to the gateway as in [Ask from Python](#ask-from-python). The gateway verifies the
streamed, inference-only bundle and runs both heads. Then it registers an immutable `sha256-...` version. An upload never changes a running
alias.

These limits apply to an upload:

- The ZIP archive can hold up to 6,000,000,000 bytes. Its extracted files can hold up to
  6,000,000,000 bytes in total. The SDK and the gateway use the same limits.
- The gateway accepts one model upload at a time.
- The gateway needs disk space for the archive and for its extracted files.

```python
import os

from blockether.vis_decisions import Decisions
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

To change an existing alias, read `decisions.get_alias("my-case")["model_ref"]` and pass it as
`expected_current=...` to `activate_model`. A concurrent change then returns a conflict, and
nothing is overwritten. Inference with the immutable `ref` or the original `laya-typed-decisions`
baseline always works independently of an alias.

If an upload times out, call `get_model("sha256-" + archive_digest)`. It shows if the gateway
registered the immutable version, so you know if a second upload is necessary. Do not publish
private training checkpoints or row files as model assets.

## Train on the gateway instead

Prepare the gateway before the first job:

1. Set `VIS_DECISION_TRAINING_PYTHON` to the Python 3.12 executable of your prepared vis-decisions
   environment. All model families use this interpreter. The gateway never downloads training
   dependencies.
2. Set `VIS_DECISION_TRAINING_DATA_ROOT` to a directory of approved JSONL and JSON files on the
   gateway.
3. Download the pinned checkpoint of the selected model with `--training`.

These limits apply to a job:

- The API accepts filenames in that directory, not paths on your computer and not raw uploads.
- Each dataset can have at most 16 MiB. Each configuration or policy file can have at most 16 KiB.
- One training job runs at a time, for up to two hours by default.
- While the trainer owns the model budget, the gateway refuses other decision inferences.
- Existing aliases and their selected versions stay unchanged.

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

For GLiNER, Decision 2.0, or D3, pass a model ID to `decisions.start_training(...)` with approved
data and a GLiNER training configuration. Take the ID from the
[GLiNER2.5 table](#choose-and-train-a-gliner2-5-model), the
[Decision 2.0 table](#choose-and-train-decision-2-0), or the [D3 table](#choose-and-train-d3). For
example, pass `model_id="gliner2.5-multi"`, `model_id="decision2.0-kai-0.6b"`, or
`model_id="d3-edge"`. Without `model_id`, the default is Laya. The job status has `model_id`, the
stage, the progress, the metrics, and the final `model_ref`.

The gateway stages the limited inputs and starts an isolated offline CPU worker. The worker saves
a private checkpoint and validates a new FP32 inference version. It uses one CPU thread for each
physical core, or for each performance core on Apple silicon. To change the number, set
`VIS_DECISION_TRAINING_THREADS` to a whole number from 1 to 1024.

To continue from the checkpoint of a job, pass the `job_id` of that completed or failed job as
`source_job_id`, with the same `model_id`. Do not delete that job first. A resume across model
families fails.

`checkpoint_steps` also saves partial checkpoints on the gateway. A job that fails or reaches the
time limit keeps its last saved step. To resume, start a new job with that `job_id` as
`source_job_id`, and the same data and configuration. Different data or settings start at step 0
from the saved weights. You cannot continue a cancelled job.

Training does not activate an alias. Review the held-out metrics, then call `activate_model`
separately. A quality failure, an interruption, or a cancellation leaves the existing versions and
aliases unchanged. Metrics on a small sample do not prove that a model is safe for your domain,
and they do not permit autonomous actions.

## See also

- To connect to a gateway with your credentials, read [Python SDK](python-sdk.md).
- To declare the training extension for a project, read [Configuration](configuration.md).
- To open the session in which Vis trains a model, read [Sessions](sessions.md).
- To secure and operate the service, read [Running a gateway](gateway-service.md).
