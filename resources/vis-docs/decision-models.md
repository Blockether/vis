# Decision models

Vis answers typed `choice`, `score` and `noul` questions with Laya and GLiNER decision models.
This classifier runs on your gateway from a downloaded ONNX bundle. Each answer also includes a
score that compares action with escalation. The published baseline is a starting point, **not** a
policy for actions on your behalf. Collect representative labels for your use case and evaluate both
heads. Then decide when a human must review a result before you rely on it.

## When to use

- **Your application classifies requests into a fixed set of labels**, such as
  whether a damaged item needs a refund or a repair. Ask a typed `choice`
  question [from Python](#ask-from-python).
- **You also need a rating or a direct answer**, such as how urgent the request is
  and whether the item can be refunded. Ask `score` and `noul` questions in the same
  call.
- **You must decide when a person should review a result.** Laya and GLiNER2.5 answers
  include an action-versus-escalation score. Evaluate it on your own labels before you
  rely on it.
- **Your requests use languages other than English.** Choose a multilingual
  [GLiNER2.5 model](#choose-and-train-a-gliner2-5-model).
- **Your gateway has little memory or CPU.** Choose the small
  [GLiNER2.5 model](#choose-and-train-a-gliner2-5-model).
- **The smaller models do not classify your English requests well enough.** If your
  computers have enough memory, evaluate the larger
  [GLiNER2.5 Decide 1B model](#choose-and-train-a-gliner2-5-model).
- **The GLiNER2.5 models do not answer your questions well enough, and you need no action
  score.** Evaluate the [Decision 2.0 models](#choose-and-train-decision-2-0) on your own labels.
- **The baseline does not fit your data.** [Train with your own
  labels](#train-locally-with-the-python-sdk), then [publish a verified version and
  select it](#publish-explicitly-and-select-a-version).
- **Long training stops before it finishes.** Save partial checkpoints and
  [resume from the last saved step](#resume-or-continue-training). The same section
  shows how to continue a trained version on new labels.
- **A long input fails or loses late context.** Check the
  [token budget and truncation policy](#handle-long-inputs).
- **You have no local bundle, or you want a second opinion from OpenAI.** Ask an
  [OpenAI decision model](#ask-openai-decision-models) with the same questions.

For open-ended work that needs files or tools, run an agent task with the [Python
SDK](python-sdk.md) instead.

This guide builds on three pages. [Configuration](configuration.md) declares the training extension
for a project. A [session](sessions.md) in that project runs the training tools. Your application
connects as [Python SDK](python-sdk.md) describes.

## Download the baseline

The `assets-pack` release contains pinned FP32 inference bundles and complete checkpoints.
It also keeps the existing voice assets. Starting a gateway never downloads weights.
The [vis-decisions extension](https://github.com/Blockether/vis-decisions) supplies the Python client and training runtime.
All model families use one environment with Transformers 5.

Install Vis, then download the pinned inference bundle on the machine running your
gateway:

```bash
vis-agent decisions models status
vis-agent decisions models download --model laya-typed-decisions
```

The status command prints the model revision and installation state. Downloads verify the archive and its file inventory.
For training, also download the complete checkpoint:

```bash
vis-agent decisions models download --model laya-typed-decisions --training
```

The command prints the inference and training directories. It does not download Python dependencies.
Install the vis-decisions runtime while you have network access.
Its pinned environment supports Laya, GLiNER2.5 and Decision 2.0, so you do not need separate environments.
Allow several gigabytes for checkpoints, FP32 bundles, dependencies and exported versions. Review model licenses and
provenance in
[`THIRD_PARTY_MODELS.md`](https://github.com/Blockether/vis/blob/main/THIRD_PARTY_MODELS.md).

For a client on another machine, use [a secured
gateway](gateway-service.md#connect-from-another-machine). Only the gateway needs the
inference bundle. It will refuse a missing model instead of downloading one during a
request.

## Ask from Python

Call gateway inference with `Decisions` from the vis-decisions package.
Install it in your application environment or the project interpreter used by the `py` extension.
The vis-decisions extension also provides local training tools in `python_execution`.

Configure `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` as the [Python SDK gateway
guide](python-sdk.md#connect-to-a-gateway-and-run-a-task) describes. Do not put the token in code or
logs. When the gateway machine has the inference bundle, you can ask typed questions:

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

These calls do not download a model or install a dependency. An uninstalled model
raises `blockether.vis.engine.GatewayError` with `status == 409`. Download the model
explicitly with the CLI above. The baseline `act_probability` in an answer is a
diagnostic score, not authorization to act. Installing vis-decisions does not download model weights.
Training and inference use the same package, but only explicit calls load weights.

### Handle long inputs

GLiNER models use `max_position_embeddings` from the installed bundle's
`encoder_config/config.json` as their token limit. The limit is at most 2,048 tokens, even if the
encoder accepts more. This keeps the memory for one question predictable. Each question has a
separate budget.
The state, question type, instructions, criterion text, action labels and structural tokens
share that budget. Object and list states use their JSON representation.

GLiNER does not truncate the state or question. The gateway rejects a sequence above the
limit before model inference. A sequence exactly at the limit is accepted. Character counts
cannot predict this boundary because each model uses its own tokenizer.

Decision 2.0 has a fixed limit of 2,048 tokens for each question. The state, question type,
instructions, option keys, option descriptions and prompt text share that budget. Decision 2.0
also does not truncate input. The gateway rejects a longer question before model inference.

For overlong input, `Decisions.infer` raises `GatewayError` with `status == 400` and
`code == "input-too-long"`. Its `input_tokens` attribute gives the rejected sequence's token
count. Its `max_input_tokens` attribute gives the configured limit. The error also tells you
what to shorten. These attributes are `None` if the gateway supplies no valid counts.

Shorten the state, question instructions or criteria before retrying. Do not remove important
context without checking the effect on your results. The SDK sends the request without
estimating a character limit. Exact validation uses the installed tokenizer on the gateway,
not an extra tokenizer dependency in your application.

The Laya baseline has a different policy. It uses `max_len` from the installed `config.json`,
with a default of 512 tokens per question. It truncates instructions and option text to fit
the question head. It then keeps only the leading state tokens that fit the remaining space.
Late state context can therefore be omitted. Keep important context early when you use Laya.

### Ask OpenAI decision models

You can also send the same questions to an OpenAI classifier model, such as GPT-6 Luna.
Use this when you have no local bundle or when you want to compare answers. Each request
is a paid OpenAI call, and the state goes to OpenAI. Images are not supported.

The gateway needs an OpenAI API key. Add the `openai` provider with an API key as
[Configuration](configuration.md) describes, or set `OPENAI_API_KEY` for the gateway. A
ChatGPT (Codex) sign-in does not work with the OpenAI Decisions API.

Name the model as `openai/<id>`. The questions and answer shapes stay the same:

```python
answer = decisions.infer(
    model="openai/gpt-6-luna",
    state="A damaged item needs a refund",
    questions={"intent": {"type": "choice", "instructions": "Choose a request", "criteria": ["refund", "repair"]}},
)
print(answer["answers"]["intent"]["choice"], answer["routing"]["provider"])  # refund openai
```

`decisions.list_models()` lists OpenAI models with `"residency": "remote"`. Their
`available` field is `true` when the gateway has a key. OpenAI answers have no `action`
score. If OpenAI declines one question, that answer is `{"type": "refusal"}`. Without a
usable key, `Decisions.infer` raises `GatewayError` with `status == 409`.

## Train locally with the Python SDK

Install the [vis-decisions runtime](https://github.com/Blockether/vis-decisions#development) with Python 3.12.
`TrainingBundle.open` reads the verified checkpoint printed by the CLI.
`TrainingBundle.fetch("laya-typed-decisions@<revision>", "cache")` is an explicit, pinned download.
Neither `open` nor `train` fetches weights.

To use training tools in Vis, add this [extension declaration](extension-packages.md#declare-packages-in-configuration):

```yaml
extensions:
  vis-decisions:
    source: https://github.com/Blockether/vis-decisions
    version: "0.3.1"
```

Then open a session in that project. For example, ask:

> Train this checkpoint with train.jsonl, eval.jsonl, config.json and policy.json. Save a new version without activating it.

Vis uses the extension tools to verify the checkpoint, train and report the validation results.

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

`result.checkpoint_dir` is a complete checkpoint. To train it further, open it with
`TrainingBundle.open`, as [Resume or continue training](#resume-or-continue-training) shows. The
separate `inference_bundle` contains the ONNX FP32 model files, tokenizer, configuration and
provenance. It does not contain private training rows or checkpoint weights.

To export an existing checkpoint without training, call
`trainer.prepare(eval_data=..., validation_policy=..., output_dir=...)`. If export or a quality
check fails, you get no deployable result. A checkpoint that was saved can stay for diagnosis.
Training and export use a lot of local CPU, RAM and disk. Lightweight clients do not load the training runtime.

### Choose and train a GLiNER2.5 model

You can use a GLiNER2.5 model from Fastino instead of Laya. Choose a model for the language of your
text and the memory of your gateway:

| Model ID | Text language | Fastino model | FP32 bundle size | Checkpoint size |
|---|---|---|---|---|
| `gliner2.5-small` | English | Small multi-task model | 293 MB | 263 MB |
| `gliner2.5-base` | English | Default multi-task model | 749 MB | 694 MB |
| `gliner2.5-decide` | English | Classification model | 1.75 GB | 1.80 GB |
| `gliner2.5-decide-1b` | English | Large classification model | 4.14 GB | 4.41 GB |
| `gliner2.5-multi` | Many languages | Multilingual multi-task model | 1.13 GB | 960 MB |
| `gliner2.5-multi-decide` | Many languages | Multilingual classification model | 1.13 GB | 998 MB |

Fastino trained the Decide models for classification. The multi-task models can also extract
entities, but Vis uses every model only for decisions. The small model needs the least memory and
CPU. Evaluate your candidates on your own held-out labels before you choose one.

`gliner2.5-decide-1b` is much larger than the other models. Check these resources before you
choose it:

- While the model is loaded, the gateway reserves about 6 GB of memory. The default
  `VIS_DECISION_MEMORY_BUDGET_MB` of 8,192 MB is enough for this model alone.
- Local training with a batch size of 2 used a peak of 23 GB of RAM. Larger batches need more.
- The FP32 export used a peak of 10 GB of RAM.
- Allow at least 25 GB of free disk for the checkpoint, a trained version and its upload archive.
- Each question can use up to 2,048 tokens.
- Each archive downloads in three parts. Vis checks each part and the joined archive.

Download the pinned FP32 bundle for the model you want to use on the gateway:

```bash
vis-agent decisions models download --model gliner2.5-base
# Or choose another model ID from the table, for example:
vis-agent decisions models download --model gliner2.5-multi
```

For training, explicitly download the same model's complete checkpoint:

```bash
vis-agent decisions models download --model gliner2.5-base --training
# Or train another model from the table, for example:
vis-agent decisions models download --model gliner2.5-multi --training
```

Each command prints the installed inference and checkpoint paths. Decide-1B archives use ordered parts below the release asset limit.
The downloader joins and verifies those parts automatically. No weight precision is reduced.

Use the same vis-decisions environment as Laya. Allow several gigabytes for checkpoints, exports and dependencies.
`TrainingBundle.open` selects the family from verified provenance.
`TrainingBundle.fetch("<model>@<revision>", "cache")` downloads a pinned checkpoint when you explicitly request it.
Neither `open` nor the trainer fetches weights.

The JSONL rows and separate held-out data have the same `state`, `question`, `target` and `action`
fields as the Laya example above. Keep training and held-out inputs separate. GLiNER uses its own
configuration, for example `{"epochs":1,"max_steps":100,"encoder_lr":0.00001,"task_lr":0.0005}`. Set
both minimum accuracies in `policy.json` from your use case, not from a baseline model. Then run:

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

While it trains, `progress` receives `training` events with `step`, `max_steps`, `epoch` and
`loss`. Before the first step, one event shows the start step without `epoch` or `loss`. After
that, you get an event at the first step, about once per percent of the steps and at the last
step. `epoch` counts passes over your training rows, so `0.5` is half a pass. Then the
`checkpoint_saved`, `exporting` and `validated` events follow.

You can reopen `result.checkpoint_dir` with `TrainingBundle.open` in a new process. Then you
can [train it again](#resume-or-continue-training) or export it without network access. To
export without training, call `trainer.prepare` on a complete checkpoint with `eval_data`,
`validation_policy` and a new `output_dir`. Use `Decisions.upload_model(result)` and the same
version, alias and inference calls below.

The archive that goes to the gateway contains only ONNX inference files, not the checkpoint or
labeled examples. Here, GLiNER inference covers decision classification and act/escalate, not entity
or JSON extraction. Export and training use a lot of CPU, RAM and disk. Do not use these weights for
autonomous actions without representative, held-out validation.

### Choose and train Decision 2.0

Decision 2.0 has two decision models from vLLM Semantic Router. Each model reads the state, the
instructions and each option with its description. Then it selects one option.

| Model | Model ID | FP32 bundle download | Checkpoint download |
| --- | --- | --- | --- |
| Decision 2.0 Eos 0.8B | `decision2.0-eos-0.8b` | 1.76 GB | 1.52 GB |
| Decision 2.0 Kai 0.6B | `decision2.0-kai-0.6b` | 1.44 GB | 1.23 GB |

Evaluate both models on your own labels. Then choose the model that answers them better.

Decision 2.0 answers `choice`, `score` and `noul` questions. It has no action head, so its answers
have no `action` field. Use your own review rule to decide when a person checks a result. A
`choice` question needs 2 to 64 options. A `score` question needs 2 to 10 levels.

Kai changes the level probabilities of five-level `score` questions with fixed offsets from its
publisher, as its upstream runtime does. The offsets fit only the published weights. A version
that you train or export from the Kai checkpoint does not use them.

Check these resources before you choose a model:

- While a model is loaded, the gateway reserves 4,312 MB of memory for Eos and 4,096 MB for Kai.
  The default `VIS_DECISION_MEMORY_BUDGET_MB` of 8,192 MB is enough for one of these models. To
  keep both models loaded at the same time, set it to at least 8,408 MB.
- On an Apple M4 Max with 4 CPU threads, a question near the 2,048-token limit took about
  2.5 seconds with Eos. With Kai, it took about 3 seconds. The first question also waits about
  1.3 seconds while the model loads.
- Local training with the default batch size of 1 used a peak of about 16 GB of RAM with Eos.
  With Kai, it used about 12 GB. These peaks include the FP32 export. A larger batch needs more
  memory.
- Allow at least 15 GB of free disk for the checkpoint, a trained version and its upload archive.

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

### Resume or continue training

Long training can fail or stop before it finishes. To keep its progress, add `checkpoint_steps`
to the training configuration. A Laya configuration can be:

```json
{"epochs":3,"learning_rate":0.00001,"max_steps":5000,"checkpoint_steps":500}
```

A GLiNER configuration can be:

```json
{"epochs":3,"batch_size":8,"encoder_lr":0.00001,"task_lr":0.0005,"checkpoint_steps":500}
```

With this setting, `train` saves a partial checkpoint every 500 steps. Each save replaces
`output_dir/checkpoint` and sends a `checkpoint_saved` event. It also writes
`output_dir/training_report.json` with the saved `steps`, the planned `max_steps` and a
`status`. One run can plan at most 100,000 steps.

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

Use the same `TrainingBundle` and `Trainer` calls for every model family.

The first event shows the saved step. Training then uses only the examples that the stopped run
did not train, in the same order. The optimizer state starts again. GLiNER also starts its
learning rate warmup again. So, the result can be a little different from a run that did not
stop. You can change `checkpoint_steps` when you resume.

To continue a trained version on new labels, open its checkpoint and train it with the new rows.
Other rows or settings always start a new run at step 0 from the saved weights. In both cases,
`PROVENANCE.json` records the start checkpoint as `parent_revision`.

A partial checkpoint records a digest of its rows and settings, not the rows. If export or
validation fails, `output_dir` keeps the final checkpoint without inference files.

## Publish explicitly and select a version

Configure `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` as described in the [Python SDK
gateway guide](python-sdk.md#connect-to-a-gateway-and-run-a-task). Do not put the token
in code or logs. The gateway verifies the streamed, inference-only bundle and runs both
heads before registering an immutable `sha256-...` version. Upload never changes a
running alias.

The ZIP archive can contain up to 6,000,000,000 bytes. Its extracted files can contain
up to 6,000,000,000 bytes in total. The SDK and gateway use the same limits.

The gateway accepts one model upload at a time. Allow disk space for both the archive
and its extracted files.

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

To switch an existing alias, read `decisions.get_alias("my-case")["model_ref"]` and pass
it as `expected_current=...` to `activate_model`. A concurrent change returns a conflict
instead of silently overwriting it. You can always infer with the immutable `ref` or the
original `laya-typed-decisions` baseline independently. If an upload times out,
`get_model("sha256-" + archive_digest)` lets you check whether the immutable version was
registered before deciding to retry. Do not publish private training checkpoints or row
files as model assets.

## Train on the gateway instead

Set `VIS_DECISION_TRAINING_PYTHON` to the Python 3.12 executable in your prepared vis-decisions environment.
All model families use this interpreter. The gateway never downloads training dependencies.
Set `VIS_DECISION_TRAINING_DATA_ROOT` to a directory of approved JSONL/JSON files on the gateway.
Before starting, download the selected model's pinned checkpoint with `--training`.

The API accepts **filenames in that directory**, not laptop paths or raw uploads.
Each dataset is limited to 16 MiB. Configuration and policy files to 16 KiB.
One training job runs at a time, for up to two hours by default. Other decision
inferences are temporarily refused while the trainer owns the model budget.
Existing aliases and their selected versions remain unchanged.

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

For GLiNER or Decision 2.0, pass a model ID from the
[GLiNER2.5 table](#choose-and-train-a-gliner2-5-model) or the
[Decision 2.0 table](#choose-and-train-decision-2-0) to `decisions.start_training(...)`. For
example, pass `model_id="gliner2.5-multi"` or `model_id="decision2.0-kai-0.6b"` with approved data
and a GLiNER training config. Without `model_id`, the default stays Laya. Job status includes
`model_id`, stage, progress, metrics and the final `model_ref`.

The gateway stages bounded inputs and starts an isolated offline CPU worker. It then saves a private
checkpoint and validates a new FP32 inference version. To continue from it, pass the completed or
failed `job_id` as `source_job_id` **with the same model_id**. Do not delete that job first. A
resume across model families fails. It does not fall back to another checkpoint.

The worker uses one CPU thread for each physical core, or for each performance core on Apple silicon.
To choose another number, set `VIS_DECISION_TRAINING_THREADS` to a whole number from 1 to 1024.
This needs vis-decisions 0.3.1 or newer. Older versions use at most four threads.

The `checkpoint_steps` setting also saves partial checkpoints on the gateway. If a job
reaches the time limit or fails, it keeps its last saved step. To resume, start a new job with
that `job_id` as `source_job_id`. Use the same data and configuration. Other data or settings
start at step 0 from the saved weights. You cannot continue a cancelled job.

Training does not activate an alias. Review the held-out metrics and use
`activate_model` separately. A quality failure, interruption or cancellation
leaves existing versions and aliases unchanged. Metrics on a small sample do
**not** establish domain safety or authorize autonomous actions.

## See also

- [Python SDK](python-sdk.md) — connect to a gateway with your credentials.
- [Configuration](configuration.md) — declare the training extension for a project.
- [Sessions](sessions.md) — open the session in which Vis trains a model.
- [Running a gateway](gateway-service.md) — secure and operate the service.
