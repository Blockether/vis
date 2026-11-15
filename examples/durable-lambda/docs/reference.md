# Reference

The details of [Durable Lambda](../README.md): the data, the addresses, the limits and the variables.

## The custom runtime

The image starts from Amazon Linux 2023. It has no AWS Lambda base image and no runtime
interface client. Its entry point is the Python interpreter of the Vis version in the
image:

```dockerfile
ENTRYPOINT ["/opt/vis/vis-agent-python/python/bin/python3", "-m", "durable"]
```

[`durable/lambda_runtime.py`](../durable/lambda_runtime.py) speaks the Lambda Runtime API with
the standard library. It supports plain answers and streamed answers.

## S3 is the database

| Key | Content | Kept for |
| --- | --- | --- |
| `lease.json` | The active worker, its last checkpoint and the work that waits | permanent |
| `state/<worker>/<n>.tar.gz` | A checkpoint of the Vis home directory: sessions, transcripts, attached files, settings | the last one; a checkpoint of a failed worker stays |
| `workflows/<name>.py` | The source of a workflow | permanent |
| `runs/index.json` | One line for each of the 300 newest runs | permanent |
| `runs/<run>/run.json` | What the pages show of a run: its workflow activities, the decisions at its gates (`approved`, `comment` and the time) and the sessions that it left with a fork. The record of the work is the transcript of the session | permanent |
| `runs/<run>/workflow.py` | The source that the run uses, also after an edit of the workflow | permanent |
| `attached/<session>/<name>` | A copy of the newest version of each file that a session attached | permanent |
| `tmp/<session>/<path>` | The files that a session wrote in `/tmp` and in its work directory | 7 days |
| `work/<name>/meta.json` | The newest copy of a work directory: its commits, its time and its session | until a person deletes the session |
| `work/<name>/<commit>.bundle` | The Git work of that copy: the commits after the release, and one commit with each file that is not committed | until the next copy |
| `relay/<call>/…` | The parts of one answer on the way to the browser | 1 day |
| `outbox/<id>.bundle` | The commits that a session publishes | 1 day |
| `repo/HEAD` | The commit of `main` at the last fetch | permanent |
| `repo/snapshots/<commit>.tar.gz` | One snapshot for each commit of `main` that the stack fetched | 30 days |

The bucket also keeps each old version of an object for 7 days.

A worker takes `lease.json` with a conditional write. It saves a checkpoint when the
home directory changed and names the checkpoint in the lease with a second conditional
write. Then it deletes the earlier checkpoint. A worker that lost the lease cannot
replace newer state. All sessions are in the one home directory, so the stack holds
many sessions and each new worker gets them all.

A worker that stops between the upload of a checkpoint and the lease write leaves that
checkpoint in `state/`. The template has no lifecycle rule for `state/`: a rule can
remove the only checkpoint of a stack that nobody used for some days. The lease names
the checkpoint that counts. You can delete each other object in `state/`.

## A commit becomes the project of the next worker

1. GitHub calls the webhook `/hooks/github`. The `front` function checks the signature and starts `repo`. A schedule also starts `repo` each 15 minutes.
2. `repo` clones `main` with the deploy key and writes `repo/snapshots/<commit>.tar.gz`.
3. `sync` extracts the snapshot into a hidden directory and renames it to `releases/<commit>`.
4. `sync` writes a new `CURRENT` file and renames it over the old one.

Steps 3 and 4 each end with one rename. A reader sees the old release or the new
release, and never a part of one. `work/main` is the directory of the one Vis project.
A worker reads `CURRENT` at its start and brings the commits of that release into
`work/main`: as a fast-forward, else as a merge. If Git cannot merge, the directory
stays as it is and the next worker tries again. Each new session and each new workflow
run reads `CURRENT` again and gets a clone of the newest release. `sync` keeps the 5
newest releases.

## Each session has its own work directory

Many persons can work at the same time. Each session that a person starts gets its own
clone of the newest release, in `work/<id>` on EFS. Each workflow run that makes its
own session gets `work/<commit>-<run>`. The work of one session does not change the
files of another session.

| Event | Result |
| --- | --- |
| A session starts | The worker makes a clone of the newest release and gives it to the session as its directory. |
| A step of a session ends | The extension tells the worker. In at most 15 seconds, the worker copies the Git work of the directory to `work/<name>/` in S3. A worker also copies the work when it stops. |
| The directory is gone | The next turn or run of the session makes it again from the copy in S3. The worker removes a directory that nothing changed for 14 days, and a new file system has no directory. |
| `main` moved | The session keeps its release. The Sessions page shows that the directory is behind. **Resync** puts the work on the newest release. |
| A person deletes the session | The worker removes the directory and the copy in S3. |

The copy is a Git bundle. It holds each branch of the directory and one commit with
each file that is not committed, also a new file. Git does not put an ignored file in
it. S3 holds only the newest copy. The copy of the work of a workflow run is the same:
it stays until a person deletes the session of the run.

A resync is a Git rebase: the commits of the session go on the newest release, and the
files that are not committed go on them again. When Git finds a conflict, the directory
stays as it was and the answer names the files. Then change those files in the session
and try again, or publish the work to a branch. A resync does not start while a turn or
a run works in the session. The model can do it in its own turn with `durable.resync()`.

A delete of a session group or of a project with its sessions removes the work of each
deleted session. A fork of a session works in the directory of its source session, so
the directory stays until the last of those sessions is deleted.

## A session publishes to GitHub

The worker has no route to GitHub and no key. The tool `durable.publish(message)`
commits the work, makes a Git bundle of the new commits and puts it in `outbox/`. The
`repo` function verifies the bundle and pushes it. When `main` did not move, the push
goes to `main`. Else it goes to a branch `durable/<id>`. The function never uses
force. A publication that repeats, with no new commit, pushes nothing and makes no
branch: the answer names the branch that has the commits and has `pushed` false. The
deploy key opens only this one repository.

Thus a session can change `main`. A release is a copy of `main`. No person reviews it
first. GitHub runs the files of `.github` with internet access, so `repo` refuses a
push that changes this directory, and `deploy.sh` turns off GitHub Actions for the
repository.

## Temporary work goes to S3

The extension has a hook after each `python_execution`, `patch` and `shell` operation.
The hook finds the files that changed in `/tmp` and in the work directory and sends
them to `tmp/<session>/tmp/` and `tmp/<session>/project/`. The Files tab of a session
shows them below the attached files. S3 removes them after 7 days. The hook copies only regular files. It does
not follow a link. These copies are for the pages. The copy that brings a work directory
back is in `work/`: see [Each session has its own work directory](#each-session-has-its-own-work-directory).

## A document is a Vis attachment

A document of a session is a native Vis attachment. The extension adds no tool for it.
The model calls the verbs that each Vis session has:

```python
attach(content.encode(), "SPEC.md")  # save a document; the same name again is the next version
list_attachments()  # the files of the session
read_attachment("SPEC.md")  # the bytes of the newest version
```

Vis keeps each version of an attached file in the session, so the transcript shows the
file and a checkpoint holds it. The pages read documents from S3. Thus the API copies
each new attached file to `attached/<session>/<name>` when a page or a workflow lists
the attached files of a session (`Files.mirror` in [`durable/vis/files.py`](../durable/vis/files.py)).
The S3 object has the metadata `version` and `turn`, and the list gives the two values
for each file. S3 holds only the newest version of a name.

## The review of a session

The image has the Vis extension [`vis-review`](../../../packages/vis-review). It records
each step of a session that changes files. After the first such step, the button
"Session apps" in the header of Vis web shows the app "Review": the changed files as a
diff, with an undo for a file, for a step and for the session. The command `/review` is
not necessary. The record of the review is in the Vis home directory, so each
checkpoint has it and a new worker shows the same review.

## One project and two groups

Vis web shows one project, with the name of the repository. Its directory is
`work/main`, so the project stays when the repository changes. The project has two
session groups:

| Session | Place in Vis web |
| --- | --- |
| A session that a person starts | The group Sessions of the project |
| The session of a workflow run | The group Workflows of the project |

Each session works in its own directory and is only listed in its group. A session
that a person starts in another group stays in that group. The worker makes the project
and the groups at its start. To change the name of
the project, use "Rename project" in the project menu of Vis web. The worker does not
replace a changed name.

## Addresses

The address of the stack is the Function URL of `front`: the output `Url` of the stack.
The stack has no sign-in: each person who has the address can use each page and each
call. Only the webhook checks its caller.

| Address | Result |
| --- | --- |
| `POST /hooks/github` | Starts a fetch of `main`. Needs the signature of GitHub |
| `GET /play/assets/*`, `GET /play/vendor/*` | The scripts, the styles, the editor and the fonts of the pages |
| `GET /<file>` | The static files of Vis web |
| `GET /`, `/play`, `/play/workflows`, `/play/factory`, `/play/architecture` | The pages |
| `/v1/*` | The Vis gateway: sessions, turns, events, attached files |
| `/play/api/*` | The Durable Lambda API of the worker |

The Durable Lambda API has these addresses below `/play/api/`. A body and an answer are JSON.

| Method and address | Body | Result |
| --- | --- | --- |
| `GET status` | | The release, the project directory, the repository, the models, the start time of the worker and the number of runs that are queued or running |
| `GET workflows` | | The workflows with their inputs and workflow activities |
| `POST workflows/check` | `{source}` | The description of a source that is not saved, or the syntax error |
| `GET workflows/<name>` | | One workflow with its source |
| `PUT workflows/<name>` | `{source}` | Saves a workflow |
| `DELETE workflows/<name>` | | Removes a workflow |
| `GET runs` | | The 100 newest runs |
| `POST runs` | `{workflow, input, session}` | Starts a run. `input` is an object of the inputs of the workflow. `session` is optional: the id of the session that the run works in. Without it, the run makes its own session |
| `GET runs/<id>` | | One run with its workflow activities, its source and its attached files |
| `POST runs/<id>/cancel` | | Cancels a run and the turn of its session |
| `POST runs/<id>/retry` | | Starts a failed run again. The run forks its session before the failed activity and continues in the fork. 409 when the run is not failed |
| `POST runs/<id>/gates/<key>` | `{approved, comment}` | Decides at the gate of a waiting run and starts the run again. `approved` is true or false. 409 when the run does not wait at this gate |
| `GET attached?session=<id>` | | The attached files of a session, each with `name`, `size`, `at`, `turn` and `version` |
| `GET attached/<session>/<name>` | | One attached file as a download |
| `GET tmp?session=<id>` | | The temporary files of a session |
| `GET tmp/<session>/<path>` | | One temporary file as a download |
| `GET sessions/<id>/workspace` | | The work directory of a session: `root`, `branch`, `base` (its release), `release` (the newest release), `is_behind`, `commits`, `changed` and `saved` (the time of the copy in S3) |
| `POST sessions/<id>/resync` | | Puts the work of a session on the newest release. 409 with `files` when Git finds a conflict, and 409 while a turn or a run works in the session |
| `POST publish` | `{root, message}` | Commits a work directory and pushes it to GitHub |
| `POST runs/<id>/events` | `{type, …}` | For the run process: records what the run did |
| `POST tmp` | `{session, files, removed}` | For the hook of the extension: copies the files that a session wrote, and records that its work directory changed |

## The extension

[`extension/durable.py`](../extension/durable.py) gives each session the alias `durable`:

| Call | Result |
| --- | --- |
| `durable.workflows()` | The workflows |
| `durable.run_workflow(name, input, session=None)` | A new run. `input` is an object of the inputs, or a text for the first input. `session` is a session id, or `"current"` for this session: then the run starts its work after this turn |
| `durable.run_status(run_id)` | The state of a run, its workflow activities and its files |
| `durable.workspace()` | The work directory of this session: its release, the newest release, its commits and its changed files |
| `durable.resync()` | Puts the work of this session on the newest release |
| `durable.publish(message)` | A push to GitHub |

The extension has no tool for documents: see [A document is a Vis attachment](#a-document-is-a-vis-attachment).
Its prompt tells the model to use `attach`.

The command `/workflows <name> <input>` starts a run from a session. Without a name,
it lists the workflows.

## The Workflows app

The worker opens one global app of Vis with the name Workflows
([`durable/vis/liveapps/workflows.py`](../durable/vis/liveapps/workflows.py)). Vis web shows each global app behind the
global apps button of its header, with or without an open session. The app has three
tabs:

| Tab | Content |
| --- | --- |
| Run | Select a workflow and its session, give the input and start a run. Then the tab shows the status of the run and its workflow activities. For a waiting run, it names the Gates tab. A failed run has the button Retry in a fork. |
| Gates | The oldest run that waits at a gate: the text of the gate, its document, a field for the comment, and the buttons Approve and Reject. |
| History | The number of runs in progress, completed and failed, and the last 30 runs with their status and their completed workflow activities. |

The worker is an SDK program: it calls `GatewayClient.attach_app` with
`scope="global"`, and one thread reads the actions of a person and paints the app. A
tab is a button with `is_selected`. The app has no Interrupt button, because an app
has no turn to interrupt.

## Read the attached files with the SDK

The Vis SDK gives the files that a session attached: of the whole conversation, of one
turn, of some turns or of the last turn.

```python
from blockether.vis.engine import GatewayClient

with GatewayClient(url) as client:
    session = client.session(session_id)
    session.attachments()  # the whole conversation, oldest turn first
    session.attachments(-1)  # the last turn; 0 is the first turn
    session.attachments(turn_id)  # one turn: a turn ID or a `Turn`
    session.attachments([0, -1])  # some turns

    turn = session.send("Write the plan. Attach it as PLAN.md.")
    turn.wait(timeout=600)
    for file in turn.attachments():
        print(file.turn_id, file.filename, file.version, file.size)
        plan = file.text()  # `file.read()` gives the bytes
```

The list holds only metadata: `read()` and `text()` download the file. When a session
attached a name more than one time, the list has each version. A file that a person
sent with a request is not in the list. These calls need `vis-agent` 0.5.18 or later
and a gateway of the same Vis version. `ctx.document` uses them.

## Limits

Runs and workflows:

- 3 runs work at the same time. The other runs wait in the queue. A waiting run uses no place.
- A run can work for 8 hours (`VIS_RUN_LIMIT_S`), which is 36 processes. Each worker change and each decision at a gate starts a new process.
- A run can have at most 1000 workflow activities.
- `runs/index.json` holds the 300 newest runs, and `GET runs` returns the 100 newest.
- The name of a workflow and the key of `ctx.gate` have the form `[a-z][a-z0-9_]{1,39}`.
- A workflow file has exactly one function with `@workflow` and at most 200 000 characters.
- The title of a gate has at most 200 characters and its text at most 2000. The comment of a decision has at most 4000 characters.
- `ctx.ask` and `ctx.document` wait 900 seconds for a turn, if you do not set `timeout=`.
- Vis continues one turn in any number of new workers, until the turn is 8 hours old. The Vis setting `turn_resume_limit` changes this time. A turn whose worker stopped three times in sequence without a handoff ends as interrupted.

Files:

- The API copies an attached file to S3 when its size is at most 2 000 000 bytes and its name starts with a letter or a digit and has at most 80 characters: letters, digits, `.`, `_`, `-` and spaces. A larger file stays only in the session.
- The hook copies a written file of at most 1 000 000 bytes. It does not copy `.git`, `__pycache__`, `node_modules`, `.cpcache`, `.venv`, `target`, `.pytest_cache`, `.ruff_cache`, `.clj-kondo` and `.lsp`. It reads at most 20 000 files of a directory.
- A Git bundle for `repo` has at most 100 000 000 bytes. A commit message has at most 200 characters.
- The copy of a work directory in S3 has at most 100 000 000 bytes. For larger work, S3 keeps the last copy that was small enough. The copy does not hold a file that Git ignores.
- The lists of sessions show a session after its first turn. The run dialog of the Workflows page offers at most 100 sessions, and the Workflows app at most 50.
- A model request has at most 5 000 000 bytes and asks for one answer.

Time:

- The `front` function waits 55 seconds for the first part of an answer. Then it answers 504.
- An event stream ends after 870 seconds. The client connects again.
- A worker without work stops after 300 seconds. A worker with work gives the work to a new worker after 810 seconds.
- `sync` keeps the 5 newest releases on EFS.
- The worker removes from EFS a work directory that nothing changed for 14 days. The copy in S3 stays, and the next turn of the session makes the directory again. The directory `work/main` stays.
- CloudWatch keeps the log of each function for 14 days. [S3 is the database](#s3-is-the-database) gives the time for each S3 key.

## Deployment

[`deploy.sh`](../deploy.sh) is safe to run again. It does these steps:

1. Checks that the repository has a `main` branch, and stops when it does not. Turns off GitHub Actions for the repository. Adds the ruleset "main is append-only": on a private repository, the ruleset needs a paid GitHub plan.
2. Builds the image and pushes it to Amazon ECR. ECR keeps the 5 newest images.
3. Stores the secrets in the Parameter Store. It makes the webhook secret and the deploy key. GitHub gets the public half of the deploy key, with write access.
4. Deploys [`template.yaml`](../template.yaml).
5. Adds the webhook to the repository.
6. Starts the first fetch. Starts the worker one time and sends one request to the stack: Lambda loads a new image at the first start, and that start is slow. Then it prints the address of the stack.

| Variable | Default | Use |
| --- | --- | --- |
| `REPOSITORY` | none; necessary | The GitHub repository, as `owner/name` |
| `DURABLE_LLM_URL` | none; necessary at the first run | The base URL of an OpenAI-compatible provider. It must use `https` |
| `DURABLE_LLM_KEY` | none; necessary at the first run | The key of the provider |
| `DURABLE_MODELS` | `glm-5.3`, `gpt-oss-120b`, `qwen3.8-max` | A JSON list of `{name, context, output_limit}`. Set it for each other provider: `llm` refuses a model that is not in the list |
| `STACK` | `durable-lambda` | The name of the stack and the prefix of each resource |
| `AWS_REGION` | `eu-central-1` | The region |
| `ENGINE` | `docker` | `docker` or `podman`. The stack of this example was made with `podman` |

The script keeps each secret that exists. To change the provider URL or the provider
key, write the parameter `/<stack>/llm-url` or `/<stack>/llm-key` with
`aws ssm put-parameter --overwrite`. The `llm` function reads its parameters when a new
instance starts.

The stack reserves 77 concurrent executions for its functions. AWS keeps 100 for the
other functions of the account, so the Lambda concurrency limit of the account must be
177 or more.

The functions read these optional variables. The template does not set them. To change
one, add it to the `Environment` of the function in `template.yaml`.

| Variable | Default | Function | Use |
| --- | --- | --- | --- |
| `VIS_IDLE_S` | `300` | `worker` | The seconds without work after which a worker stops |
| `VIS_LIFETIME_S` | `900` | `worker`, `front` | The Lambda time limit of the worker. It must be equal to the `Timeout` of the worker in the template |
| `VIS_HANDOFF_MARGIN_S` | `90` | `worker`, `front` | The seconds before the time limit at which a worker gives its work to a new worker |
| `VIS_STREAM_S` | `870` | `front` | The seconds after which an event stream ends |
| `VIS_RUN_LIMIT_S` | `28800` | `worker` | The seconds that one run can work |

To remove the stack:

1. Delete the stack in CloudFormation. This also deletes the EFS file system with each work directory. Their copies stay in the bucket.
2. Empty the bucket with all its versions, then delete it. The bucket stays after the stack. This deletes the copies of the work: publish the work first.
3. Delete the ECR repository and the parameters `/<stack>/*`.
4. Remove the webhook, the deploy key and the ruleset from the GitHub repository.

## The local server

`python -m durable.local` reads these variables. Here the list of models is in `VIS_MODELS`: `deploy.sh` reads `DURABLE_MODELS` and
the stack gives the value to the functions as `VIS_MODELS`.

| Variable | Default | Use |
| --- | --- | --- |
| `DURABLE_LOCAL` | `/tmp/local` | The directory of the data |
| `DURABLE_SEED` | none | The first content of the repository: a clone of the project with a `main` branch, or a directory of files |
| `PORT` | `8080` | The port of the server |

## The package `durable`

Each module has one task. A function imports only the modules of its role.

`settings.py` is the one module that reads the environment. Each path, limit, time and
name of the stack is there, in sections with comments. A constant in another module is
the form of the data of that module, for example the pattern of a route. It is not a
setting. `local.py` is the exception: it sets the environment before the package starts.

The worker calls the Vis gateway with the Vis SDK, and a workflow uses a Vis session for
each model call. The `llm` function is not a model client. It is the only code that has
the provider key, and the only way from the gateway to the provider: the worker has no
route to the internet and no key.

| Module | Task |
| --- | --- |
| [`handler.py`](../durable/handler.py) | The handler of each role. `python -m durable` starts the runtime with the handler that `VIS_ROLE` selects |
| [`lambda_runtime.py`](../durable/lambda_runtime.py) | The Lambda Runtime API |
| [`settings.py`](../durable/settings.py) | Each setting of the stack: the environment, the paths, the limits, the times and the names |
| [`helpers.py`](../durable/helpers.py) | The log, private files and the filter of an archive |
| [`domain.py`](../durable/domain.py) | The rules of the API: the names, the limits, the statuses of a run and the refusals |
| [`aws.py`](../durable/aws.py) | Each call to S3, SQS, the Parameter Store and Lambda |
| [`wire.py`](../durable/wire.py) | The form of an answer that goes through S3 |
| [`core.py`](../durable/core.py) | The lease and the checkpoints of the Vis home |
| [`git.py`](../durable/git.py) | The `repo` function: the fetch from GitHub and the push to GitHub |
| [`worktree.py`](../durable/worktree.py) | The `sync` function and the directories on EFS: the releases, the work directories and their copy in S3 |
| [`llm.py`](../durable/llm.py) | The `llm` function: it adds the provider key to a model call of the gateway and streams the answer |
| [`worker.py`](../durable/worker.py) | The `worker` function: the loop of the one live worker |
| [`app.py`](../durable/app.py) | The routes of the Durable Lambda API |
| [`dashboard/`](../durable/dashboard) | The `front` function: `front.py` has the pages and the webhook, `relay.py` sends a call to the worker, `web.py` has the request and the answer |
| [`vis/`](../durable/vis) | What the worker does with Vis: `gateway.py` runs the gateway and calls it with the Vis SDK, `relay.py` answers the calls of the queue, `broker.py` sends the model calls of the gateway to the `llm` function, `sessions.py` gives each session its work directory, `files.py` copies the files of a session, `registry.py` registers the project, the groups and the apps, `liveapps/` has the Workflows app |
| [`workflows/`](../durable/workflows) | The workflows: `sdk.py` has `@workflow` and `@activity`, `source.py` reads a workflow file, `runs.py` keeps the runs, `process.py` is the process of one run, `client.py` calls the API from that process |
| [`local.py`](../durable/local.py) | All the functions in one process, without AWS |
