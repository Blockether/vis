"""The settings of the stack: each environment variable, path, limit, time and name.

No other module of the package reads the environment, and no other module holds a
value that a person can change: a module imports such a value from here. A module
keeps only the form of its own data: a wire format, a table of routes, the pattern of
a name or a header policy.

`template.yaml` sets the `VIS_*` variables of the stack. `local.py` sets them for a
run without AWS, before it imports the package. The Vis extension
`extension/durable.py` runs in a Vis session without this package, so it reads
`DURABLE_BROKER` and `DURABLE_PRIVATE_TMP` itself.
"""

from __future__ import annotations

import json
import os

ENV = os.environ

# -- the functions of the stack ------------------------------------------------------

# The function that this process is: front, worker, llm, repo or sync (see `handler.py`).
ROLE = ENV.get("VIS_ROLE", "")
WORKER_FUNCTION = ENV.get("VIS_WORKER_FUNCTION", "")
LLM_FUNCTION = ENV.get("VIS_LLM_FUNCTION", "")
REPO_FUNCTION = ENV.get("VIS_REPO_FUNCTION", "")
SYNC_FUNCTION = ENV.get("VIS_SYNC_FUNCTION", "")
# The address of the Lambda Runtime API. Lambda sets it.
RUNTIME_API = ENV.get("AWS_LAMBDA_RUNTIME_API", "")

# -- S3, the queue and the secrets ---------------------------------------------------

BUCKET = ENV.get("VIS_BUCKET", "")
QUEUE = ENV.get("VIS_QUEUE_URL", "")
# The S3 object that names the one live worker of the shard `main` and its last checkpoint.
LEASE = "lease.json"
# The shard of the sessions of people. Each other shard has the sessions of one workflow run (see `shards.py`).
MAIN = "main"
# The names of the SSM parameters that hold the secrets. Only the function that
# needs a secret gets the name of its parameter.
LLM_URL_PARAMETER = ENV.get("VIS_LLM_URL_PARAMETER", "")
LLM_KEY_PARAMETER = ENV.get("VIS_LLM_KEY_PARAMETER", "")
DEPLOY_KEY_PARAMETER = ENV.get("VIS_DEPLOY_KEY_PARAMETER", "")
# A stack without this parameter has no webhook of GitHub.
WEBHOOK_PARAMETER = ENV.get("VIS_WEBHOOK_PARAMETER", "")

# -- the files of the image ----------------------------------------------------------

VIS_BINARY = "/opt/vis/vis-agent-native"
VIS_PYTHON_NATIVE = "/opt/vis/vis-agent-python/libvispython.so"
# The Vis web app, the Software factory pages and the pinned files of other projects.
VIS_WEB, PAGES_WEB, VENDOR = ENV.get("VIS_WEB_DIR", "/opt/vis-web"), "/opt/durable/web", "/opt/durable/vendor"
# The workflows that a new stack starts with.
SAMPLES = "/opt/durable/workflows"
# The pinned host keys of GitHub.
KNOWN_HOSTS = "/opt/durable/known_hosts"

# -- the directories of a function ---------------------------------------------------

# EFS holds the releases of the repository and the work directories (see `worktree.py`).
EFS = ENV.get("VIS_EFS", "/mnt/efs")
RELEASES, WORK, CURRENT = f"{EFS}/releases", f"{EFS}/work", f"{EFS}/CURRENT"
# The name of the one project directory in WORK. It follows the releases, so its sessions stay in one project.
PROJECT_DIR = "main"
# The home directory of Vis. A checkpoint saves it (see `core.py`).
HOME = ENV.get("VIS_HOME", "/tmp/home")
# The files of the worker itself. A session must not copy them as its work.
WORKER_DIR = ENV.get("VIS_WORKER_DIR", "/tmp/worker")
# The entries of /tmp that are not the work of a session.
PRIVATE_TMP = ["home", "worker", *filter(None, ENV.get("VIS_PRIVATE_TMP", "").split(":"))]
# The deploy key of the repo function, as a file for SSH.
DEPLOY_KEY_FILE = "/tmp/deploy-key"

# -- the repository ------------------------------------------------------------------

# The GitHub repository of the project, as `owner/name`.
REPO = ENV.get("VIS_REPO", "")
# Another address is for a test with a repository in a directory.
REPO_URL = ENV.get("VIS_REPO_URL") or f"git@github.com:{REPO}.git"
# The branch that the stack follows and publishes to.
BRANCH = "main"
# The name of the Vis project. Vis uses it only when it makes the project.
PROJECT_NAME = REPO.rpartition("/")[2] or "durable"
# The directory of the repository with the workflow files. S3 has a copy of each of them.
WORKFLOW_DIR = "workflows"
# GitHub runs the workflow files of this directory with internet access, so a session cannot change it.
GUARDED = ".github"
# The author of each commit that the stack makes.
GIT_NAME, GIT_EMAIL = "Software factory", "durable-lambda@blockether.com"
# The time that one Git command can take.
GIT_S = 240
# The largest Git bundle of a workspace or of a publication, in bytes.
BUNDLE_LIMIT = 100_000_000
# The releases that stay on EFS, the days that an unused work directory stays and
# the seconds that the parts of a failed sync stay.
RELEASES_KEPT, WORK_DAYS, STALE_S = 5, 14, 3600

# -- the models and the provider -----------------------------------------------------

# The models that the gateway offers and that the llm function accepts.
MODELS = json.loads(ENV.get("VIS_MODELS") or "null") or [
    {"name": "glm-5.3", "context": 131072, "output_limit": 16384},
    {"name": "gpt-oss-120b", "context": 131072, "output_limit": 16384},
    {"name": "qwen3.8-max", "context": 131072, "output_limit": 16384},
]
# The id of the one provider in the Vis configuration. It is the broker of the worker.
PROVIDER = "durable"
# The headers of a model call that go to the provider.
PROVIDER_HEADERS = ("content-type", "accept")
# The largest model request, in bytes, and the time that the provider has for one answer.
REQUEST_LIMIT, PROVIDER_S = 5_000_000, 600

# -- the Vis gateway of the worker ---------------------------------------------------

# The gateway and the broker listen only on the loopback interface.
LOOPBACK, GATEWAY_PORT = "127.0.0.1", int(ENV.get("VIS_GATEWAY_PORT", "7890"))
GATEWAY_URL = f"http://{LOOPBACK}:{GATEWAY_PORT}"
# The token of the gateway. The worker makes a new token at each start.
TOKEN_FILE = f"{WORKER_DIR}/gateway.token"
# The time that the gateway has to start.
GATEWAY_START_S = 60
# The session group of the workflow runs, and the group of the sessions that a
# person starts. The two groups keep each session in the one project.
RUNS_GROUP, PEOPLE_GROUP = "Workflows", "Sessions"
# What the worker gives to each process that it starts next to the gateway: the
# address of the broker, and the id of the group of the runs for a run process.
BROKER = ENV.get("DURABLE_BROKER", "")
RUN_GROUP = ENV.get("DURABLE_GROUP", "")

# -- the life of a worker ------------------------------------------------------------

# A worker stops MARGIN seconds before the Lambda time limit and starts its successor.
MARGIN = int(ENV.get("VIS_HANDOFF_MARGIN_S", "90"))
LIFETIME = int(ENV.get("VIS_LIFETIME_S", "900")) - MARGIN
# A worker without work stops after IDLE_S.
IDLE_S = int(ENV.get("VIS_IDLE_S", "300"))
# A worker of a run that only answered reads stops after READ_IDLE_S: Vis web reads the first sessions of its list at each load.
READ_IDLE_S = min(IDLE_S, 60)
# A worker starts, or stays alive, when an automation is due in DUE_S.
DUE_S = 120
# A worker that stops without a release loses the lease after LEASE_TTL.
LEASE_TTL = 90
# The worker renews the lease each RENEW_S and looks for state to save each SAVE_S.
RENEW_S, SAVE_S = 30, 15
# At its end the gateway gets at most DRAIN_S to park the running turns, and a turn
# gets DRAIN_GRACE_S to end by itself. LAST_SAVE_S stay for the last checkpoint.
DRAIN_S, DRAIN_GRACE_S, LAST_SAVE_S = 45, 10, 40
# The open calls then get SETTLE_S to write their last part.
SETTLE_S = 8

# -- the relay between the front function and the worker -----------------------------

# The calls that the worker answers at the same time.
CALLS = 64
# The front waits ANSWER_S for the first part of an answer.
ANSWER_S = 55
# A relayed event stream ends after STREAM_S; the client connects again from its cursor.
STREAM_S = int(ENV.get("VIS_STREAM_S", "870"))
# An event stream collects its bytes for COLLECT_S, writes one S3 object, then waits PAUSE_S.
COLLECT_S, PAUSE_S = 0.2, 0.15
# The front starts the worker of a shard at most each WAKE_S.
WAKE_S = 5
# A caller asks S3 again for the shard of a session after ROUTE_S, and a worker
# publishes the new sessions of its shard each ROUTE_S.
ROUTE_S = 2
# A worker reads the bell of its shard each INBOX_S, and lists the calls each INBOX_LIST_S also without a bell.
INBOX_S, INBOX_LIST_S = 0.2, 10
# The schedule starts a worker for a shard that has no lease only after START_S: its first worker can still start.
START_S = 90
# The worker knows an answered call for ANSWERED_S, so a repeated message gets no second answer.
ANSWERED_S = 1200
# A larger body of a call goes in its own S3 object: an SQS message has a size limit.
MESSAGE_LIMIT = 150_000
# The bytes of one read of a streamed answer.
CHUNK = 65536

# -- the workflow runs ---------------------------------------------------------------

# The runs that work at the same time in one worker. The other runs wait.
PARALLEL = 3
# The activities that one run can have.
STEPS = 1000
# The time that one run can work. A worker handoff starts a new process, and one
# process works for at most the lifetime of a worker.
RUN_LIMIT_S = int(ENV.get("VIS_RUN_LIMIT_S", "28800"))
STARTS = -(-RUN_LIMIT_S // LIFETIME)
# The time that one turn of a run can take, and one call of the Software factory API.
TURN_S, API_S = 900, 120
# The correction requests that a session gets for an answer that does not fit its model.
CORRECTIONS = 2
# The runs that the index keeps, that the API lists and that the Workflows app lists.
RUNS_KEPT, RUNS_LISTED, APP_RUNS = 300, 100, 30
# The seconds between two looks of the Workflows app at the runs.
LOOK_S = 2
# The sessions that the Workflows app offers for a run.
PLACES = 50

# -- the files and the sessions ------------------------------------------------------

# The largest source of a workflow and the largest attached file that S3 gets, in bytes.
WORKFLOW_LIMIT, ATTACHED_LIMIT = 200_000, 2_000_000
# The largest comment of a person at a gate, in characters.
COMMENT_LIMIT = 4_000
# The worker keeps the directory of a session for HOME_S before it asks the gateway again.
HOME_S = 60
