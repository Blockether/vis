"""The settings of the stack, from the environment of the function."""

from __future__ import annotations

import json
import os

ENV = os.environ
ROLE = ENV.get("VIS_ROLE", "")
BUCKET = ENV.get("VIS_BUCKET", "")
QUEUE = ENV.get("VIS_QUEUE_URL", "")
WORKER_FUNCTION = ENV.get("VIS_WORKER_FUNCTION", "")
LLM_FUNCTION = ENV.get("VIS_LLM_FUNCTION", "")
REPO_FUNCTION = ENV.get("VIS_REPO_FUNCTION", "")
SYNC_FUNCTION = ENV.get("VIS_SYNC_FUNCTION", "")
EFS = ENV.get("VIS_EFS", "/mnt/efs")
HOME = ENV.get("VIS_HOME", "/tmp/home")
# The files of the worker itself. A session must not copy them as its work.
WORKER_DIR = "/tmp/worker"
# The entries of /tmp that are not the work of a session.
PRIVATE_TMP = ["home", "worker", *filter(None, ENV.get("VIS_PRIVATE_TMP", "").split(":"))]
# The models that the gateway offers and that the llm function accepts.
MODELS = json.loads(ENV.get("VIS_MODELS") or "null") or [
    {"name": "glm-5.3", "context": 131072, "output_limit": 16384},
    {"name": "gpt-oss-120b", "context": 131072, "output_limit": 16384},
    {"name": "qwen3.8-max", "context": 131072, "output_limit": 16384},
]

# A worker stops MARGIN seconds before the Lambda time limit and starts its successor.
MARGIN = int(ENV.get("VIS_HANDOFF_MARGIN_S", "90"))
LIFETIME = int(ENV.get("VIS_LIFETIME_S", "900")) - MARGIN
# A worker without work stops after IDLE_S.
IDLE_S = int(ENV.get("VIS_IDLE_S", "300"))
# A relayed event stream ends after STREAM_S; the client connects again from its cursor.
STREAM_S = int(ENV.get("VIS_STREAM_S", "870"))
# A worker starts, or stays alive, when an automation is due in DUE_S.
DUE_S = 120
# The front waits ANSWER_S for the first part of an answer.
ANSWER_S = 55
# A worker that stops without a release loses the lease after LEASE_TTL.
LEASE, LEASE_TTL = "lease.json", 90


def private(name: str) -> str:
    """Return the path of a worker file, in a directory that only the worker user reads."""
    os.makedirs(WORKER_DIR, mode=0o700, exist_ok=True)
    return os.path.join(WORKER_DIR, name)


def write_secret(path: str, text: str) -> None:
    """Write a file that only the user of this function can read."""
    with open(os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600), "w", encoding="utf-8") as file:
        file.write(text)


def log(*parts: object) -> None:
    """Write one JSON line to the function log."""
    print(json.dumps({"vis": ROLE, "msg": " ".join(map(str, parts))}), flush=True)
