"""Run the whole playground on one machine, without AWS: `python -m playground.local`.

The front function, the relay, the worker, the llm function, the repo function and
the sync function run as they are. Only the AWS services are replaced: S3 is a
directory, the queue is a list, a parameter is an environment variable and an
invocation is a function call. GitHub is a bare repository in a directory.

Set `PLAYGROUND_PASSWORD`, `PLAYGROUND_LLM_URL` and `PLAYGROUND_LLM_KEY`. Then open
http://localhost:8080. Use this only for development: nothing here isolates the key.
"""

from __future__ import annotations

import base64
import datetime
import hashlib
import io
import json
import os
import queue
import shutil
import subprocess
import threading
import time
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any

from botocore.exceptions import ClientError

ROOT = os.environ.get("PLAYGROUND_LOCAL", "/tmp/local")
SETTINGS = {
    "VIS_BUCKET": "local",
    "VIS_QUEUE_URL": "local",
    "VIS_WORKER_FUNCTION": "worker",
    "VIS_LLM_FUNCTION": "llm",
    "VIS_REPO_FUNCTION": "repo",
    "VIS_SYNC_FUNCTION": "sync",
    "VIS_EFS": f"{ROOT}/efs",
    "VIS_REPO": "local/vis-playground",
    "VIS_REPO_URL": f"{ROOT}/github.git",
    "VIS_TOKEN_PARAMETER": "PLAYGROUND_PASSWORD",
    "VIS_LLM_URL_PARAMETER": "PLAYGROUND_LLM_URL",
    "VIS_LLM_KEY_PARAMETER": "PLAYGROUND_LLM_KEY",
    "VIS_DEPLOY_KEY_PARAMETER": "PLAYGROUND_DEPLOY_KEY",
    # This process is also the repo function and the bucket.
    "VIS_PRIVATE_TMP": "local:deploy-key",
}
for name, value in SETTINGS.items():
    os.environ.setdefault(name, value)
os.environ.setdefault("PLAYGROUND_DEPLOY_KEY", "not used: GitHub is a directory")

from playground import aws, front, llm, repo, runtime, sync, worker  # noqa: E402 - the settings come first


def refusal(code: str, operation: str) -> ClientError:
    return ClientError({"Error": {"Code": code, "Message": code}}, operation)


class Bucket:
    """S3 as a directory. A write can be conditional, as in S3."""

    def __init__(self) -> None:
        self.root, self.lock = f"{ROOT}/s3", threading.Lock()

    def path(self, key: str) -> str:
        return os.path.join(self.root, key)

    def tag(self, key: str) -> str | None:
        try:
            with open(self.path(key), "rb") as file:
                return '"' + hashlib.md5(file.read()).hexdigest() + '"'  # noqa: S324 - an ETag, as in S3
        except OSError:
            return None

    def get_object(self, Bucket: str, Key: str) -> dict[str, Any]:  # noqa: N803 - the names of boto3
        tag = self.tag(Key)
        if tag is None:
            raise refusal("NoSuchKey", "GetObject")
        with open(self.path(Key), "rb") as file:
            return {"Body": io.BytesIO(file.read()), "ETag": tag}

    def head_object(self, Bucket: str, Key: str) -> dict[str, Any]:  # noqa: N803
        if self.tag(Key) is None:
            raise refusal("404", "HeadObject")
        return {"ContentLength": os.path.getsize(self.path(Key))}

    def put_object(self, Bucket: str, Key: str, Body: bytes, **options: Any) -> dict[str, Any]:  # noqa: N803
        with self.lock:
            tag = self.tag(Key)
            if ("IfNoneMatch" in options and tag) or ("IfMatch" in options and options["IfMatch"] != tag):
                raise refusal("PreconditionFailed", "PutObject")
            os.makedirs(os.path.dirname(self.path(Key)), exist_ok=True)
            temporary = f"{self.path(Key)}.{uuid.uuid4().hex}"
            with open(temporary, "wb") as file:
                file.write(Body)
            os.replace(temporary, self.path(Key))
            return {"ETag": self.tag(Key)}

    def delete_object(self, Bucket: str, Key: str) -> None:  # noqa: N803
        if os.path.exists(self.path(Key)):
            os.remove(self.path(Key))

    def get_paginator(self, name: str) -> Bucket:
        return self

    def paginate(self, Bucket: str, Prefix: str) -> list[dict[str, Any]]:  # noqa: N803
        found = []
        for folder, _, names in os.walk(self.root):
            for name in names:
                path = os.path.join(folder, name)
                key = os.path.relpath(path, self.root)
                if key.startswith(Prefix):
                    changed = datetime.datetime.fromtimestamp(os.path.getmtime(path), datetime.UTC)
                    found.append({"Key": key, "Size": os.path.getsize(path), "LastModified": changed})
        return [{"Contents": sorted(found, key=lambda item: item["Key"])}]


class Queue:
    """SQS as a list."""

    def __init__(self) -> None:
        self.messages: queue.Queue[str] = queue.Queue()

    def send_message(self, QueueUrl: str, MessageBody: str) -> None:  # noqa: N803
        self.messages.put(MessageBody)

    def receive_message(self, QueueUrl: str, MaxNumberOfMessages: int, WaitTimeSeconds: int) -> dict[str, Any]:  # noqa: N803
        try:
            return {"Messages": [{"Body": self.messages.get(timeout=WaitTimeSeconds), "ReceiptHandle": uuid.uuid4().hex}]}
        except queue.Empty:
            return {}

    def delete_message(self, QueueUrl: str, ReceiptHandle: str) -> None:  # noqa: N803
        pass


class Parameters:
    def get_parameter(self, Name: str, WithDecryption: bool) -> dict[str, Any]:  # noqa: N803
        return {"Parameter": {"Value": os.environ[Name]}}


def context(seconds: float = 900) -> runtime.Context:
    return runtime.Context(uuid.uuid4().hex, int((time.time() + seconds) * 1000), "local")


class Functions:
    """Lambda as function calls."""

    wake = threading.Event()

    def invoke(self, FunctionName: str, Payload: bytes = b"{}", InvocationType: str = "RequestResponse") -> dict[str, Any]:  # noqa: N803
        if FunctionName == "worker":
            self.wake.set()
            return {}
        handler = {"repo": repo.handler, "sync": sync.handler}[FunctionName]
        try:
            return {"Payload": io.BytesIO(json.dumps(handler(json.loads(Payload), context())).encode())}
        except Exception as failure:  # as Lambda reports a failed function
            return {"FunctionError": "Unhandled", "Payload": io.BytesIO(json.dumps({"errorMessage": str(failure)}).encode())}

    def invoke_with_response_stream(self, FunctionName: str, Payload: bytes) -> dict[str, Any]:  # noqa: N803
        answer = llm.handler(json.loads(Payload), context())
        return {"EventStream": ({"PayloadChunk": {"Payload": chunk}} for chunk in answer.parts())}


SERVICES = {"s3": Bucket(), "sqs": Queue(), "ssm": Parameters(), "lambda": Functions()}
aws.client = lambda name: SERVICES[name]  # type: ignore[assignment]


def github(seed: str) -> None:
    """Make the bare repository that stands in for GitHub, with one commit on main."""
    if os.path.isdir(SETTINGS["VIS_REPO_URL"]):
        return
    work = f"{ROOT}/seed"
    shutil.copytree(seed, work) if os.path.isdir(seed) else os.makedirs(work)
    if not os.listdir(work):
        with open(f"{work}/README.md", "w", encoding="utf-8") as file:
            file.write("# vis-playground\n")
    identity = ["-c", "user.name=Vis Playground", "-c", "user.email=playground@blockether.com"]
    for command in (["init", "--quiet", "--initial-branch", "main"], ["add", "--all"], [*identity, "commit", "--quiet", "--message", "Start the project"]):
        subprocess.run(["git", *command], cwd=work, check=True)
    subprocess.run(["git", "clone", "--quiet", "--bare", work, SETTINGS["VIS_REPO_URL"]], check=True)


class Page(BaseHTTPRequestHandler):
    """The Function URL: it calls the front function for each request."""

    protocol_version = "HTTP/1.1"

    def log_message(self, *args: Any) -> None:
        pass

    def serve(self) -> None:
        path, _, query = self.path.partition("?")
        body = self.rfile.read(int(self.headers.get("content-length") or 0))
        event = {
            "requestContext": {"http": {"method": self.command}},
            "rawPath": path,
            "rawQueryString": query,
            "headers": {name.lower(): value for name, value in self.headers.items()},
            "cookies": [part.strip() for part in (self.headers.get("cookie") or "").split(";") if part.strip()],
            "body": base64.b64encode(body).decode(),
            "isBase64Encoded": True,
        }
        answer = front.handler(event, context(840))
        self.send_response(answer.status)
        for name, value in answer.headers.items():
            self.send_header(name, value)
        for cookie in answer.cookies:
            self.send_header("set-cookie", cookie)
        if answer.status in (204, 304) or self.command == "HEAD":  # these answers have no body
            self.end_headers()
            return
        self.send_header("transfer-encoding", "chunked")
        self.end_headers()
        try:
            for chunk in answer.chunks:
                if chunk:
                    self.wfile.write(b"%x\r\n%s\r\n" % (len(chunk), chunk))
                    self.wfile.flush()
            self.wfile.write(b"0\r\n\r\n")
        except (BrokenPipeError, ConnectionResetError):
            self.close_connection = True

    do_GET = do_POST = do_PUT = do_PATCH = do_DELETE = do_HEAD = serve


def main() -> None:
    os.makedirs(ROOT, exist_ok=True)
    github(os.environ.get("PLAYGROUND_SEED", "/opt/playground/repo-seed"))
    print("first release:", repo.fetch(), flush=True)
    server = ThreadingHTTPServer(("0.0.0.0", int(os.environ.get("PORT", "8080"))), Page)  # noqa: S104 - a container port
    server.daemon_threads = True
    threading.Thread(target=server.serve_forever, daemon=True).start()
    print("playground on http://localhost:8080", flush=True)
    while True:
        # The schedule of the stack: a tick each minute, and a worker when one is wanted.
        if Functions.wake.wait(timeout=60):
            Functions.wake.clear()
            print("worker:", worker.handler({"kind": "wake"}, context()), flush=True)
        else:
            front.tick()


if __name__ == "__main__":
    main()
