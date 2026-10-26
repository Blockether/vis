"""Run the Vis gateway durably on AWS Lambda.

Two functions run this file behind the Lambda Web Adapter:

- front (VIS_ROLE=front) answers the Function URL. It serves the web app,
  checks the bearer token and relays API calls to the live worker: the
  request goes to SQS, the response comes back as S3 objects.
- worker (VIS_ROLE=worker) holds a lease in S3, restores ~/.vis from S3,
  runs the native gateway and answers relayed calls. Before the Lambda
  time limit it drains the gateway, saves the state, starts its successor
  and gives the lease away. The new gateway continues the parked turns.

S3 is the database of record. Each save uploads a new checkpoint under
state/ with a consistent SQLite copy. lease.json names the one live worker
and the current checkpoint. A save counts only after the conditional lease
write, so a worker that lost the lease cannot replace the state.
"""

import base64
import hashlib
import hmac
import http.client
import http.cookies
import http.server
import io
import json
import mimetypes
import os
import pathlib
import queue
import shutil
import sqlite3
import struct
import subprocess
import tarfile
import threading
import time
import urllib.parse
import uuid
from concurrent.futures import ThreadPoolExecutor

import boto3
from botocore.exceptions import ClientError

ENV = os.environ
ROLE, BUCKET, QUEUE = ENV["VIS_ROLE"], ENV["VIS_BUCKET"], ENV["VIS_QUEUE_URL"]
WORKER = ENV.get("VIS_WORKER_FUNCTION", "")
MARGIN = int(ENV.get("VIS_HANDOFF_MARGIN_S", "90"))
LIFETIME = int(ENV.get("VIS_LIFETIME_S", "900")) - MARGIN
# The drain gives running turns this time to park; the save and the handoff use the rest.
DRAIN_S = max(5, MARGIN // 2)
IDLE_S = int(ENV.get("VIS_IDLE_S", "600"))
# A relayed event stream ends after STREAM_S; the client reconnects from its cursor.
# Gateway heartbeats (15 s) keep the CloudFront 60 s origin read timeout open.
STREAM_S = int(ENV.get("VIS_STREAM_S", "600"))
# The front waits ANSWER_S for the first answer, under the CDN's 60 s read timeout.
ANSWER_S = 55
HOME, WEB, VIS = "/tmp/home", "/opt/vis-web", "/opt/vis"
LEASE, LEASE_TTL = "lease.json", 90
# The state of a lease that names no checkpoint, for example from an older worker.
FIRST_STATE = "state/home.tar.gz"
# Logs, download caches and models stay out of the saved state; tools fetch them again.
UNSAVED = {
    HOME + p for p in ("/.vis/logs", "/.vis/models", "/.m2", "/.gitlibs", "/.cache")
}
# The image holds the speech models in MODELS and the bytecode of the code under /tmp in PYCACHE.
MODELS, PYCACHE = "/opt/vis-models", "/tmp/pycache"
# A change in the home folder is saved after at most SAVE_S seconds.
SAVE_S = int(ENV.get("VIS_SAVE_S", "15"))
PORT = 7890
HOP = {"connection", "content-length", "transfer-encoding", "host", "keep-alive"}
s3, sqs, lam = boto3.client("s3"), boto3.client("sqs"), boto3.client("lambda")
TOKEN = boto3.client("ssm").get_parameter(
    Name=ENV["VIS_TOKEN_PARAMETER"], WithDecryption=True
)["Parameter"]["Value"]
# The browser cookie is derived from the token, so it never holds the token itself.
COOKIE = hmac.new(TOKEN.encode(), b"vis-web", "sha256").hexdigest()


def log(*a):
    print(json.dumps({"vis": ROLE, "msg": " ".join(map(str, a))}), flush=True)


LOGIN_PAGE = pathlib.Path(__file__).with_name("login.html").read_text()


def get(key):
    """Return (bytes, etag) of an S3 object, or (None, None)."""
    try:
        r = s3.get_object(Bucket=BUCKET, Key=key)
        return r["Body"].read(), r["ETag"]
    except s3.exceptions.NoSuchKey:
        return None, None


def put(key, body, **cond):
    return s3.put_object(Bucket=BUCKET, Key=key, Body=body, **cond)["ETag"]


def lease():
    raw, etag = get(LEASE)
    return (json.loads(raw) if raw else {}), etag


def alive(data):
    return data.get("expires", 0) > time.time()


# ---------------------------------------------------------------- front


def frame(head, body=b""):
    h = json.dumps(head).encode()
    return struct.pack(">I", len(h)) + h + body


class Front(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"
    checked = 0.0

    def log_message(self, *a):
        pass

    def do_GET(self):
        self.route()

    do_POST = do_PUT = do_PATCH = do_DELETE = do_HEAD = do_OPTIONS = do_GET

    def send(self, status, body, ctype="application/json", cache=None):
        self.send_response(status)
        if cache:
            self.send_header("cache-control", cache)
        self.send_header("content-type", ctype)
        self.send_header("content-length", str(len(body)))
        self.end_headers()
        if self.command != "HEAD":
            self.wfile.write(body)

    def route(self):
        path = self.path.split("?")[0]
        if ROLE == "worker":
            return self.send(200, worker(self.body()) if path == "/events" else b"{}")
        if path == "/login" and self.command == "POST":
            return self.login(
                urllib.parse.parse_qs(self.body().decode()).get("password", [""])[0]
            )
        if path.startswith("/v1/hooks/"):
            return self.relay()
        authed = self.authed()
        if not authed and path in ("/", "/index.html"):
            return self.send(200, LOGIN_PAGE.encode(), "text/html")
        # The web app files are public release assets; only the API needs the password.
        if self.command in ("GET", "HEAD") and self.static(path):
            return
        if not authed:
            return self.send(401, b'{"error":"a valid bearer token is required"}')
        self.relay()

    def authed(self):
        """A bearer token, or the session cookie that /login sets."""
        cookie = http.cookies.SimpleCookie(self.headers.get("cookie") or "").get("vis")
        return (
            hmac.compare_digest(
                (self.headers.get("authorization") or "").encode(),
                b"Bearer " + TOKEN.encode(),
            )
            or bool(cookie)
            and hmac.compare_digest(cookie.value.encode(), COOKIE.encode())
        )

    def login(self, password):
        if not hmac.compare_digest(password.encode(), TOKEN.encode()):
            time.sleep(1)
            return self.send(
                401,
                LOGIN_PAGE.replace("<!--error-->", "Wrong password.").encode(),
                "text/html",
            )
        self.send_response(303)
        self.send_header("location", "/")
        self.send_header(
            "set-cookie",
            f"vis={COOKIE}; Path=/; Max-Age=31536000; HttpOnly; Secure; SameSite=Strict",
        )
        self.send_header("content-length", "0")
        self.end_headers()

    def body(self):
        n = int(self.headers.get("content-length") or 0)
        return self.rfile.read(n) if n else b""

    def static(self, path):
        rel = "index.html" if path == "/" else path.lstrip("/")
        file = os.path.realpath(os.path.join(WEB, rel))
        if not file.startswith(WEB + "/") or not os.path.isfile(file):
            return False
        with open(file, "rb") as f:
            # Hashed bundle files never change; the browser reloads them from its cache.
            # no-store keeps Safari from restoring an old app page from its page cache.
            cache = (
                "public, max-age=31536000, immutable"
                if rel.startswith("assets/")
                else "no-store"
            )
            self.send(
                200,
                f.read(),
                mimetypes.guess_type(file)[0] or "application/octet-stream",
                cache,
            )
        return True

    def relay(self):
        stream = "text/event-stream" in (self.headers.get("accept") or "")
        rid, body, deadline = (
            str(uuid.uuid4()),
            self.body(),
            time.time() + (STREAM_S if stream else LIFETIME),
        )
        msg = {
            "id": rid,
            "method": self.command,
            "path": self.path,
            "deadline": deadline,
            # After this time no client waits for the answer, so the worker drops the call.
            "answer_by": time.time() + ANSWER_S,
            "headers": {
                k: v
                for k, v in self.headers.items()
                if k.lower() not in HOP | {"cookie", "authorization"}
            },
        }
        if not self.path.startswith(
            "/v1/hooks/"
        ):  # a webhook proves itself with its own secret
            msg["headers"]["Authorization"] = "Bearer " + TOKEN
        if len(body) > 200_000:
            put(f"relay/{rid}/request", body)
            msg["body_key"] = f"relay/{rid}/request"
        else:
            msg["body"] = base64.b64encode(body).decode()
        sqs.send_message(QueueUrl=QUEUE, MessageBody=json.dumps(msg))
        index, wait, started = 0, 0.03, time.time()
        while time.time() < deadline:
            chunk, _ = get(f"relay/{rid}/{index}")
            if chunk is None:
                if index == 0 and time.time() - started > ANSWER_S:
                    return self.send(504, b'{"error":"the Vis worker did not answer"}')
                if index == 0:
                    self.wake()
                time.sleep(wait)
                wait = min(wait * 1.5, 0.25)
                continue
            wait, last, data = 0.03, chunk[:1] == b"E", chunk[1:]
            if index == 0:
                n = struct.unpack(">I", data[:4])[0]
                head, data = json.loads(data[4 : 4 + n]), data[4 + n :]
                self.send_response(head["status"])
                for k, v in head["headers"]:
                    self.send_header(k, v)
                if last:
                    self.send_header("content-length", str(len(data)))
                else:
                    self.send_header("transfer-encoding", "chunked")
                self.end_headers()
                if last:
                    return self.wfile.write(data)
            if data:
                self.wfile.write(b"%x\r\n%s\r\n" % (len(data), data))
                self.wfile.flush()
            if last:
                return self.wfile.write(b"0\r\n\r\n")
            index += 1

    def wake(self):
        """Start a worker when no live worker holds the lease (at most every 5 s)."""
        if time.time() - Front.checked < 5:
            return
        Front.checked = time.time()
        if not alive(lease()[0]):
            log("waking worker")
            lam.invoke(
                FunctionName=WORKER, InvocationType="Event", Payload=b'{"kind":"wake"}'
            )


# ---------------------------------------------------------------- worker


class Lease:
    def __init__(self):
        self.id, self.etag, self.data, self.saves = str(uuid.uuid4()), None, {}, 0

    def write(self, **data):
        self.data = {**self.data, **data, "owner": self.id}
        cond = {"IfMatch": self.etag} if self.etag else {"IfNoneMatch": "*"}
        self.etag = put(LEASE, json.dumps(self.data).encode(), **cond)

    def acquire(self):
        data, self.etag = lease()
        if alive(data):
            return None
        # The new lease keeps the checkpoint and the pending work of the last worker.
        self.data = {k: v for k, v in data.items() if k != "owner"}
        self.write(expires=time.time() + LEASE_TTL)
        return data


def gateway(method, path, body=None, timeout=30):
    c = http.client.HTTPConnection("127.0.0.1", PORT, timeout=timeout)
    c.request(
        method,
        path,
        body=json.dumps(body) if body is not None else None,
        headers={
            "authorization": "Bearer " + TOKEN,
            "content-type": "application/json",
            "x-vis-protocol": PROTOCOL[0],
        },
    )
    r = c.getresponse()
    return json.loads(r.read() or b"null")


PROTOCOL = ["0"]


def link_image():
    """Use the models and the bytecode cache of the image from the new home."""
    os.makedirs(PYCACHE, exist_ok=True)
    models = os.path.join(HOME, ".vis", "models")
    os.makedirs(models, exist_ok=True)
    for name in os.listdir(MODELS) if os.path.isdir(MODELS) else []:
        target = os.path.join(models, name)
        if not os.path.lexists(target):
            os.symlink(os.path.join(MODELS, name), target)


def restore(key):
    """Unpack checkpoint key, or seed a new home when no state exists yet."""
    shutil.rmtree(HOME, ignore_errors=True)
    os.makedirs(HOME + "/work", exist_ok=True)
    try:
        raw, _ = get(key or FIRST_STATE)
        if raw:
            with tarfile.open(fileobj=io.BytesIO(raw)) as t:
                t.extractall(HOME, filter="data")
            return log("restored state", key or FIRST_STATE, len(raw))
        if key:  # a new seed would replace the saved sessions
            raise RuntimeError(f"the checkpoint {key} is missing")
        seed()
    finally:
        link_image()


def seed():
    for page in s3.get_paginator("list_objects_v2").paginate(
        Bucket=BUCKET, Prefix="seed/"
    ):
        for o in page.get("Contents", []):
            target = os.path.join(HOME, o["Key"][len("seed/") :])
            os.makedirs(os.path.dirname(target), exist_ok=True)
            s3.download_file(BUCKET, o["Key"], target)
    log("seeded a new home")


def auth_changed(since):
    """A provider rotated its refresh token: save it before a lost worker can lose it."""
    return any(
        p.stat().st_mtime > since
        for p in pathlib.Path(HOME, ".vis").glob("*auth*.json")
    )


def saved_files():
    """Yield (path, name) of every file that the saved state holds."""
    for root, dirs, files in os.walk(HOME):
        dirs[:] = [d for d in dirs if os.path.join(root, d) not in UNSAVED]
        for f in files:
            path = os.path.join(root, f)
            if f.endswith(("-wal", "-shm", ".lock")) or not os.path.isfile(path):
                continue
            yield path, os.path.relpath(path, HOME)


def fingerprint():
    """Digest the name, size and time of each saved file and SQLite journal."""
    digest = hashlib.sha256()
    for root, dirs, files in os.walk(HOME):
        dirs[:] = [d for d in dirs if os.path.join(root, d) not in UNSAVED]
        for f in sorted(files):
            try:
                st = os.lstat(os.path.join(root, f))
            except OSError:
                continue
            digest.update(f"{root}/{f}\0{st.st_size}\0{st.st_mtime_ns}\n".encode())
    return digest.hexdigest()


def drop(key):
    """Delete an S3 object that no lease names."""
    try:
        s3.delete_object(Bucket=BUCKET, Key=key)
    except ClientError as e:
        log("could not delete", key, repr(e))


def save(held):
    """Upload a new checkpoint of the home folder, then point the lease at it.

    Each checkpoint has a new key, so an upload never replaces the saved state.
    The conditional lease write is the commit: it fails when another worker holds the lease.
    """
    out = "/tmp/home.tar.gz"
    with tarfile.open(out, "w:gz") as t:
        for path, name in saved_files():
            if path.endswith(".db"):
                copy = "/tmp/snapshot.db"
                src, dst = sqlite3.connect(path), sqlite3.connect(copy)
                src.backup(dst)
                dst.close()
                src.close()
                t.add(copy, name)
                os.remove(copy)
            else:
                t.add(path, name)
    held.saves += 1
    key = f"state/{held.id}/{held.saves:06d}.tar.gz"
    with open(out, "rb") as f:
        put(key, f.read(), IfNoneMatch="*")
    previous = held.data.get("state")
    try:
        held.write(expires=time.time() + LEASE_TTL, state=key)
    except ClientError:
        drop(key)  # the lease was lost, so this checkpoint never became the state
        raise
    if previous and previous != key:
        drop(previous)
    log("saved state", key, os.path.getsize(out))


def running_sessions():
    """Ids of the running sessions, from every page of the session list."""
    running, after = set(), None
    while True:
        query = "limit=200" + (f"&after={urllib.parse.quote(after)}" if after else "")
        page = gateway("GET", "/v1/sessions?" + query)
        rows = page.get("sessions", []) + page.get("awaiting", [])
        running |= {r["id"] for r in rows if r.get("status") == "running"}
        after = page.get("next_cursor")
        if not (page.get("has_more") and after):
            return sorted(running)


def drain():
    """Park the running turns, so that the next gateway continues them."""
    try:
        result = gateway(
            "POST",
            "/v1/admin/drain",
            {"timeout_ms": DRAIN_S * 1000, "grace_ms": 10_000},
            timeout=DRAIN_S + 30,
        )
    except (OSError, ValueError) as e:  # the running turns become interrupted
        log("drain failed", repr(e))
        return {}
    log("drained", json.dumps(result))
    return result if isinstance(result, dict) else {}


def next_due():
    """Earliest next automation run (epoch seconds), or None."""
    times = [
        a.get("next_run_at")
        for a in gateway("GET", "/v1/automations").get("automations", [])
        if a.get("enabled", True)
    ]
    times = [t / 1000 for t in times if isinstance(t, (int, float))]
    return min(times) if times else None


def delete_message(receipt, rid):
    try:
        sqs.delete_message(QueueUrl=QUEUE, ReceiptHandle=receipt)
    except ClientError as e:  # SQS gives the call again; accept() then deletes it
        log("could not delete relay message", rid, repr(e))


class Relay:
    def __init__(self):
        self.inflight, self.last = 0, time.time()
        # The newest SQS receipt of each relay id, and the ids that have an answer.
        self.receipts, self.answered, self.lock = {}, set(), threading.Lock()

    def accept(self, m):
        """Return the relay call of SQS message m, or None when this worker must not answer it.

        A message stays in SQS until its call has an answer. A worker that stops
        before it answers leaves the call to the next worker.
        """
        try:
            msg = json.loads(m["Body"])
            rid, answer_by = msg["id"], float(msg["answer_by"])
        except (ValueError, KeyError, TypeError) as e:
            log("dropped a malformed relay message", m.get("MessageId"), repr(e))
            delete_message(m["ReceiptHandle"], m.get("MessageId"))
            return None
        with self.lock:
            known, answered = rid in self.receipts, rid in self.answered
            self.receipts[rid] = m["ReceiptHandle"]
        if answered or answer_by <= time.time():
            if not answered:
                log("dropped a relay call that no client waits for", rid)
            self.done(rid)
            return None
        return None if known else msg  # a known call came again while it runs

    def done(self, rid):
        """The call has an answer: delete its message."""
        with self.lock:
            self.answered.add(rid)
            receipt = self.receipts[rid]
        delete_message(receipt, rid)

    def handle(self, m):
        self.inflight += 1
        try:
            self.answer(m)
        except Exception as e:  # noqa: BLE001 - every failure must answer the waiting client
            log("relay failed", m["path"], repr(e))
            put(
                f"relay/{m['id']}/0",
                b"E"
                + frame(
                    {"status": 502, "headers": []}, b'{"error":"worker relay failed"}'
                ),
            )
            self.done(m["id"])
        finally:
            self.inflight -= 1
            self.last = time.time()

    def answer(self, m):
        rid = m["id"]
        body = get(m["body_key"])[0] if "body_key" in m else base64.b64decode(m["body"])
        c = http.client.HTTPConnection(
            "127.0.0.1", PORT, timeout=max(1, m["deadline"] - time.time())
        )
        c.request(m["method"], m["path"], body=body or None, headers=m["headers"])
        r = c.getresponse()
        head = {
            "status": r.status,
            "headers": [(k, v) for k, v in r.getheaders() if k.lower() not in HOP],
        }
        if "text/event-stream" not in (r.getheader("content-type") or ""):
            put(f"relay/{rid}/0", b"E" + frame(head, r.read()))
            return self.done(rid)
        chunks = queue.Queue()

        def read():
            try:
                while data := r.read1(65536):
                    chunks.put(data)
            finally:
                chunks.put(None)

        threading.Thread(target=read, daemon=True).start()
        index, buf, done = 0, frame(head), False
        while not done:
            flushed = time.time()
            try:
                item = chunks.get(timeout=0.2)
                while item is not None:
                    buf += item
                    item = chunks.get_nowait() if not chunks.empty() else b""
                    if not item:
                        break
                done = item is None
            except queue.Empty:
                pass
            done = done or time.time() > m["deadline"] - 2
            if buf or done:
                put(f"relay/{rid}/{index}", (b"E" if done else b"M") + buf)
                if index == 0:
                    self.done(rid)
                index, buf, self.last = index + 1, b"", time.time()
                time.sleep(max(0.0, 0.15 - (time.time() - flushed)))
        c.close()


def start_gateway():
    with open("/tmp/token", "w") as f:
        f.write(TOKEN)
    gw = subprocess.Popen(
        [
            VIS + "/vis-agent-native",
            "-Duser.home=" + HOME,
            "gateway",
            "start",
            "--host",
            "127.0.0.1",
            "--port",
            str(PORT),
            "--token-file",
            "/tmp/token",
            "--require-token",
        ],
        cwd=HOME + "/work",
        env={
            **ENV,
            "HOME": HOME,
            "VIS_PYTHON_NATIVE_PATH": VIS + "/vis-agent-python/libvispython.so",
        },
    )
    for _ in range(600):
        try:
            PROTOCOL[0] = str(gateway("GET", "/healthz")["protocol"]["protocol"])
            return gw
        except OSError:
            time.sleep(0.1)
    raise RuntimeError("the gateway did not start")


def worker(raw):
    event, held = json.loads(raw or b"{}"), Lease()
    if event.get("kind") == "tick":
        data, _ = lease()
        due = data.get("next_due")
        if alive(data) or not (
            data.get("running")
            or data.get("parked")
            or (due and due - time.time() < 120)
        ):
            return b'{"worker":"nothing to do"}'
    started, previous = time.time(), held.acquire()
    if previous is None:
        return b'{"worker":"another worker holds the lease"}'
    log("lease acquired by", event.get("kind"))
    restore(previous.get("state"))
    # The gateway continues the parked turns and the queued turns of the last worker.
    gw = start_gateway()
    log("gateway ready", round(time.time() - started, 1))
    relay, pool = Relay(), ThreadPoolExecutor(64)
    renewed = saved = time.time()
    stored = None
    try:
        while True:
            for m in sqs.receive_message(
                QueueUrl=QUEUE, MaxNumberOfMessages=10, WaitTimeSeconds=1
            ).get("Messages", []):
                if msg := relay.accept(m):
                    relay.last = time.time()
                    pool.submit(relay.handle, msg)
            now = time.time()
            if now - renewed > 30:
                running = running_sessions()
                held.write(
                    expires=now + LEASE_TTL,
                    running=running,
                    parked=0,
                    next_due=next_due(),
                )
                renewed, relay.last = now, (now if running else relay.last)
            # Save a changed home within SAVE_S seconds, and a rotated token at once.
            if now - saved >= SAVE_S or auth_changed(saved):
                current = fingerprint()
                if current != stored or auth_changed(saved):
                    save(held)
                    stored = current
                saved = now
            due = held.data.get("next_due")
            handoff = now - started > LIFETIME
            if handoff or (
                relay.inflight == 0
                and now - relay.last > IDLE_S
                and not (due and due - now < 120)
            ):
                break
        running, due = running_sessions(), next_due()
    except ClientError as e:  # a lost lease: another worker owns the state now
        log("lease lost", repr(e))
        gw.kill()
        return b'{"worker":"lease lost"}'
    parked = drain().get("parked", 0)
    gw.terminate()
    gw.wait(30)
    pool.shutdown(wait=True, cancel_futures=True)
    try:
        save(held)
        held.write(expires=0, running=running, parked=parked, next_due=due)
    except ClientError as e:
        log("lease lost", repr(e))
        return b'{"worker":"lease lost"}'
    # A successor starts only for work: running or parked turns, recent requests or a due automation.
    handoff = handoff and bool(
        running
        or parked
        or time.time() - relay.last < IDLE_S
        or (due and due - time.time() < 120)
    )
    if handoff:
        lam.invoke(
            FunctionName=ENV["AWS_LAMBDA_FUNCTION_NAME"],
            InvocationType="Event",
            Payload=b'{"kind":"handoff"}',
        )
    log("worker done", "handoff" if handoff else "idle", running)
    return json.dumps(
        {"worker": "handoff" if handoff else "idle", "running": running}
    ).encode()


if __name__ == "__main__":
    http.server.ThreadingHTTPServer(("127.0.0.1", 8080), Front).serve_forever()
