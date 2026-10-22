"""Run the Vis gateway durably on AWS Lambda.

Two functions run this file behind the Lambda Web Adapter:

- front (VIS_ROLE=front) answers the Function URL. It serves the web app,
  checks the bearer token and relays API calls to the live worker: the
  request goes to SQS, the response comes back as S3 objects.
- worker (VIS_ROLE=worker) holds a lease in S3, restores ~/.vis from S3,
  runs the native gateway and answers relayed calls. Before the Lambda
  time limit it saves the state, starts its successor and gives the lease
  away. The successor continues the turns that were still running.

S3 is the database of record: state/home.tar.gz holds the home folder with
a consistent SQLite copy, lease.json names the one live worker.
"""

import base64
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
LIFETIME = int(ENV.get("VIS_LIFETIME_S", "900")) - int(
    ENV.get("VIS_HANDOFF_MARGIN_S", "90")
)
IDLE_S = int(ENV.get("VIS_IDLE_S", "600"))
# A relayed event stream ends after STREAM_S; the client reconnects from its cursor.
# Gateway heartbeats (15 s) keep the CloudFront 60 s origin read timeout open.
STREAM_S = int(ENV.get("VIS_STREAM_S", "600"))
HOME, WEB, VIS = "/tmp/home", "/opt/vis-web", "/opt/vis"
LEASE, STATE, LEASE_TTL = "lease.json", "state/home.tar.gz", 90
# Logs and download caches stay out of the saved state; tools fetch them again.
UNSAVED = {HOME + p for p in ("/.vis/logs", "/.m2", "/.gitlibs", "/.cache")}
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
                if (
                    index == 0 and time.time() - started > 55
                ):  # under the CDN's 60 s read timeout
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
        self.id, self.etag, self.data = str(uuid.uuid4()), None, {}

    def write(self, **data):
        self.data = {**self.data, **data, "owner": self.id}
        cond = {"IfMatch": self.etag} if self.etag else {"IfNoneMatch": "*"}
        self.etag = put(LEASE, json.dumps(self.data).encode(), **cond)

    def acquire(self):
        data, self.etag = lease()
        if alive(data):
            return None
        self.write(expires=time.time() + LEASE_TTL)
        return data


def gateway(method, path, body=None):
    c = http.client.HTTPConnection("127.0.0.1", PORT, timeout=30)
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


def restore():
    shutil.rmtree(HOME, ignore_errors=True)
    os.makedirs(HOME + "/work", exist_ok=True)
    raw, _ = get(STATE)
    if raw:
        with tarfile.open(fileobj=io.BytesIO(raw)) as t:
            t.extractall(HOME, filter="data")
        return log("restored state", len(raw))
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


def save(lease_obj):
    """Upload the home folder with a consistent copy of every SQLite file."""
    lease_obj.write(
        expires=time.time() + LEASE_TTL
    )  # fence: fails when the lease was lost
    out = "/tmp/home.tar.gz"
    with tarfile.open(out, "w:gz") as t:
        for root, dirs, files in os.walk(HOME):
            dirs[:] = [d for d in dirs if os.path.join(root, d) not in UNSAVED]
            for f in files:
                path = os.path.join(root, f)
                name = os.path.relpath(path, HOME)
                if f.endswith(("-wal", "-shm", ".lock")) or not os.path.isfile(path):
                    continue
                if f.endswith(".db"):
                    copy = "/tmp/snapshot.db"
                    src, dst = sqlite3.connect(path), sqlite3.connect(copy)
                    src.backup(dst)
                    dst.close()
                    src.close()
                    t.add(copy, name)
                    os.remove(copy)
                else:
                    t.add(path, name)
    with open(out, "rb") as f:
        put(STATE, f.read())
    log("saved state", os.path.getsize(out))


def running_sessions():
    rows = gateway("GET", "/v1/sessions?limit=500").get("sessions", [])
    return [r["id"] for r in rows if r.get("status") == "running"]


def next_due():
    """Earliest next automation run (epoch seconds), or None."""
    times = [
        a.get("next_run_at")
        for a in gateway("GET", "/v1/automations").get("automations", [])
        if a.get("enabled", True)
    ]
    times = [t / 1000 for t in times if isinstance(t, (int, float))]
    return min(times) if times else None


class Relay:
    def __init__(self):
        self.inflight, self.last = 0, time.time()

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
            return put(f"relay/{rid}/0", b"E" + frame(head, r.read()))
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


def resume(sessions, held):
    for sid in sessions:
        gateway(
            "POST",
            f"/v1/sessions/{sid}/turns",
            {
                "request": "Vis moved this session to a new worker, which stopped your last turn. "
                "Continue the task from where it stopped. Check the work that is done before you repeat a step.",
                "idempotency_key": f"handoff-{held.id}-{sid}",
            },
        )
        log("resumed session", sid)


def worker(raw):
    event, held = json.loads(raw or b"{}"), Lease()
    if event.get("kind") == "tick":
        data, _ = lease()
        due = data.get("next_due")
        if alive(data) or not (
            data.get("running") or (due and due - time.time() < 120)
        ):
            return b'{"worker":"nothing to do"}'
    started, previous = time.time(), held.acquire()
    if previous is None:
        return b'{"worker":"another worker holds the lease"}'
    log("lease acquired by", event.get("kind"))
    restore()
    gw = start_gateway()
    log("gateway ready", round(time.time() - started, 1))
    resume(previous.get("running", []), held)
    relay, pool = Relay(), ThreadPoolExecutor(64)
    renewed = saved = time.time()
    try:
        while True:
            for m in sqs.receive_message(
                QueueUrl=QUEUE, MaxNumberOfMessages=10, WaitTimeSeconds=1
            ).get("Messages", []):
                sqs.delete_message(QueueUrl=QUEUE, ReceiptHandle=m["ReceiptHandle"])
                msg = json.loads(m["Body"])
                if msg["deadline"] > time.time():
                    relay.last = time.time()
                    pool.submit(relay.handle, msg)
            now = time.time()
            if now - renewed > 30:
                running = running_sessions()
                held.write(
                    expires=now + LEASE_TTL, running=running, next_due=next_due()
                )
                renewed, relay.last = now, (now if running else relay.last)
            if now - saved > 60 or auth_changed(saved):
                save(held)
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
    gw.terminate()
    gw.wait(30)
    pool.shutdown(wait=True, cancel_futures=True)
    save(held)
    held.write(expires=0, running=running, next_due=due)
    # A successor starts only for work: running turns, recent requests or a due automation.
    handoff = handoff and bool(
        running
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
