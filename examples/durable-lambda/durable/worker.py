"""The worker function: one long invocation that runs the Vis gateway.

The worker takes the lease, restores the last checkpoint from S3 and starts the
gateway in the project directory `work/main` on EFS. Then it answers the
calls from the queue until it is idle or near the Lambda time limit. At the end it
parks the running turns, saves a checkpoint and starts its successor when work remains.

The worker has no route to the internet and holds no secret. Its only ways out are
S3, the queue and three functions: llm, repo and itself.
"""

from __future__ import annotations

import hashlib
import os
import time
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from blockether.vis.engine import GatewayClient

from durable import aws, gateway, state, sync
from durable.api import Api
from durable.apps import Workflows
from durable.broker import Broker
from durable.env import (
    DUE_S,
    ENV,
    HOME,
    IDLE_S,
    LIFETIME,
    MARGIN,
    QUEUE,
    REPO_FUNCTION,
    WORKER_FUNCTION,
    log,
)
from durable.lease import Lease
from durable.relay import Relay
from durable.runtime import Context

# The worker renews the lease each RENEW_S and looks for state to save each SAVE_S.
RENEW_S, SAVE_S = 30, 15


def keep_review(old: str, new: str) -> None:
    """Give the review journals of a directory to its new name.

    Vis Review files the journals of a session below a hash of the project path.
    """
    base = os.path.join(HOME, ".vis", "review")
    before, after = (os.path.join(base, hashlib.sha256(root.encode()).hexdigest()[:16]) for root in (old, new))
    if os.path.isdir(before) and not os.path.exists(after):
        os.rename(before, after)
        with open(os.path.join(after, "ROOT"), "w", encoding="utf-8") as file:
            file.write(new + "\n")


def project() -> tuple[str, str]:
    """Return the project directory and the commit of the current release.

    `work/main` is the directory of the one project of Vis. It takes each new release,
    so the project stays when the repository changes. Each session works in its own
    clone next to it: see `durable/workspaces.py`.
    """
    release = sync.current()
    if not release:
        # The first worker of a new stack asks for the first release.
        aws.invoke(REPO_FUNCTION, action="fetch", force=True)
        release = sync.current()
    if not release:
        raise RuntimeError("EFS has no release of the repository")
    sync.sweep(sync.MAIN)
    main, moved = f"{sync.WORK}/{sync.MAIN}", f"{sync.WORK}/{sync.MAIN}/.git/durable-earlier"
    earlier = "" if os.path.isdir(main) else sync.earlier()
    if earlier:
        # The directory keeps its work. The note is for a worker that stops before it saves the journals.
        os.rename(earlier, main)
        with open(moved, "w", encoding="utf-8") as file:
            file.write(earlier)
    if os.path.isfile(moved):
        with open(moved, encoding="utf-8") as file:
            keep_review(file.read().strip(), main)
    return sync.advance(release, gateway.environment("")), release


def work(held: Lease, context: Context) -> dict[str, Any]:
    started = time.time()
    state.restore(held.data.get("state"))
    restored = time.time()
    root, release = project()
    cloned = time.time()
    # The API needs the address of the broker, and the broker serves the API.
    api = Api(root, release, lambda: gateway.environment(broker.url))
    broker = Broker(api)
    broker.start()
    engine = gateway.Gateway(root, broker.url)
    engine.start()
    api.engine, api.runs.busy = engine, engine.busy
    api.runs.group = engine.organize(root, ENV.get("VIS_REPO", "").rpartition("/")[2] or "durable")
    api.people = engine.group(root, gateway.PEOPLE)
    # Each call gets its own SDK client: one client serves one thread.
    client = lambda: GatewayClient(f"http://{gateway.HOST}:{gateway.PORT}", token=engine.token)  # noqa: E731
    api.session = lambda session: client().session(session)
    Workflows(api, client()).start()
    relay = Relay(engine, api)
    pool = ThreadPoolExecutor(max_workers=64)
    queue = aws.client("sqs")
    saved, renewed, checked = state.fingerprint(), 0.0, time.time()
    # One thread at a time copies the changed work directories to S3.
    copying = pool.submit(int)
    running, due, reason = 0, None, "idle"
    ready = time.time()
    log(f"worker ready in {ready - started:.1f} s: state {restored - started:.1f}, project {cloned - restored:.1f}, gateway {ready - cloned:.1f}, release {release[:12]}")
    try:
        while True:
            answer = queue.receive_message(QueueUrl=QUEUE, MaxNumberOfMessages=10, WaitTimeSeconds=1)
            for message in answer.get("Messages", []):
                call = relay.accept(message)
                if call:
                    pool.submit(relay.handle, call)
            api.runs.fill()
            now = time.time()
            if engine.process is None or engine.process.poll() is not None:
                raise RuntimeError("the gateway stopped")
            if now - renewed > RENEW_S:
                running, due, renewed = engine.running(), engine.next_due(), now
                held.renew(running=running, parked=0, runs=api.runs.live(), next_due=due)
            if now - checked > SAVE_S:
                checked, current = now, state.fingerprint()
                if current != saved:
                    state.save(held)
                    saved = current
                if copying.done():
                    copying = pool.submit(api.workspaces.save)
            if now - started > LIFETIME or context.remaining_s() < MARGIN:
                reason = "handoff"
                break
            busy = relay.inflight or running or api.runs.live()
            if not busy and now - relay.last > IDLE_S and not (due and due - now < DUE_S):
                break
    except aws.ClientError as failure:
        # Another worker took the lease. This worker must not write state again.
        log("lease lost", repr(failure))
        api.runs.stop()
        engine.stop(kill=True)
        return {"worker": "lost the lease"}
    api.runs.stop()
    # The event streams stay open while the gateway drains: a client sees the end of a
    # turn that completes now. A later gateway does not send those events again.
    parked = engine.drain(max(5.0, min(45.0, context.remaining_s() - 40)))
    engine.stop()
    relay.settle(8.0)
    pool.shutdown(wait=False, cancel_futures=True)
    broker.stop()
    # The gateway and the runs stopped, so each work directory has its last files.
    api.workspaces.save(is_last=True)
    state.save(held)
    runs = api.runs.live()
    held.release(running=0, parked=parked, runs=runs, next_due=due)
    log("worker stops:", reason, "parked", parked, "runs", runs)
    if reason == "handoff" and (parked or runs):
        aws.start(WORKER_FUNCTION, kind="handoff")
    return {"worker": reason, "parked": parked, "runs": runs}


def handler(event: dict[str, Any], context: Context) -> dict[str, Any]:
    """Work as the one live worker, or return at once when another worker lives."""
    held = Lease.acquire()
    if held is None:
        return {"worker": "another worker holds the lease"}
    try:
        return work(held, context)
    except Exception:
        # Give the lease up, so the next call starts a worker at once.
        try:
            held.release()
        except aws.ClientError:
            pass
        raise
