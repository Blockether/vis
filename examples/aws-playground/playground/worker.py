"""The worker function: one long invocation that runs the Vis gateway.

The worker takes the lease, restores the last checkpoint from S3 and starts the
gateway in the project directory of the current release on EFS. Then it answers the
calls from the queue until it is idle or near the Lambda time limit. At the end it
parks the running turns, saves a checkpoint and starts its successor when work remains.

The worker has no route to the internet and holds no secret. Its only ways out are
S3, the queue and three functions: llm, repo and itself.
"""

from __future__ import annotations

import time
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from blockether.vis.engine import GatewayClient

from playground import aws, gateway, state, sync
from playground.api import Api
from playground.broker import Broker
from playground.env import (
    DUE_S,
    IDLE_S,
    LIFETIME,
    MARGIN,
    QUEUE,
    REPO_FUNCTION,
    WORKER_FUNCTION,
    log,
)
from playground.lease import Lease
from playground.relay import Relay
from playground.runtime import Context

# The worker renews the lease each RENEW_S and looks for state to save each SAVE_S.
RENEW_S, SAVE_S = 30, 15


def project() -> tuple[str, str]:
    """Return the project directory of the current release and its commit.

    The directory is a clone of the release, for the sessions that a person starts.
    A session of an older release keeps its own directory.
    """
    release = sync.current()
    if not release:
        # The first worker of a new stack asks for the first release.
        aws.invoke(REPO_FUNCTION, action="fetch", force=True)
        release = sync.current()
    if not release:
        raise RuntimeError("EFS has no release of the repository")
    sync.sweep(release[:12])
    return sync.clone(release, release[:12], gateway.environment("")), release


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
    api.cancel_turn = engine.cancel
    # Each call gets its own SDK client: one client serves one thread.
    api.session = lambda session: GatewayClient(f"http://{gateway.HOST}:{gateway.PORT}", token=engine.token).session(session)
    relay = Relay(engine, api)
    pool = ThreadPoolExecutor(max_workers=64)
    queue = aws.client("sqs")
    saved, renewed, checked = state.fingerprint(), 0.0, time.time()
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
