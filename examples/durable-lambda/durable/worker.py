"""The worker function: one long invocation that runs the Vis gateway of one shard.

The worker takes the lease of its shard, restores the last checkpoint from S3 and starts the
gateway in the project directory `work/main` on EFS. Then it answers the
calls of the shard until it is idle or near the Lambda time limit. At the end it
parks the running turns, saves a checkpoint and starts its successor when work remains.
The event names the shard. Without a name, it is `main` (see `shards.py`).

The worker has no route to the internet and holds no secret. Its only ways out are
S3 and three functions: llm, repo and itself.
"""

from __future__ import annotations

import time
from concurrent.futures import ThreadPoolExecutor, wait
from typing import Any

from durable import aws, core, shards
from durable.app import Api
from durable.domain import OPEN
from durable.helpers import log
from durable.lambda_runtime import Context
from durable.settings import CALLS, DRAIN_S, DUE_S, IDLE_S, LAST_SAVE_S, LIFETIME, MAIN, MARGIN, RENEW_S, RESTARTS, ROUTE_S, SAVE_S, SETTLE_S, WORKER_FUNCTION
from durable.vis import gateway, registry
from durable.vis.broker import Broker
from durable.vis.machine import Machine
from durable.vis.relay import Relay


def work(held: core.Lease, context: Context, is_sweep: bool = False) -> dict[str, Any]:
    """Run the gateway and answer the calls until the worker is idle or near its time limit.

    `is_sweep` is True when the schedule started the worker for a shard that had no turn and no call for BLANK_S.

    The steps: restore the state, start the broker and the gateway, loop over the
    calls, then park the turns, save the state and release the lease.
    """
    started, shard = time.time(), held.shard
    # The record of the last worker is true only until this worker changes a session.
    known = shards.reads(shard)
    shards.keep(shard, None, known)
    core.restore(held.data.get("state"))
    restored = time.time()
    root, release = registry.project(shard)
    cloned = time.time()
    # The API needs the address of the broker, and the broker serves the API.
    api = Api(root, release, lambda: gateway.environment(broker.url), shard)
    broker = Broker(api)
    broker.start()
    engine = gateway.Gateway(root, broker.url)
    engine.start()
    registry.register(engine, api)
    machine = Machine(engine, root, shard)
    # The worker of `main` writes the state that each worker shares, and each other worker copies it.
    machine.record() if shard == MAIN else machine.follow()
    inbox = shards.Inbox(shard)
    relay = Relay(engine, api, inbox, known, machine)
    pool = ThreadPoolExecutor(max_workers=CALLS)
    saved, renewed, checked, published = core.fingerprint(), 0.0, time.time(), 0.0
    # One thread at a time copies the changed work directories to S3, and one records the shard of the new sessions.
    copying = publishing = pool.submit(int)
    running, due, reason, worked = 0, None, "idle", time.time()
    ready = time.time()
    log(f"worker of {shard} ready in {ready - started:.1f} s: state {restored - started:.1f}, project {cloned - restored:.1f}, gateway {ready - cloned:.1f}, release {release[:12]}")
    try:
        while True:
            for message in inbox.receive():
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
                checked, current = now, core.fingerprint()
                if current != saved:
                    core.save(held)
                    saved = current
                if copying.done():
                    copying = pool.submit(api.workspaces.save)
                if shard == MAIN:
                    # The gateway also writes the machine store without a call.
                    pool.submit(machine.record, False)
            # A session of people needs no record: a session without a record is in `main`.
            if shard != MAIN and now - published > ROUTE_S and publishing.done():
                published, publishing = now, pool.submit(engine.publish, shard)
            if now - started > LIFETIME or context.remaining_s() < MARGIN:
                reason = "handoff"
                break
            # An idle worker stops, unless an automation is due soon. An event stream and a read that a tab repeats
            # do not keep a worker: the front holds the stream and answers the read (see `shards.py`).
            working = running or api.runs.live()
            worked = now if working else worked
            if not (working or relay.open()) and now - max(relay.last, worked) > IDLE_S and not (due and due - now < DUE_S):
                break
    except aws.ClientError as failure:
        # Another worker took the lease. This worker must not write state again.
        log("lease lost", repr(failure))
        api.runs.stop()
        engine.stop(kill=True)
        return {"worker": "lost the lease"}
    except Exception:
        # The worker fails, but its home has each turn that ended: the next worker starts from it.
        api.runs.stop()
        engine.stop(kill=True)
        pool.shutdown(wait=False, cancel_futures=True)
        broker.stop()
        core.save(held)
        raise
    api.runs.stop()
    # The worker takes no call after this point. Each call that it took gets its answer before the gateway drains.
    relay.calm(SETTLE_S)
    # The event streams stay open while the gateway drains: a client sees the end of a
    # turn that completes now. A later gateway does not send those events again.
    parked = engine.drain(max(5.0, min(DRAIN_S, context.remaining_s() - LAST_SAVE_S)))
    runs = api.runs.live()
    if shard != MAIN:
        # The list of the front shows the last state of each session while no worker runs.
        engine.publish(shard)
    is_blank = shard != MAIN and not (parked or runs or due or relay.open()) and engine.blank()
    # Nobody comes back to a session without a turn: it is in no list. A run that waits at a gate keeps its shard.
    is_stale = is_sweep and relay.last < ready and not any(line["status"] in OPEN for line in api.runs.mine())
    is_vacant = is_blank and (is_stale or engine.vacant())
    if not (parked or relay.open() or is_vacant):
        # Nothing changes the sessions after this point, so the front can answer the reads of an open tab.
        relay.record()
    engine.stop()
    relay.settle(SETTLE_S)
    pool.shutdown(wait=False, cancel_futures=True)
    broker.stop()
    # The gateway and the runs stopped, so each work directory has its last files.
    api.workspaces.save(is_last=True)
    if is_vacant:
        # No session of the shard stays, so nothing of the shard stays in the bucket. No thread writes after this point.
        wait([publishing, copying], timeout=SETTLE_S)
        shards.drop(shard, engine.mine(), held.key, held.data.get("state"))
        log("worker of", shard, "stops: the shard has no session and is gone")
        return {"worker": "gone"}
    if shard != MAIN:
        shards.blank(shard, is_blank)
    core.save(held)
    held.release(running=0, parked=parked, runs=runs, next_due=due)
    log("worker of", shard, "stops:", reason, "parked", parked, "runs", runs)
    if shard != MAIN and not (parked or runs or due):
        # The front starts the worker again for a call. The schedule has nothing to look at.
        shards.unmark(shard)
    if reason == "handoff" and (parked or runs):
        aws.start(WORKER_FUNCTION, kind="handoff", shard=shard)
    return {"worker": reason, "parked": parked, "runs": runs}


def handler(event: dict[str, Any], context: Context) -> dict[str, Any]:
    """Work as the one live worker of a shard, or return at once when another worker of the shard lives."""
    shard = event.get("shard") or MAIN
    if not shards.valid(shard):
        return {"worker": "the shard is not valid"}
    held = core.Lease.acquire(shard)
    if held is None:
        return {"worker": "another worker holds the lease"}
    try:
        if shard != MAIN:
            # The schedule starts a worker again when this one stops without a release.
            shards.mark(shard)
        return work(held, context, event.get("kind") == "sweep")
    except Exception:
        # Give the lease up, so the next call starts a worker at once. Work can wait, so the first failures start the next worker here.
        failures = int(event.get("failures") or 0)
        try:
            held.release()
            if failures < RESTARTS:
                aws.start(WORKER_FUNCTION, kind="failure", shard=shard, failures=failures + 1)
        except aws.ClientError:
            pass
        raise
