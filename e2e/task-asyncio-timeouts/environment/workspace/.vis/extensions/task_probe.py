"""Local Task operations with a protected record of their execution."""

import asyncio
import json
import os
import threading
import time
from pathlib import Path

import blockether.vis.extension as vis

LOG = Path(__file__).resolve().parents[1] / "calls" / "tasks.jsonl"


def record(event, label):
    with LOG.open("a", encoding="utf-8") as output:
        output.write(json.dumps({"event": event, "label": label}) + "\n")
        output.flush()
        os.fsync(output.fileno())


class TaskProbe:
    """Hold an operation until cancellation or release."""

    def __init__(self):
        self._lock = threading.Lock()
        self._holds = {}

    async def hold(self, label: str) -> str:
        """Start a hold for this label. Return 'released' when it is released."""
        gate = threading.Event()
        completed = False
        with self._lock:
            if label in self._holds:
                raise ValueError(f"Duplicate hold: {label}")
            record("started", label)
            self._holds[label] = gate
        try:
            end = time.monotonic() + 45
            while not gate.is_set():
                if time.monotonic() >= end:
                    record("expired", label)
                    raise TimeoutError(f"Hold expired: {label}")
                await asyncio.sleep(0.02)
            record("completed", label)
            completed = True
            return "released"
        except asyncio.CancelledError:
            record("cancelled", label)
            completed = True
            raise
        finally:
            # The host can also unwind the interpreter without a CancelledError.
            if not completed and time.monotonic() < end:
                record("cancelled", label)

    async def started(self, label: str) -> bool:
        """Wait up to three seconds for the hold to start."""
        end = time.monotonic() + 3
        while time.monotonic() < end:
            with self._lock:
                if label in self._holds:
                    record("observed", label)
                    return True
            await asyncio.sleep(0.02)
        return False

    def release(self, label: str) -> bool:
        """Release a hold that started."""
        with self._lock:
            gate = self._holds[label]
            record("released", label)
            gate.set()
        return True


vis.register_extension(
    vis.Extension(
        name="task-probe",
        description="Local Task lifecycle probe.",
        alias="task_probe",
        symbols=[vis.Symbol(TaskProbe(), name="task_probe")],
    )
)
