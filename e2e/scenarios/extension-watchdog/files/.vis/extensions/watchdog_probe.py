"""Side-effect-free waits for issue #187's sandbox/extension boundary tests."""

import time
from dataclasses import dataclass

import blockether.vis.extension as vis


@dataclass(frozen=True)
class Observation:
    count: int
    timed_out: bool = False


class WatchdogProbe:
    """A local observer: no network, shell, live view or remote mutation."""

    def __init__(self):
        self._count = 0

    def poll(self, duration: float = 310) -> Observation:
        """Wait silently for duration seconds and return a numbered observation."""
        time.sleep(duration)
        self._count += 1
        return Observation(self._count)

    def fail(self, duration: float) -> Observation:
        """Wait, then raise an ordinary extension error."""
        time.sleep(duration)
        raise RuntimeError("extension observation failed")

    def deadline(self, duration: float) -> Observation:
        """Return an extension-owned timeout without failing Python execution."""
        time.sleep(duration)
        return Observation(self._count, timed_out=True)


vis.register(
    vis.Extension(
        name="watchdog-probe",
        description="Local extension-lifetime regression probe.",
        alias="watchdog_probe",
        symbols=[vis.Symbol(WatchdogProbe(), name="watchdog_probe")],
    )
)
