JOB_ATTEMPTS_METRIC = "job_attempts_total"


class Metrics:
    """Minimal counter boundary used by the worker and replaced in production."""

    def __init__(self) -> None:
        self.counters: dict[str, int] = {}

    def increment(self, name: str) -> None:
        self.counters[name] = self.counters.get(name, 0) + 1
