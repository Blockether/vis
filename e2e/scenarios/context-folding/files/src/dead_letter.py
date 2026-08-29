class DeadLetterSink:
    """Records jobs that exhausted the worker's bounded retry policy."""

    def __init__(self) -> None:
        self.entries: list[tuple[str, str]] = []

    def write(self, job_id: str, reason: str) -> None:
        self.entries.append((job_id, reason))
