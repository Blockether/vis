class IdempotencyIndex:
    """Maps a caller's stable request key to the one durable job id."""

    def __init__(self) -> None:
        self._jobs: dict[str, str] = {}

    def find(self, request_key: str) -> str | None:
        return self._jobs.get(request_key)

    def remember(self, request_key: str, job_id: str) -> None:
        if request_key in self._jobs and self._jobs[request_key] != job_id:
            raise RuntimeError("request key already belongs to another job")
        self._jobs[request_key] = job_id
