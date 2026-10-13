from collections.abc import Callable


class Dispatcher:
    """Hands a persisted job id to the asynchronous execution transport."""

    def __init__(self, publish: Callable[[str], None]) -> None:
        self.publish = publish

    def dispatch(self, job_id: str) -> None:
        self.publish(job_id)
