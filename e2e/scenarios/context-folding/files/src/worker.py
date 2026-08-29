from .dead_letter import DeadLetterSink
from .metrics import JOB_ATTEMPTS_METRIC, Metrics
from .store import JobStore

MAX_ATTEMPTS = 3


class Worker:
    def __init__(
        self, store: JobStore, dead_letters: DeadLetterSink, metrics: Metrics
    ) -> None:
        self.store = store
        self.dead_letters = dead_letters
        self.metrics = metrics

    def run(self, job_id: str) -> None:
        for attempt in range(MAX_ATTEMPTS):
            self.metrics.increment(JOB_ATTEMPTS_METRIC)
            try:
                self.store.complete(job_id, {"status": "ok"})
                return
            except RuntimeError as error:
                if attempt + 1 == MAX_ATTEMPTS:
                    self.dead_letters.write(job_id, str(error))
                    raise
