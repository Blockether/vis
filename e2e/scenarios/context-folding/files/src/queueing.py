from .dispatcher import Dispatcher
from .idempotency import IdempotencyIndex
from .store import JobStore
from .validation import validate_payload


class JobQueue:
    def __init__(
        self, store: JobStore, idempotency: IdempotencyIndex, dispatcher: Dispatcher
    ) -> None:
        self.store = store
        self.idempotency = idempotency
        self.dispatcher = dispatcher

    def enqueue(self, payload: dict) -> str:
        validated = validate_payload(payload)
        request_key = validated["request_key"]
        existing = self.idempotency.find(request_key)
        if existing:
            return existing
        job_id = self.store.create(validated)
        self.idempotency.remember(request_key, job_id)
        self.dispatcher.dispatch(job_id)
        return job_id
