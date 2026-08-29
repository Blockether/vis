from src.config import DEFAULT_JOB_DATABASE, JOB_DATABASE_ENV
from src.dead_letter import DeadLetterSink
from src.dispatcher import Dispatcher
from src.idempotency import IdempotencyIndex
from src.intake import submit_job
from src.metrics import JOB_ATTEMPTS_METRIC
from src.store import JobStore
from src.validation import validate_payload
from src.worker import MAX_ATTEMPTS


def test_lifecycle_contract() -> None:
    assert submit_job
    assert validate_payload
    assert IdempotencyIndex
    assert JobStore
    assert Dispatcher
    assert MAX_ATTEMPTS == 3
    assert DeadLetterSink
    assert JOB_ATTEMPTS_METRIC == "job_attempts_total"
    assert JOB_DATABASE_ENV == "VIS_JOB_DB"
    assert DEFAULT_JOB_DATABASE == "jobs.sqlite3"
