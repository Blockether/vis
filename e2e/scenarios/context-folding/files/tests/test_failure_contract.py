from src.dead_letter import DeadLetterSink
from src.metrics import JOB_ATTEMPTS_METRIC
from src.policy import LEASE_SECONDS, RESULT_RETENTION_DAYS
from src.worker import MAX_ATTEMPTS


def test_failure_contract() -> None:
    assert MAX_ATTEMPTS == 3
    assert JOB_ATTEMPTS_METRIC == "job_attempts_total"
    assert DeadLetterSink
    assert LEASE_SECONDS == 30
    assert RESULT_RETENTION_DAYS == 7
