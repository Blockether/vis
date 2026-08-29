LEASE_SECONDS = 30
RESULT_RETENTION_DAYS = 7


def lease_deadline(started_at: float) -> float:
    """Keep transport leasing separate from the worker retry count."""
    return started_at + LEASE_SECONDS
