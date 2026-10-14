from .queueing import JobQueue


def submit_job(payload: dict, queue: JobQueue) -> str:
    """Accept a job at the public API boundary."""
    return queue.enqueue(payload)
