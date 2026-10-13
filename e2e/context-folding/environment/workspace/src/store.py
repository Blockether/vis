class JobStore:
    """SQLite persistence boundary for queued jobs and their results."""

    def __init__(self, database_path: str) -> None:
        self.database_path = database_path

    def create(self, payload: dict) -> str:
        return "job-001"

    def complete(self, job_id: str, result: dict) -> None:
        pass
