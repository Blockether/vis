from dataclasses import dataclass


@dataclass(frozen=True)
class JobEnvelope:
    job_id: str
    operation: str
    arguments: dict


@dataclass(frozen=True)
class JobResult:
    job_id: str
    status: str
    value: dict | None
