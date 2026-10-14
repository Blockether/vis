# Failure modes and ownership

Validation failures happen before persistence and are returned to the caller. An idempotency
collision is rejected rather than replacing the earlier mapping. A store failure prevents dispatch,
which avoids a worker receiving a job it cannot reload. A publish failure leaves a durable job that
an outbox scanner may dispatch later; it does not create another id.

Worker failures are bounded by `MAX_ATTEMPTS = 3`. Every attempted execution increments
`job_attempts_total`, including the final failed attempt. The third failure writes the job id and
reason through `DeadLetterSink`, then propagates the error for transport acknowledgement policy.
Lease expiry can redeliver the same durable id, but the idempotency index and result state prevent a
second logical job.

Database location is the only deployment setting in this fixture: environment key `VIS_JOB_DB`,
default `jobs.sqlite3`. The architecture intentionally has no hidden fallback path and no unbounded
retry loop.
