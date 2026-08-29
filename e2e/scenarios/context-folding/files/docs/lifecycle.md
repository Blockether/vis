# Job lifecycle

The synchronous boundary is `submit_job(payload, queue)` in `src/intake.py`. It delegates to
`JobQueue.enqueue`; the queue first calls `validate_payload`, then asks `IdempotencyIndex` whether
the caller's `request_key` already owns a job. Existing keys return their original job id.

For a new key, `JobStore.create` is the durable SQLite boundary. Only after that succeeds does the
queue remember the key and call `Dispatcher.dispatch`. The dispatcher publishes only an opaque job
id, never the caller's payload, so workers reload canonical state from the store.

`Worker.run` owns execution attempts. It increments `job_attempts_total` for every try and stops
after `MAX_ATTEMPTS`. Successful work calls `JobStore.complete`; an exhausted `RuntimeError` is
written to `DeadLetterSink` before the error is re-raised. Transport leases in `policy.py` are not
retries and must not be added to the attempt count.

Configuration stays at the composition boundary. `job_database_path()` reads `VIS_JOB_DB` and
falls back to `jobs.sqlite3`; callers inject that path into `JobStore`. Tests pin the identifiers
that operators and dashboards depend on.
