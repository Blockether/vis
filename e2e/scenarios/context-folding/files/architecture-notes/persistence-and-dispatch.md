# Persistence and dispatch review

`JobStore` in `src/store.py` is the SQLite persistence boundary. Its constructor receives the chosen
database path; `create(payload)` owns the durable job record and returns a job id, while
`complete(job_id, result)` owns the terminal result write. The fixture methods are intentionally
skeletal, but no other class claims job-state persistence.

`JobQueue.enqueue` calls `JobStore.create` before recording idempotency and before dispatch. A store
exception therefore leaves nothing publishable. After a successful create, the queue records the
request key and invokes `Dispatcher.dispatch(job_id)`.

`Dispatcher` in `src/dispatcher.py` wraps an injected publish callback. It publishes only the opaque
job id, never the caller payload. Workers must reload canonical state through the store. A publish
failure leaves a durable job for a future outbox scan rather than creating a second logical job.

The database path comes from `job_database_path()` in `src/config.py`. It reads environment key
`VIS_JOB_DB` through `JOB_DATABASE_ENV` and otherwise returns `jobs.sqlite3` through
`DEFAULT_JOB_DATABASE`. There is one configuration key, one default, and no hidden fallback path.
