# Operations and test evidence

Operators correlate one job id across intake, persistence, dispatch, worker attempts, completion, and
dead-letter handling. The stable dashboard counter is `job_attempts_total`. Terminal failure records
carry the job id and reason. Database configuration is visible as `VIS_JOB_DB`, with
`jobs.sqlite3` as the local default.

`tests/test_lifecycle.py` pins the public entry point `submit_job`, validator `validate_payload`,
`IdempotencyIndex`, `JobStore`, `Dispatcher`, `MAX_ATTEMPTS`, `DeadLetterSink`, the attempt metric,
and both database configuration strings. These identifiers are the compatibility surface expected by
the audit.

`tests/test_failure_contract.py` separately pins three attempts, the metric value, the dead-letter
boundary, the 30-second lease, and seven-day retention. The tests are read as contract evidence in
this scenario; the audit does not execute them or modify the fixture.

The complete lifecycle is: submit, validate, deduplicate, persist, remember identity, dispatch the
opaque id, execute with a counter and bounded retries, then complete or dead-letter. Lease expiry and
retention remain operational policies outside retry accounting.
