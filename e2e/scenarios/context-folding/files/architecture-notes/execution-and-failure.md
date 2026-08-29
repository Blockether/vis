# Execution and failure review

`Worker.run(job_id)` owns execution attempts. `MAX_ATTEMPTS = 3` is the bounded retry constant. Each
loop iteration increments `JOB_ATTEMPTS_METRIC`, whose stable value is `job_attempts_total`, before
attempting completion. The final failed attempt is therefore included in the counter.

A successful attempt calls `JobStore.complete(job_id, {"status": "ok"})` and returns. The worker
retries only `RuntimeError`. On the third failure it calls `DeadLetterSink.write(job_id, reason)` and
then re-raises, preserving both an operator-visible terminal record and transport failure semantics.

`DeadLetterSink` in `src/dead_letter.py` is the named exhausted-job boundary. Its in-memory `entries`
list is fixture storage, not a second job repository. `Metrics` similarly stands in for the production
counter backend while preserving the public metric name.

Transport policy is separate. `LEASE_SECONDS = 30` may cause redelivery and
`RESULT_RETENTION_DAYS = 7` controls result cleanup; neither changes the execution attempt count.
Duplicate delivery retains the same durable job id and relies on stored state rather than allocating a
new request.
