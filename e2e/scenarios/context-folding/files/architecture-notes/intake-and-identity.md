# Intake and identity review

The public synchronous boundary is `submit_job(payload, queue)` in `src/intake.py`. It accepts a
caller payload and delegates immediately to `JobQueue.enqueue`; it does not validate, persist, or
publish independently. This keeps one ordering owner in the queue.

`validate_payload` in `src/validation.py` runs first. It requires `request_key`, `operation`, and
`arguments`, rejects a blank request key, and returns a copy with surrounding request-key whitespace
removed. A validation failure therefore happens before any durable mutation or transport call.

`IdempotencyIndex` maps the normalized request key to one durable job id. `find` lets a replay return
the original id without a second store write or dispatch. `remember` rejects an attempt to map the
same request key to a different id. The index is an in-memory fixture here, but its semantic boundary
is the identity decision, not persistence of job payloads.

The audited intake sequence is validate, look up the normalized key, allocate a durable job only when
absent, remember the key-to-id mapping, then dispatch that id. The caller receives the existing or new
job id. No path in the fixture allocates a replacement id for a duplicate request.
