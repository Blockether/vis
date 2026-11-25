Test how you handle a subagent that its provider rate-limits. Use `[child] = await council.run([...])` with exactly one task dict. Set provider "busy", model "busy-1" and this task text, unchanged:

Write the file reroute.txt in the workspace root with three lines: reroute-ok, then the value of session["agent"]["role"], then the value of session["routing"]["provider"]. Then answer written.

Await `child` for its final record. If `error` reports `rate_limited`, choose a provider and model from `alternatives`. Start the same task once on that route as a new child, then await its final record. After each await, print one line that starts with `child-attempt `, then a JSON object with `task_id` from the handle and `id`, `session_id`, `status`, `provider`, `model`, `error`, `alternatives`, and `answer` from the awaited record.

Do not write reroute.txt yourself or change other files. When the replacement succeeds and reroute.txt exists, finish with reroute-done and the `provider/model` pair from the completed record.
