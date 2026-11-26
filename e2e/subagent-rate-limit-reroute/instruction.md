Start one subagent with `[child] = await council.run([...])`. Supply one task dictionary. Set the provider to `busy` and the model to `busy-1`. Give the child this exact task:

Write the file reroute.txt in the workspace root with three lines: reroute-ok, then the value of session["agent"]["role"], then the value of session["routing"]["provider"]. Then answer written.

Await the `child` Task. Read `error` and `alternatives` from its `SubagentRecord`. If the error code is `rate_limited`, choose a provider and model from `alternatives`. Start the same task once on that route as a new child. Await the new Task.

After each Task ends, print `child-attempt ` followed by one JSON object on the same line. Read `task_id` from the Task and `id`, `session_id`, `status`, `provider`, `model`, `error`, `alternatives` and `answer` from its record.

Do not write `reroute.txt` yourself or change other files. When the new child completes the task and `reroute.txt` exists, answer with `reroute-done` and the `provider/model` pair from the new record.
