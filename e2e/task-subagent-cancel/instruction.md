Start one subagent with `council.run`. Set `files` to `{"read_only": ["**"]}`. Give the subagent this exact task: “Ask the leader the question `task-cancel-ready` through `council.ask`. Wait for the reply. Do not change files.”

Set `watch = council.changed()`. Await `watch` once to get the open request. Check that the request names the child and that the child Task is not done. Read the Task record and check that its question and entry ID match the request. Cancel the child Task. Await that same Task and catch `asyncio.CancelledError`. If the question still needs a reply, reply that the Task was cancelled. Do not start another child.

Print one JSON object with keys `task_id`, `session_id`, `question`, `question_entry_id`, `ready`, `cancelled`, and `error`. Set `ready` to the result of the request checks. Set `error` to the name of the exception that you caught. Answer `subagent-task-ok` only if all checks pass.
