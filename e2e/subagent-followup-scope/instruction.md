Start one subagent with `[child] = await council.run([...])`. Supply one task dictionary. Set `files` to `{"read_only": ["**"]}`. Give the child this exact task:

Read notes.txt in the workspace root and answer with its only line.

Await the `child` Task. Check that its `SubagentRecord` has status `completed` and answer `scope-ok-7431`. Start a follow-up with `[follow] = await council.run([...])`. Set `"child": child` and `"files": {"write": ["audit.txt"]}` in the task dictionary. Give the child this exact task:

Write the file audit.txt in the workspace root with two lines: the line from notes.txt, then the value of session["agent"]["role"]. Then answer written.

Await the `follow` Task. Check that its `SubagentRecord` has status `completed` and answer `written`. Check that the two Tasks have the same `id` and `session_id` but different `task_id` values. After each Task ends, print `child-attempt ` followed by one JSON object on the same line. Read `task_id` from the Task and `id`, `session_id`, `status` and `answer` from its record.

Do not write `audit.txt` yourself, start a second child, call `council.grant`, or change other files. When `audit.txt` exists, answer `followup-done`.
