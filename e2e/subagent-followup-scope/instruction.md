Test how you give one subagent a new file scope for a follow-up task. Use `[child] = await council.run([...])` with exactly one task dict. Set files to {"read_only": ["**"]} and this task text, unchanged:

Read notes.txt in the workspace root and answer with its only line.

Await `child` and check that the final record has status `completed` and answer `scope-ok-7431`. Then use `[follow] = await council.run([...])` with `"child": child` and `"files": {"write": ["audit.txt"]}`. Use this task text, unchanged:

Write the file audit.txt in the workspace root with two lines: the line from notes.txt, then the value of session["agent"]["role"]. Then answer written.

Await `follow`. Check that its final record has status `completed` and answer `written`. Check that the two handles have the same `id` and `session_id`, and different `task_id` values. After each await, print one line that starts with `child-attempt `, then a JSON object with `task_id` from the handle and `id`, `session_id`, `status`, and `answer` from the awaited record.

Do not write audit.txt yourself, start a second child, call `council.grant`, or change other files. When audit.txt exists, finish with followup-done.
