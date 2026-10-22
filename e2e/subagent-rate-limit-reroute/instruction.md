Test how you handle a subagent that its provider rate-limits. Call council.run with exactly one task dict. Set provider "busy", model "busy-1" and this task text, unchanged:

Write the file reroute.txt in the workspace root with three lines: reroute-ok, then the value of session["agent"]["role"], then the value of session["routing"]["provider"]. Then answer written.

Do not write reroute.txt yourself and do not change other files. If a child fails, follow your subagent rules for that case. When reroute.txt exists, finish with reroute-done and the provider/model pair that finished the task.
