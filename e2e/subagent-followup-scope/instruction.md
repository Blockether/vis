Test how you give one subagent a new file scope for a follow-up task. Call council.run with exactly one task dict. Set files to {"read_only": ["**"]} and this task text, unchanged:

Read notes.txt in the workspace root and answer with its only line.

When the child answers, give the same child this follow-up task, unchanged:

Write the file audit.txt in the workspace root with two lines: the line from notes.txt, then the value of session["agent"]["role"]. Then answer written.

The child needs write access only to audit.txt. Do not write audit.txt yourself, do not start a second child and do not change other files. When audit.txt exists, finish with followup-done.
