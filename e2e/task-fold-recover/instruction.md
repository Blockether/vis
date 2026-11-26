Start `python3 fold_probe.py` as a shell Task with `stdin="tty"`. Print its Task name and the output that shows `ready`. Save only the Task name for your next block. Do not send input to the command yet.

In your next Python block, first fold the completed prefix with the fold key from the previous tool result. Include the Task name in the gist. Use `asyncio.all_tasks()` after the fold to find the running Task by name. Do not use the original Task variable. Send the line `release` to the recovered Task. Await it and read its exit code and output. Answer `fold-task-ok` and the final line from the command. Do not start a second process or change files.

After you recover the running Task, print its name. Then send input and print the result exit code and output as a JSON object with keys `exit` and `out`.
