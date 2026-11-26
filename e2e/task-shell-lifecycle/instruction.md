Start `python3 shell_probe.py hold` and save its ShellTask as `sh`. Wait up to one second with `done, pending = await asyncio.wait([sh], timeout=1)`. Check that `sh` is in `pending`. Read `sh.out` while the process runs. If `hold-started` is not visible yet, read `sh.out` again after a short `asyncio.sleep`.

Cancel `sh` and await it. Catch `asyncio.CancelledError` and check that `sh` is cancelled. Then start `python3 shell_probe.py finish`. Await that ShellTask itself and read the exit code and output. Do not stop either command with an external command. Do not change files.

Print one JSON object with keys `running`, `start_output`, `cancelled`, `exit`, and `output`. Get `running` from the pending set and `start_output` from the live read. Get the other values from the Tasks and the final result. Answer `shell-task-ok` only if you saw `hold-started` while the hold Task was running, its cancellation succeeded, and the finish command exited with code 0 and printed `finish-ok`.
