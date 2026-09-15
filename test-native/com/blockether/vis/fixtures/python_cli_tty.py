"""Regression #229: type into the CLI's controlling terminal after each prompt."""

import errno
import os
import pty
import select
import signal
import sys
import tempfile
import time
from pathlib import Path

SOURCE = """import sys
print('isatty:', sys.stdin.isatty(), flush=True)
print('received:', repr(input('value: ')), flush=True)
print('empty:', repr(input('empty value: ')), flush=True)
try:
    input('eof value: ')
except EOFError:
    print('EOF received', flush=True)
else:
    raise AssertionError('Expected terminal EOF')
"""


def exercise(command, mode, directory):
    """Require two completed reads and terminal EOF without prefeeding stdin."""
    pid, master = pty.fork()
    if pid == 0:
        os.chdir(directory)
        os.environ["PYTHONPATH"] = str(directory)
        args = ["-m", "prompt_fixture"] if mode == "module" else ["prompt_fixture.py"]
        os.execv(command[0], [*command, *args])

    output = bytearray()
    exited = False

    def read_output(timeout):
        ready, _, _ = select.select([master], [], [], timeout)
        if not ready:
            return None
        try:
            chunk = os.read(master, 65536)
        except OSError as error:
            if error.errno != errno.EIO:
                raise
            return False
        output.extend(chunk)
        return bool(chunk)

    def read_until(marker, timeout):
        deadline = time.monotonic() + timeout
        while marker not in output and time.monotonic() < deadline:
            if read_output(0.1) is False:
                break
        assert marker in output, f"Missing {marker!r}"

    try:
        read_until(b"value: ", 90)
        assert b"isatty: True" in output, "The guest did not receive a terminal"
        os.write(master, "hello zażółć\n".encode())
        read_until("received: 'hello zażółć'".encode(), 10)
        read_until(b"empty value: ", 10)
        os.write(master, b"\n")
        read_until(b"empty: ''", 10)
        read_until(b"eof value: ", 10)
        os.write(master, b"\x04")
        read_until(b"EOF received", 10)
        deadline = time.monotonic() + 10
        while time.monotonic() < deadline:
            read_output(0.05)
            child, status = os.waitpid(pid, os.WNOHANG)
            if child:
                exited = True
                while read_output(0) is True:
                    pass
                assert os.waitstatus_to_exitcode(status) == 0, f"Child status: {status}"
                for text in (
                    "isatty: True",
                    "received: 'hello zażółć'",
                    "empty: ''",
                    "EOF received",
                ):
                    assert output.count(text.encode()) == 1, (
                        f"Duplicated output: {text!r}"
                    )
                print(f"{mode}: interactive input and EOF passed")
                return
        raise AssertionError("CLI did not exit after terminal EOF")
    except AssertionError as error:
        raise AssertionError(
            f"{mode}: {error}\n{output.decode(errors='replace')}"
        ) from error
    finally:
        os.close(master)
        if not exited:
            try:
                os.kill(pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            os.waitpid(pid, 0)


def main():
    """Run the same terminal regression against a JVM command or native binary."""
    command = sys.argv[1:]
    if not command:
        raise SystemExit("Usage: python_cli_tty.py COMMAND [ARG ...]")
    with tempfile.TemporaryDirectory(prefix="python-cli-tty-", dir=".") as temporary:
        directory = Path(temporary).resolve()
        (directory / "prompt_fixture.py").write_text(SOURCE, encoding="utf-8")
        failed = False
        for mode in ("file", "module"):
            try:
                exercise(command, mode, directory)
            except AssertionError as error:
                failed = True
                print(error, file=sys.stderr)
        return int(failed)


if __name__ == "__main__":
    raise SystemExit(main())
