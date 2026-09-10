"""Exercise the shipped TUI on a real PTY with a hermetic gateway stub."""

import errno
import fcntl
import os
import pty
import re
import select
import signal
import struct
import sys
import tempfile
import termios
import time
from pathlib import Path


def check_resize(binary, home, gateway):
    """Resize the kernel window and require a repaint at its new bottom row."""
    rows, cols = 24, 80
    pid, master = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
        os.environ["TERM"] = "xterm-256color"
        os.execv(
            binary,
            [
                binary,
                f"-Duser.home={home}",
                "--gateway",
                gateway,
                "--gateway-token",
                "resize-test",
            ],
        )
    pending = b""
    cursor_row, cursor_col = 1, 1
    sequence = re.compile(rb"\x1b\[([0-9;?]*)([ -/]*)([@-~])")

    def await_bottom(timeout):
        nonlocal pending, cursor_row, cursor_col
        deadline = time.monotonic() + timeout
        seen_rows = set()
        while time.monotonic() < deadline:
            if not select.select([master], [], [], 0.1)[0]:
                continue
            try:
                chunk = os.read(master, 65536)
            except OSError as error:
                if error.errno == errno.EIO:
                    raise AssertionError(
                        "native TUI exited before resize repaint"
                    ) from error
                raise
            if not chunk:
                raise AssertionError(
                    f"native TUI closed its terminal: {pending[-2000:]!r}"
                )
            pending += chunk
            consumed = 0
            for match in sequence.finditer(pending):
                params, _, command = match.groups()
                if command in (b"H", b"f"):
                    values = params.split(b";")
                    cursor_row = int(values[0] or 1)
                    cursor_col = int(values[1] or 1) if len(values) > 1 else 1
                    if cursor_row <= rows:
                        seen_rows.add(cursor_row)
                    cursor_row = min(rows, cursor_row)
                    cursor_col = min(cols, cursor_col)
                elif command == b"n" and params == b"6":
                    os.write(master, f"\x1b[{cursor_row};{cursor_col}R".encode())
                consumed = match.end()
            pending = pending[consumed:][-4096:]
            if rows in seen_rows:
                return
        raise AssertionError(
            f"no repaint at row {rows} after resize to {cols}x{rows}; painted rows: {sorted(seen_rows)}"
        )

    try:
        await_bottom(20)
        print("initial 80x24 painted", flush=True)
        for rows, cols in [(45, 120), (18, 70), (35, 100)]:
            pending = b""
            fcntl.ioctl(
                master, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0)
            )
            # TIOCSWINSZ delivers SIGWINCH to the foreground process group.
            # Do not send a key: resizing alone must wake and repaint the TUI.
            await_bottom(8)
            print(f"resized to {cols}x{rows}", flush=True)
    finally:
        os.close(master)
        try:
            os.kill(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        os.waitpid(pid, 0)


if __name__ == "__main__":
    with tempfile.TemporaryDirectory(prefix="vis-tui-resize-") as home:
        try:
            check_resize(os.path.abspath(sys.argv[1]), home, sys.argv[2])
        except Exception:
            for log in (Path(home) / ".vis/logs").glob("*.log"):
                print(log.read_text()[-4000:], file=sys.stderr)
            raise
