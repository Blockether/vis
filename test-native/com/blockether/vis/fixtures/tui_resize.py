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


def check_resize(binary, home, gateway, model_key=None):
    """Resize the kernel window and require a repaint at its new bottom row."""
    rows, cols = 24, 80
    pid, master = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
        os.environ["TERM"] = "xterm-256color"
        os.environ.pop("TSLP_NATIVE_PATH", None)
        os.execv(
            binary,
            [
                binary,
                f"-Duser.home={home}",
                "--gateway",
                gateway,
                "--gateway-token",
                "resize-test",
                "--session-id",
                "00000000-0000-0000-0000-000000000001",
            ],
        )
    pending = b""
    cursor_row, cursor_col = 1, 1
    sequence = re.compile(rb"\x1b\[([0-9;?]*)([ -/]*)([@-~])")
    output = b""

    def token_colors():
        foreground = None
        colors = {}
        end = 0
        for match in sequence.finditer(output):
            text = output[end : match.start()]
            for token in (b"vis_identifier_marker", b"vis_string_marker"):
                if token in text:
                    colors[token] = foreground
            params, _, command = match.groups()
            if command == b"m":
                values = params.split(b";")
                if len(values) == 5 and values[:2] == [b"38", b"2"]:
                    foreground = tuple(values[2:])
                elif params in (b"", b"0", b"39"):
                    foreground = None
            end = match.end()
        return colors

    def await_bottom(timeout, highlighting=False, text=None):
        nonlocal pending, cursor_row, cursor_col, output
        deadline = time.monotonic() + timeout
        seen_rows = set()
        while time.monotonic() < deadline:
            if text is not None and text in sequence.sub(b"", output):
                return
            colors = token_colors()
            if (
                highlighting
                and len(colors) == 2
                and None not in colors.values()
                and len(set(colors.values())) == 2
            ):
                return
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
            output = (output + chunk)[-131072:]
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
            if text is None and not highlighting and rows in seen_rows:
                return
        if text is not None:
            raise AssertionError(
                f"native TUI did not respond with {text!r} within {timeout}s"
            )
        if highlighting:
            raise AssertionError(
                f"native Python syntax colors are missing or identical: {token_colors()}"
            )
        raise AssertionError(
            f"no repaint at row {rows} after resize to {cols}x{rows}; painted rows: {sorted(seen_rows)}"
        )

    try:
        await_bottom(20)
        print("initial 80x24 painted", flush=True)
        for rows, cols in [] if model_key else [(45, 120), (18, 70), (35, 100)]:
            pending = b""
            fcntl.ioctl(
                master, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0)
            )
            # TIOCSWINSZ delivers SIGWINCH to the foreground process group.
            # Do not send a key: resizing alone must wake and repaint the TUI.
            await_bottom(8)
            print(f"resized to {cols}x{rows}", flush=True)
        if model_key:
            await_bottom(8, text=b"vis_identifier_marker")
        else:
            await_bottom(8, highlighting=True)
            print("native Python syntax colors verified", flush=True)
        if model_key:
            output = b""
            os.write(master, b"\x18" + model_key.encode())
            if model_key == "c":
                await_bottom(2, text=b"Session model")
                os.write(master, b"\x1b[B\r")
            os.write(master, b"responsive-marker")
            await_bottom(2, text=b"responsive-marker")
            print(
                f"C-x {model_key}: input responsive during slow model HTTP", flush=True
            )
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
            check_resize(
                os.path.abspath(sys.argv[1]),
                home,
                sys.argv[2],
                sys.argv[3] if len(sys.argv) > 3 else None,
            )
        except Exception:
            for log in (Path(home) / ".vis/logs").glob("*.log"):
                print(log.read_text()[-4000:], file=sys.stderr)
            raise
