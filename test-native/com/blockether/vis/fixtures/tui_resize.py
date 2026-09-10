"""Exercise the shipped TUI on a real PTY with a hermetic gateway stub."""

import base64
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


def check_resize(binary, home, gateway, mode=None):
    """Exercise production rendering and input on a controlling terminal."""
    model_key = mode if mode in ("c", "m") else None
    clipboard_mode = mode if mode in ("osc52", "clip.exe") else None
    clipboard_file = Path(home) / "clipboard.bin"
    if clipboard_mode:
        # Deterministic helper boundary: inspect clip.exe stdin without changing
        # the host clipboard. Actual Windows clipboard integration is not simulated.
        helpers = Path(home) / "helpers"
        helpers.mkdir()
        for name in ("pbcopy", "wl-copy", "xclip", "xsel", "clip.exe"):
            helper = helpers / name
            body = (
                'exec /bin/cat > "$VIS_TEST_CLIPBOARD"'
                if name == clipboard_mode == "clip.exe"
                else "exit 1"
            )
            helper.write_text(f"#!/bin/sh\n{body}\n")
            helper.chmod(0o755)
    rows, cols = 24, 80
    pid, master = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
        os.environ["TERM"] = "xterm-256color"
        os.environ.pop("TSLP_NATIVE_PATH", None)
        if clipboard_mode:
            os.environ["PATH"] = f"{helpers}:{os.environ.get('PATH', '')}"
            os.environ["VIS_TEST_CLIPBOARD"] = str(clipboard_file)
            os.environ.pop("TMUX", None)
            os.environ.pop("STY", None)
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

    def screen_lines():
        """Reconstruct Lanterna's absolute-positioned writes, including delta frames."""
        row, col, end = 1, 1, 0
        cells = {}
        for match in sequence.finditer(output + b"\x1b[m"):
            for char in output[end : match.start()].decode("utf-8", errors="replace"):
                if char == "\r":
                    col = 1
                elif char == "\n":
                    row += 1
                elif char.isprintable():
                    cells[row, col] = char
                    col += 1
            params, _, command = match.groups()
            if command in (b"H", b"f"):
                values = params.split(b";")
                row = int(values[0] or 1)
                col = int(values[1] or 1) if len(values) > 1 else 1
            elif command == b"J" and params == b"2":
                cells.clear()
            end = match.end()
        return {
            r: "".join(cells.get((r, c), " ") for c in range(1, cols + 1))
            for r in range(1, rows + 1)
        }

    def text_position(text):
        # Copy targets begin with ASCII, before the fixture's wide characters.
        for row, line in screen_lines().items():
            col = line.find(text.decode())
            if col >= 0:
                return row, col + 1
        raise AssertionError(f"Missing copy target: {text!r}")

    def clipboard_matches(text):
        if clipboard_mode == "clip.exe":
            expected = b"\xff\xfe" + text.encode("utf-16le")
            return clipboard_file.exists() and clipboard_file.read_bytes() == expected
        expected = b"\x1b]52;c;" + base64.b64encode(text.encode()) + b"\x07"
        return expected in output

    def await_bottom(timeout, highlighting=False, text=None, copied=None):
        nonlocal pending, cursor_row, cursor_col, output
        deadline = time.monotonic() + timeout
        seen_rows = set()
        while time.monotonic() < deadline:
            if copied is not None and clipboard_matches(copied):
                return
            if text is not None and any(
                text.decode() in line for line in screen_lines().values()
            ):
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
            if (
                copied is None
                and text is None
                and not highlighting
                and rows in seen_rows
            ):
                return
        if copied is not None:
            captured = clipboard_file.read_bytes() if clipboard_file.exists() else b""
            raise AssertionError(
                f"Native {clipboard_mode} did not copy {copied!r}; "
                f"helper bytes={captured!r}; terminal={output[-2000:]!r}"
            )
        if text is not None:
            raise AssertionError(
                f"native TUI did not respond with {text!r} within {timeout}s; "
                f"terminal={sequence.sub(b'', output)[-2000:]!r}"
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
        if clipboard_mode:
            await_bottom(8, text=b"Copy:")
            # Click the actual user bubble, then drag-select its first word.
            row, col = text_position(b"Copy:")
            output = b""
            os.write(master, f"\x1b[<0;{col};{row}M\x1b[<0;{col};{row}m".encode())
            await_bottom(8, copied="Copy: Zażółć gęślą jaźń 中文 😀")
            output = b""
            if clipboard_file.exists():
                clipboard_file.unlink()
            os.write(
                master,
                (
                    f"\x1b[<0;{col};{row}M"
                    f"\x1b[<32;{col + 3};{row}M"
                    f"\x1b[<0;{col + 3};{row}m"
                ).encode(),
            )
            await_bottom(8, copied="Copy")
            logs = list((Path(home) / ".vis/logs").glob("*.log"))
            assert logs, "TUI did not create its redirected log"
            assert all(b"\x1b]52;" not in log.read_bytes() for log in logs)
            print(f"native clipboard verified: {clipboard_mode}", flush=True)
            return
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
