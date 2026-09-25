"""Exercise the shipped TUI on a real PTY with a hermetic gateway stub."""

import base64
import errno
import fcntl
import json
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
    clipboard_mode = (
        "osc52" if mode == "prose" else mode if mode in ("osc52", "clip.exe") else None
    )
    theme_mode = mode in ("theme", "theme-restart")
    config_file = Path(home) / ".vis/tui/config.json"
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
        if mode == "images":
            os.environ["TERM_PROGRAM"] = "kitty"
            os.environ.pop("TMUX", None)
            os.environ.pop("STY", None)
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
        """The truecolor foreground each marker token was last painted with."""
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

    def await_bottom(
        timeout, highlighting=False, text=None, copied=None, background=None, idle=False
    ):
        nonlocal pending, cursor_row, cursor_col, output
        deadline = time.monotonic() + timeout
        seen_rows = set()
        while time.monotonic() < deadline:
            if (
                background is not None
                and b"\x1b]11;rgb:" + background + b"\x07" in output
            ):
                return
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
            output = (output + chunk)[-(1048576 if mode == "images" else 131072) :]
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
                and background is None
                and not idle
                and rows in seen_rows
            ):
                return
        if idle:
            return
        if background is not None:
            sent_backgrounds = re.findall(rb"\x1b]11;rgb:([^\x07]+)\x07", output)
            raise AssertionError(
                f"native TUI did not apply background {background!r}; "
                f"sent={sent_backgrounds!r}; "
                f"config={json.loads(config_file.read_text()) if config_file.exists() else None}; "
                f"screen={screen_lines()}"
            )
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
        if mode == "prose":
            original = (
                "A quiet paragraph can become much more comfortable when its lines share "
                "a reasonably even rhythm of spaces instead of alternating between very tight "
                "and very loose arrangements."
            )
            layouts = []
            for rows, cols in [(24, 80), (45, 32), (35, 68), (35, 100)]:
                if layouts:
                    output = b""
                    fcntl.ioctl(
                        master,
                        termios.TIOCSWINSZ,
                        struct.pack("HHHH", rows, cols, 0, 0),
                    )
                    await_bottom(8)
                await_bottom(8, text=b"arrangements.")
                screen = screen_lines()
                first_row = next(
                    row
                    for row, line in screen.items()
                    if re.search(r"A +quiet +paragraph", line)
                )
                last_row = next(
                    row for row, line in screen.items() if "arrangements." in line
                )
                lines = [
                    re.sub(r"^[^A-Za-z]*|[^A-Za-z.]*$", "", screen[row])
                    for row in range(first_row, last_row + 1)
                ]
                assert " ".join(" ".join(lines).split()) == original, screen
                assert all(len(line) <= cols - 2 for line in lines), lines
                assert not any("   " in line for line in lines), lines
                assert "  " not in lines[-1], lines
                layouts.append(lines)
            assert len(layouts[1]) > len(layouts[-1]), layouts
            assert any("  " in line for lines in layouts for line in lines[:-1]), (
                layouts
            )
            # Copy uses the source paragraph, never the added layout spacing.
            line = screen[first_row]
            col = line.index("A") + 1
            output = b""
            os.write(
                master, f"\x1b[<0;{col};{first_row}M\x1b[<0;{col};{first_row}m".encode()
            )
            await_bottom(8, copied=original)
            print("native Justice prose reflow and copy verified", flush=True)
            return
        if mode == "images":
            # #257: inspect the actual native Kitty stream across both image boxes.
            await_bottom(1, idle=True)
            os.write(master, b"\x1b[<64;20;8M" * 30)
            await_bottom(1, idle=True)
            for _ in range(25):
                os.write(master, b"\x1b[<65;20;8M")
                await_bottom(0.2, idle=True)
            uploads, placed, placement_owners = {}, {}, {}
            visible_sizes = set()
            saw_both = False
            deletes = 0
            upload_id, payload = None, b""
            for match in re.finditer(rb"\x1b_G([^\x1b]*)\x1b\\|\x1b8", output):
                command = match.group(1)
                if command is None:
                    # End of a graphics frame: departed placements are gone now.
                    saw_both |= len(set(placed.values())) == 2
                    continue
                head, _, data = command.partition(b";")
                fields = dict(
                    item.split(b"=", 1) for item in head.split(b",") if b"=" in item
                )
                action = fields.get(b"a")
                if action == b"t":
                    upload_id, payload = int(fields[b"i"]), b""
                if upload_id is not None and (action == b"t" or b"m" in fields):
                    payload += data
                    if fields.get(b"m", b"0") == b"0":
                        png = base64.b64decode(payload, validate=True)
                        assert png[:8] == b"\x89PNG\r\n\x1a\n"
                        uploads[upload_id] = struct.unpack(">II", png[16:24])
                        upload_id = None
                elif action == b"p":
                    image_id, placement_id = int(fields[b"i"]), int(fields[b"p"])
                    assert image_id in uploads, f"Placement without upload: {fields}"
                    previous_owner = placement_owners.setdefault(placement_id, image_id)
                    assert previous_owner == image_id, "Two images share a placement ID"
                    width, height = uploads[image_id]
                    assert (
                        int(fields.get(b"y", 0)) + int(fields.get(b"h", height))
                        <= height
                    )
                    assert int(fields.get(b"w", width)) <= width
                    placed[image_id, placement_id] = (width, height)
                    visible_sizes.add((width, height))
                elif action == b"d" and fields.get(b"d") == b"i":
                    assert b"p" in fields, "Deletion must target one placement"
                    pair = int(fields[b"i"]), int(fields[b"p"])
                    assert pair in placed, f"Deleting an unknown placement: {pair}"
                    del placed[pair]
                    deletes += 1
                elif action == b"d" and fields.get(b"d") == b"I":
                    image_id = int(fields[b"i"])
                    uploads.pop(image_id, None)
                    assert all(pair[0] != image_id for pair in placed)
                elif action == b"d" and fields.get(b"d") == b"A":
                    placed.clear()
                    uploads.clear()
            assert visible_sizes == {(1358, 1030), (702, 648)}, visible_sizes
            assert saw_both, "Never placed both images in a viewport transition"
            assert deletes, "Scrolling did not remove departed placements"
            assert (702, 648) in placed.values(), (
                "Second image is blank after scrolling"
            )
            print("native Kitty image scrolling verified", flush=True)
            return
        if theme_mode:
            initial_bg = b"1a/1b/26" if mode == "theme" else b"0c/0e/12"
            await_bottom(8, background=initial_bg)
            if mode == "theme-restart":
                print("native theme restored after restart", flush=True)
                return
            output = b""
            os.write(master, b"\x18o")
            await_bottom(8, text=b"Settings")
            os.write(master, b"Theme\r")
            await_bottom(8, text=b"Themes")
            # At 80x24 the picker has one theme per page; Vis Dark is second.
            os.write(master, b"n")
            await_bottom(8, text=b"Vis Dark")
            output = b""
            os.write(master, b"a")
            await_bottom(8, background=b"0c/0e/12")
            await_bottom(8, text=b"current")  # Paint completes after persistence.
            saved = json.loads(config_file.read_text())
            assert saved["theme_name"] == "vis-dark", saved
            assert saved["show_python_code"] is False, saved
            assert saved["unrelated"] == "preserved", saved
            # Close the picker, clear Settings' search, then close Settings.
            os.write(master, b"\x07\x07\x07theme-live-marker")
            await_bottom(8, text=b"theme-live-marker")
            # The reported reset occurred after a few minutes, without exiting.
            await_bottom(125, idle=True)
            assert json.loads(config_file.read_text())["theme_name"] == "vis-dark"
            assert all(
                color == b"0c/0e/12"
                for color in re.findall(rb"\x1b]11;rgb:([^\x07]+)\x07", output)
            ), "native theme changed while idle"
            output = b""
            rows, cols = 35, 100
            fcntl.ioctl(
                master, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0)
            )
            await_bottom(8)
            assert b"\x1b[48;2;12;14;18m" in output, "resize restored the old palette"
            print("native theme persisted through idle and repaint", flush=True)
            return
        if clipboard_mode:
            await_bottom(8, text=b"Copy:")
            # Click the actual user bubble, then drag-select its first word.
            row, col = text_position(b"Copy:")
            output = b""
            os.write(master, f"\x1b[<0;{col};{row}M\x1b[<0;{col};{row}m".encode())
            await_bottom(8, copied="Copy: Zażółć gęślą jaźń 中文 😀")
            # Separate gestures: within 500 ms a second press intentionally
            # selects the whole line. Fast Linux helpers finish inside that window.
            time.sleep(0.6)
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
            # A partial escape sequence can continue in the next PTY read.
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
            # The Python fence is colored by the built-in highlighter: the string
            # must paint in a different color than the name it is assigned to.
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
            mode = sys.argv[3] if len(sys.argv) > 3 else None
            if mode == "theme":
                config_file = Path(home) / ".vis/tui/config.json"
                config_file.parent.mkdir(parents=True)
                config_file.write_text(
                    json.dumps(
                        {
                            "theme_name": "tokyonight-night",
                            "show_python_code": False,
                            "unrelated": "preserved",
                        }
                    )
                )
            check_resize(os.path.abspath(sys.argv[1]), home, sys.argv[2], mode)
            if mode == "theme":
                check_resize(
                    os.path.abspath(sys.argv[1]), home, sys.argv[2], "theme-restart"
                )
        except Exception:
            for log in (Path(home) / ".vis/logs").glob("*.log"):
                content = log.read_text()
                for line in content.splitlines()[-3:]:
                    print(line[:2000], file=sys.stderr)
                print(content[-4000:], file=sys.stderr)
            raise
