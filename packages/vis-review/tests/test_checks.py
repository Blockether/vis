"""Rules of the review checks."""

from types import SimpleNamespace

from vis_review.checks import added_lines, diff_checks, language_checks
from vis_review.journal import FileDiff, unified
from vis_review.store import digest


def change(path, old, new):
    """A FileDiff and the content lookup that the checks read."""
    status = "added" if old is None else "modified"
    blobs = {digest(new.encode()): new.encode()}
    diff = FileDiff(
        path,
        status,
        0,
        0,
        False,
        unified(path, status, old or "", new),
        None if old is None else digest(old.encode()),
        digest(new.encode()),
    )
    return diff, blobs.get


def checks_of(path, old, new):
    diff, content = change(path, old, new)
    return {(f.check, f.line) for f in diff_checks([diff], content)}


def test_added_lines_counts_lines_of_the_new_file_across_hunks():
    old = "".join(f"{n}\n" for n in range(1, 21))
    new = old.replace("2\n", "2\nX\n", 1).replace("18\n", "18\nY\n")
    assert added_lines(unified("a", "modified", old, new)) == [(3, "X"), (20, "Y")]


def test_a_conflict_marker_is_an_error():
    assert ("conflict-marker", 2) in checks_of("a.py", "a\n", "a\n<<<<<<< HEAD\n")


def test_a_secret_is_an_error_and_its_value_stays_out_of_the_message():
    diff, content = change("a.py", "", "key = 'AKIAABCDEFGHIJKLMNOP'\n")
    (secret,) = [f for f in diff_checks([diff], content) if f.check == "secret"]
    assert secret.level == "error" and "AKIA" not in secret.message


def test_debugging_code_counts_only_in_its_language():
    assert ("debug-leftover", 1) in checks_of("a.py", "", "breakpoint()\n")
    assert ("debug-leftover", 1) in checks_of("a.ts", "", "console.log(x)\n")
    assert not {
        c for c in checks_of("a.md", "", "breakpoint()\n") if c[0] == "debug-leftover"
    }


def test_an_added_todo_is_a_note():
    assert ("todo", 1) in checks_of("a.py", "", "# TODO later\n")


def test_trailing_whitespace_counts_outside_markdown():
    assert ("trailing-whitespace", 1) in checks_of("a.py", "", "x = 1  \n")
    assert not {
        c for c in checks_of("a.md", "", "line  \n") if c[0] == "trailing-whitespace"
    }


def test_a_missing_final_newline_counts_once_for_the_file():
    assert ("final-newline", 0) in checks_of("a.py", "a\n", "a\nb")


def test_an_added_binary_file_is_a_note_and_has_no_line_checks():
    diff = FileDiff("x.bin", "added", 0, 0, True, "Binary files differ", None, "s")
    assert [(f.check, f.level) for f in diff_checks([diff], {"s": b"\0x"}.get)] == [
        ("binary-file", "info")
    ]


def test_a_large_file_is_a_warning(monkeypatch):
    from vis_review import checks

    monkeypatch.setattr(checks, "LARGE_BYTES", 3)
    assert ("large-file", 0) in checks_of("a.py", "", "abcdef\n")


def test_unchanged_lines_find_nothing():
    old = "x = 1  \n# TODO\n"
    assert checks_of("a.py", old, old + "y = 2\n") == set()


def fake_call(answers, calls):
    def call(tool, verb, **arguments):
        calls.append((tool, verb, arguments))
        answer = answers.get((tool, verb))
        if isinstance(answer, Exception):
            raise answer
        return answer

    return call


def test_language_checks_map_lint_format_and_tests_to_findings():
    lint = SimpleNamespace(
        diagnostics=[
            SimpleNamespace(
                path="/r/src/a.py", line=3, level="error", message="unused", rule="F401"
            )
        ]
    )
    fmt = SimpleNamespace(changed=["/r/src/a.py"])
    tests = SimpleNamespace(
        failures=[
            SimpleNamespace(
                test="t", path="/r/tests/t.py", line=9, message="boom\ndetail"
            )
        ],
        is_passed=False,
    )
    calls = []
    call = fake_call(
        {
            ("py", "lint_code"): lint,
            ("py", "format_code"): fmt,
            ("py", "run_tests"): tests,
        },
        calls,
    )
    found = {
        (f.check, f.path, f.line, f.message)
        for f in language_checks("/r", ["src/a.py", "README.md"], call, is_tested=True)
    }
    assert found == {
        ("lint", "src/a.py", 3, "F401: unused"),
        ("format", "src/a.py", 0, "The py formatter would change this file."),
        ("test", "tests/t.py", 9, "t: boom"),
    }
    assert calls[0] == ("py", "lint_code", {"paths": ["/r/src/a.py"], "cwd": "/r"})


def test_tests_run_only_when_asked():
    calls = []
    language_checks("/r", ["a.clj"], fake_call({}, calls))
    assert [verb for _, verb, _ in calls] == ["lint_code", "format_code"]


def test_a_tool_that_does_not_run_is_a_note():
    call = fake_call(
        {
            ("py", "lint_code"): RuntimeError("no ruff"),
            ("py", "format_code"): SimpleNamespace(changed=[]),
        },
        [],
    )
    (note,) = language_checks("/r", ["a.py"], call)
    assert (note.check, note.level) == ("tool", "info") and "no ruff" in note.message


def test_files_of_no_language_call_no_tool():
    calls = []
    assert (
        language_checks("/r", ["README.md"], fake_call({}, calls)) == [] and calls == []
    )
