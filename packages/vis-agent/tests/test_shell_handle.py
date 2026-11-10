"""The shell handle of an extension answers the calls of the sandbox handle."""

import json
import time

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside


def _handle():
    """A handle on a fake host, and the options that the host received."""
    sent = []

    def call(opts):
        sent.append(dict(opts))
        return {
            "id": "s1",
            "stage": opts.get("op", "run"),
            "out": "one\ntwo\n",
            "exit": 0,
            "offset": 0,
            "next_offset": 8,
            "is_eof": True,
        }

    return vis.Shell(call({"command": "true"}), call), sent


@pytest.fixture
def four_lines():
    return vis.shell({"command": "printf 'one\\ntwo\\nthree\\nfour\\n'"}).wait(10)


@pytest.mark.parametrize(
    ("read", "options"),
    [
        (lambda sh: sh.logs(), {}),
        (lambda sh: sh.logs(-5), {"offset": -5}),
        (lambda sh: sh.logs(0, 10), {"offset": 0, "lines": 10}),
        (lambda sh: sh.logs(8, -2), {"offset": 8, "lines": -2}),
        (lambda sh: sh.logs(lines=5), {"lines": 5}),
        (lambda sh: sh.logs(tail=5), {"lines": 5}),
        (lambda sh: sh.logs(n=5), {"lines": 5}),
        (lambda sh: sh.logs(start=4), {"offset": 4}),
        (lambda sh: sh.logs(limit=64), {"limit": 64}),
        (lambda sh: sh.logs(bytes=64), {"limit": 64}),
    ],
)
def test_logs_sends_the_options_of_the_sandbox_handle(read, options):
    sh, sent = _handle()
    read(sh)
    assert sent[-1] == {"op": "logs", "id": "s1", **options}


@pytest.mark.parametrize(
    "wait",
    [
        lambda sh: sh.wait(7),
        lambda sh: sh.wait(seconds=7),
        lambda sh: sh.wait(secs=7),
        lambda sh: sh.wait(timeout=7),
    ],
)
def test_wait_takes_each_name_of_its_seconds(wait):
    sh, sent = _handle()
    assert wait(sh) is sh
    assert sent[-1] == {"op": "wait", "id": "s1", "seconds": 7}


def test_an_unknown_keyword_names_the_keywords_and_sends_nothing():
    sh, sent = _handle()
    with pytest.raises(TypeError, match="logs: no keyword 'rows'. Keywords: offset"):
        sh.logs(rows=3)
    with pytest.raises(TypeError, match="logs: lines named twice, as 'tail' too"):
        sh.logs(lines=1, tail=2)
    with pytest.raises(TypeError, match="wait: no keyword 'until'. Keywords: seconds"):
        sh.wait(until=3)
    assert len(sent) == 1


def test_a_field_answers_by_attribute_and_a_miss_names_the_fields():
    sh, _ = _handle()
    assert sh.exit == 0
    assert sh.out == sh.stdout == sh.stderr == "one\ntwo\n"
    assert list(sh.keys()) == list(dict(sh))
    assert json.loads(json.dumps(sh)) == dict(sh)
    with pytest.raises(AttributeError) as attribute:
        str(sh.outp)
    assert "'outp' is not a field of this shell result" in str(attribute.value)
    assert "Did you mean 'out'?" in str(attribute.value)
    with pytest.raises(KeyError) as key:
        sh["nope"]
    assert "'nope' is not a key of this shell result. Keys: 'id'" in str(key.value)


def test_a_log_page_walks_forward_by_lines(four_lines):
    page = four_lines.logs(0, 2)
    assert (page["out"], page["offset"], page["next_offset"]) == ("one\ntwo\n", 0, 8)
    assert page["is_eof"] is False
    more = next(page)
    assert (more["out"], more["offset"], more["is_eof"]) == ("three\nfour\n", 8, True)
    assert next(more, None) is None
    pages = [each["out"] for each in four_lines.logs(0, 1).pages()]
    assert pages == ["one\n", "two\n", "three\n", "four\n"]
    assert len(list(four_lines.logs(0, 1).pages(2))) == 2


def test_a_log_page_walks_backward_from_the_end(four_lines):
    page = four_lines.logs(lines=-1)
    assert page["out"] == "four\n"
    above = next(page)
    assert (above["out"], above["offset"]) == ("three\n", 8)
    pages = [each["out"] for each in four_lines.logs(lines=-3).pages()]
    assert pages == ["two\nthree\nfour\n", "one\n"]


def test_the_line_window_takes_each_of_its_names(four_lines):
    assert four_lines.logs(lines=1)["out"] == "four\n"
    assert four_lines.logs(tail=2)["out"] == "three\nfour\n"
    assert four_lines.logs(n=2)["out"] == "three\nfour\n"
    assert four_lines.logs(start=8)["out"] == "three\nfour\n"
    assert four_lines.logs(0, bytes=3)["out"] == "one"
    assert four_lines.logs(14, -1)["out"] == "three\n"


def test_a_log_page_reads_as_text(four_lines):
    page = four_lines.logs(0)
    assert page[-5:] == "four\n"
    assert "log: " + page == "log: one\ntwo\nthree\nfour\n"
    assert page + "end" == "one\ntwo\nthree\nfour\nend"
    assert page.splitlines()[0] == "one"
    assert page.find("three") == 8
    assert four_lines.logs[-5:] == "four\n"
    assert four_lines.logs["exit"] == 0
    assert four_lines.logs.splitlines() == ["one", "two", "three", "four"]
    assert four_lines.logs.count("o") == 3


def test_only_a_log_page_continues(four_lines):
    with pytest.raises(RuntimeError, match="shell log paging starts on a log page"):
        next(four_lines)
    page = four_lines.logs(0, 1)
    assert page.wait(secs=1) is page
    assert page["stage"] == "wait"
    with pytest.raises(RuntimeError, match="shell log paging starts on a log page"):
        next(page)


def test_the_outside_host_reads_the_option_names_of_the_engine(four_lines):
    def logs(**options):
        return _outside.shell({"op": "logs", "id": four_lines["id"], **options})

    assert logs(n=1)["out"] == "four\n"
    assert logs(tail=1)["out"] == "four\n"
    assert logs(start=14)["out"] == "four\n"
    assert logs(offset=0, bytes=3)["out"] == "one"
    first = logs(offset=0, lines=1)
    assert (first["out"], first["next_offset"], first["is_eof"]) == ("one\n", 4, False)


def test_the_outside_wait_reads_each_name_of_its_seconds(monkeypatch):
    sleeper = vis.shell({"command": "sleep 30"})
    clock = [time.time()]
    try:
        with monkeypatch.context() as patched:
            patched.setattr(_outside.time, "time", lambda: clock[0])
            patched.setattr(
                _outside.time,
                "sleep",
                lambda seconds: clock.__setitem__(0, clock[0] + seconds),
            )
            for name in ("seconds", "secs", "timeout"):
                before = clock[0]
                options = {"op": "wait", "id": sleeper["id"], name: 3}
                assert _outside.shell(options)["status"] == "running"
                assert 3 <= clock[0] - before < 4
    finally:
        sleeper.stop()
