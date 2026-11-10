"""`vis.attach` takes the same call forms as `attach` in `python_execution`.

`vis-contract/fixtures/attachment-verbs.json` holds calls of the five verbs and the
engine answers. The engine test `door-parity-test` runs the same cases at its doors.
"""

import base64
import json
import re

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts, _outside

TABLE = json.loads(
    (_contracts._DATA / "fixtures" / "attachment-verbs.json").read_text(
        encoding="utf-8"
    )
)
VERBS = (
    "attach",
    "list_attachments",
    "get_attachment",
    "read_attachment",
    "show_attachment",
)
_ID = re.compile(r"[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}")


@pytest.fixture(autouse=True)
def _empty_store(monkeypatch):
    monkeypatch.setattr(_outside, "_ARTIFACTS", [])
    monkeypatch.setattr(_outside, "_ARTIFACT_BYTES", {})
    monkeypatch.setattr(vis, "_host", _outside.host)


class Picture:
    """A Pillow image double: it refuses JPEG for a mode with alpha, as Pillow does."""

    size = (3, 2)

    def __init__(self, mode="RGBA"):
        self.mode = mode

    def convert(self, mode):
        return Picture(mode)

    def save(self, target, format="PNG"):
        if format == "JPEG" and self.mode not in ("RGB", "L"):
            raise OSError(f"cannot write mode {self.mode} as JPEG")
        target.write(b"\xff\xd8\xff\xe0" + bytes(32))


def _answer(code, expect):
    """Run the code of one case and return the fields that `expect` names.

    `value` is the whole JSON value of `out`, with each attachment id as `<id>`.
    """
    scope = {verb: getattr(vis, verb) for verb in VERBS}
    try:
        exec("\n".join(code), scope)
    except Exception as error:
        refusal = {"error": type(error).__name__, "message": str(error)}
        return {field: refusal.get(field) for field in expect}
    out = json.loads(_ID.sub("<id>", json.dumps(scope["out"])))
    return {field: out if field == "value" else None for field in expect}


@pytest.mark.parametrize("case", TABLE["cases"], ids=lambda case: case["name"])
def test_the_outside_verbs_give_the_engine_answer(case):
    # A named exception holds the answer of this host; each other case holds the
    # answer of the engine.
    expect = case.get("outside", case)["expect"]
    assert _answer(case["code"], expect) == expect


def test_each_exception_names_its_reason_and_is_a_real_difference():
    for case in TABLE["cases"]:
        exception = case.get("outside")
        if exception is not None:
            assert exception["reason"] in TABLE["exceptions"], case["name"]
            assert exception["expect"] != case["expect"], case["name"]


def test_attach_takes_the_keyword_aliases_of_the_sandbox(tmp_path):
    source = tmp_path / "notes.txt"
    source.write_text("words\n", encoding="utf-8")

    row = vis.attach(path=source, name="renamed.txt", title="Notes", mime="text/x-note")

    assert [row["filename"], row["media_type"]] == ["renamed.txt", "text/x-note"]
    with pytest.raises(
        TypeError, match="filename named twice, as 'name' and as 'filename'"
    ):
        vis.attach(b"x", "a.txt", name="b.txt")
    with pytest.raises(TypeError, match="no keyword 'colour'"):
        vis.attach(b"x", "a.txt", colour="red")


def test_attach_takes_each_option_by_position():
    row = vis.attach(b"words\n", "w.txt", "note", "text/x-note", "Cap", "user")

    assert [row["kind"], row["media_type"], row["audience"]] == [
        "note",
        "text/x-note",
        "user",
    ]


def test_attach_stores_an_image_with_alpha_as_jpeg():
    row = vis.attach(Picture("RGBA"), "shot.jpg")

    assert row["media_type"] == "image/jpeg"
    assert vis.read_attachment(row).startswith(b"\xff\xd8\xff")


def test_attach_names_a_directory(tmp_path):
    with pytest.raises(IsADirectoryError) as refusal:
        vis.attach(tmp_path)

    assert str(refusal.value) == f"attach: {tmp_path} is a directory - attach one file"


# A line break in a caption breaks the transcript fence of the attachment.
def test_attach_sends_a_caption_of_one_line(monkeypatch):
    sent = []
    store = _outside.host.attach
    monkeypatch.setattr(
        _outside.host, "attach", lambda options: sent.append(options) or store(options)
    )

    vis.attach(b"x\n", "n.txt", label="Two\n  lines\t here ")

    assert sent[0]["label"] == "Two lines here"


def test_attach_writes_an_image_in_the_format_of_its_filename():
    asked = []

    class Recorder(Picture):
        def save(self, target, format="PNG"):
            asked.append(format)
            super().save(target, format)

    for name in ("a.gif", "a.webp", "a.bmp", "a.png", "a.jpeg"):
        vis.attach(Recorder("RGB"), name)

    assert asked == ["GIF", "WEBP", "BMP", "PNG", "JPEG"]


def test_attach_converts_only_a_mode_that_jpeg_refuses():
    saved = []

    class Recorder(Picture):
        def convert(self, mode):
            return Recorder(mode)

        def save(self, target, format="PNG"):
            saved.append((self.mode, format))
            super().save(target, format)

    for mode, name in (
        ("CMYK", "a.jpg"),
        ("1", "a.jpg"),
        ("L", "a.jpg"),
        ("RGB", "A.JPG"),
        ("RGBA", "a.png"),
        ("RGB", "a.tif"),
        ("RGB", "a.tiff"),
        ("RGB", "a.xyz"),
    ):
        vis.attach(Recorder(mode), name)

    assert saved == [
        ("RGB", "JPEG"),
        ("RGB", "JPEG"),
        ("L", "JPEG"),
        ("RGB", "JPEG"),
        ("RGBA", "PNG"),
        ("RGB", "TIFF"),
        ("RGB", "TIFF"),
        ("RGB", "PNG"),
    ]


def test_attach_saves_a_figure_as_png():
    asked = []

    class Figure:
        def savefig(self, target, format=None):
            asked.append(format)
            target.write(b"\x89PNG\r\n\x1a\n" + bytes(8))

    row = vis.attach(Figure())

    assert [asked, row["filename"], row["media_type"]] == [
        ["png"],
        "figure.png",
        "image/png",
    ]


def test_attach_reads_a_path_with_a_variable_or_a_home_prefix(tmp_path, monkeypatch):
    (tmp_path / "notes.txt").write_text("words\n", encoding="utf-8")
    monkeypatch.setenv("ATTACH_PARITY_DIR", str(tmp_path))
    monkeypatch.setenv("HOME", str(tmp_path))

    for spelled in ("$ATTACH_PARITY_DIR/notes.txt", "~/notes.txt"):
        assert vis.read_attachment(vis.attach(spelled)) == b"words\n"


# The host keeps a reference to a file that is larger than its limit, and reads
# each other file.
def test_attach_sends_a_reference_only_above_the_limit_of_the_host(
    tmp_path, monkeypatch
):
    sent = []
    monkeypatch.setattr(_outside.host, "attachment_limit", lambda: 4096)
    for door in ("attach", "attach_reference"):
        monkeypatch.setattr(
            _outside.host,
            door,
            lambda options, door=door: (
                sent.append((door, options))
                or json.dumps({"filename": options["filename"]})
            ),
        )
    for size in (4096, 4097):
        (tmp_path / f"{size}.bin").write_bytes(bytes(size))
        vis.attach(tmp_path / f"{size}.bin")

    (stored, whole), (reference, large) = sent
    assert [stored, len(base64.b64decode(whole["base64"])), "path" in whole] == [
        "attach",
        4096,
        False,
    ]
    assert [reference, large["path"], "base64" in large] == [
        "attach_reference",
        str(tmp_path / "4097.bin"),
        False,
    ]


def test_the_app_door_of_a_session_takes_no_callback():
    attach = vis._attachment_verbs(lambda: _outside.host, app_door=object())[0]

    with pytest.raises(TypeError) as refusal:
        attach(vis.ui.app("Review"), on_event=print)

    assert str(refusal.value) == (
        "attach: on_event is only for an extension: the app door of "
        "python_execution takes no callback."
    )


def test_an_app_names_the_arguments_that_it_refuses():
    with pytest.raises(TypeError) as refusal:
        vis.attach(vis.ui.app("Review"), "review.json")

    assert str(refusal.value) == (
        "attach: an app takes no filename, kind, media_type, label or audience."
    )
