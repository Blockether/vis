"""`vis.attach` takes the same call forms as `attach` in `python_execution`.

`vis-contract/fixtures/attachment-verbs.json` holds calls of the five verbs and the
engine answers. The engine test `door-parity-test` runs the same cases at its doors.
"""

import json

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
    """Run the code of one case and return the fields that `expect` names."""
    scope = {verb: getattr(vis, verb) for verb in VERBS}
    try:
        exec("\n".join(code), scope)
    except Exception as error:
        refusal = {"error": type(error).__name__, "message": str(error)}
        return {field: refusal.get(field) for field in expect}
    out = json.loads(json.dumps(scope["out"]))
    fields = {
        "json": lambda: {key: out[key] for key in expect["json"] if key in out},
        "keys": lambda: sorted(out),
    }
    return {field: fields[field]() if field in fields else None for field in expect}


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
