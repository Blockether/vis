"""`vis.attach` takes the same call forms as `attach` in `python_execution`."""

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside


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
