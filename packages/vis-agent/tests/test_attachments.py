"""`vis.attach` and the attachment readers, against the outside host."""

import base64
import json

import blockether.vis.extension as vis
import pytest
from blockether.vis import _outside

PNG = b"\x89PNG\r\n\x1a\n" + b"\0" * 16


@pytest.fixture(autouse=True)
def _empty_store(monkeypatch):
    monkeypatch.setattr(_outside, "_ARTIFACTS", [])
    monkeypatch.setattr(_outside, "_ARTIFACT_BYTES", {})
    monkeypatch.setattr(vis, "_host", _outside.host)


def test_attach_stores_a_path_and_returns_its_descriptor(tmp_path):
    source = tmp_path / "report.csv"
    source.write_text("a,b\n1,2\n", encoding="utf-8")

    row = vis.attach(source, label="Totals")

    assert row["filename"] == "report.csv"
    assert row["version"] == 1
    assert row["media_type"] == "text/csv"
    assert row["kind"] == "file"
    assert row["size"] == 8
    assert vis.read_attachment(row) == b"a,b\n1,2\n"


def test_same_filename_adds_a_version_and_targets_resolve_it():
    first = vis.attach(PNG, "chart.png")
    second = vis.attach(PNG + b"x", "chart.png", audience="user")

    assert second["version"] == 2
    assert second["kind"] == "image"
    assert [r["version"] for r in vis.list_attachments("chart.png")] == [1, 2]
    assert vis.get_attachment("chart.png")["id"] == second["id"]
    assert vis.get_attachment("chart.png", version=-2)["id"] == first["id"]
    assert vis.get_attachment(first["id"])["version"] == 1
    assert vis.read_attachment("chart.png", 1) == PNG
    assert vis.show_attachment(second) == {
        "id": second["id"],
        "filename": "chart.png",
        "media_type": "image/png",
        "size": len(PNG) + 1,
    }


def test_attach_sends_one_options_map_to_the_host(monkeypatch):
    sent = []

    class Host:
        def attach(self, options):
            sent.append(options)
            return json.dumps(
                {"id": "a1", "filename": options["filename"], "media-type": "x/y"}
            )

    monkeypatch.setattr(vis, "_host", Host())

    # `.txt` maps to text/plain on every Python version; `.md` does not before 3.13.
    assert vis.attach(b"data", "notes.txt", media_type="text/markdown") == {
        "id": "a1",
        "filename": "notes.txt",
        "media_type": "x/y",
    }
    assert sent == [
        {
            "base64": base64.b64encode(b"data").decode("ascii"),
            "filename": "notes.txt",
            "media_type": "text/markdown",
            "media_type_hint": "text/plain",
            "audience": "both",
        }
    ]


def test_attach_refuses_wrong_sources_and_options(tmp_path):
    with pytest.raises(TypeError, match="source must be a path"):
        vis.attach(object())
    with pytest.raises(IsADirectoryError):
        vis.attach(tmp_path)
    with pytest.raises(FileNotFoundError, match="no such file"):
        vis.attach(tmp_path / "missing.png")
    with pytest.raises(ValueError, match="audience"):
        vis.attach(PNG, "a.png", audience="everyone")
    with pytest.raises(ValueError, match="empty payload"):
        vis.attach(b"", "a.png")


def test_readers_name_an_unknown_target_and_a_non_image():
    vis.attach(b"text", "notes.txt")

    with pytest.raises(LookupError, match="unknown id or filename"):
        vis.get_attachment("other.txt")
    with pytest.raises(LookupError, match="has no version 3"):
        vis.read_attachment("notes.txt", 3)
    with pytest.raises(ValueError, match="not an image"):
        vis.show_attachment("notes.txt")
    with pytest.raises(TypeError, match="version must be an integer"):
        vis.get_attachment("notes.txt", "1")
