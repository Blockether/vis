"""Pictures in apps: `vis.ui.image` reads bytes or a file, and compares two pictures.

The review app shows a changed picture as its old and new version.
"""

import json
import struct
import zlib
from pathlib import Path

import blockether.vis.extension as vis
import pytest
from blockether.vis import _contracts
from blockether.vis._contracts import validate


def png(width, height):
    rows = b"".join(b"\x00" + b"\x00\x00\x00" * width for _ in range(height))

    def chunk(kind, data):
        crc = struct.pack(">I", zlib.crc32(kind + data))
        return struct.pack(">I", len(data)) + kind + data + crc

    header = struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)
    return (
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", header)
        + chunk(b"IDAT", zlib.compress(rows))
        + chunk(b"IEND", b"")
    )


GIF = b"GIF89a" + struct.pack("<HH", 7, 9) + b"\x00\x00\x00;"
# A JFIF segment first, so the size comes from a later frame segment.
JPEG = (
    b"\xff\xd8\xff\xe0"
    + struct.pack(">H", 16)
    + b"JFIF\x00"
    + b"\x00" * 9
    + b"\xff\xc0"
    + struct.pack(">HBHHB", 17, 8, 33, 44, 3)
    + b"\x00" * 9
    + b"\xff\xd9"
)
WEBP = (
    b"RIFF"
    + struct.pack("<I", 30)
    + b"WEBPVP8X"
    + struct.pack("<I", 10)
    + b"\x00" * 4
    + (99).to_bytes(3, "little")
    + (49).to_bytes(3, "little")
)


@pytest.mark.parametrize(
    ("data", "media_type", "size"),
    [
        (png(3, 2), "image/png", (3, 2)),
        (GIF, "image/gif", (7, 9)),
        (JPEG, "image/jpeg", (44, 33)),
        (WEBP, "image/webp", (100, 50)),
    ],
)
def test_image_reads_its_type_and_size_from_the_bytes(data, media_type, size):
    node = vis.ui.image("shot", data, alt="A screenshot")
    assert node["media_type"] == media_type
    assert (node["width"], node["height"]) == size
    validate("view", "component", node)


def test_image_with_before_compares_two_pictures_from_files(tmp_path):
    old, new = tmp_path / "old.png", tmp_path / "new.gif"
    old.write_bytes(png(5, 4))
    new.write_bytes(GIF)
    node = vis.ui.image("logo", new, alt="The logo", label="logo.gif", before=old)
    assert node["before"]["media_type"] == "image/png"
    assert (node["before"]["width"], node["before"]["height"]) == (5, 4)
    validate("view", "component", node)
    recorder = vis._LiveRecorder(object())
    recorder.host_live(json.dumps({"op": "open", "view": vis.ui.app("Pics", node)}))
    assert recorder.node("logo")["before"] == node["before"]


def test_image_refuses_a_large_picture_and_bytes_of_another_type(tmp_path):
    most = _contracts.definition("view", "picture")["properties"]["data"]["maxLength"]
    large = png(1, 1) + b"\x00" * (most // 4 * 3)
    with pytest.raises(ValueError, match="at most"):
        vis.ui.image("big", large, alt="Big")
    (tmp_path / "big.png").write_bytes(large)
    with pytest.raises(ValueError, match="at most"):
        vis.ui.image("big", tmp_path / "big.png", alt="Big")
    with pytest.raises(ValueError, match="not a picture"):
        vis.ui.image("text", b"hello", alt="Text")
    with pytest.raises(ValueError, match="not image/png"):
        vis.ui.image("gif", GIF, alt="Gif", media_type="image/png")
    with pytest.raises(ValueError, match="alt"):
        vis.ui.image("gif", GIF, alt=" ")


FIXTURES = Path(__file__).parents[2] / "vis-contract/resources/vis-contract/fixtures"
PICTURES = json.loads((FIXTURES / "app-pictures.json").read_text())
# The words of each refused rule, as `read_picture` (and the engine) gives them.
REASONS = {
    # The schema enum refuses it first where a view is checked as a whole.
    "media-type": "needs a media type|oneOf rule",
    "magic": "is not image/",
    "base64": "base64",
    "size-unreadable": "does not give its size",
    "pixels": "pixels; the most is",
    "declared-size": "differs from the header size",
}


@pytest.mark.parametrize("case", PICTURES["accepted"], ids=lambda c: c["name"])
def test_a_fixture_picture_passes_with_its_header_size(case):
    # One rule for the engine and the SDK (audit M2): app-pictures.json.
    read = _contracts.read_picture(case["picture"])
    assert (read["width"], read["height"]) == (case["width"], case["height"])
    node = {"id": "p", "type": "image", "alt": "A picture", **case["picture"]}
    recorder = vis._LiveRecorder(object())
    recorder.host_live(json.dumps({"op": "open", "view": vis.ui.app("Pics", node)}))
    assert recorder.node("p")["width"] == case["width"]


@pytest.mark.parametrize("case", PICTURES["refused"], ids=lambda c: c["name"])
def test_a_fixture_picture_is_refused_by_its_rule(case):
    with pytest.raises(ValueError, match=REASONS[case["rule"]]):
        _contracts.read_picture(case["picture"])
    # The test recorder and a `before` mapping refuse it too (audit M6).
    node = {"id": "p", "type": "image", "alt": "A picture", **case["picture"]}
    recorder = vis._LiveRecorder(object())
    with pytest.raises(RuntimeError, match=REASONS[case["rule"]]):
        recorder.host_live(json.dumps({"op": "open", "view": vis.ui.app("P", node)}))
    with pytest.raises(ValueError, match=REASONS[case["rule"]]):
        vis.ui.image("p", png(1, 1), alt="New", before=case["picture"])
    good = vis.ui.image("p", png(1, 1), alt="New")
    with pytest.raises(RuntimeError, match=REASONS[case["rule"]]):
        recorder.host_live(
            json.dumps(
                {
                    "op": "open",
                    "view": vis.ui.app("P", dict(good, before=case["picture"])),
                }
            )
        )


def test_picture_data_over_the_length_bound_is_refused_before_decoding():
    # Review audit (goal 4b41c770): a valid picture with 4 MB of padding was decoded
    # and accepted, also as the `before` mapping of vis.ui.image.
    png_data = next(
        c["picture"]["data"]
        for c in PICTURES["accepted"]
        if c["picture"]["media_type"] == "image/png"
    )
    most = _contracts.definition("view", "picture")["properties"]["data"]["maxLength"]
    data = png_data + "AAAA" * ((most - len(png_data)) // 4 + 1)
    with pytest.raises(ValueError, match="characters; the most is"):
        _contracts.read_picture({"media_type": "image/png", "data": data})


def test_the_pictures_of_one_view_stay_under_the_view_bound():
    most = _contracts.definition("view", "live_view")["x-vis-max-picture-bytes"]
    picture = png(1, 1) + b"\x00" * (2 * 1024 * 1024 - 100)
    count = most // len(picture) + 1
    recorder = vis._LiveRecorder(object())
    recorder.host_live(
        json.dumps({"op": "open", "view": vis.ui.app("P", vis.ui.button("b", "B"))})
    )
    for n in range(count - 1):
        node = vis.ui.image(f"p{n}", picture, alt="A picture")
        recorder._patch_ops([{"op": "add-node", "node_spec": node}])
    over = vis.ui.image("last", picture, alt="A picture")
    with pytest.raises(RuntimeError, match="the most is"):
        recorder._patch_ops([{"op": "add-node", "node_spec": over}])
    # Review audit (goal 4b41c770): the refused picture stayed in the view.
    ids = [node.get("id") for node in recorder._view["nodes"]]
    assert "last" not in ids and f"p{count - 2}" in ids
