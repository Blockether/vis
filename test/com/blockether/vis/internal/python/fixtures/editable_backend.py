"""Offline PEP 517/660 backend for the editable integration regression."""

from pathlib import Path
from zipfile import ZipFile

NAME = "vis_editable_fixture-0.0.1-py3-none-any.whl"
DIST = "vis_editable_fixture-0.0.1.dist-info/"


def _build(wheel_directory, editable):
    root = Path(__file__).parent
    with ZipFile(Path(wheel_directory) / NAME, "w") as wheel:
        wheel.writestr(
            DIST + "METADATA",
            "Metadata-Version: 2.1\nName: vis-editable-fixture\nVersion: 0.0.1\n",
        )
        wheel.writestr(
            DIST + "WHEEL",
            "Wheel-Version: 1.0\nRoot-Is-Purelib: true\nTag: py3-none-any\n",
        )
        wheel.writestr(DIST + "RECORD", "")
        if editable:
            wheel.writestr("fixture.pth", str(root / "src") + "\n")
        else:
            for file in (root / "src").rglob("*.py"):
                wheel.write(file, str(file.relative_to(root / "src")))
    return NAME


def build_wheel(wheel_directory, config_settings=None, metadata_directory=None):
    return _build(wheel_directory, False)


def build_editable(wheel_directory, config_settings=None, metadata_directory=None):
    return _build(wheel_directory, True)
