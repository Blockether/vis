"""Verify and reproducibly pack prepared Laya release assets, without network access.

The prepared folders contain PROVENANCE.json, complete model/lock inventories and
license notices. This command checks every declared digest before writing a ZIP;
it does not train or download a model. Prepare and validate exports separately.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import zipfile
from pathlib import Path

_TIMESTAMP = (2025, 1, 1, 0, 0, 0)
_NAME = re.compile(r"[a-z0-9][a-z0-9+._-]*\.zip\Z")


def digest(path: Path) -> str:
    hash_ = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(4 * 1024 * 1024), b""):
            hash_.update(block)
    return hash_.hexdigest()


def inventory(source: Path) -> list[tuple[str, Path]]:
    """Reject unsafe paths and check every model file or pinned dependency wheel."""
    if not source.is_dir() or source.is_symlink():
        raise ValueError(f"Not a prepared bundle directory: {source}")
    metadata = json.loads((source / "PROVENANCE.json").read_text())
    kind = metadata.get("kind")
    if kind in {"inference", "training"}:
        if metadata.get("format") not in {"onnx", "safetensors"}:
            raise ValueError("Unknown model bundle format")
        if not metadata.get("revision") or not metadata.get("model"):
            raise ValueError("Model revision and name are required")
        if kind == "inference" and metadata.get("precision") != "fp32":
            raise ValueError("Only full FP32 inference bundles may be released")
        declared = metadata.get("files")
        mandatory = {"LICENSE.txt", "rl_agent_config.json", "tokenizer/tokenizer.json"}
        mandatory.add("model.onnx" if kind == "inference" else "model.safetensors")
    elif kind == "training-dependencies":
        if metadata.get("python") != "CPython 3.12":
            raise ValueError("Unsupported training interpreter")
        declared = {f"wheels/{wheel['wheel']}": wheel for wheel in metadata["wheels"]}
        mandatory = {"install.sh", "requirements.txt"}
        for wheel in metadata["wheels"]:
            if not wheel.get("license") or not wheel.get("source_url"):
                raise ValueError("A wheel lacks license or source provenance")
    else:
        raise ValueError("Unknown bundle kind")
    if not declared:
        raise ValueError("Empty bundle inventory")
    seen = set()
    result = []
    for path in sorted(source.rglob("*")):
        if path.is_symlink():
            raise ValueError(f"Unsafe symlink: {path}")
        if path.is_dir():
            continue
        if not path.is_file():
            raise ValueError(f"Unsafe file: {path}")
        name = path.relative_to(source).as_posix()
        if name == ".vis-verified":
            continue
        if (
            name in seen
            or name.startswith("/")
            or any(part in {".", "..", ""} for part in name.split("/"))
        ):
            raise ValueError(f"Unsafe archive path: {name}")
        seen.add(name)
        result.append((name, path))
    extras = (
        {"PROVENANCE.json", "LICENSE.txt"}
        if kind != "training-dependencies"
        else {"PROVENANCE.json", "README.md", "install.sh", "requirements.txt"}
        | {
            f"licenses/{package}-LICENSE.txt"
            for package in metadata.get("supplemental_licenses", {})
        }
    )
    if seen != set(declared) | extras:
        raise ValueError("Bundle contains an untracked or missing file")
    if not mandatory <= seen or not set(declared) <= seen:
        raise ValueError("A required file is missing")
    for name, item in declared.items():
        path = source / name
        if path.stat().st_size != item["bytes"] or digest(path) != item["sha256"]:
            raise ValueError(f"File digest disagrees with provenance: {name}")
    if kind == "training-dependencies":
        if len(declared) != len(metadata["wheels"]):
            raise ValueError("Duplicate wheel in provenance")
        for package, item in metadata.get("supplemental_licenses", {}).items():
            path = source / "licenses" / f"{package}-LICENSE.txt"
            if not path.is_file() or digest(path) != item["sha256"]:
                raise ValueError(f"Missing license notice: {package}")
    return result


def pack(source: Path, destination: Path) -> str:
    """Write fixed-order ZIP64 bytes, then publish only the fully written archive."""
    entries = inventory(source)
    store = (
        json.loads((source / "PROVENANCE.json").read_text())["kind"]
        == "training-dependencies"
    )
    if not _NAME.fullmatch(destination.name):
        raise ValueError("Bundle filename must be a safe .zip basename")
    if destination.exists():
        raise FileExistsError(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_name(destination.name + ".tmp")
    if temporary.exists():
        raise FileExistsError(temporary)
    try:
        with zipfile.ZipFile(temporary, "w", allowZip64=True) as archive:
            for name, path in entries:
                info = zipfile.ZipInfo(name, date_time=_TIMESTAMP)
                info.external_attr = 0o100644 << 16
                info.compress_type = (
                    zipfile.ZIP_STORED if store else zipfile.ZIP_DEFLATED
                )
                info._compresslevel = 1
                with (
                    archive.open(info, "w", force_zip64=True) as output,
                    path.open("rb") as input_,
                ):
                    shutil.copyfileobj(input_, output, 4 * 1024 * 1024)
        temporary.replace(destination)
    finally:
        temporary.unlink(missing_ok=True)
    return digest(destination)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument(
        "--bundle",
        action="append",
        nargs=2,
        metavar=("ZIP", "DIRECTORY"),
        required=True,
        help="Verified prepared bundle and release filename",
    )
    parser.add_argument(
        "--existing",
        action="append",
        type=Path,
        default=[],
        help="Unchanged speech release asset to retain",
    )
    args = parser.parse_args()
    output = args.output
    output.mkdir(parents=True, exist_ok=True)
    lines = []
    names = [name for name, _ in args.bundle] + [path.name for path in args.existing]
    if len(names) != len(set(names)) or any(
        not _NAME.fullmatch(name) for name, _ in args.bundle
    ):
        parser.error("Bundle names must be distinct safe ZIP basenames")
    for name, directory in args.bundle:
        checksum = pack(Path(directory), output / name)
        lines.append(f"{checksum}  {name}")
    for path in args.existing:
        if not path.is_file() or path.name == "SHA256SUMS.txt":
            parser.error("Only existing speech release payloads may be retained")
        target = output / path.name
        if path.resolve() != target.resolve():
            if target.exists():
                raise FileExistsError(target)
            shutil.copyfile(path, target)
        lines.append(f"{digest(target)}  {target.name}")
    (output / "SHA256SUMS.txt").write_text("\n".join(sorted(lines)) + "\n")


if __name__ == "__main__":
    main()
