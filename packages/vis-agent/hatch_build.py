"""Bundle the canonical contracts and the release tag of the matching Vis engine."""

import os
import re
import tempfile
from pathlib import Path

from hatchling.builders.hooks.plugin.interface import BuildHookInterface

# A beta SDK (X.Y.Z.devN) cannot name its engine from its version, so CI records it.
_TAG = re.compile(r"^(v\d+\.\d+\.\d+|beta-[0-9a-f]{40})$")


class ContractResources(BuildHookInterface):
    def initialize(self, version, build_data):
        root = Path(self.root)
        source = root / "contracts"
        if not source.is_dir():
            source = root.parent / "vis-contract/resources/vis-contract"
        if not source.is_dir():
            raise FileNotFoundError("canonical Vis contract resources are missing")
        destination = (
            "blockether/vis/_data" if self.target_name == "wheel" else "contracts"
        )
        build_data["force_include"][str(source)] = destination
        self._release_tag(root, build_data)

    def _release_tag(self, root, build_data):
        """Record VIS_SDK_RELEASE_TAG, or the tag of the sdist this wheel comes from."""
        tag = os.environ.get("VIS_SDK_RELEASE_TAG", "").strip()
        recorded = root / "release-tag"
        if not tag and recorded.is_file():
            tag = recorded.read_text(encoding="utf-8").strip()
        if not tag:
            return
        if not _TAG.match(tag):
            raise ValueError(f"VIS_SDK_RELEASE_TAG={tag!r} is not vX.Y.Z or beta-<sha>")
        written = Path(tempfile.mkdtemp(prefix="vis-sdk-tag-")) / "release-tag"
        written.write_text(f"{tag}\n", encoding="utf-8")
        destination = (
            "blockether/vis/engine/release-tag"
            if self.target_name == "wheel"
            else "release-tag"
        )
        build_data["force_include"][str(written)] = destination
