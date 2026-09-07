"""Bundle the canonical contracts from a checkout or a self-contained sdist."""

from pathlib import Path

from hatchling.builders.hooks.plugin.interface import BuildHookInterface


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
