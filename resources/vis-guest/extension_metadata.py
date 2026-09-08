"""Read PEP 723 extension declarations without executing the extension."""

import re
import sys
import tomllib


def extension_metadata(source):
    """Return validated dependencies and import roots for the embedded interpreter."""
    starts = list(re.finditer(r"(?m)^# /// script\r?$", source))
    if not starts:
        return {"dependencies": [], "source_paths": []}
    if len(starts) != 1:
        raise ValueError("extension must contain at most one PEP 723 script block")
    lines = []
    for line in source[starts[0].end() :].splitlines()[1:]:
        if line == "# ///":
            break
        if line != "#" and not line.startswith("# "):
            raise ValueError("unterminated PEP 723 script block")
        lines.append(line[2:] if line.startswith("# ") else "")
    else:
        raise ValueError("unterminated PEP 723 script block")
    try:
        metadata = tomllib.loads("\n".join(lines))
    except tomllib.TOMLDecodeError:
        raise ValueError("invalid TOML in PEP 723 script block") from None
    if metadata.keys() - {"dependencies", "requires-python", "tool"}:
        raise ValueError("unknown PEP 723 script metadata field")
    dependencies = metadata.get("dependencies", [])
    if not isinstance(dependencies, list) or not all(
        isinstance(x, str) for x in dependencies
    ):
        raise ValueError("script dependencies must be a list of requirement strings")
    if dependencies or "requires-python" in metadata:
        from pip._vendor.packaging.requirements import InvalidRequirement, Requirement
        from pip._vendor.packaging.specifiers import InvalidSpecifier, SpecifierSet

        for dependency in dependencies:
            try:
                requirement = Requirement(dependency)
            except InvalidRequirement:
                raise ValueError("invalid extension dependency requirement") from None
            if requirement.url:
                raise ValueError(
                    "extension dependencies must use index packages, not URLs or paths"
                )
        if "requires-python" in metadata:
            version = metadata["requires-python"]
            if not isinstance(version, str) or not version.strip():
                raise ValueError("requires-python must be a nonempty version specifier")
            try:
                supported = SpecifierSet(version).contains(
                    ".".join(map(str, sys.version_info[:3]))
                )
            except InvalidSpecifier:
                raise ValueError("invalid requires-python specifier") from None
            if not supported:
                raise ValueError(
                    "extension requires a different Python version than the embedded runtime"
                )
    tool = metadata.get("tool", {})
    if not isinstance(tool, dict):
        raise ValueError("script tool metadata must be a table")
    if tool.get("uv"):
        raise ValueError(
            "put uv settings in pyproject.toml and select it with tool.vis.project"
        )
    vis = tool.get("vis", {})
    if not isinstance(vis, dict) or vis.keys() - {"source_paths", "project"}:
        raise ValueError("tool.vis accepts only source_paths and project")
    project = vis.get("project")
    if "project" in vis and (not isinstance(project, str) or not project.strip()):
        raise ValueError("tool.vis.project must be a nonempty project directory")
    if project and dependencies:
        raise ValueError(
            "declare project dependencies in pyproject.toml, not the script"
        )
    paths = vis.get("source_paths", [])
    if not isinstance(paths, list) or not all(
        isinstance(x, str) and x.strip() for x in paths
    ):
        raise ValueError(
            "tool.vis.source_paths must be a list of nonempty directory paths"
        )
    return {"dependencies": dependencies, "source_paths": paths, "project": project}
