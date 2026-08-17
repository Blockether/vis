"""Protected paths — refuse every touch of a sensitive path, in any language.

Demonstrates a GATE op hook. `fs_access` is asked BEFORE any path is
opened, and it is asked under the interpreter itself: `open(p, "w")`,
`Path.unlink`, `shutil.move` and `os.rename` in python_execution reach it
exactly as `patch` does. So a rule written here
cannot be routed around by picking a different tool or a different language.

The callable receives {"operation": "file-read" | "file-write", "path":
<absolute path>}; returning `vis.block(reason)` refuses with that sentence
(the model reads it as a normal tool failure and asks the user), returning
None allows. A gate fails CLOSED: an error inside this function refuses the
operation rather than opening the path.
"""

import os

import vis

# Written against the workspace, matched against the absolute path the
# operation actually resolved to — a symlink or "../" cannot dodge it.
READ_WRITE_DENIED = (".env", "secrets/", "node_modules/")
WRITE_DENIED = (".git/",)


def _relative(path):
    try:
        return os.path.relpath(path, os.getcwd()).replace(os.sep, "/")
    except ValueError:  # a different drive; not our business
        return None


def _guard(access):
    rel = _relative(access["path"])
    if rel is None or rel.startswith("../"):
        return None  # outside the workspace: not this extension's business
    denied = READ_WRITE_DENIED
    if access["operation"].endswith("-write"):
        denied = denied + WRITE_DENIED
    segments = rel.split("/")
    for marker in denied:
        if marker.rstrip("/") in segments:
            return vis.block(
                f'"{rel}" is protected by the protected-paths extension; '
                "ask the user before touching it"
            )
    return None  # allow


vis.extension(
    name="protected-paths",
    description="Refuses reads and writes of protected paths, in every language.",
    version="0.1.0",
    kind="guard",
    op_hooks=[
        vis.op_hook(["fs_access"], _guard),
    ],
)
