"""Protected paths — block file mutations on sensitive paths.

Demonstrates op hooks: a `phase="before"` hook over the foundation
file ops that can BLOCK a call with a reason the model reads as a
normal tool failure (so it routes around it — e.g. asks the user).

The hook callable receives {"op": <name>, "args": [...]} before the
op executes; returning `vis.block(reason)` refuses it, returning None
allows it. Ops are the sandbox tool names: write, patch, struct_patch,
move, copy, delete.
"""

import vis

PROTECTED = (".env", ".git/", "node_modules/", "secrets/")


def _guard(call):
    # Scanning every string argument keeps the example simple; file ops
    # take path strings (or dicts/lists of edits carrying path keys).
    for arg in vis.strings_of(call["args"]):
        for marker in PROTECTED:
            if marker in arg:
                return vis.block(
                    f'"{arg}" is protected; ask the user before touching it'
                )
    return None  # allow


vis.extension(
    name="protected-paths",
    description="Refuses write/patch/move/copy/delete on protected paths.",
    version="0.1.0",
    kind="guard",
    op_hooks=[
        vis.op_hook(
            ["write", "patch", "struct_patch", "move", "copy", "delete"],
            _guard,
            phase="before",
        ),
    ],
)
