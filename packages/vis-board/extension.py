"""Register the Board session app and its tools."""

import blockether.vis.extension as vis

from vis_board import tools
from vis_board.app import BOARDS, SURFACES


def open_board(app):
    """Open the Board app in the current workspace."""
    if session_id := app.get("session_id"):
        BOARDS.open(session_id, vis.workspace_root(), open_board)


vis.register_extension(
    vis.Extension(
        name="vis-board",
        description="Plan and track the issues of a workspace.",
        alias="board",
        symbols=[
            vis.Symbol(
                fn, tag=tag, activity=vis.Activity(label=label, show_start=False)
            )
            for fn, tag, label in (
                (tools.list_issues, "observation", "List board issues"),
                (tools.create_issue, "mutation", "Make a board issue"),
                (tools.change_issue, "mutation", "Change a board issue"),
                (tools.move_issue, "mutation", "Move a board issue"),
                (tools.delete_issue, "mutation", "Delete a board issue"),
            )
        ],
        session_app=vis.SessionApp(
            "board",
            "Board",
            open_board,
            icon="board",
            surfaces=SURFACES,
            roles=("leader",),
        ),
    )
)
