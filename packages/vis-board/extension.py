"""Register the global Board app and its tools."""

import blockether.vis.extension as vis

from vis_board import start, tools
from vis_board.app import BOARD


def open_board(app):
    """Open the Board app of all projects."""
    BOARD.open(open_board)


vis.register_extension(
    vis.Extension(
        name="vis-board",
        description="Plan and track the issues of all projects through the SDLC flow.",
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
        global_app=vis.GlobalApp("board", "Board", open_board, icon="board"),
        settings=start.SETTINGS,
    )
)
