"""Register the global Board app and its tools."""

import blockether.vis.extension as vis

from vis_board import start, tools
from vis_board.app import BOARD


def open_board(app):
    """Open the Board app of all projects."""
    BOARD.open(open_board)


class _BoardTools:
    list_issues = staticmethod(
        vis.method(
            tools.list_issues,
            tag="observation",
            activity=vis.Activity(label="List board issues", show_start=False),
        )
    )
    create_issue = staticmethod(
        vis.method(
            tools.create_issue,
            tag="mutation",
            activity=vis.Activity(label="Make a board issue", show_start=False),
        )
    )
    change_issue = staticmethod(
        vis.method(
            tools.change_issue,
            tag="mutation",
            activity=vis.Activity(label="Change a board issue", show_start=False),
        )
    )
    move_issue = staticmethod(
        vis.method(
            tools.move_issue,
            tag="mutation",
            activity=vis.Activity(label="Move a board issue", show_start=False),
        )
    )
    delete_issue = staticmethod(
        vis.method(
            tools.delete_issue,
            tag="mutation",
            activity=vis.Activity(label="Delete a board issue", show_start=False),
        )
    )


vis.register_extension(
    vis.Extension(
        name="vis-board",
        description="Plan and track the issues of all projects through the SDLC flow.",
        alias="board",
        symbols=[vis.Symbol(_BoardTools(), name="board")],
        global_app=vis.GlobalApp("board", "Board", open_board, icon="board"),
        settings=start.SETTINGS,
    )
)
