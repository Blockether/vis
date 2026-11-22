import blockether.vis.extension as vis
import pytest


def open_board(app):
    return None


def test_extension_carries_the_function_of_a_session_app_by_name():
    app = vis.SessionApp(
        "board", "Board", open_board, icon="review", surfaces={"session": "sidebar"}
    )
    spec = vis.Extension(
        name="board", description="Board fixture.", session_app=app
    )._spec()
    # The host finds the function by this name in the code of a new process.
    assert spec["session_app"] == {
        "marker": "session_app",
        "key": "board",
        "title": "Board",
        "open": f"{__name__}:open_board",
        "icon": "review",
        "surfaces": {"session": "sidebar"},
    }


def test_session_app_refuses_a_function_that_a_new_process_cannot_find():
    with pytest.raises(TypeError, match="module level"):
        vis.SessionApp("board", "Board", lambda app: None)


@pytest.mark.parametrize(
    ("kwargs", "error"),
    [
        ({"key": " "}, ValueError),
        ({"title": ""}, ValueError),
        ({"icon": "typo"}, ValueError),
        ({"surfaces": {"session": "typo"}}, TypeError),
    ],
)
def test_session_app_refuses_a_wrong_field(kwargs, error):
    fields = {"key": "board", "title": "Board", "open": open_board, **kwargs}
    with pytest.raises(error):
        vis.SessionApp(**fields)
