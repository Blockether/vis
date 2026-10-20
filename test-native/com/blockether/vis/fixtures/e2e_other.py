"""Phase 8 fixture for #405: a second extension that owns one session."""

import blockether.vis.extension as vis


def _render(*, result, error, **_):
    return vis.ActivityPresentation(
        "Other sessions", str(error) if error else "Created"
    )


class Other:
    """Sessions that only this extension owns."""

    @vis.method(
        tag="mutation",
        activity=vis.Activity(
            label="Create a session", show_start=False, render=_render
        ),
    )
    def create(self) -> dict:
        """Create a session that only this extension owns."""
        return {"session_id": vis.sessions.create().id}


vis.register_extension(
    vis.Extension(
        name="e2e-other",
        alias="e2e_other",
        description="Second owner for the Phase 8 session checks",
        symbols=[vis.Symbol(Other(), name="e2e_other")],
    )
)
