"""Vis entrypoint; business logic lives in vis_greeter, not this file."""

import blockether.vis.extension as vis
from vis_greeter import Greeter


def greeting_activity(*, phase, result, **_):
    """Show the greeting and its character count, not the result object's repr."""
    if phase != "success":
        return None
    return vis.ActivityPresentation(
        "Greet person",
        f"{result.characters} characters",
        (vis.ActivityText(result.text[:1000]),),
    )


Greeter.hello = vis.method(
    activity=vis.Activity(
        label="Greet person", show_start=False, render=greeting_activity
    )
)(Greeter.hello)

vis.register(
    vis.Extension(
        name="vis-greeter",
        description="Typed greeting tools and an optional greeting procedure.",
        alias="greet",
        symbols=[vis.Symbol(Greeter(), name="greet")],
    )
)
