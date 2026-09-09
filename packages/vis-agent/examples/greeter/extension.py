"""Vis entrypoint; business logic lives in vis_greeter, not this file."""

import blockether.vis.extension as vis
from vis_greeter import Greeter

vis.register(
    vis.Extension(
        name="vis-greeter",
        description="Typed greeting tools and an optional greeting procedure.",
        alias="greet",
        symbols=[vis.Symbol(Greeter(), name="greet")],
    )
)
