import blockether.vis.extension as vis


def greet(name: str) -> str:
    """Return a greeting for a name."""
    return f"Hello, {name}!"


vis.register_extension(
    vis.Extension(
        name="vis-greeter",
        description="Small greeting tools for Vis.",
        alias="greeter",
        symbols=[vis.Symbol(greet)],
    )
)
