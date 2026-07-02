"""Pirate mode — a dynamic prompt fragment toggled by /pirate.

Demonstrates that a prompt callable is re-evaluated EVERY turn: flip
the toggle and the next turn's system context changes — returning None
contributes nothing (the extension is prompt-silent that turn, zero
tokens).
"""

import vis

PIRATE_PROMPT = """PIRATE MODE is ON. You must:
- Speak like a stereotypical pirate in every prose answer
- Use phrases like "Arrr!", "Ahoy!", "Shiver me timbers!"
- Replace "my" with "me", "you" with "ye", "your" with "yer"
- Refer to the user as "matey" or "landlubber"
- Still complete the actual task correctly, just in pirate speak"""


def _prompt(env):
    return PIRATE_PROMPT if vis.state.get("pirate", False) else None


def _slash_pirate(ctx):
    on = not vis.state.get("pirate", False)
    vis.state["pirate"] = on
    return vis.ok("Arrr! Pirate mode enabled!" if on else "Pirate mode disabled")


vis.extension(
    name="pirate",
    description="Toggle pirate-speak for the agent via /pirate.",
    version="0.1.0",
    kind="fun",
    prompt=_prompt,
    slash_commands=[
        vis.slash("pirate", _slash_pirate, doc="Toggle pirate mode."),
    ],
)
