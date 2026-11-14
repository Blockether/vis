"""Software factory: wait for a README, write its specification, check it, then ship it to GitHub."""

from typing import Literal

from pydantic import BaseModel, Field

from playground.workflows import activity, workflow


class Inspection(BaseModel):
    verdict: Literal["pass", "rework"] = Field(
        description="pass: a developer can build the product from SPEC.md alone. rework: SPEC.md misses a requirement of the README, states one wrongly, or has one that no test can check"
    )
    score: int = Field(ge=0, le=100, description="How much of the README is in SPEC.md, as a percentage")
    gaps: list[str] = Field(description="Each requirement that SPEC.md misses or states wrongly. Empty for pass")


class Shipment(BaseModel):
    branch: str
    commit: str
    url: str


@activity(title="README intake")
def intake(ctx):
    return ctx.receive("readme", "Give the README of the product", accept=".md,.txt")


@activity(title="Write the SPEC")
def specify(ctx, product: str, readme: str):
    return ctx.document(
        "SPEC.md",
        f"You are a systems analyst. Write the specification of the product {product!r} from its README. Use only facts from the README. "
        "Sections: Purpose, Actors, Components, Interfaces, Data, Functional requirements, Security requirements, Limits, Acceptance criteria. "
        "Give each requirement an identifier (FR-1, SR-1) and write it in one sentence that a test can check. "
        "Write at most 250 lines: join related facts in one requirement and do not repeat the README.\n\n"
        f"The README:\n\n{readme}",
    )


@activity(title="Quality gate")
def inspect(ctx) -> Inspection:
    return ctx.ask(
        "You are the quality inspector. Read your specification again with `read_attachment('SPEC.md').decode()`. "
        "Compare it with the README of the first request, section by section. Do not change the specification.",
        Inspection,
    )


@activity(title="Rework the SPEC")
def rework(ctx, inspection: Inspection):
    gaps = "\n".join(f"- {gap}" for gap in inspection.gaps)
    return ctx.document("SPEC.md", f"Correct the specification. Attach the complete new version, not a list of changes. Close each gap:\n{gaps}")


@activity(title="Ship to GitHub")
def ship(ctx, product: str) -> Shipment:
    return ctx.ask(
        "Put the specification in the repository. In one Python block, write the newest attachment to the project directory: "
        "`(project_root_path / 'SPEC.md').write_bytes(read_attachment('SPEC.md'))`. "
        f"Then call `playground.publish('Add the specification of {product}')` and report its result.",
        Shipment,
    )


@workflow(title="Software factory", description="Wait for a README. Write SPEC.md from it, check it against the README, then push it to GitHub.")
def software_factory(ctx, product: str = "Vis playground"):
    readme = intake(ctx)
    spec = specify(ctx, product, readme.text)
    inspection = inspect(ctx)
    if inspection.verdict == "rework":
        spec = rework(ctx, inspection)
    shipment = ship(ctx, product)
    ctx.note(f"Shipped {spec.name} as {shipment.commit[:12]} to {shipment.branch}")
    return {"verdict": inspection.verdict, "score": inspection.score, "commit": shipment.commit[:12], "url": shipment.url, "files": [spec.name]}
