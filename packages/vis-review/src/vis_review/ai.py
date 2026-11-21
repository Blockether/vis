"""An AI review of a change: one model session reads the patch and answers findings.

`ask(request, response_model)` runs the model. Each door gives it: an extension
uses `vis.run`, an SDK program `Session.run` of a session it creates. The reviewer
reads files under the root when the patch is not enough, and it changes nothing.
"""

from __future__ import annotations

from collections.abc import Callable, Iterable, Sequence
from typing import Literal

from pydantic import BaseModel, Field

from vis_review.checks import Finding
from vis_review.journal import FileDiff

# Characters of patch text one review request carries; larger files are cut.
MOST_PROMPT_CHARS = 120_000


class AiFinding(BaseModel):
    """One problem the reviewer found."""

    path: str = Field(description="File path exactly as the patch names it after b/.")
    line: int = Field(
        ge=0, description="1-based line in the new file; 0 for the whole file."
    )
    level: Literal["error", "warning", "info"] = Field(
        description="error: a bug or a broken contract; warning: a risk; info: a suggestion."
    )
    message: str = Field(
        description="The problem and the fix, in one or two sentences."
    )


class AiReview(BaseModel):
    """The review of one change."""

    summary: str = Field(
        description="What the change does and whether it is safe, in at most 3 sentences."
    )
    findings: list[AiFinding] = Field(
        description="Each new problem; empty when there is none."
    )
    still_open: list[int] = Field(
        default_factory=list,
        description="The number of each earlier finding that the code still has.",
    )


def prompt_of(
    root: str,
    diffs: Sequence[FileDiff],
    findings: Iterable[Finding] = (),
    budget: int = MOST_PROMPT_CHARS,
    earlier: Sequence[Finding] = (),
) -> str:
    """The review request: the rules, the known findings, the numbered findings
    `earlier` of the last review, and the patch within `budget`."""
    shown = [diff for diff in diffs if diff.patch and not diff.is_binary]
    share = max(2000, budget // max(1, len(shown)))
    parts = []
    for diff in shown:
        patch = diff.patch
        if len(patch) > share:
            patch = (
                patch[:share]
                + f"\n[... {len(diff.patch) - share} more characters of {diff.path} cut]\n"
            )
        parts.append(patch)
    others = [
        f"- {d.path}: {d.status}, {d.note or 'binary'}" for d in diffs if d not in shown
    ]
    known = [f"- {f.path}:{f.line} {f.check}: {f.message}" for f in findings]
    again = [
        f"{n}. {f.path}:{f.line} {f.level}: {f.message}"
        for n, f in enumerate(earlier, 1)
    ]
    return "\n".join(
        [
            "Review this change like a senior engineer before it is merged.",
            f"The files are under {root}. Read a file there when the patch is not enough.",
            "Do not change any file and do not run commands that change files.",
            "Report bugs, broken contracts, missing error handling, security problems,",
            "missing tests for changed behaviour and code that does not match nearby code.",
            "Do not report style that a formatter fixes, or the findings listed as known.",
            "Give each finding the path after b/ and the line in the new file.",
            "",
            "Known findings:" if known else "Known findings: none",
            *known,
            *(
                [
                    "",
                    "Earlier findings of this review. Check each one again. Put its number",
                    "in still_open when the code still has the problem, and do not report",
                    "it again as a new finding:",
                    *again,
                ]
                if again
                else []
            ),
            *(["", "Files without a line diff:", *others] if others else []),
            "",
            "Patch:",
            *parts,
        ]
    )


def review(
    root: str,
    diffs: Sequence[FileDiff],
    ask: Callable[[str, type[BaseModel]], BaseModel],
    findings: Iterable[Finding] = (),
    earlier: Sequence[Finding] = (),
) -> tuple[str, list[Finding]]:
    """The summary and the findings of one AI review of `diffs`: each finding of
    `earlier` that the code still has, unchanged, then the new findings."""
    if not diffs:
        return "Nothing changed.", []
    answer = ask(prompt_of(root, diffs, findings, earlier=earlier), AiReview)
    if not isinstance(answer, AiReview):
        answer = AiReview.model_validate(answer)
    paths = {diff.path for diff in diffs}
    numbers = dict.fromkeys(n for n in answer.still_open if 1 <= n <= len(earlier))
    found = [earlier[n - 1] for n in numbers]
    for item in answer.findings:
        path = item.path.removeprefix("b/").removeprefix("./")
        finding = Finding(
            "ai",
            item.level,
            path,
            item.line if path in paths else 0,
            item.message.strip() or "No reason given.",
            "ai",
        )
        if finding not in found:
            found.append(finding)
    return answer.summary.strip(), found
