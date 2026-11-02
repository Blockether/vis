"""Rules of the AI review request and answer."""

from vis_review.ai import AiFinding, AiReview, prompt_of, review
from vis_review.checks import Finding
from vis_review.journal import FileDiff, unified


def diff(path, old, new):
    return FileDiff(
        path, "modified", 1, 1, False, unified(path, "modified", old, new), "a", "b"
    )


def test_the_prompt_cuts_each_file_to_its_share_and_keeps_every_file():
    big = diff("big.py", "", "x\n" * 50_000)
    small = diff("small.py", "a\n", "b\n")
    text = prompt_of("/r", [big, small], budget=4000)
    assert "more characters of big.py cut" in text and "+++ b/small.py" in text
    assert len(text) < 8000


def test_the_prompt_lists_known_findings_and_files_without_lines():
    binary = FileDiff("x.png", "added", 0, 0, True, "Binary files differ", None, "s")
    text = prompt_of(
        "/r", [binary], [Finding("lint", "error", "a.py", 3, "F401: unused")]
    )
    assert "- a.py:3 lint: F401: unused" in text and "- x.png: added, binary" in text


def test_review_turns_answers_into_findings_on_the_changed_files():
    asked = []

    def ask(request, model):
        asked.append(model)
        return AiReview(
            summary=" Fine. ",
            findings=[
                AiFinding(path="b/a.py", line=2, level="error", message="Off by one."),
                AiFinding(path="other.py", line=9, level="info", message="Elsewhere."),
            ],
        )

    summary, found = review("/r", [diff("a.py", "a\n", "b\n")], ask)
    assert asked == [AiReview] and summary == "Fine."
    assert [(f.path, f.line, f.level, f.source) for f in found] == [
        ("a.py", 2, "error", "ai"),
        ("other.py", 0, "info", "ai"),
    ]


def test_review_of_no_change_asks_no_model():
    assert review("/r", [], lambda *_: (_ for _ in ()).throw(AssertionError)) == (
        "Nothing changed.",
        [],
    )


def test_the_answer_schema_has_no_external_reference():
    from blockether.vis.engine._agent import _ResponseContract

    _ResponseContract(AiReview)
