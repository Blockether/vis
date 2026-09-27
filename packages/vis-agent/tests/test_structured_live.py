"""Opt-in structured output checks against an installed runtime and real model."""

import os

import blockether.vis.engine as engine
import pytest
from pydantic import BaseModel, Field, field_validator

README = "Disposable SDK test. Do not change files.\n"

live = pytest.mark.skipif(
    os.environ.get("VIS_TEST_LIVE_STRUCTURED") != "1",
    reason="live structured model call requires VIS_TEST_LIVE_STRUCTURED=1",
)


class Quote(BaseModel):
    cents: int = Field(ge=0)
    express: bool


@pytest.fixture
def project(tmp_path):
    work = tmp_path / "disposable-project"
    work.mkdir()
    (work / "README.md").write_text(README)
    return work


@live
def test_live_structured_result(project):
    with engine.Agent(project) as agent:
        quote = agent.run(
            "For a 1000 gram parcel, quote 1200 cents with express false. "
            "Do not inspect or change files.",
            response_model=Quote,
            timeout=180,
        )

    assert quote == Quote(cents=1200, express=False)
    assert (project / "README.md").read_text() == README


@live
def test_live_structured_answer_is_corrected_from_feedback(project):
    answers = []

    class Codeword(BaseModel):
        word: str

        @field_validator("word")
        @classmethod
        def known(cls, value):
            answers.append(value)
            if value != "heliotrope":
                raise ValueError("the codeword is heliotrope")
            return value

    with engine.Agent(project) as agent:
        codeword = agent.run(
            "Choose one English word as a codeword. Do not inspect or change files.",
            response_model=Codeword,
            timeout=240,
        )

    assert codeword.word == "heliotrope"
    assert len(answers) >= 2, "only validator feedback reveals the codeword"
    assert (project / "README.md").read_text() == README


@live
def test_live_structured_error_reports_every_attempt(project):
    class Refused(BaseModel):
        amount: int

        @field_validator("amount")
        @classmethod
        def refuse(cls, value):
            raise ValueError("no amount is accepted")

    with engine.Agent(project) as agent:
        with pytest.raises(engine.StructuredOutputError) as caught:
            agent.run(
                "Reply with the amount 5. Do not inspect or change files.",
                response_model=Refused,
                max_corrections=1,
                timeout=240,
            )

    error = caught.value
    assert str(error).startswith("Refused answer is invalid after 2 attempts")
    assert [attempt["turn"]["status"] for attempt in error.attempts] == [
        "completed",
        "completed",
    ]
    first = error.attempts[0]["errors"]
    assert [(item["path"], item["source"]) for item in first] == [
        ("$.amount", "pydantic")
    ]
    assert "no amount is accepted" in first[0]["message"]
    assert error.errors and error.turn is error.attempts[-1]["turn"]
    assert (project / "README.md").read_text() == README
