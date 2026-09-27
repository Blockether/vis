"""Opt-in structured output check against an installed runtime and real model."""

import os

import blockether.vis.engine as engine
import pytest
from pydantic import BaseModel, Field


class Quote(BaseModel):
    cents: int = Field(ge=0)
    express: bool


@pytest.mark.skipif(
    os.environ.get("VIS_TEST_LIVE_STRUCTURED") != "1",
    reason="live structured model call requires VIS_TEST_LIVE_STRUCTURED=1",
)
def test_live_structured_result(tmp_path):
    work = tmp_path / "disposable-project"
    work.mkdir()
    readme = work / "README.md"
    readme.write_text("Disposable SDK test. Do not change files.\n")
    before = readme.read_bytes()

    with engine.Agent(work) as agent:
        quote = agent.run_structured(
            "For a 1000 gram parcel, quote 1200 cents with express false. "
            "Do not inspect or change files.",
            response_model=Quote,
            timeout=180,
        )

    assert quote == Quote(cents=1200, express=False)
    assert readme.read_bytes() == before
