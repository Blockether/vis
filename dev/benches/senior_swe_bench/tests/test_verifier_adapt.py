from pathlib import Path
import importlib.util
import sys

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("verifier_adapt", ROOT / "verifier_adapt.py")
verifier_adapt = importlib.util.module_from_spec(spec)
sys.modules["verifier_adapt"] = verifier_adapt
spec.loader.exec_module(verifier_adapt)  # type: ignore[union-attr]


def test_adapt_dataset_patches_llm_utils_tool_choice(tmp_path):
    llm_utils = tmp_path / "tasks" / "task-a" / "tests" / "ssb_lib" / "llm_utils.py"
    llm_utils.parent.mkdir(parents=True)
    llm_utils.write_text(
        """from typing import Any
import os

def complete(  # noqa: PLR0913
    *,
    tool_choice: Any = None,
) -> Any:
    call_kwargs = {}
    if tool_choice is not None:
        call_kwargs["tool_choice"] = tool_choice
    return call_kwargs
"""
    )

    result = verifier_adapt.adapt_dataset(tmp_path, "required")

    patched = llm_utils.read_text()
    assert result["files"] == [{"path": str(llm_utils), "changed": True}]
    assert "SSB_OPENAI_COMPAT_TOOL_CHOICE" in patched
    assert 'call_kwargs["tool_choice"] = _vis_openai_compat_tool_choice(tool_choice)' in patched

    second = verifier_adapt.adapt_dataset(tmp_path, "required")

    assert second["files"] == [{"path": str(llm_utils), "changed": False}]
