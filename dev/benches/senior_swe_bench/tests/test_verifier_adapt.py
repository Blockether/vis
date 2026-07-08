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
import json
import os

def tool_call_args(response: Any, name: str | None = None) -> dict[str, Any] | None:
    try:
        message = response.choices[0].message
    except (AttributeError, IndexError):
        return None
    for call in getattr(message, "tool_calls", None) or []:
        fn = getattr(call, "function", None)
        if fn is None:
            continue
        if name is None or fn.name == name:
            try:
                return json.loads(fn.arguments)
            except (TypeError, json.JSONDecodeError):
                return None
    return None


def complete(  # noqa: PLR0913
    *,
    model: str = "",
    messages: list[dict[str, Any]] | None = None,
    max_tokens: int = 0,
    tools: list[dict[str, Any]] | None = None,
    tool_choice: Any = None,
    system: str | None = None,
    thinking_budget: int = 0,
    attempts: int = 1,
    **kwargs: Any,
) -> Any:
    call_kwargs = {"model": model, "messages": messages or [], "max_tokens": max_tokens}
    if tools is not None:
        call_kwargs["tools"] = tools
    if tool_choice is not None:
        call_kwargs["tool_choice"] = tool_choice
    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    return call_kwargs
"""
    )

    result = verifier_adapt.adapt_dataset(tmp_path, "required")

    patched = llm_utils.read_text()
    assert result["files"] == [{"path": str(llm_utils), "changed": True}]
    assert result["env"]["SSB_OPENAI_COMPAT_RESPONSE_FORMAT"] == "1"
    assert result["response_format_compat"] == "json_schema"
    assert "SSB_OPENAI_COMPAT_TOOL_CHOICE" in patched
    assert "SSB_OPENAI_COMPAT_PARSE_CONTENT_JSON" in patched
    assert "SSB_OPENAI_COMPAT_RESPONSE_FORMAT" in patched
    assert 'call_kwargs["tool_choice"] = _vis_openai_compat_tool_choice(tool_choice)' in patched
    assert "response_format = _vis_forced_tool_response_format(tools, tool_choice)" in patched
    assert "return _vis_content_json_args(response, name)" in patched

    second = verifier_adapt.adapt_dataset(tmp_path, "required")

    assert second["files"] == [{"path": str(llm_utils), "changed": False}]


def test_adapted_llm_utils_parses_plain_json_for_forced_emit(monkeypatch, tmp_path):
    llm_utils = tmp_path / "tasks" / "task-a" / "tests" / "ssb_lib" / "llm_utils.py"
    llm_utils.parent.mkdir(parents=True)
    llm_utils.write_text(
        """from typing import Any
import json
import os

def tool_call_args(response: Any, name: str | None = None) -> dict[str, Any] | None:
    try:
        message = response.choices[0].message
    except (AttributeError, IndexError):
        return None
    for call in getattr(message, "tool_calls", None) or []:
        fn = getattr(call, "function", None)
        if fn is None:
            continue
        if name is None or fn.name == name:
            try:
                return json.loads(fn.arguments)
            except (TypeError, json.JSONDecodeError):
                return None
    return None


def complete(  # noqa: PLR0913
    *,
    model: str = "",
    messages: list[dict[str, Any]] | None = None,
    max_tokens: int = 0,
    tools: list[dict[str, Any]] | None = None,
    tool_choice: Any = None,
    system: str | None = None,
    thinking_budget: int = 0,
    attempts: int = 1,
    **kwargs: Any,
) -> Any:
    call_kwargs = {"model": model, "messages": messages or [], "max_tokens": max_tokens}
    if tools is not None:
        call_kwargs["tools"] = tools
    if tool_choice is not None:
        call_kwargs["tool_choice"] = tool_choice
    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    return call_kwargs
"""
    )
    verifier_adapt.adapt_dataset(tmp_path, "required")

    spec = importlib.util.spec_from_file_location("patched_llm_utils", llm_utils)
    patched = importlib.util.module_from_spec(spec)
    sys.modules["patched_llm_utils"] = patched
    spec.loader.exec_module(patched)  # type: ignore[union-attr]

    class Message:
        content = '```json\n{"criteria":[{"name":"fast-tags","score":1,"reason":"ok"}]}\n```'
        tool_calls = []

    class Choice:
        message = Message()

    class Response:
        choices = [Choice()]

    monkeypatch.setenv("SSB_OPENAI_COMPAT_PARSE_CONTENT_JSON", "1")

    assert patched.tool_call_args(Response(), "submit_scores") == {
        "criteria": [{"name": "fast-tags", "score": 1, "reason": "ok"}],
    }


def test_adapted_llm_utils_uses_response_format_for_forced_single_tool(monkeypatch, tmp_path):
    llm_utils = tmp_path / "tasks" / "task-a" / "tests" / "ssb_lib" / "llm_utils.py"
    llm_utils.parent.mkdir(parents=True)
    llm_utils.write_text(
        """from typing import Any
import json
import os

def tool_call_args(response: Any, name: str | None = None) -> dict[str, Any] | None:
    try:
        message = response.choices[0].message
    except (AttributeError, IndexError):
        return None
    for call in getattr(message, "tool_calls", None) or []:
        fn = getattr(call, "function", None)
        if fn is None:
            continue
        if name is None or fn.name == name:
            try:
                return json.loads(fn.arguments)
            except (TypeError, json.JSONDecodeError):
                return None
    return None


def complete(  # noqa: PLR0913
    *,
    model: str = "",
    messages: list[dict[str, Any]] | None = None,
    max_tokens: int = 0,
    tools: list[dict[str, Any]] | None = None,
    tool_choice: Any = None,
    system: str | None = None,
    thinking_budget: int = 0,
    attempts: int = 1,
    **kwargs: Any,
) -> Any:
    call_kwargs = {"model": model, "messages": messages or [], "max_tokens": max_tokens}
    if tools is not None:
        call_kwargs["tools"] = tools
    if tool_choice is not None:
        call_kwargs["tool_choice"] = tool_choice
    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    return call_kwargs
"""
    )
    verifier_adapt.adapt_dataset(tmp_path, "required")

    spec = importlib.util.spec_from_file_location("patched_llm_utils_response_format", llm_utils)
    patched = importlib.util.module_from_spec(spec)
    sys.modules["patched_llm_utils_response_format"] = patched
    spec.loader.exec_module(patched)  # type: ignore[union-attr]

    tool = {
        "type": "function",
        "function": {
            "name": "submit_scores",
            "parameters": {
                "type": "object",
                "properties": {"criteria": {"type": "array"}},
            },
        },
    }

    monkeypatch.setenv("SSB_OPENAI_COMPAT_TOOL_CHOICE", "required")
    monkeypatch.setenv("SSB_OPENAI_COMPAT_RESPONSE_FORMAT", "1")

    call_kwargs = patched.complete(
        model="openai/local",
        messages=[],
        max_tokens=128,
        tools=[tool],
        tool_choice={"type": "function", "function": {"name": "submit_scores"}},
    )

    assert "tools" not in call_kwargs
    assert "tool_choice" not in call_kwargs
    assert call_kwargs["response_format"] == {
        "type": "json_schema",
        "json_schema": {
            "name": "submit_scores",
            "schema": tool["function"]["parameters"],
        },
    }


def test_adapted_llm_utils_uses_json_object_response_format(monkeypatch, tmp_path):
    llm_utils = tmp_path / "tasks" / "task-a" / "tests" / "ssb_lib" / "llm_utils.py"
    llm_utils.parent.mkdir(parents=True)
    llm_utils.write_text(
        """from typing import Any
import json
import os

def tool_call_args(response: Any, name: str | None = None) -> dict[str, Any] | None:
    try:
        message = response.choices[0].message
    except (AttributeError, IndexError):
        return None
    for call in getattr(message, "tool_calls", None) or []:
        fn = getattr(call, "function", None)
        if fn is None:
            continue
        if name is None or fn.name == name:
            try:
                return json.loads(fn.arguments)
            except (TypeError, json.JSONDecodeError):
                return None
    return None


def complete(  # noqa: PLR0913
    *,
    model: str = "",
    messages: list[dict[str, Any]] | None = None,
    max_tokens: int = 0,
    tools: list[dict[str, Any]] | None = None,
    tool_choice: Any = None,
    system: str | None = None,
    thinking_budget: int = 0,
    attempts: int = 1,
    **kwargs: Any,
) -> Any:
    call_kwargs = {"model": model, "messages": messages or [], "max_tokens": max_tokens}
    if tools is not None:
        call_kwargs["tools"] = tools
    if tool_choice is not None:
        call_kwargs["tool_choice"] = tool_choice
    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    return call_kwargs
"""
    )
    result = verifier_adapt.adapt_dataset(tmp_path, "required", "json_object")

    spec = importlib.util.spec_from_file_location("patched_llm_utils_json_object", llm_utils)
    patched = importlib.util.module_from_spec(spec)
    sys.modules["patched_llm_utils_json_object"] = patched
    spec.loader.exec_module(patched)  # type: ignore[union-attr]

    tool = {
        "type": "function",
        "function": {
            "name": "submit_review",
            "parameters": {
                "type": "object",
                "properties": {"score": {"type": "number"}},
                "required": ["score"],
            },
        },
    }

    monkeypatch.setenv("SSB_OPENAI_COMPAT_TOOL_CHOICE", "required")
    monkeypatch.setenv("SSB_OPENAI_COMPAT_RESPONSE_FORMAT", "json_object")

    call_kwargs = patched.complete(
        model="openai/glm-5.2",
        messages=[{"role": "user", "content": "judge this"}],
        max_tokens=128,
        tools=[tool],
        tool_choice={"type": "function", "function": {"name": "submit_review"}},
    )

    assert result["env"]["SSB_OPENAI_COMPAT_RESPONSE_FORMAT"] == "json_object"
    assert result["response_format_compat"] == "json_object"
    assert "tools" not in call_kwargs
    assert "tool_choice" not in call_kwargs
    assert call_kwargs["response_format"] == {"type": "json_object"}
    assert call_kwargs["messages"][0]["role"] == "system"
    assert "submit_review" in call_kwargs["messages"][0]["content"]
    assert "\"score\"" in call_kwargs["messages"][0]["content"]
