#!/usr/bin/env python3
"""Run-local adaptations for Senior SWE-Bench verifier compatibility."""
from __future__ import annotations

import argparse
import json
from dataclasses import asdict, dataclass
from pathlib import Path


HELPER_MARKER = "# VIS_BENCH_OPENAI_COMPAT_TOOL_CHOICE_START"
HELPER_END_MARKER = "# VIS_BENCH_OPENAI_COMPAT_TOOL_CHOICE_END"
HELPER_ANCHOR = "\ndef complete(  # noqa: PLR0913\n"
MODEL_VALIDATE_HELPER = '''def _vis_model_validate(schema: Any, args: dict[str, Any]) -> Any:
    try:
        return schema.model_validate(args)
'''
CALL_ORIGINAL = '    if tool_choice is not None:\n        call_kwargs["tool_choice"] = tool_choice\n'
CALL_PATCHED = '    if tool_choice is not None:\n        call_kwargs["tool_choice"] = _vis_openai_compat_tool_choice(tool_choice)\n'
USAGE_CALL_ORIGINAL = "    resp = _call_with_retry(litellm.completion, attempts, call_kwargs)\n"
USAGE_CALL_PATCHED = '''    resp = _vis_record_usage(
        _call_with_retry(litellm.completion, attempts, call_kwargs),
        model=routing.model,
        tools=tools,
        tool_choice=tool_choice,
    )
'''
TOOL_CALL_ARGS_ORIGINAL = '''            except (TypeError, json.JSONDecodeError):
                return None
    return None


'''
TOOL_CALL_ARGS_PATCHED = '''            except (TypeError, json.JSONDecodeError):
                return None
    return _vis_content_json_args(response, name)


'''
RESPONSE_FORMAT_ORIGINAL = '''    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)

'''
RESPONSE_FORMAT_COMPACT_ORIGINAL = '''    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    return call_kwargs
'''
RESPONSE_FORMAT_LEGACY_PATCHED = '''    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    response_format = _vis_forced_tool_response_format(tools, tool_choice)
    if response_format is not None and "response_format" not in call_kwargs:
        call_kwargs["response_format"] = response_format
        call_kwargs.pop("tools", None)
        call_kwargs.pop("tool_choice", None)

'''
RESPONSE_FORMAT_PRE_DISABLE_PATCHED = '''    if thinking_budget > 0:
    call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    response_format = _vis_forced_tool_response_format(tools, tool_choice)
    if response_format is not None and "response_format" not in call_kwargs:
        instruction = _vis_forced_tool_json_instruction(tools, tool_choice)
        if instruction:
            call_kwargs["messages"] = [{"role": "system", "content": instruction}, *call_kwargs["messages"]]
        call_kwargs["response_format"] = response_format
        call_kwargs.pop("tools", None)
        call_kwargs.pop("tool_choice", None)

'''
RESPONSE_FORMAT_PATCHED = '''    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    response_format = _vis_forced_tool_response_format(tools, tool_choice)
    if response_format is not None and "response_format" not in call_kwargs:
        instruction = _vis_forced_tool_json_instruction(tools, tool_choice)
        if instruction:
            call_kwargs["messages"] = [{"role": "system", "content": instruction}, *call_kwargs["messages"]]
        call_kwargs["response_format"] = response_format
        if _vis_disable_thinking_for_response_format():
            extra_body = call_kwargs.get("extra_body")
            extra_body = dict(extra_body) if isinstance(extra_body, dict) else {}
            extra_body["thinking"] = {"type": "disabled"}
            call_kwargs["extra_body"] = extra_body
        call_kwargs.pop("tools", None)
        call_kwargs.pop("tool_choice", None)

'''
RESPONSE_FORMAT_COMPACT_PATCHED = RESPONSE_FORMAT_PATCHED + "    return call_kwargs\n"
STRUCTURED_VALIDATE_ORIGINAL = "    return schema.model_validate(args)\n"
STRUCTURED_VALIDATE_PATCHED = "    return _vis_model_validate(schema, args)\n"
EXPLORE_VALIDATE_ORIGINAL = "                return emit_schema.model_validate(args)\n"
EXPLORE_VALIDATE_PATCHED = "                return llm_utils._vis_model_validate(emit_schema, args)\n"
FORCED_VALIDATE_ORIGINAL = "    return emit_schema.model_validate(emit_args) if emit_args is not None else None\n"
FORCED_VALIDATE_PATCHED = (
    "    return llm_utils._vis_model_validate(emit_schema, emit_args) if emit_args is not None else None\n"
)
LITELLM_REQUIREMENT = "litellm>=1.0,<2.0\n"
PINNED_LITELLM_REQUIREMENT = "litellm==1.92.0\n"
FASTAPI_REQUIREMENT = "fastapi>=0.136.3,<1.0\n"
ORJSON_REQUIREMENT = "orjson>=3.11.6,<4.0\n"

HELPER = f'''
{HELPER_MARKER}
def _vis_content_json_enabled() -> bool:
    return os.environ.get("SSB_OPENAI_COMPAT_PARSE_CONTENT_JSON", "").strip().lower() in {{
        "1",
        "true",
        "yes",
    }} or os.environ.get("SSB_OPENAI_COMPAT_TOOL_CHOICE", "").strip().lower() == "required"


def _vis_strip_json_fence(text: str) -> str:
    stripped = text.strip()
    if not stripped.startswith("```"):
        return stripped
    lines = stripped.splitlines()
    if lines and lines[0].lstrip().startswith("```"):
        lines = lines[1:]
    if lines and lines[-1].strip().startswith("```"):
        lines = lines[:-1]
    return "\\n".join(lines).strip()


def _vis_load_json_object(text: str) -> dict[str, Any] | None:
    candidates = [_vis_strip_json_fence(text)]
    stripped = candidates[0]
    start = stripped.find("{{")
    end = stripped.rfind("}}")
    if start != -1 and end > start:
        candidates.append(stripped[start : end + 1])
    for candidate in candidates:
        try:
            parsed = json.loads(candidate)
        except (TypeError, json.JSONDecodeError):
            continue
        if isinstance(parsed, dict):
            return parsed
    return None


def _vis_top_level_validation_error_fields(error: ValueError) -> set[str]:
    errors = getattr(error, "errors", None)
    if not callable(errors):
        return set()
    try:
        items = errors()
    except Exception:
        return set()
    fields: set[str] = set()
    for item in items:
        location = item.get("loc") if isinstance(item, dict) else None
        if isinstance(location, (list, tuple)) and location and isinstance(location[0], str):
            fields.add(location[0])
    return fields


def _vis_decode_top_level_json_values(args: dict[str, Any], fields: set[str]) -> dict[str, Any]:
    """Decode providers that serialize object-valued tool fields as strings."""
    normalized = dict(args)
    for key, value in args.items():
        if key not in fields or not isinstance(value, str):
            continue
        stripped = value.strip()
        if not (stripped.startswith("{{") or stripped.startswith("[")):
            continue
        try:
            decoded = json.loads(stripped)
        except json.JSONDecodeError:
            continue
        if isinstance(decoded, (dict, list)):
            normalized[key] = decoded
    return normalized


def _vis_model_validate(schema: Any, args: dict[str, Any]) -> Any:
    try:
        return schema.model_validate(args)
    except ValueError as error:
        normalized_args = _vis_decode_top_level_json_values(
            args,
            _vis_top_level_validation_error_fields(error),
        )
        if normalized_args == args:
            raise
        return schema.model_validate(normalized_args)


def _vis_content_json_args(response: Any, name: str | None = None) -> dict[str, Any] | None:
    if not _vis_content_json_enabled():
        return None
    try:
        content = response.choices[0].message.content
    except (AttributeError, IndexError):
        return None
    if not isinstance(content, str) or not content.strip():
        return None
    parsed = _vis_load_json_object(content)
    if parsed is None:
        return None
    if name and isinstance(parsed.get(name), dict):
        return parsed[name]
    if isinstance(parsed.get("arguments"), dict):
        return parsed["arguments"]
    return parsed


def _vis_response_format_mode() -> str:
    value = os.environ.get("SSB_OPENAI_COMPAT_RESPONSE_FORMAT", "").strip().lower()
    if value in {{
        "1",
        "true",
        "yes",
        "json_schema",
    }}:
        return "json_schema"
    if value == "json_object":
        return "json_object"
    return ""


def _vis_disable_thinking_for_response_format() -> bool:
    return os.environ.get("SSB_OPENAI_COMPAT_DISABLE_THINKING", "").strip().lower() in {{
        "1",
        "true",
        "yes",
    }}


def _vis_forced_tool_name(tool_choice: Any) -> str | None:
    if not isinstance(tool_choice, dict):
        return None
    fn = tool_choice.get("function")
    if not isinstance(fn, dict):
        return None
    name = fn.get("name")
    return name if isinstance(name, str) and name else None


def _vis_forced_tool_response_format(tools: Any, tool_choice: Any) -> dict[str, Any] | None:
    mode = _vis_response_format_mode()
    if not mode:
        return None
    forced_name = _vis_forced_tool_name(tool_choice)
    if forced_name is None or not isinstance(tools, list) or len(tools) != 1:
        return None
    tool = tools[0]
    if not isinstance(tool, dict):
        return None
    fn = tool.get("function")
    if not isinstance(fn, dict) or fn.get("name") != forced_name:
        return None
    parameters = fn.get("parameters")
    if not isinstance(parameters, dict):
        return None
    if mode == "json_object":
        return {{"type": "json_object"}}
    return {{
        "type": "json_schema",
        "json_schema": {{
            "name": forced_name,
            "schema": parameters,
        }},
    }}


def _vis_forced_tool_json_instruction(tools: Any, tool_choice: Any) -> str | None:
    if _vis_response_format_mode() != "json_object":
        return None
    forced_name = _vis_forced_tool_name(tool_choice)
    if forced_name is None or not isinstance(tools, list) or len(tools) != 1:
        return None
    tool = tools[0]
    if not isinstance(tool, dict):
        return None
    fn = tool.get("function")
    if not isinstance(fn, dict) or fn.get("name") != forced_name:
        return None
    parameters = fn.get("parameters")
    if not isinstance(parameters, dict):
        return None
    return (
        "Return only a valid JSON object containing the arguments for the forced "
        f"tool {{forced_name!r}}. Do not include markdown, prose, or a tool-call "
        "wrapper. The JSON object must conform to this schema:\\n"
        + json.dumps(parameters, sort_keys=True)
    )


def _vis_openai_compat_tool_choice(tool_choice: Any) -> Any:
    """Translate forced OpenAI function tool_choice for compatible local servers.

    LM Studio accepts string tool_choice values such as "required", but rejects
    the function-specific OpenAI object form. Senior SWE-Bench only uses the
    object form for single-tool forced emits, so "required" preserves intent in
    that mode while leaving normal OpenAI runs unchanged.
    """
    compat = os.environ.get("SSB_OPENAI_COMPAT_TOOL_CHOICE", "").strip().lower()
    if compat == "required" and isinstance(tool_choice, dict):
        return "required"
    return tool_choice


def _vis_json_dict(value: Any) -> dict[str, Any]:
    if isinstance(value, dict):
        return value
    for method_name in ("model_dump", "dict"):
        method = getattr(value, method_name, None)
        if callable(method):
            try:
                result = method()
            except Exception:
                continue
            if isinstance(result, dict):
                return result
    return {{}}


def _vis_usage_purpose(tools: Any, tool_choice: Any) -> str:
    names: list[str] = []
    if isinstance(tools, list):
        for tool in tools:
            fn = tool.get("function") if isinstance(tool, dict) else None
            name = fn.get("name") if isinstance(fn, dict) else None
            if isinstance(name, str) and name:
                names.append(name)
    forced = _vis_forced_tool_name(tool_choice)
    if forced and forced not in names:
        names.append(forced)
    purposes = {{
        "classify_files": "classifier",
        "submit_scores": "rubric_judge",
        "submit_taste_scores": "taste_judge",
        "submit_review": "validation_judge",
    }}
    for name in names:
        if name in purposes:
            return purposes[name]
    return forced or "judge"


def _vis_record_usage(response: Any, *, model: str, tools: Any, tool_choice: Any) -> Any:
    """Append usage metadata without persisting prompts or response content."""
    usage = _vis_json_dict(getattr(response, "usage", None))
    if not usage:
        return response
    hidden = _vis_json_dict(getattr(response, "_hidden_params", None))
    cost = hidden.get("response_cost")
    if not isinstance(cost, (int, float)) or isinstance(cost, bool):
        cost = None
    event = {{
        "source": "litellm",
        "model": model,
        "purpose": _vis_usage_purpose(tools, tool_choice),
        "usage": usage,
        "reported_cost_usd": cost,
    }}
    path = os.environ.get("SSB_LLM_USAGE_PATH", "/logs/verifier/llm_usage.jsonl")
    try:
        parent = os.path.dirname(path)
        if parent:
            os.makedirs(parent, exist_ok=True)
        with open(path, "a", encoding="utf-8") as handle:
            handle.write(json.dumps(event, sort_keys=True) + "\\n")
    except (OSError, TypeError, ValueError):
        pass
    return response
{HELPER_END_MARKER}
'''


class AdaptationError(RuntimeError):
    pass


@dataclass(frozen=True)
class AdaptedFile:
    path: str
    changed: bool


def _patch_llm_utils(path: Path) -> AdaptedFile:
    text = path.read_text()
    original = text
    if HELPER_MARKER in text:
        if (
            "_vis_content_json_args" not in text
                or "_vis_forced_tool_response_format" not in text
                or "_vis_forced_tool_json_instruction" not in text
                or "_vis_disable_thinking_for_response_format" not in text
                or "_vis_record_usage" not in text
                or MODEL_VALIDATE_HELPER not in text
        ):
            start = text.index(HELPER_MARKER)
            line_start = text.rfind("\n", 0, start) + 1
            end = text.index(HELPER_END_MARKER, start) + len(HELPER_END_MARKER)
            text = text[:line_start] + HELPER.strip("\n") + text[end:]
    else:
        if HELPER_ANCHOR not in text:
            raise AdaptationError(f"{path}: cannot find complete() anchor")
        text = text.replace(HELPER_ANCHOR, HELPER + HELPER_ANCHOR, 1)

    if TOOL_CALL_ARGS_ORIGINAL in text:
        text = text.replace(TOOL_CALL_ARGS_ORIGINAL, TOOL_CALL_ARGS_PATCHED, 1)
    elif TOOL_CALL_ARGS_PATCHED not in text:
        raise AdaptationError(f"{path}: cannot find tool_call_args fallback point")

    if CALL_ORIGINAL in text:
        text = text.replace(CALL_ORIGINAL, CALL_PATCHED, 1)
    elif CALL_PATCHED not in text:
        raise AdaptationError(f"{path}: cannot find tool_choice assignment")

    if USAGE_CALL_ORIGINAL in text:
        text = text.replace(USAGE_CALL_ORIGINAL, USAGE_CALL_PATCHED)
    elif USAGE_CALL_PATCHED not in text and "def _call_with_retry" in text:
        raise AdaptationError(f"{path}: cannot find completion call for usage capture")

    if RESPONSE_FORMAT_ORIGINAL in text:
        text = text.replace(RESPONSE_FORMAT_ORIGINAL, RESPONSE_FORMAT_PATCHED, 1)
    elif RESPONSE_FORMAT_COMPACT_ORIGINAL in text:
        text = text.replace(RESPONSE_FORMAT_COMPACT_ORIGINAL, RESPONSE_FORMAT_COMPACT_PATCHED, 1)
    elif RESPONSE_FORMAT_LEGACY_PATCHED in text:
        text = text.replace(RESPONSE_FORMAT_LEGACY_PATCHED, RESPONSE_FORMAT_PATCHED, 1)
    elif RESPONSE_FORMAT_PRE_DISABLE_PATCHED in text:
        text = text.replace(RESPONSE_FORMAT_PRE_DISABLE_PATCHED, RESPONSE_FORMAT_PATCHED, 1)
    elif RESPONSE_FORMAT_PATCHED not in text:
        raise AdaptationError(f"{path}: cannot find response_format insertion point")

    if "def complete_structured" in text:
        structured_start = text.index("def complete_structured")
        prefix, structured = text[:structured_start], text[structured_start:]
        if STRUCTURED_VALIDATE_ORIGINAL in structured:
            structured = structured.replace(STRUCTURED_VALIDATE_ORIGINAL, STRUCTURED_VALIDATE_PATCHED, 1)
            text = prefix + structured
        elif STRUCTURED_VALIDATE_PATCHED not in structured:
            raise AdaptationError(f"{path}: cannot find structured validation return")

    if text != original:
        path.write_text(text)
    return AdaptedFile(path=str(path), changed=text != original)


def _patch_llm_tools(path: Path) -> AdaptedFile:
    text = path.read_text()
    original = text
    if EXPLORE_VALIDATE_ORIGINAL in text:
        text = text.replace(EXPLORE_VALIDATE_ORIGINAL, EXPLORE_VALIDATE_PATCHED, 1)
    elif EXPLORE_VALIDATE_PATCHED not in text:
        raise AdaptationError(f"{path}: cannot find explore structured validation")
    if FORCED_VALIDATE_ORIGINAL in text:
        text = text.replace(FORCED_VALIDATE_ORIGINAL, FORCED_VALIDATE_PATCHED, 1)
    elif FORCED_VALIDATE_PATCHED not in text:
        raise AdaptationError(f"{path}: cannot find forced structured validation")
    if text != original:
        path.write_text(text)
    return AdaptedFile(path=str(path), changed=text != original)


def _patch_test_sh(path: Path) -> AdaptedFile:
    text = path.read_text()
    original = text
    if LITELLM_REQUIREMENT in text:
        text = text.replace(LITELLM_REQUIREMENT, PINNED_LITELLM_REQUIREMENT, 1)
    elif PINNED_LITELLM_REQUIREMENT not in text:
        raise AdaptationError(f"{path}: cannot find litellm verifier requirement")
    missing = "".join(
        requirement
        for requirement in (FASTAPI_REQUIREMENT, ORJSON_REQUIREMENT)
        if requirement not in text
    )
    if missing:
        text = text.replace(PINNED_LITELLM_REQUIREMENT, PINNED_LITELLM_REQUIREMENT + missing, 1)
    if text != original:
        path.write_text(text)
    return AdaptedFile(path=str(path), changed=text != original)


def adapt_dataset(
    dataset_copy: Path,
    tool_choice_compat: str = "",
    response_format_compat: str = "json_schema",
) -> dict[str, object]:
    if tool_choice_compat not in {"", "required"}:
        raise AdaptationError(f"unsupported tool_choice compatibility mode: {tool_choice_compat}")
    if response_format_compat not in {"json_schema", "json_object"}:
        raise AdaptationError(f"unsupported response_format compatibility mode: {response_format_compat}")
    llm_paths = sorted(dataset_copy.glob("tasks/*/tests/ssb_lib/llm_utils.py"))
    if not llm_paths:
        raise AdaptationError(f"no task verifier llm_utils.py files found under {dataset_copy}")
    test_paths = sorted(dataset_copy.glob("tasks/*/tests/test.sh"))
    adapted = [_patch_llm_utils(path) for path in llm_paths]
    adapted.extend(
        _patch_llm_tools(path)
        for path in sorted(dataset_copy.glob("tasks/*/tests/ssb_lib/llm_tools.py"))
    )
    adapted.extend(_patch_test_sh(path) for path in test_paths)
    env = {"SSB_LLM_USAGE_PATH": "/logs/verifier/llm_usage.jsonl"}
    if tool_choice_compat:
        env.update({
            "SSB_OPENAI_COMPAT_PARSE_CONTENT_JSON": "1",
            "SSB_OPENAI_COMPAT_RESPONSE_FORMAT": (
                "json_object" if response_format_compat == "json_object" else "1"
            ),
            "SSB_OPENAI_COMPAT_TOOL_CHOICE": tool_choice_compat,
        })
    return {
        "enabled": True,
        "usage_capture": "litellm_jsonl",
        "tool_choice_compat": tool_choice_compat,
        "response_format_compat": response_format_compat,
        "env": env,
        "files": [asdict(item) for item in adapted],
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset-copy", type=Path, required=True)
    parser.add_argument("--tool-choice-compat", default="", choices=["", "required"])
    parser.add_argument("--response-format-compat", default="json_schema", choices=["json_schema", "json_object"])
    parser.add_argument("--out", type=Path)
    args = parser.parse_args()

    result = adapt_dataset(args.dataset_copy, args.tool_choice_compat, args.response_format_compat)
    text = json.dumps(result, indent=2, sort_keys=True) + "\n"
    if args.out:
        args.out.parent.mkdir(parents=True, exist_ok=True)
        args.out.write_text(text)
    print(text, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
