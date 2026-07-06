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
CALL_ORIGINAL = '    if tool_choice is not None:\n        call_kwargs["tool_choice"] = tool_choice\n'
CALL_PATCHED = '    if tool_choice is not None:\n        call_kwargs["tool_choice"] = _vis_openai_compat_tool_choice(tool_choice)\n'
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
RESPONSE_FORMAT_PATCHED = '''    if thinking_budget > 0:
        call_kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}
    call_kwargs.update(kwargs)
    response_format = _vis_forced_tool_response_format(tools, tool_choice)
    if response_format is not None and "response_format" not in call_kwargs:
        call_kwargs["response_format"] = response_format
        call_kwargs.pop("tools", None)
        call_kwargs.pop("tool_choice", None)

'''

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


def _vis_response_format_enabled() -> bool:
    return os.environ.get("SSB_OPENAI_COMPAT_RESPONSE_FORMAT", "").strip().lower() in {{
        "1",
        "true",
        "yes",
        "json_schema",
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
    if not _vis_response_format_enabled():
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
    return {{
        "type": "json_schema",
        "json_schema": {{
            "name": forced_name,
            "schema": parameters,
        }},
    }}


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
        if "_vis_content_json_args" not in text or "_vis_forced_tool_response_format" not in text:
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

    if RESPONSE_FORMAT_ORIGINAL in text:
        text = text.replace(RESPONSE_FORMAT_ORIGINAL, RESPONSE_FORMAT_PATCHED, 1)
    elif RESPONSE_FORMAT_PATCHED not in text:
        raise AdaptationError(f"{path}: cannot find response_format insertion point")

    if text != original:
        path.write_text(text)
    return AdaptedFile(path=str(path), changed=text != original)


def adapt_dataset(dataset_copy: Path, tool_choice_compat: str) -> dict[str, object]:
    if tool_choice_compat != "required":
        raise AdaptationError(f"unsupported tool_choice compatibility mode: {tool_choice_compat}")
    paths = sorted(dataset_copy.glob("tasks/*/tests/ssb_lib/llm_utils.py"))
    if not paths:
        raise AdaptationError(f"no task verifier llm_utils.py files found under {dataset_copy}")
    adapted = [_patch_llm_utils(path) for path in paths]
    return {
        "enabled": True,
        "tool_choice_compat": tool_choice_compat,
        "env": {
            "SSB_OPENAI_COMPAT_PARSE_CONTENT_JSON": "1",
            "SSB_OPENAI_COMPAT_RESPONSE_FORMAT": "1",
            "SSB_OPENAI_COMPAT_TOOL_CHOICE": tool_choice_compat,
        },
        "files": [asdict(item) for item in adapted],
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset-copy", type=Path, required=True)
    parser.add_argument("--tool-choice-compat", required=True)
    parser.add_argument("--out", type=Path)
    args = parser.parse_args()

    result = adapt_dataset(args.dataset_copy, args.tool_choice_compat)
    text = json.dumps(result, indent=2, sort_keys=True) + "\n"
    if args.out:
        args.out.parent.mkdir(parents=True, exist_ok=True)
        args.out.write_text(text)
    print(text, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
