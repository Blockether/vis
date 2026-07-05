#!/usr/bin/env python3
"""Run-local adaptations for Senior SWE-Bench verifier compatibility."""
from __future__ import annotations

import argparse
import json
from dataclasses import asdict, dataclass
from pathlib import Path


HELPER_MARKER = "# VIS_BENCH_OPENAI_COMPAT_TOOL_CHOICE_START"
HELPER_ANCHOR = "\ndef complete(  # noqa: PLR0913\n"
CALL_ORIGINAL = '    if tool_choice is not None:\n        call_kwargs["tool_choice"] = tool_choice\n'
CALL_PATCHED = '    if tool_choice is not None:\n        call_kwargs["tool_choice"] = _vis_openai_compat_tool_choice(tool_choice)\n'

HELPER = f'''
{HELPER_MARKER}
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
# VIS_BENCH_OPENAI_COMPAT_TOOL_CHOICE_END
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
    if HELPER_MARKER not in text:
        if HELPER_ANCHOR not in text:
            raise AdaptationError(f"{path}: cannot find complete() anchor")
        text = text.replace(HELPER_ANCHOR, HELPER + HELPER_ANCHOR, 1)

    if CALL_ORIGINAL in text:
        text = text.replace(CALL_ORIGINAL, CALL_PATCHED, 1)
    elif CALL_PATCHED not in text:
        raise AdaptationError(f"{path}: cannot find tool_choice assignment")

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
        "env": {"SSB_OPENAI_COMPAT_TOOL_CHOICE": tool_choice_compat},
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
