"""Grade one Vis trial from its saved files and write Harbor rewards.

The verifier of a Vis Harbor task runs this module. Its ``tests/test.sh`` runs
``"$VIS_HARBOR_PYTHON" -m blockether.vis.evals.grade "$VIS_HARBOR_TESTS/checks.json"``.
The module reads the checks of the task, the
workspace after the run and the agent logs (``trajectory.json``, ``run.json`` and
``stderr.txt``). It writes two files to the verifier logs:

- ``reward.json``: named rewards. ``reward`` is the verdict. ``converged``,
  ``correct`` and ``no_errors`` are its parts. Each other key is one guard that
  the checks ask for, with 1.0 for a pass and 0.0 for a failure.
- ``grade.json``: the full result record with the details and the evidence.

The grader needs no Harbor install. The SDK reads all evidence of the run
from its ATIF trajectory: code blocks, output, Activity, provider calls, answer,
token totals, persisted usage and the saved goal.
"""

from __future__ import annotations

import argparse
import ast
import collections
import hashlib
import json
import os
import re
from collections.abc import Iterable, Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Any, cast

from .trajectory import Trajectory, load_trajectory

TRACE_FILE = "trace.jsonl"
STDERR_FILE = "stderr.txt"
TRAJECTORY_FILE = "trajectory.json"
RUN_FILE = "run.json"
REWARD_FILE = "reward.json"
WORKSPACE_FILESYSTEM_FILE = "workspace_filesystem.json"
GRADE_FILE = "grade.json"


TERMINAL_ACTIVITY_STATES = {"succeeded", "failed", "cancelled"}


def literal_fold_keys(code):
    """Return literal first arguments from direct fold_session calls in one form."""
    try:
        tree = ast.parse(code)
    except SyntaxError:
        return []
    return [
        node.args[0].value
        for node in ast.walk(tree)
        if isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id == "fold_session"
        and node.args
        and isinstance(node.args[0], ast.Constant)
        and isinstance(node.args[0].value, str)
    ]


CACHE_USAGE_FIELDS = (
    "input_tokens",
    "input_cache_read_tokens",
    "prompt_cache_reusable_tokens",
    "prompt_cache_reused_tokens",
    "prompt_cache_sample_count",
    "prompt_cache_estimated_sample_count",
    "prompt_cache_rebuild_count",
    "prompt_cache_expired_count",
    "cache_read_share_percent",
    "reusable_prefix_coverage_percent",
)


def usage_percent(part, total):
    """Match the gateway's nonnegative, nearest-integer percentage."""
    if total <= 0:
        return 0
    return min(100, int((100.0 * part / total) + 0.5))


def fold_count_failures(usage):
    """Verify executed prefix folds from engine accounting, never printed receipts."""
    count = usage.get("fold_count")
    if type(count) is not int or count != 1:
        return [f"one prefix fold produced {count!r} recorded folds, expected 1"]
    return []


CONTEXT_BUDGET_KEYS = ("auto_compress_above", "model_input_limit")


def context_budget_failures(usage, form_outputs, answer, expected):
    """Match the budget a model saw and reported with persisted request health.

    Every route must keep a positive soft budget within 90% of its input limit and
    a reminder at 75% of that budget. `expected` optionally pins both values for
    the requested route; None applies only those rules.
    """
    health = usage.get("health")
    if not isinstance(health, dict):
        return ["usage has no persisted request health"]
    budget, limit, reminder = (
        health.get(key)
        for key in ("budget_tokens", "model_input_limit", "reminder_tokens")
    )
    failures = [
        f"health {key} is not a positive integer: {value!r}"
        for key, value in (
            ("budget_tokens", budget),
            ("model_input_limit", limit),
            ("reminder_tokens", reminder),
        )
        if type(value) is not int or value <= 0
    ]
    if failures:
        return failures
    budget, limit, reminder = cast("tuple[int, int, int]", (budget, limit, reminder))
    if budget > max(1, limit * 9 // 10):
        failures.append(f"budget_tokens {budget} exceeds 90% of limit {limit}")
    if reminder != (3 * budget + 3) // 4:
        failures.append(f"reminder_tokens {reminder} is not 75% of {budget}")
    persisted = {"auto_compress_above": budget, "model_input_limit": limit}
    if expected is not None and persisted != expected:
        failures.append(f"persisted budget {persisted!r} != expected {expected!r}")
    printed = []
    for item in form_outputs:
        for line in item["stdout"].splitlines():
            try:
                value = exact_json(line)
            except ValueError:
                continue
            if (
                isinstance(value, dict)
                and sorted(value) == sorted(CONTEXT_BUDGET_KEYS)
                and all(type(number) is int for number in value.values())
            ):
                printed.append(value)
    if not printed:
        failures.append("no sandbox form printed the session utilization budget")
    elif printed[-1] != persisted:
        failures.append(
            f"model-visible budget {printed[-1]!r} != persisted health {persisted!r}"
        )
    text = unfenced_json(answer.strip())
    try:
        reported = exact_json(text)
    except ValueError:
        reported = None
    if reported != persisted:
        failures.append(f"answer {text!r} does not report the persisted budget")
    return failures


def token_summary(tokens):
    """Keep provider totals separate from cache shares and character counts."""
    if not isinstance(tokens, dict):
        return {}, ["provider tokens are not an object"]
    failures = []
    values = {}
    for key in ("input", "cached", "output", "reasoning", "cache_created", "total"):
        value = tokens.get(key)
        if value is None and key not in {"input", "cached", "output"}:
            values[key] = None
        elif type(value) is not int or value < 0:
            failures.append(
                f"provider tokens {key} is not a nonnegative integer: {value!r}"
            )
        else:
            values[key] = value
    if failures:
        return {}, failures
    if values["cached"] > values["input"]:
        failures.append("provider cached tokens exceed input tokens")
    if (
        values["total"] is not None
        and values["total"] != values["input"] + values["output"]
    ):
        failures.append("provider total tokens != input + output")
    values["uncached"] = values["input"] - values["cached"]
    values["cached_input_percent"] = (
        round(100 * values["cached"] / values["input"], 2) if values["input"] else 0.0
    )
    return values, failures


def exact_json(text):
    """Reject ambiguous duplicate keys in answer and fixture evidence."""

    def object_pairs(pairs):
        result = dict(pairs)
        if len(result) != len(pairs):
            raise ValueError("duplicate JSON keys")
        return result

    return json.loads(text, object_pairs_hook=object_pairs)


def unfenced_json(text):
    """Unwrap one whole-answer ```json fence, the only wrapper exact answers allow."""
    if text.startswith("```json\n") and text.endswith("\n```"):
        return text[8:-4]
    return text


def council_recovery_evidence(form_outputs, activities, expected):
    """Accept one reply-ID error only after a verified read of its real thread."""
    errors = [event for event in form_outputs if event.get("error")]
    failed_reads = [
        row
        for row in activities
        if row.get("operation") == "council.read"
        and row.get("state") in {"failed", "cancelled"}
    ]
    if len(errors) != 1 or len(failed_reads) != 1:
        return (
            ["Council recovery requires exactly one surfaced read failure"],
            set(),
            [],
        )
    event, failed = errors[0], failed_reads[0]
    error = event["error"]
    message = error.get("message", "") if isinstance(error, dict) else ""
    data = error.get("data") if isinstance(error, dict) else None
    match = re.fullmatch(
        r"Council entry_id=(\d+) is a reply in thread_id=(\d+)\. "
        r"Use thread_id=\2 to read or continue this thread\.",
        message,
    )
    if not match or not isinstance(data, dict) or data.get("error") != "invalid-thread":
        return ["Council recovery did not receive the reply-ID repair"], set(), []
    entry_id, thread_id = match.groups()
    if int(entry_id) <= 0 or int(thread_id) <= 0 or entry_id == thread_id:
        return (
            ["Council recovery requires distinct positive message and thread IDs"],
            set(),
            [],
        )
    if any(
        type(data.get(key)) is not int or data[key] != int(value)
        for key, value in (("entry_id", entry_id), ("thread_id", thread_id))
    ):
        return (
            ["Council recovery error data must name the real reply and thread"],
            set(),
            [],
        )
    order = {output["scope"]: index for index, output in enumerate(form_outputs)}

    def position(row):
        return order.get(row.get("scope"), -1), row.get("sequence", 0)

    def references(row, kind):
        return {
            resource.get("id")
            for resource in row.get("resources", [])
            if resource.get("type") == kind
        }

    def bodies(row):
        presentation = row.get("presentation") or {}
        blocks = list(presentation.get("content") or [])
        for section in presentation.get("sections") or []:
            blocks.extend(section.get("content") or [])
        return [
            block.get("text") for block in blocks if block.get("type") == "markdown"
        ]

    publications = [
        row
        for row in activities
        if row.get("operation") == "council.publish" and row.get("state") == "succeeded"
    ]
    roots = [
        row
        for row in publications
        if references(row, "council-entry") == {thread_id}
        and references(row, "council-thread") == {thread_id}
        and bodies(row) == [expected["root_content"]]
    ]
    replies = [
        row
        for row in publications
        if references(row, "council-entry") == {entry_id}
        and references(row, "council-thread") == {thread_id}
        and bodies(row) == [expected["reply_content"]]
    ]
    reads = [
        row
        for row in activities
        if row.get("operation") == "council.read" and row.get("state") == "succeeded"
    ]
    if (
        len(publications) != 2
        or len(roots) != 1
        or len(replies) != 1
        or len(reads) != 1
    ):
        return (
            ["Council recovery lacks the two real publications and one repaired read"],
            set(),
            [],
        )
    root, reply, repaired = roots[0], replies[0], reads[0]
    failures = []
    if failed.get("state") != "failed" or failed.get("scope") != event.get("scope"):
        failures.append("Council recovery error does not match its failed Activity")
    if any(
        not row.get("scope") or row["scope"] not in order
        for row in (root, reply, failed, repaired)
    ):
        failures.append("Council recovery is missing a form scope")
    if not position(root) < position(reply) < position(failed) < position(repaired):
        failures.append(
            "Council recovery did not read the thread after the reply-ID error"
        )
    if references(repaired, "council-thread") != {thread_id} or references(
        repaired, "council-entry"
    ) != {thread_id, entry_id}:
        failures.append("Council recovery read another thread or missed a message")
    if bodies(repaired) != [expected["root_content"], expected["reply_content"]]:
        failures.append("Council recovery did not read both complete message bodies")
    if failures:
        return failures, set(), []
    return [], {failed["id"]}, [message]


def structured_failures(sc, work, answer, activities):
    """Check exact fixture truth, not numbers or JSON fragments embedded in prose."""

    def same_json_value(actual, expected):
        if type(actual) in (int, float) and type(expected) in (int, float):
            return actual == expected
        if type(actual) is not type(expected):
            return False
        if isinstance(actual, dict):
            return actual.keys() == expected.keys() and all(
                same_json_value(actual[key], expected[key]) for key in actual
            )
        if isinstance(actual, list):
            return len(actual) == len(expected) and all(
                same_json_value(left, right)
                for left, right in zip(actual, expected, strict=True)
            )
        return actual == expected

    failures = []
    if "want_answer_json" in sc:
        text = answer.strip()
        if sc.get("want_goal_complete") and text.startswith("Goal complete: "):
            text = text.removeprefix("Goal complete: ")
        text = unfenced_json(text)
        try:
            actual = exact_json(text)
        except ValueError:
            failures.append("answer is not a single JSON value")
        else:
            if not same_json_value(actual, sc["want_answer_json"]):
                failures.append(
                    "answer JSON does not match the requested facts exactly"
                )
    for name, expected in (sc.get("want_json_files") or {}).items():
        try:
            with open(os.path.join(work, name)) as stream:
                actual = [exact_json(line) for line in stream if line.strip()]
        except (OSError, ValueError) as exc:
            failures.append(f"invalid JSONL evidence {name}: {exc}")
        else:
            if json.dumps(actual, sort_keys=True) != json.dumps(
                expected, sort_keys=True
            ):
                failures.append(
                    f"JSONL evidence {name} does not match exact rows/order/arguments"
                )
    if "want_activity_sequence" in sc:
        expected = sc["want_activity_sequence"]
        namespaces = {op.split(".")[0] for op in expected}
        actual = [
            row["operation"]
            for row in activities
            if row["operation"].split(".")[0] in namespaces
        ]
        if actual != expected:
            failures.append(f"activity sequence {actual!r} != expected {expected!r}")
    forbidden = set(sc.get("forbid_tools") or [])
    for operation in sorted({row["operation"] for row in activities} & forbidden):
        failures.append(f"forbidden tool {operation!r} was used")
    return failures


def helper_reuse_evidence(forms, want):
    """Prove a helper was defined once and then CALLED by a later sandbox form.

    Saved definitions persist, so reuse means a later top-level form calls a
    function an earlier form defined. The audit is name-agnostic and accepts a
    `def` or a name bound to a `lambda`, the two shapes the runtime saves as a
    helper. Retyping the same definition instead of calling it fails, and so
    does a helper that no later form ever uses.
    """
    if want is True:
        wanted = 1
    elif type(want) is int and want > 0:
        wanted = want
    else:
        return {}, ["want_helper_reuse must be true or a positive integer"]
    defined = {}
    lambdas = set()
    sources = collections.defaultdict(set)
    reuse = collections.defaultdict(set)
    retyped = set()
    for index, form in enumerate(forms):
        try:
            tree = ast.parse(form)
        except SyntaxError:
            continue
        for node in tree.body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                name = node.name
            elif (
                isinstance(node, ast.Assign)
                and len(node.targets) == 1
                and isinstance(node.targets[0], ast.Name)
                and isinstance(node.value, ast.Lambda)
            ):
                name = node.targets[0].id
                lambdas.add(name)
            else:
                continue
            defined.setdefault(name, index)
            source = ast.unparse(node)
            if source in sources[name]:
                retyped.add(name)
            sources[name].add(source)
        for node in ast.walk(tree):
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
                first = defined.get(node.func.id)
                if first is not None and first < index:
                    reuse[node.func.id].add(index)
    metrics = {
        "helpers_defined": len(defined),
        "lambda_helpers": len(lambdas),
        "reused_helpers": len(reuse),
        "reuse_forms": max((len(seen) for seen in reuse.values()), default=0),
        "retyped_helpers": len(retyped),
    }
    failures = [
        f"helper {name!r} was retyped in a later form instead of called"
        for name in sorted(retyped)
    ]
    if not defined:
        failures.append("no sandbox form defined a helper to reuse")
    elif metrics["reuse_forms"] < wanted:
        failures.append(
            f"a helper was reused in {metrics['reuse_forms']} later form(s), want {wanted}"
        )
    return metrics, failures


def discovery_evidence(forms, activities, rules):
    """Audit simple Python syntax; runtime Activities independently prove host calls.

    This is not execution tracing for stdlib inspection. Resolve ordinary aliases
    and literal loops, ignore comments/strings/uninvoked definitions, and reject
    opaque dynamic execution rather than claiming it proves inspection.
    """
    aliases = {}
    signatures = []
    contracts = []
    failures = []
    other_inspection = []
    syntax_lookups = []
    allowed_imports = {
        "inspect",
        "json",
        "re",
        "dataclasses",
        "collections",
        "typing",
        "asyncio",
        "builtins",
    }

    def resolve(node):
        if isinstance(node, ast.Name):
            return aliases.get(node.id, node.id)
        if isinstance(node, ast.Attribute):
            return f"{resolve(node.value)}.{node.attr}"
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            return node.value
        if isinstance(node, (ast.Tuple, ast.List)):
            return [resolve(item) for item in node.elts]
        if (
            isinstance(node, ast.Call)
            and resolve(node.func) == "getattr"
            and len(node.args) >= 2
        ):
            return f"{resolve(node.args[0])}.{resolve(node.args[1])}"
        return "?"

    def call_name(node):
        name = resolve(node.func)
        return name if isinstance(name, str) else "?"

    audited_symbols = {
        *rules.get("symbols", []),
        *rules.get("signatures", []),
        *rules.get("contracts", []),
    }
    audited_symbols |= {name.split(".")[0] for name in audited_symbols}

    def inspects_audited_tool(node):
        """`dir()`/`vars()` on the TOOL is discovery; on a result or a namespace it is not."""
        target = resolve(node.args[0]) if node.args else "?"
        return isinstance(target, str) and target in audited_symbols

    local_bodies = {}
    sensitive_calls = {
        "doc",
        "apropos",
        "open",
        "exec",
        "eval",
        "compile",
        "__import__",
        "read_text",
        "read_bytes",
        "write_text",
        "write_bytes",
        "system",
        "popen",
        "shell",
        "cat",
        "grep",
        "ls",
    }

    def hidden_discovery(body, seen):
        for statement in body:
            for node in ast.walk(statement):
                if isinstance(node, ast.Attribute) and node.attr == "contract":
                    return True
                if isinstance(node, ast.Call):
                    name = call_name(node)
                    if (
                        name.startswith("inspect.")
                        or name.rsplit(".", 1)[-1] in sensitive_calls
                    ):
                        return True
                    if (
                        name in local_bodies
                        and name not in seen
                        and hidden_discovery(local_bodies[name], seen | {name})
                    ):
                        return True
        return False

    class Visitor(ast.NodeVisitor):
        def visit_Import(self, node):
            for item in node.names:
                if item.name.split(".")[0] not in allowed_imports:
                    failures.append(f"forbidden import {item.name!r}")
                aliases[item.asname or item.name] = item.name

        def visit_ImportFrom(self, node):
            if (node.module or "").split(".")[0] not in allowed_imports:
                failures.append(f"forbidden import {node.module!r}")
            for item in node.names:
                aliases[item.asname or item.name] = f"{node.module}.{item.name}"

        def visit_Assign(self, node):
            self.visit(node.value)
            for target in node.targets:
                if isinstance(target, ast.Name):
                    if isinstance(node.value, ast.Lambda):
                        aliases[target.id] = target.id
                        local_bodies[target.id] = [node.value.body]
                    else:
                        aliases[target.id] = resolve(node.value)

        def visit_For(self, node):
            items = resolve(node.iter)
            if isinstance(node.target, ast.Name) and isinstance(items, list):
                for item in items:
                    aliases[node.target.id] = item
                    for statement in node.body:
                        self.visit(statement)
            else:
                self.generic_visit(node)

        def visit_ListComp(self, node):
            self._visit_comprehension(node)

        def visit_SetComp(self, node):
            self._visit_comprehension(node)

        def visit_GeneratorExp(self, node):
            self._visit_comprehension(node)

        def _visit_comprehension(
            self, node: ast.ListComp | ast.SetComp | ast.GeneratorExp
        ):
            generator = node.generators[0]
            items = resolve(generator.iter)
            if (
                len(node.generators) == 1
                and isinstance(generator.target, ast.Name)
                and isinstance(items, list)
                and not generator.ifs
            ):
                previous = aliases.copy()
                for item in items:
                    aliases[generator.target.id] = item
                    self.visit(node.elt)
                aliases.clear()
                aliases.update(previous)
            else:
                self.generic_visit(node)

        def visit_FunctionDef(self, node):
            self._visit_definition(node)

        def visit_AsyncFunctionDef(self, node):
            self._visit_definition(node)

        def visit_ClassDef(self, node):
            self._visit_definition(node)

        def _visit_definition(
            self, node: ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef
        ):
            local_bodies[node.name] = node.body
            for item in node.decorator_list:
                self.visit(item)

        def visit_Lambda(self, node):
            pass

        def visit_If(self, node):
            if isinstance(node.test, ast.Constant):
                for statement in node.body if node.test.value else node.orelse:
                    self.visit(statement)
            else:
                self.generic_visit(node)

        def visit_Attribute(self, node):
            if node.attr == "contract":
                contracts.append(resolve(node.value))
            self.generic_visit(node)

        def visit_Call(self, node):
            name = call_name(node)
            body = (
                [node.func.body]
                if isinstance(node.func, ast.Lambda)
                else local_bodies.get(name, [])
            )
            if body and hidden_discovery(body, {name}):
                failures.append(
                    f"cannot verify discovery/source access hidden in local helper {name!r}"
                )
            if name == "inspect.signature":
                signatures.append(resolve(node.args[0]) if node.args else "?")
            elif name in {"doc", "apropos"}:
                argument = (
                    node.args[0]
                    if node.args
                    else next(
                        (
                            item.value
                            for item in node.keywords
                            if item.arg in {"name", "pattern"}
                        ),
                        None,
                    )
                )
                syntax_lookups.append((name, resolve(argument)))
            elif name.startswith("inspect.") or name == "help":
                other_inspection.append(name)
            elif name in {"dir", "vars"} and inspects_audited_tool(node):
                other_inspection.append(name)
            if name in rules.get("forbid_tools", []):
                failures.append(f"forbidden tool call {name!r}")
            if name in {
                "open",
                "builtins.open",
                "exec",
                "eval",
                "compile",
                "__import__",
            } or name.rsplit(".", 1)[-1] in {
                "read_text",
                "read_bytes",
                "write_text",
                "write_bytes",
                "open",
                "system",
                "popen",
            }:
                failures.append(f"forbidden source/file/dynamic call {name!r}")
            if (
                name == "getattr"
                and len(node.args) > 1
                and resolve(node.args[1]) == "contract"
            ):
                contracts.append(resolve(node.args[0]))
            self.generic_visit(node)

    visitor = Visitor()
    for code in forms:
        try:
            visitor.visit(ast.parse(code))
        except SyntaxError:
            failures.append("cannot audit discovery in an invalid Python form")
    lookups = [
        (row["operation"], row.get("argument_key", row["id"]))
        for row in activities
        if row["operation"] in {"doc", "apropos"}
    ]
    # Fast local reads may have no Activity rows. Prefer runtime evidence for
    # each operation when available, never count it twice with syntax evidence.
    for operation in ("doc", "apropos"):
        runtime = [item for item in lookups if item[0] == operation]
        syntax = [item for item in syntax_lookups if item[0] == operation]
        if len(syntax) > len(runtime):
            lookups = [item for item in lookups if item[0] != operation] + syntax
    counts = collections.Counter(
        [
            *(("signature", name) for name in signatures),
            *(("contract", name) for name in contracts),
            *lookups,
        ]
    )
    redundant = sum(count - 1 for count in counts.values())
    if redundant:
        failures.append(f"redundant discovery: {redundant} repeated unchanged lookups")
    if rules.get("known") and (signatures or contracts or lookups or other_inspection):
        failures.append("known contracts were unnecessarily rediscovered")
    # `doc(name)` IS the registered contract -- signature, defaults, schema and
    # effects -- so it settles arguments at least as well as `inspect.signature`.
    documented = {
        argument for operation, argument in syntax_lookups if operation == "doc"
    }
    for name in rules.get("signatures", []):
        if name not in signatures and name not in documented:
            failures.append(f"no signature or doc call found for {name}")
    for name in rules.get("contracts", []):
        if name not in contracts:
            failures.append(f"no focused contract access found for {name}")
    return {
        "signature_calls": len(signatures),
        "contract_reads": len(contracts),
        "doc_calls": sum(operation == "doc" for operation, _ in lookups),
        "apropos_calls": sum(operation == "apropos" for operation, _ in lookups),
        "other_inspection_calls": len(other_inspection),
        "redundant_discovery": redundant,
    }, failures


def cache_metric_failures(usage, result_tokens, provider_call_count, folded_prefix):
    """Independently reconcile one real run's provider, DB, and wire cache totals."""
    failures = []
    values = {}
    for key in CACHE_USAGE_FIELDS:
        value = usage.get(key)
        if not isinstance(value, int) or isinstance(value, bool) or value < 0:
            failures.append(f"usage {key} is not a nonnegative integer: {value!r}")
        else:
            values[key] = value
    if failures:
        return failures

    input_tokens = values["input_tokens"]
    cached_tokens = values["input_cache_read_tokens"]
    reusable_tokens = values["prompt_cache_reusable_tokens"]
    reused_tokens = values["prompt_cache_reused_tokens"]
    samples = values["prompt_cache_sample_count"]
    estimated_samples = values["prompt_cache_estimated_sample_count"]
    rebuilds = values["prompt_cache_rebuild_count"]
    expired = values["prompt_cache_expired_count"]
    for key in ("input", "cached"):
        value = result_tokens.get(key)
        if type(value) is not int or value < 0:
            failures.append(
                f"provider tokens {key} is not a nonnegative integer: {value!r}"
            )
    if failures:
        return failures
    result_input = result_tokens["input"]
    result_cached = result_tokens["cached"]
    if cached_tokens > input_tokens or result_cached > result_input:
        failures.append("cached tokens exceed input tokens")

    if input_tokens != result_input:
        failures.append(f"usage input {input_tokens} != provider result {result_input}")
    if cached_tokens != result_cached:
        failures.append(
            f"usage cache read {cached_tokens} != provider result {result_cached}"
        )
    if reused_tokens > cached_tokens:
        failures.append(
            f"reused prefix {reused_tokens} exceeds cache reads {cached_tokens}"
        )
    if reused_tokens > reusable_tokens:
        failures.append(
            f"reused prefix {reused_tokens} exceeds reusable prefix {reusable_tokens}"
        )

    expected_share = usage_percent(cached_tokens, input_tokens)
    if values["cache_read_share_percent"] != expected_share:
        failures.append(
            f"cache-read share {values['cache_read_share_percent']}% != recomputed {expected_share}%"
        )
    expected_coverage = usage_percent(reused_tokens, reusable_tokens)
    if values["reusable_prefix_coverage_percent"] != expected_coverage:
        failures.append(
            "reusable-prefix coverage "
            f"{values['reusable_prefix_coverage_percent']}% != recomputed {expected_coverage}%"
        )

    expected_samples = max(0, provider_call_count - 1)
    if samples != expected_samples:
        failures.append(
            f"cache samples {samples} != post-baseline calls {expected_samples}"
        )
    if estimated_samples > samples:
        failures.append(
            f"estimated samples {estimated_samples} exceed all samples {samples}"
        )
    if folded_prefix:
        failures.extend(fold_count_failures(usage))
        if estimated_samples != 1:
            failures.append(
                f"one prefix fold produced {estimated_samples} estimated samples, expected 1"
            )
        if rebuilds != 1:
            failures.append(f"one prefix fold produced {rebuilds} rebuilds, expected 1")
        if expired != 0:
            failures.append(f"fresh run reported {expired} expired prefixes")
    return failures


def stdout_recovery_failures(form_outputs, form_events, expected):
    """Require each saved stdout by its own scope and tool-call id."""
    cases = expected if isinstance(expected, list) else [expected]
    seen = set()
    failures = []
    for case in cases:
        minimum = case["min_chars"]
        head, middle, tail = (case[key] for key in ("head", "middle", "tail"))
        label = f"{head!r}: " if len(cases) > 1 else ""
        originals = [
            item
            for item in form_outputs
            if len(item["stdout"]) >= minimum
            and item["stdout"].startswith(head)
            and middle in item["stdout"]
            and item["stdout"].rstrip().endswith(tail)
        ]
        if not originals:
            failures.append(
                label
                + "no oversized raw stdout with the expected head, middle and tail"
            )
            continue
        first = next(
            (
                item
                for item in originals
                if (item["scope"], item.get("tool_call_id")) not in seen
            ),
            None,
        )
        if first is None:
            failures.append(label + "no distinct oversized stdout for this expectation")
            continue
        scope = first["scope"].split("/f", 1)[0]
        call_id = first.get("tool_call_id")
        if not scope or not call_id:
            failures.append(
                label + "oversized stdout has no recoverable scope and tool-call id"
            )
            continue
        seen.add((first["scope"], call_id))
        digest = hashlib.sha256(first["stdout"].encode("utf-8")).hexdigest()
        readers = {
            event["iteration"]
            for event in form_events
            if isinstance(event["iteration"], int)
            and event["iteration"] > first["iteration"]
            and "read_session()" in event["code"]
            and ("['stdout']" in event["code"] or '["stdout"]' in event["code"])
            and "svar_tool_call_id" in event["code"]
            and any(f"{q}{scope}{q}" in event["code"] for q in ('"', "'"))
            and any(f"{q}{call_id}{q}" in event["code"] for q in ('"', "'"))
        }
        goal_status = case.get("goal_status", "active")
        if not any(
            any(item["iteration"] >= iteration for iteration in readers)
            and item["iteration"] > first["iteration"]
            and f"LEN: {len(first['stdout'])}" in item["stdout"]
            and f"SHA256: {digest}" in item["stdout"]
            and middle in item["stdout"]
            and (goal_status is None or f"GOAL_STATUS: {goal_status}" in item["stdout"])
            for item in form_outputs
        ):
            failures.append(
                label + "no later read_session() block verified the exact stdout"
            )
    return failures


@dataclass(frozen=True)
class TrialFiles:
    """The saved output of one agent run.

    ``trajectory`` is the ATIF trajectory of the run. It is None when the file is
    missing or invalid, and ``trajectory_problem`` then tells why.
    """

    trajectory: Trajectory | None
    trajectory_problem: str | None
    stderr: str
    exit_code: int | None
    wall_s: float
    is_cancelled: bool
    provider: str | None
    model: str | None
    reasoning_effort: str | None


def _json_data(value: Any) -> Any:
    """Return frozen trajectory data as plain JSON data: dicts and lists."""
    if isinstance(value, Mapping):
        return {key: _json_data(item) for key, item in value.items()}
    if isinstance(value, tuple):
        return [_json_data(item) for item in value]
    return value


def load_trial(agent_logs: str | os.PathLike[str]) -> TrialFiles:
    """Read the files of the agent. A missing or invalid file gives empty values."""
    logs = Path(agent_logs)
    run_path = logs / RUN_FILE
    record = json.loads(run_path.read_text()) if run_path.is_file() else {}
    stderr_path = logs / STDERR_FILE
    stderr = stderr_path.read_text(errors="replace") if stderr_path.is_file() else ""
    trajectory = problem = None
    trajectory_path = logs / TRAJECTORY_FILE
    if not trajectory_path.is_file():
        problem = f"no {TRAJECTORY_FILE}"
    else:
        try:
            trajectory = load_trajectory(trajectory_path, strict=False)
        except ValueError as failure:
            problem = f"an invalid {TRAJECTORY_FILE}: {failure}"
    return TrialFiles(
        trajectory=trajectory,
        trajectory_problem=problem,
        stderr=stderr,
        exit_code=record.get("exit_code"),
        wall_s=float(record.get("wall_s") or 0.0),
        is_cancelled=bool(record.get("is_cancelled")),
        provider=record.get("provider"),
        model=record.get("model"),
        reasoning_effort=record.get("reasoning_effort"),
    )


def _result_tokens(run: Trajectory) -> dict[str, int]:
    """The token totals of the run, with the keys of a Vis result."""
    totals = run.final_metrics
    if totals is None:
        return {}
    values = {
        "input": totals.total_prompt_tokens,
        "output": totals.total_completion_tokens,
        "cached": totals.total_cached_tokens,
        "cache_created": totals.cache_creation_tokens,
        "reasoning": totals.reasoning_tokens,
        "total": totals.total_tokens,
    }
    return {key: value for key, value in values.items() if value is not None}


class _Checks:
    """Record each guard of a trial and the details of its failures."""

    def __init__(self) -> None:
        self.guards: dict[str, bool] = {}
        self.detail: list[str] = []

    def use(self, name: str) -> None:
        self.guards.setdefault(name, True)

    def fail(self, name: str, *messages: str) -> None:
        self.guards[name] = False
        self.detail.extend(messages)

    def check(self, name: str, failures: Iterable[str]) -> None:
        failures = list(failures)
        self.use(name)
        if failures:
            self.fail(name, *failures)


@dataclass(frozen=True)
class Grade:
    """The full result record and the named rewards of one trial."""

    result: dict[str, Any]
    rewards: dict[str, float]

    @property
    def is_passed(self) -> bool:
        return bool(self.result["passed"])


def _workspace_text(work: str, name: str) -> str:
    try:
        with open(os.path.join(work, name)) as handle:
            return handle.read()
    except FileNotFoundError:
        return ""


def grade_trial(
    sc: Mapping[str, Any], work: str | os.PathLike[str], trial: TrialFiles
) -> Grade:
    """Grade one trial with the checks ``sc`` of its task."""
    work = os.fspath(work)
    provider, model = trial.provider, trial.model
    reasoning_effort = trial.reasoning_effort
    exit_code = trial.exit_code
    forms = []
    form_events = []
    form_outputs = []
    provider_calls = []
    tools = []
    largest_form_output = 0
    total_form_output = 0
    activities = {}
    surfaced_scopes = set()
    errs = []
    done = False
    answer = ""
    result_tokens = {}
    result_cost = {}
    result_session_id = None
    result_eval = {}
    session_usage = saved_goal = None
    run = trial.trajectory
    if run is None and exit_code == 0:
        errs.append(f"vis-agent wrote {trial.trajectory_problem}")
    for step in run.agent_steps if run is not None else ():
        provider_calls.append(
            {
                "provider": step.provider,
                "model": step.model_name,
                "iteration": step.iteration,
            }
        )
        for call in step.tool_calls:
            scope = call.ref or ""
            code = call.code or ""
            forms.append(code)
            form_events.append(
                {"code": code, "scope": scope, "iteration": step.iteration}
            )
            stdout = call.stdout
            error = _json_data(call.error)
            form_outputs.append(
                {
                    "scope": scope,
                    "iteration": step.iteration,
                    "stdout": stdout,
                    "tool_call_id": call.tool_call_id,
                    "error": error,
                }
            )
            largest_form_output = max(largest_form_output, len(stdout))
            total_form_output += len(stdout)
            if error:
                surfaced_scopes.add(scope)
                errs.append(error.get("message", "?"))
            if call.activity_problem:
                errs.append(
                    f"invalid Activity in {scope or call.tool_call_id}: "
                    f"{call.activity_problem}"
                )
            # The receipt of a block holds the final state of each top-level row.
            # Child rows are parts of their parent, as in the live snapshots.
            receipt = _json_data(call.extra.get("activity")) if call.activity else {}
            for row in receipt.get("rows") or []:
                if row.get("id") and row.get("operation"):
                    activities[row["id"]] = {**row, "scope": scope}
    if run is not None:
        if run.error:
            errs.append(str(run.error))
        result_tokens = _result_tokens(run)
        result_cost = {"provider": run.provider, "model": run.model_name}
        result_session_id = run.session_id
        result_eval = _json_data(run.evaluation) or {}
        session_usage = _json_data(run.session_usage)
        saved_goal = _json_data(run.goal)
        answer = run.final_answer or ""
        done = run.final_step is not None and not run.error

    if exit_code is None:
        errs.append(f"vis-agent timed out after {round(trial.wall_s)}s")
    elif exit_code:
        first = next((line for line in trial.stderr.splitlines() if line.strip()), "")
        suffix = f": {first.strip()[:120]}" if first else ""
        errs.append(f"vis-agent exited {exit_code}{suffix}")
    activity_rows = list(activities.values())
    failed_activities = [
        row for row in activity_rows if row.get("state") in {"failed", "cancelled"}
    ]
    incomplete_activities = [
        row
        for row in activity_rows
        if row.get("state") not in {"succeeded", "failed", "cancelled"}
    ]
    caught_failures = [
        row
        for row in failed_activities
        if row["scope"] and row["scope"] not in surfaced_scopes
    ]
    unscoped_failures = [row for row in failed_activities if not row["scope"]]
    surfaced_errors = len(errs)
    recovery_failures, expected_failed_ids, expected_error_messages = [], set(), []
    if expected := sc.get("want_council_recovery"):
        recovery_failures, expected_failed_ids, expected_error_messages = (
            council_recovery_evidence(form_outputs, activity_rows, expected)
        )
    for message in expected_error_messages:
        errs.remove(message)
    errs.extend(
        f"activity {row['operation']} {row.get('state')}"
        for row in [*caught_failures, *unscoped_failures, *incomplete_activities]
        if row["id"] not in expected_failed_ids
    )
    tools = [
        row["operation"] for row in activity_rows if row.get("state") == "succeeded"
    ]

    checks = _Checks()
    checks.use("activities")
    if (
        any(row["id"] not in expected_failed_ids for row in failed_activities)
        or incomplete_activities
    ):
        checks.fail("activities")
    if sc.get("want_council_recovery"):
        checks.check("council_recovery", recovery_failures)
    if sc.get("want") or sc.get("wantnot"):
        checks.use("files")
    for name, subs in (sc.get("want") or {}).items():
        txt = _workspace_text(work, name)
        for s in subs:
            if s not in txt:
                checks.fail("files", f"missing {name}:{s!r}")
    for name, subs in (sc.get("wantnot") or {}).items():
        txt = _workspace_text(work, name)
        for s in subs:
            if s in txt:
                checks.fail("files", f"still present {name}:{s!r}")
    if sc.get("want_answer"):
        checks.use("answer")
    for s in sc.get("want_answer") or []:
        if s not in answer:
            checks.fail("answer", f"answer missing {s!r}")
    if sc.get("want_forms"):
        checks.use("forms")
    for needle in sc.get("want_forms") or []:
        if not any(needle in form for form in forms):
            checks.fail("forms", f"form containing {needle!r} not used")
    if sc.get("want_stdout_recovery"):
        checks.check(
            "stdout_recovery",
            stdout_recovery_failures(
                form_outputs, form_events, sc["want_stdout_recovery"]
            ),
        )
    checks.check("structured", structured_failures(sc, work, answer, activity_rows))
    discovery = {}
    if "discovery" in sc:
        discovery, discovery_failures = discovery_evidence(
            forms,
            activity_rows,
            {
                **sc["discovery"],
                "symbols": sc.get("want_tools", []),
                "forbid_tools": sc.get("forbid_tools", []),
            },
        )
        checks.check("discovery", discovery_failures)
    helper_reuse = {}
    behavior = []
    measurement = bool(sc.get("measurement"))
    if "want_helper_reuse" in sc:
        helper_reuse, helper_failures = helper_reuse_evidence(
            forms, sc["want_helper_reuse"]
        )
        if measurement:
            behavior.extend(helper_failures)
        else:
            checks.check("helper_reuse", helper_failures)
    total_limit = sc.get("max_total_output_chars")
    if total_limit is not None:
        checks.use("total_output")
        if type(total_limit) is not int or total_limit < 0:
            checks.fail(
                "total_output", "max_total_output_chars must be a nonnegative integer"
            )
        elif total_form_output > total_limit:
            checks.fail(
                "total_output",
                f"total output {total_form_output} chars exceeds max_total_output_chars={total_limit}",
            )
    output_limit = sc.get("max_form_output_chars")
    if output_limit is not None:
        checks.use("form_output")
        if type(output_limit) is not int or output_limit < 0:
            checks.fail(
                "form_output", "max_form_output_chars must be a nonnegative integer"
            )
        elif largest_form_output > output_limit:
            checks.fail(
                "form_output",
                f"form output {largest_form_output} chars exceeds "
                f"max_form_output_chars={output_limit}",
            )

    if reasoning_effort:
        checks.use("reasoning_effort")
        effort_evidence = result_eval.get("reasoning_effort") or {}
        iterations = effort_evidence.get("iterations") or []
        if (
            result_eval.get("is_valid") is not True
            or result_eval.get("invalid_reasons")
            or effort_evidence.get("requested") != reasoning_effort
            or not iterations
            or any(
                item.get("requested") != reasoning_effort
                or item.get("effective") != reasoning_effort
                or (item.get("provider"), item.get("model")) != (provider, model)
                or item.get("selected") != {"provider": provider, "model": model}
                or item.get("is_fallback") is not False
                for item in iterations
            )
        ):
            checks.fail(
                "reasoning_effort",
                f"invalid reasoning-effort evidence: {result_eval!r}",
            )

    if sc.get("want_requested_route") or reasoning_effort:
        checks.use("requested_route")
        expected_route = (provider, model)
        actual_routes = [(call["provider"], call["model"]) for call in provider_calls]
        if not actual_routes or any(route != expected_route for route in actual_routes):
            checks.fail(
                "requested_route",
                f"provider calls {actual_routes!r}, expected only {expected_route!r}",
            )
        cost_route = (result_cost.get("provider"), result_cost.get("model"))
        if cost_route != expected_route:
            checks.fail(
                "requested_route",
                f"billed route {cost_route!r}, expected {expected_route!r}",
            )

    fold_forms = [event for event in form_events if "fold_session(" in event["code"]]
    if sc.get("want_folded_prefix"):
        checks.use("folded_prefix")
        if len(fold_forms) != 1:
            checks.fail(
                "folded_prefix", f"expected one prefix fold, observed {len(fold_forms)}"
            )
        else:
            fold_event = fold_forms[0]
            scope_match = re.match(r"t(\d+)/i(\d+)(?:/|$)", fold_event["scope"])
            if not scope_match:
                checks.fail(
                    "folded_prefix",
                    f"fold form has invalid scope {fold_event['scope']!r}",
                )
            else:
                turn = int(scope_match.group(1))
                iteration = int(scope_match.group(2))
                expected_key = f"-t{turn}/i{iteration - 1}"
                fold_keys = literal_fold_keys(fold_event["code"])
                if iteration <= 1 or fold_keys != [expected_key]:
                    checks.fail(
                        "folded_prefix",
                        f"fold form did not use exact prior-prefix key: expected {expected_key!r}, got {fold_keys!r}",
                    )
                if not any(
                    isinstance(call["iteration"], int) and call["iteration"] > iteration
                    for call in provider_calls
                ):
                    checks.fail(
                        "folded_prefix",
                        "no provider continuation followed the prefix fold",
                    )

    tokens, token_failures = token_summary(result_tokens)
    if result_tokens or sc.get("want_cache_metrics"):
        checks.check("token_metrics", token_failures)
    cached_tokens = tokens.get("cached", 0)
    if sc.get("want_cache_read"):
        checks.use("cache_read")
        if cached_tokens <= 0:
            checks.fail("cache_read", "provider reported zero prompt-cache read tokens")

    cache_usage = None
    usage_guards = []
    if sc.get("want_cache_metrics"):
        usage_guards.append("cache_metrics")
    elif sc.get("want_folded_prefix"):
        usage_guards.append("fold_count")
    if sc.get("want_context_budget"):
        usage_guards.append("context_budget")
    for name in usage_guards:
        checks.use(name)
    if usage_guards and not result_session_id:
        for name in usage_guards:
            checks.fail(name)
        checks.detail.append("persistent run returned no session id")
    elif usage_guards and not isinstance(session_usage, dict):
        for name in usage_guards:
            checks.fail(name)
        checks.detail.append("the trajectory has no session usage")
    elif usage_guards and isinstance(session_usage, dict):
        cache_usage = session_usage
        if sc.get("want_cache_metrics"):
            metric_failures = cache_metric_failures(
                cache_usage,
                result_tokens,
                len(provider_calls),
                bool(sc.get("want_folded_prefix")),
            )
            output_tokens = cache_usage.get("output_tokens")
            if (
                type(output_tokens) is not int
                or output_tokens < 0
                or output_tokens != result_tokens.get("output")
            ):
                metric_failures.append("usage output_tokens != provider output tokens")
            checks.check("cache_metrics", metric_failures)
        elif sc.get("want_folded_prefix"):
            checks.check("fold_count", fold_count_failures(cache_usage))
        if want_budget := sc.get("want_context_budget"):
            checks.check(
                "context_budget",
                context_budget_failures(
                    cache_usage,
                    form_outputs,
                    answer,
                    want_budget.get(f"{provider}/{model}")
                    if isinstance(want_budget, dict)
                    else None,
                ),
            )
    goal_state = None
    if sc.get("want_goal_complete"):
        checks.use("goal_complete")
        if not result_session_id:
            checks.fail("goal_complete", "goal run returned no persisted session id")
        else:
            goal_state = saved_goal
            if (
                not isinstance(goal_state, dict)
                or goal_state.get("status") != "complete"
            ):
                checks.fail("goal_complete", "goal status is not complete")
    toolset = {t for t in tools if t}
    if sc.get("want_tools"):
        checks.use("tools")
    for t in sc.get("want_tools") or []:
        if t not in toolset:
            checks.fail("tools", f"tool {t!r} not used")

    correct = all(checks.guards.values())
    detail = checks.detail
    used_patch = "patch" in toolset
    path = "patch" if used_patch else "cat-only"
    if path == "cat-only" or errs or not (done and correct):
        detail.append(
            "tools=" + ",".join(f"{t}×{tools.count(t)}" for t in sorted(toolset))
        )
    evidence = []
    if type(output_limit) is int and output_limit >= 0:
        evidence.append(f"max-form-output={largest_form_output}/{output_limit} chars")
    evidence.append(
        f"stdout={total_form_output} total chars; surfaced-errors={surfaced_errors}; activity-failures={len(failed_activities)} ({len(caught_failures)} without a form error)"
    )
    if expected_error_messages:
        evidence.append(
            "verified Council recovery: 1 expected error and 1 failed Activity"
        )
    if tokens and not token_failures:
        evidence.append(
            f"tokens=input {tokens['input']} (cached {tokens['cached']}, uncached {tokens['uncached']}, share {tokens['cached_input_percent']}%), output {tokens['output']}, reasoning {tokens['reasoning'] if tokens['reasoning'] is not None else 'unavailable'}"
        )
    if discovery:
        evidence.append(
            "discovery="
            + ", ".join(f"{key}:{value}" for key, value in discovery.items())
        )
    if helper_reuse:
        evidence.append(
            "helper_reuse="
            + ", ".join(f"{key}:{value}" for key, value in helper_reuse.items())
        )
    if reasoning_effort:
        evidence.append(f"reasoning-effort={reasoning_effort}")
    if sc.get("want_requested_route") or reasoning_effort:
        evidence.append(
            f"requested-route={provider}/{model} calls={len(provider_calls)}"
        )
    if sc.get("want_folded_prefix") and fold_forms:
        evidence.append(f"fold={fold_forms[0]['scope']}→prior-prefix")
    health = cache_usage.get("health") if isinstance(cache_usage, dict) else None
    if sc.get("want_context_budget") and isinstance(health, dict):
        evidence.append(
            f"context-budget={health.get('budget_tokens')}/"
            f"{health.get('model_input_limit')} tokens, "
            f"reminder {health.get('reminder_tokens')}"
        )
    if sc.get("want_cache_read"):
        evidence.append(
            f"cache-read={cached_tokens}/{tokens.get('input', 'unavailable')} input tokens"
        )
    if isinstance(goal_state, dict):
        evidence.append(
            f"goal={goal_state.get('status')} after {goal_state.get('iterations_used')} iterations"
        )
    if isinstance(cache_usage, dict):
        samples = cache_usage.get("prompt_cache_sample_count")
        estimated = cache_usage.get("prompt_cache_estimated_sample_count")
        evidence.append(
            "cache-metrics="
            f"cached-input share {cache_usage.get('cache_read_share_percent')}% "
            f"({cache_usage.get('input_cache_read_tokens')}/{cache_usage.get('input_tokens')}), "
            f"reuse {cache_usage.get('reusable_prefix_coverage_percent')}% "
            f"({cache_usage.get('prompt_cache_reused_tokens')}/"
            f"{cache_usage.get('prompt_cache_reusable_tokens')}), "
            f"samples {samples} (estimated {estimated}), "
            f"rebuilds {cache_usage.get('prompt_cache_rebuild_count')}, "
            f"expired {cache_usage.get('prompt_cache_expired_count')}"
        )

    passed = bool(done and correct and not errs)
    result = {
        "id": sc.get("id"),
        "provider": provider,
        "model": model,
        "converged": done,
        "correct": correct,
        "errors": len(errs),
        "err_msgs": errs[:2],
        "wall": round(trial.wall_s, 1),
        "forms": len(forms),
        "provider_calls": len(provider_calls),
        "tokens": tokens,
        "token_errors": token_failures,
        "cache_usage": {
            key: cache_usage.get(key)
            for key in (*CACHE_USAGE_FIELDS, "output_tokens", "output_reasoning_tokens")
        }
        if isinstance(cache_usage, dict)
        else None,
        "max_form_output_chars": largest_form_output,
        "total_output_chars": total_form_output,
        "surfaced_errors": surfaced_errors,
        "expected_errors": len(expected_error_messages),
        "expected_activity_failures": len(expected_failed_ids),
        "activity_successes": len(tools),
        "activity_failures": len(failed_activities),
        "caught_activity_failures": len(caught_failures),
        "unscoped_activity_failures": len(unscoped_failures),
        "incomplete_activities": len(incomplete_activities),
        "discovery": discovery,
        "helper_reuse": helper_reuse,
        "measurement": measurement,
        "used_patch": used_patch,
        "edit_path": path,
        "detail": detail,
        "behavior": behavior,
        "evidence": evidence,
        "passed": passed,
        "guards": dict(checks.guards),
    }
    rewards = {
        "reward": float(passed),
        "converged": float(bool(done)),
        "correct": float(correct),
        "no_errors": float(not errs),
        **{name: float(ok) for name, ok in checks.guards.items()},
    }
    return Grade(result=result, rewards=rewards)


def _env_path(name: str) -> Path | None:
    value = os.environ.get(name)
    return Path(value) if value else None


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="python -m blockether.vis.evals.grade",
        description="Grade one Vis trial and write Harbor rewards.",
    )
    parser.add_argument("checks", type=Path, help="The checks.json of the task.")
    parser.add_argument("--task", default=os.environ.get("VIS_HARBOR_TASK"))
    parser.add_argument(
        "--workspace", type=Path, default=_env_path("VIS_HARBOR_WORKSPACE")
    )
    parser.add_argument(
        "--agent-logs", type=Path, default=_env_path("VIS_HARBOR_AGENT_LOGS")
    )
    parser.add_argument(
        "--verifier-logs", type=Path, default=_env_path("VIS_HARBOR_VERIFIER_LOGS")
    )
    args = parser.parse_args(argv)
    for name in ("workspace", "agent_logs", "verifier_logs"):
        if getattr(args, name) is None:
            parser.error(f"--{name.replace('_', '-')} is required outside Vis Harbor")
    checks = json.loads(args.checks.read_text())
    if args.task:
        checks["id"] = args.task
    grade = grade_trial(checks, args.workspace, load_trial(args.agent_logs))
    args.verifier_logs.mkdir(parents=True, exist_ok=True)
    (args.verifier_logs / GRADE_FILE).write_text(
        json.dumps(grade.result, indent=2) + "\n"
    )
    (args.verifier_logs / REWARD_FILE).write_text(
        json.dumps(grade.rewards, indent=2) + "\n"
    )
    verdict = "PASS" if grade.is_passed else "FAIL"
    print(verdict, " ".join(f"{k}={v:g}" for k, v in grade.rewards.items()))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
