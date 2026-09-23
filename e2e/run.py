#!/usr/bin/env python3
"""End-to-end editing harness — drives the REAL vis-agent CLI on a battery of editing
scenarios and checks, per scenario:

  - CONVERGED   the loop reached a final answer (no hang / crash)
  - CORRECT     resulting files, answers and observed operations satisfy the scenario
  - NO-ERROR    no surfaced errors or failed/cancelled/unfinished Activities
  - FAST PATH   the anchored `patch` wrote the edit, rather than the model
                wandering through the file with `cat` alone

Scenarios are SELF-CONTAINED FOLDERS under `e2e/scenarios/` — editing tasks over
real files in several formats, all driven through the same sandbox tools:

    e2e/scenarios/<id>/
      scenario.json   task, fixture expectations and optional benchmark guards
      files/          real files seeded into a fresh git repo before the run

`want`/`wantnot` are {path: [substring, ...]} checks on the resulting files;
`want_answer` is substrings the final answer must contain (non-file scenarios);
`want_tools` are extension tools that MUST have fired (e.g.
`contract_probe.record`); `want_forms` are source substrings that MUST occur in a top-level
sandbox form. The four boolean benchmark guards pin the requested route, the
canonical oldest-prefix fold, real provider cache reads, and the persisted
cache-metric arithmetic. Peak/cumulative stdout guards count characters, not tokens.
Exact JSON answers, JSONL fixture journals, operation sequences, helper reuse and
discovery audits are described in README.md. Every run reports provider token totals
separately from output size; VIS_E2E_REPEATS adds repeated-run measurements to results.json.
`workspace_filesystem` registers fixture directories; `files_from` reuses sibling input.

Each scenario runs in its own throwaway git repo through a source-owned gateway on an
isolated temporary DB, so an installed gateway cannot mask working-tree edits. Fixtures
with project extensions get a separate gateway started from their seeded workspace.
Runs are parallel. Usage:

    VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.3-flash python3 run.py [scenario-id ...]
"""

import ast
import collections
import concurrent.futures
import hashlib
import json
import os
import re
import shutil
import socket
import statistics
import subprocess
import sys
import tempfile
import time
from functools import cache

HERE = os.path.dirname(os.path.abspath(__file__))  # <repo>/e2e
REPO = os.path.dirname(HERE)
CLOJURE = os.environ.get("VIS_E2E_CLOJURE", "clojure")
NATIVE_BIN = os.environ.get("VIS_E2E_NATIVE_BIN")
PROVIDER = os.environ.get("VIS_PROVIDER", "zai-coding-plan")
MODEL = os.environ.get("VIS_MODEL", "glm-5.3-flash")
REASONING_EFFORT = os.environ.get("VIS_REASONING_EFFORT", "").strip() or None
# Cross-validation gate: a scenario passes only if EVERY model passes it.
MODELS = [
    m.strip() for m in os.environ.get("VIS_MODELS", MODEL).split(",") if m.strip()
]
TIMEOUT = int(os.environ.get("VIS_E2E_TIMEOUT", "300"))
WORKERS = int(os.environ.get("VIS_E2E_WORKERS", "5"))
TRACES = os.environ.get("VIS_E2E_TRACES", "/tmp/vis_e2e/traces")
TERMINAL_ACTIVITY_STATES = {"succeeded", "failed", "cancelled"}


@cache
def source_classpath():
    """Resolve once at the checkout; changing user.dir must not relocate source roots."""
    classpath = subprocess.check_output(
        [CLOJURE, "-Spath", "-A:vis"], cwd=REPO, text=True, timeout=120
    ).strip()
    return os.pathsep.join(
        entry if os.path.isabs(entry) else os.path.join(REPO, entry)
        for entry in classpath.split(os.pathsep)
    )


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


def agent_prefix(work):
    """Choose the actual engine under test; never silently fall back to the JVM."""
    if NATIVE_BIN:
        binary = os.path.abspath(NATIVE_BIN)
        with open(binary, "rb") as executable:
            magic = executable.read(4)
        if magic not in (
            b"\x7fELF",
            b"\xcf\xfa\xed\xfe",
            b"\xfe\xed\xfa\xcf",
            b"\xca\xfe\xba\xbe",
        ):
            raise RuntimeError(
                "VIS_E2E_NATIVE_BIN must be a raw native executable, not a launcher"
            )
        return [binary, f"-Duser.dir={work}"]
    return [CLOJURE, "-Scp", source_classpath(), f"-J-Duser.dir={work}", "-M:vis"]


def gateway_eval(env, form, timeout, *, cwd=REPO):
    """Use the canonical client, spawning the selected engine in the fixture workspace."""
    if NATIVE_BIN:
        binary, workspace = map(json.dumps, agent_prefix(cwd))
        form = (
            "(require '[com.blockether.vis.internal.gateway.discovery :as gateway-discovery] "
            "'[com.blockether.vis.internal.gateway.client :as gateway-client]) "
            f"(with-redefs [gateway-discovery/base-argv (constantly [{binary} {workspace}])] "
            f"{form})"
        )
    return subprocess.run(
        [CLOJURE, "-Scp", source_classpath(), "-M", "-e", form],
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        timeout=timeout,
    )


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


def summarize_results(results):
    """Summarize all repetitions, including failures, without averaging cache ratios."""
    groups = collections.defaultdict(list)
    for result in results:
        groups[(result["id"], result["provider"], result["model"])].append(result)
    summaries = []
    for (scenario, provider, model), rows in sorted(groups.items()):
        valid = [row for row in rows if row["tokens"] and not row["token_errors"]]
        summary = {
            "id": scenario,
            "provider": provider,
            "model": model,
            "runs": len(rows),
            "passed": sum(
                row["converged"] and row["correct"] and row["errors"] == 0
                for row in rows
            ),
            "measurement": any(row.get("measurement") for row in rows),
            "behavior_passed": sum(not row.get("behavior") for row in rows),
            "token_samples": len(valid),
        }
        totals = {
            key: sum(row["tokens"][key] for row in valid)
            for key in ("input", "cached", "uncached", "output")
        }
        summary["token_totals"] = totals
        summary["cached_input_percent"] = (
            round(100 * totals["cached"] / totals["input"], 2)
            if totals["input"]
            else 0.0
        )
        for key in (
            "wall",
            "forms",
            "provider_calls",
            "max_form_output_chars",
            "total_output_chars",
            "input",
            "cached",
            "uncached",
            "output",
        ):
            values = (
                [row["tokens"][key] for row in valid]
                if key in totals
                else [row[key] for row in rows]
            )
            summary[key] = (
                {
                    "min": min(values),
                    "median": statistics.median(values),
                    "max": max(values),
                }
                if values
                else None
            )
        summaries.append(summary)
    return summaries


def exact_json(text):
    """Reject ambiguous duplicate keys in answer and fixture evidence."""

    def object_pairs(pairs):
        result = dict(pairs)
        if len(result) != len(pairs):
            raise ValueError("duplicate JSON keys")
        return result

    return json.loads(text, object_pairs_hook=object_pairs)


def structured_failures(sc, work, answer, activities):
    """Check exact fixture truth, not numbers or JSON fragments embedded in prose."""
    failures = []
    if "want_answer_json" in sc:
        text = answer.strip()
        if text.startswith("```json\n") and text.endswith("\n```"):
            text = text[8:-4]
        try:
            actual = exact_json(text)
        except ValueError:
            failures.append("answer is not a single JSON value")
        else:
            if json.dumps(actual, sort_keys=True) != json.dumps(
                sc["want_answer_json"], sort_keys=True
            ):
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
                    name = resolve(node.func)
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

        visit_SetComp = visit_ListComp
        visit_GeneratorExp = visit_ListComp

        def visit_FunctionDef(self, node):
            local_bodies[node.name] = node.body
            for item in node.decorator_list:
                self.visit(item)

        visit_AsyncFunctionDef = visit_FunctionDef
        visit_ClassDef = visit_FunctionDef

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
            name = resolve(node.func)
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
        (row["operation"], row.get("argument-key", row["id"]))
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


def fetch_session_resource(env, session_id, gateway_port, resource):
    """Read a persisted session resource through the canonical authenticated client."""
    if not re.fullmatch(r"[0-9a-fA-F-]{36}", str(session_id or "")):
        raise ValueError(f"invalid persisted session id {session_id!r}")
    path = f"/v1/sessions/{session_id}" + ("/usage" if resource == "usage" else "")
    marker_prefix = f"VIS_E2E_{resource.upper()}\t"
    form = (
        "(require '[com.blockether.vis.internal.gateway.client :as gateway-client]) "
        f'(gateway-client/ensure-gateway! {{:host "127.0.0.1" :port {gateway_port}}}) '
        f'(let [response (gateway-client/request! :get "{path}" {{:timeout-ms 30000}})] '
        f'(println (str "{marker_prefix}" (:status response) "\t" (:body response))))'
    )
    try:
        result = gateway_eval(env, form, 60)
    except subprocess.TimeoutExpired as exc:
        raise RuntimeError(f"{resource} query timed out after {exc.timeout}s") from exc
    marker = next(
        (
            line
            for line in reversed(result.stdout.splitlines())
            if line.startswith(marker_prefix)
        ),
        None,
    )
    if result.returncode or marker is None:
        lines = (result.stderr or result.stdout or "").strip().splitlines()
        suffix = f": {lines[-1]}" if lines else ""
        raise RuntimeError(f"{resource} query exited {result.returncode}{suffix}")
    _, status, body = marker.split("\t", 2)
    return int(status), body


def fetch_session_usage(env, session_id, gateway_port):
    """Read persisted usage through the canonical authenticated gateway client."""
    status, body = fetch_session_resource(env, session_id, gateway_port, "usage")
    return status, decode_usage_body(body)


def fetch_session_goal(env, session_id, gateway_port):
    """Read the saved goal from the session soul, not the model's assertion."""
    status, body = fetch_session_resource(env, session_id, gateway_port, "soul")
    payload = json.loads(body)
    goal = payload.get("goal") if isinstance(payload, dict) else None
    return status, goal


def stdout_recovery_failures(form_outputs, form_events, expected):
    """Require exact saved stdout, recovered by its original scope and tool-call id."""
    minimum = expected["min_chars"]
    head, middle, tail = (expected[key] for key in ("head", "middle", "tail"))
    originals = [
        item
        for item in form_outputs
        if len(item["stdout"]) >= minimum
        and item["stdout"].startswith(head)
        and middle in item["stdout"]
        and item["stdout"].rstrip().endswith(tail)
    ]
    if not originals:
        return ["no oversized raw stdout with the expected head, middle and tail"]
    first = originals[0]
    scope = first["scope"].split("/f", 1)[0]
    call_id = first.get("tool_call_id")
    if not scope or not call_id:
        return ["oversized stdout has no recoverable scope and tool-call id"]
    digest = hashlib.sha256(first["stdout"].encode("utf-8")).hexdigest()
    readers = {
        event["scope"]
        for event in form_events
        if isinstance(event["iteration"], int)
        and event["iteration"] > first["iteration"]
        and "read_session()" in event["code"]
        and ("['stdout']" in event["code"] or '["stdout"]' in event["code"])
        and "svar_tool_call_id" in event["code"]
        and any(f"{q}{scope}{q}" in event["code"] for q in ('"', "'"))
        and any(f"{q}{call_id}{q}" in event["code"] for q in ('"', "'"))
    }
    if not any(
        item["scope"] in readers
        and item["iteration"] > first["iteration"]
        and f"LEN: {len(first['stdout'])}" in item["stdout"]
        and f"SHA256: {digest}" in item["stdout"]
        and middle in item["stdout"]
        and "GOAL_STATUS: active" in item["stdout"]
        for item in form_outputs
    ):
        return [
            "no later read_session() block verified the exact stdout while the goal was active"
        ]
    return []


def decode_usage_body(body):
    """Decode the usage endpoint's public `{"usage": ...}` envelope."""
    payload = json.loads(body)
    usage = payload.get("usage") if isinstance(payload, dict) else None
    if not isinstance(usage, dict):
        raise ValueError("usage response has no usage object")
    return usage


def stop_source_gateway(gateway):
    """Stop and remove the isolated source gateway; return its client exit code."""
    form = (
        "(require '[com.blockether.vis.internal.gateway.client :as gateway-client]) "
        f'(gateway-client/ensure-gateway! {{:host "127.0.0.1" :port {gateway["port"]}}}) '
        "(gateway-client/stop-daemon!)"
    )
    try:
        return gateway_eval(gateway["env"], form, 60).returncode
    except (OSError, subprocess.SubprocessError):
        return -1
    finally:
        shutil.rmtree(gateway["runtime"], ignore_errors=True)


def start_source_gateway(*, cwd=REPO):
    """Start a current-classpath gateway on an isolated DB and free loopback port."""
    runtime = tempfile.mkdtemp(prefix="vis_e2e_gateway_")
    env = os.environ.copy()
    env.pop("VIS_GATEWAY_URL", None)
    env["VIS_DB_PATH"] = os.path.join(runtime, "vis.mdb")
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind(("127.0.0.1", 0))
        port = sock.getsockname()[1]
    gateway = {"env": env, "port": port, "runtime": runtime}
    form = (
        "(require '[com.blockether.vis.internal.gateway.client :as gateway-client]) "
        f'(gateway-client/ensure-gateway! {{:host "127.0.0.1" :port {port}}})'
    )
    try:
        result = gateway_eval(env, form, 120, cwd=cwd)
    except (OSError, subprocess.SubprocessError):
        stop_source_gateway(gateway)
        raise
    if result.returncode:
        detail = (result.stderr or result.stdout or "").strip().splitlines()
        stop_source_gateway(gateway)
        raise RuntimeError(
            f"source gateway failed (exit {result.returncode})"
            + (": " + " | ".join(detail[:6])[:800] if detail else "")
        )
    return gateway


# Every scenario lives here: editing tasks over real files in several formats.
SCENARIO_ROOTS = [os.path.join(HERE, "scenarios")]


def load_scenarios(pick):
    out = []
    for root in SCENARIO_ROOTS:
        if not os.path.isdir(root):
            continue
        for sid in sorted(os.listdir(root)):
            meta = os.path.join(root, sid, "scenario.json")
            if not os.path.isfile(meta):
                continue
            if pick and sid not in pick:
                continue
            with open(meta) as fh:
                sc = json.load(fh)
            sc["id"] = sid
            sc["_dir"] = os.path.join(root, sid)
            out.append(sc)
    return out


def seed_files(sc, work):
    fixture_dir = sc["_dir"]
    if source := sc.get("files_from"):
        if not re.fullmatch(r"[a-z0-9-]+", source):
            raise ValueError("files_from must name a sibling scenario")
        fixture_dir = os.path.join(os.path.dirname(fixture_dir), source)
        if not os.path.isfile(os.path.join(fixture_dir, "scenario.json")):
            raise ValueError(f"files_from scenario does not exist: {source}")
    fdir = os.path.join(fixture_dir, "files")
    for root, _, names in os.walk(fdir):
        for n in names:
            src = os.path.join(root, n)
            dst = os.path.join(work, os.path.relpath(src, fdir))
            os.makedirs(os.path.dirname(dst), exist_ok=True)
            shutil.copyfile(src, dst)
    if registrations := sc.get("workspace_filesystem"):
        entries = [
            {"id": name, "path": os.path.realpath(os.path.join(work, relative))}
            for name, relative in registrations.items()
        ]
        config = {
            "workspace": {"filesystem": entries},
            "jail": {"filesystem": {"allow": list(registrations)}},
        }
        with open(os.path.join(work, "vis.yml"), "w") as fh:
            json.dump(config, fh, indent=2)
            fh.write("\n")


def run_one(job):
    sc, model, run_env, gateway_port = job
    timeout = int(os.environ.get("VIS_E2E_TIMEOUT", sc.get("timeout_s", TIMEOUT)))
    work = tempfile.mkdtemp(prefix=f"vis_e2e_{sc['id']}_")
    local_gateway = None
    try:
        seed_files(sc, work)
        for cmd in (
            ["git", "init", "-q", "."],
            ["git", "config", "user.email", "t@t.co"],
            ["git", "config", "user.name", "t"],
            ["git", "add", "-A"],
            ["git", "commit", "-qm", "init"],
        ):
            subprocess.run(cmd, cwd=work, check=True)

        # Project extensions load when the gateway starts, not when files appear
        # in a later client's workspace. Keep their registry isolated per run.
        if os.path.isdir(os.path.join(work, ".vis", "extensions")):
            local_gateway = start_source_gateway(cwd=work)
            run_env, gateway_port = local_gateway["env"], local_gateway["port"]

        t0 = time.time()
        exit_code = None
        try:
            command = agent_prefix(work) + [
                "--full-trace-json-stream",
                "--provider",
                PROVIDER,
                "--model",
                model,
            ]
            if REASONING_EFFORT:
                command.extend(["--reasoning-effort", REASONING_EFFORT])
            command.append("--persist")
            command.append(sc["prompt"])
            p = subprocess.run(
                command,
                cwd=REPO,
                env=run_env,
                capture_output=True,
                text=True,
                timeout=timeout,
            )
            out = p.stdout
            exit_code = p.returncode
        except subprocess.TimeoutExpired as e:
            out = (
                (e.stdout or b"").decode()
                if isinstance(e.stdout, bytes)
                else (e.stdout or "")
            )
        wall = time.time() - t0

        os.makedirs(TRACES, exist_ok=True)
        tag = sc["id"] + ("__" + model if len(MODELS) > 1 else "")
        if "_repeat" in sc:
            tag += f"__run{sc['_repeat']}"
        with open(os.path.join(TRACES, tag + ".jsonl"), "w") as fh:
            fh.write(out)

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
        unparsed = []
        done = False
        answer = ""
        result_tokens = {}
        result_cost = {}
        result_session_id = None
        result_eval = {}
        for line in out.splitlines():
            line = line.strip()
            if not line:
                continue
            try:
                o = json.loads(line)
            except ValueError:
                unparsed.append(line)
                continue
            ev = o.get("event")
            pl = o.get("payload", {})
            if ev == "result":
                result_tokens = pl.get("tokens") or {}
                if not isinstance(result_tokens, dict):
                    errs.append("provider tokens are not an object")
                    result_tokens = {}
                result_cost = pl.get("cost") or {}
                result_session_id = pl.get("session-id")
                result_eval = pl.get("eval") or {}
                content_errors = [
                    block.get("message") or block.get("title") or "Provider error"
                    for block in pl.get("content") or []
                    if isinstance(block, dict) and block.get("type") == "error"
                ]
                errs.extend(content_errors)
                a = pl.get("answer")
                if isinstance(a, dict):
                    answer = a.get("answer", "")
                elif a is not None:
                    answer = str(a)
                elif isinstance(pl.get("content"), list):
                    answer = "\n".join(
                        str(block.get("markdown") or block.get("text") or "")
                        if isinstance(block, dict)
                        else str(block)
                        for block in pl["content"]
                    )
                if answer and not pl.get("error") and not content_errors:
                    done = True
                continue
            ph = pl.get("phase")
            if ph == "provider-call":
                provider_calls.append(
                    {
                        "provider": pl.get("provider"),
                        "model": pl.get("model"),
                        "iteration": pl.get("iteration"),
                    }
                )
            elif ph == "form-start":
                code = pl.get("code", "")
                forms.append(code)
                form_events.append(
                    {
                        "code": code,
                        "scope": pl.get("scope", ""),
                        "iteration": pl.get("iteration"),
                    }
                )
            elif ph == "form-activity":
                scope = pl.get("scope", "")
                rows = [
                    row
                    for row in (pl.get("activity") or {}).get("rows", [])
                    if row.get("id") and row.get("operation")
                ]
                present = {row["id"] for row in rows}
                # A later snapshot of the SAME form collapses finished rows into one
                # group row. A non-terminal row that snapshot no longer lists was
                # superseded, not abandoned; terminal rows stay as evidence.
                for row_id, previous in list(activities.items()):
                    if (
                        previous.get("scope") == scope
                        and row_id not in present
                        and previous.get("state") not in TERMINAL_ACTIVITY_STATES
                    ):
                        del activities[row_id]
                for row in rows:
                    previous = activities.get(row["id"], {})
                    if previous.get("state") not in TERMINAL_ACTIVITY_STATES:
                        activities[row["id"]] = {**row, "scope": scope}
                    elif row.get("state") in {"failed", "cancelled"}:
                        activities[row["id"]] = {**row, "scope": scope}
            elif ph == "form-result":
                stdout = pl.get("stdout") or ""
                form_outputs.append(
                    {
                        "scope": pl.get("scope", ""),
                        "iteration": pl.get("iteration"),
                        "stdout": stdout,
                        "tool_call_id": pl.get("tool-call-id"),
                    }
                )
                largest_form_output = max(largest_form_output, len(stdout))
                total_form_output += len(stdout)
                if pl.get("error"):
                    surfaced_scopes.add(pl.get("scope", ""))
                    e = pl.get("error")
                    errs.append(
                        e.get("message", "?") if isinstance(e, dict) else str(e)
                    )
            elif ph == "iteration-final" and pl.get("done?"):
                done = True

        if exit_code is None:
            errs.append(f"vis-agent timed out after {timeout}s")
        elif exit_code:
            suffix = f": {unparsed[0][:120]}" if unparsed else ""
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
        errs.extend(
            f"activity {row['operation']} {row.get('state')}"
            for row in [*caught_failures, *unscoped_failures, *incomplete_activities]
        )
        tools = [
            row["operation"] for row in activity_rows if row.get("state") == "succeeded"
        ]
        correct = not (failed_activities or incomplete_activities)
        detail = []
        for name, subs in (sc.get("want") or {}).items():
            try:
                txt = open(os.path.join(work, name)).read()
            except FileNotFoundError:
                txt = ""
            for s in subs:
                if s not in txt:
                    correct = False
                    detail.append(f"missing {name}:{s!r}")
        for name, subs in (sc.get("wantnot") or {}).items():
            try:
                txt = open(os.path.join(work, name)).read()
            except FileNotFoundError:
                txt = ""
            for s in subs:
                if s in txt:
                    correct = False
                    detail.append(f"still present {name}:{s!r}")
        for s in sc.get("want_answer") or []:
            if s not in answer:
                correct = False
                detail.append(f"answer missing {s!r}")
        for needle in sc.get("want_forms") or []:
            if not any(needle in form for form in forms):
                correct = False
                detail.append(f"form containing {needle!r} not used")
        if sc.get("want_stdout_recovery"):
            detail.extend(
                stdout_recovery_failures(
                    form_outputs, form_events, sc["want_stdout_recovery"]
                )
            )
        detail.extend(structured_failures(sc, work, answer, activity_rows))
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
            detail.extend(discovery_failures)
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
                detail.extend(helper_failures)
        if detail:
            correct = False
        total_limit = sc.get("max_total_output_chars")
        if total_limit is not None:
            if type(total_limit) is not int or total_limit < 0:
                correct = False
                detail.append("max_total_output_chars must be a nonnegative integer")
            elif total_form_output > total_limit:
                correct = False
                detail.append(
                    f"total output {total_form_output} chars exceeds max_total_output_chars={total_limit}"
                )
        output_limit = sc.get("max_form_output_chars")
        if output_limit is not None:
            if type(output_limit) is not int or output_limit < 0:
                correct = False
                detail.append("max_form_output_chars must be a nonnegative integer")
            elif largest_form_output > output_limit:
                correct = False
                detail.append(
                    f"form output {largest_form_output} chars exceeds "
                    f"max_form_output_chars={output_limit}"
                )

        if REASONING_EFFORT:
            effort_evidence = result_eval.get("reasoning_effort") or {}
            iterations = effort_evidence.get("iterations") or []
            if (
                result_eval.get("is_valid") is not True
                or result_eval.get("invalid_reasons")
                or effort_evidence.get("requested") != REASONING_EFFORT
                or not iterations
                or any(
                    item.get("requested") != REASONING_EFFORT
                    or item.get("effective") != REASONING_EFFORT
                    or (item.get("provider"), item.get("model")) != (PROVIDER, model)
                    or item.get("selected") != {"provider": PROVIDER, "model": model}
                    or item.get("is_fallback") is not False
                    for item in iterations
                )
            ):
                correct = False
                detail.append(f"invalid reasoning-effort evidence: {result_eval!r}")

        if sc.get("want_requested_route") or REASONING_EFFORT:
            expected_route = (PROVIDER, model)
            actual_routes = [
                (call["provider"], call["model"]) for call in provider_calls
            ]
            if not actual_routes or any(
                route != expected_route for route in actual_routes
            ):
                correct = False
                detail.append(
                    f"provider calls {actual_routes!r}, expected only {expected_route!r}"
                )
            cost_route = (result_cost.get("provider"), result_cost.get("model"))
            if cost_route != expected_route:
                correct = False
                detail.append(
                    f"billed route {cost_route!r}, expected {expected_route!r}"
                )

        fold_forms = [
            event for event in form_events if "fold_session(" in event["code"]
        ]
        if sc.get("want_folded_prefix"):
            if len(fold_forms) != 1:
                correct = False
                detail.append(f"expected one prefix fold, observed {len(fold_forms)}")
            else:
                fold_event = fold_forms[0]
                scope_match = re.match(r"t(\d+)/i(\d+)(?:/|$)", fold_event["scope"])
                if not scope_match:
                    correct = False
                    detail.append(
                        f"fold form has invalid scope {fold_event['scope']!r}"
                    )
                else:
                    turn = int(scope_match.group(1))
                    iteration = int(scope_match.group(2))
                    expected_key = f"-t{turn}/i{iteration - 1}"
                    fold_keys = literal_fold_keys(fold_event["code"])
                    if iteration <= 1 or fold_keys != [expected_key]:
                        correct = False
                        detail.append(
                            f"fold form did not use exact prior-prefix key: expected {expected_key!r}, got {fold_keys!r}"
                        )
                    if not any(
                        isinstance(call["iteration"], int)
                        and call["iteration"] > iteration
                        for call in provider_calls
                    ):
                        correct = False
                        detail.append(
                            "no provider continuation followed the prefix fold"
                        )

        tokens, token_failures = token_summary(result_tokens)
        if token_failures and (result_tokens or sc.get("want_cache_metrics")):
            correct = False
            detail.extend(token_failures)
        cached_tokens = tokens.get("cached", 0)
        if sc.get("want_cache_read") and cached_tokens <= 0:
            correct = False
            detail.append("provider reported zero prompt-cache read tokens")

        cache_usage = None
        if sc.get("want_cache_metrics") or sc.get("want_folded_prefix"):
            if not result_session_id:
                correct = False
                detail.append("persistent run returned no session id")
            else:
                try:
                    usage_status, cache_usage = fetch_session_usage(
                        run_env, result_session_id, gateway_port
                    )
                except (OSError, ValueError, RuntimeError, json.JSONDecodeError) as exc:
                    correct = False
                    detail.append(f"could not read persisted usage metrics: {exc}")
                else:
                    if usage_status != 200 or not isinstance(cache_usage, dict):
                        correct = False
                        detail.append(
                            f"usage endpoint returned status {usage_status} and {type(cache_usage).__name__}"
                        )
                    else:
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
                                metric_failures.append(
                                    "usage output_tokens != provider output tokens"
                                )
                        else:
                            metric_failures = fold_count_failures(cache_usage)
                        if metric_failures:
                            correct = False
                            detail.extend(metric_failures)
        goal_state = None
        if sc.get("want_goal_complete"):
            if not result_session_id:
                correct = False
                detail.append("goal run returned no persisted session id")
            else:
                try:
                    goal_status, goal_state = fetch_session_goal(
                        run_env, result_session_id, gateway_port
                    )
                except (OSError, ValueError, RuntimeError, json.JSONDecodeError) as exc:
                    correct = False
                    detail.append(f"could not read persisted goal: {exc}")
                else:
                    if (
                        goal_status != 200
                        or not isinstance(goal_state, dict)
                        or goal_state.get("status") != "complete"
                    ):
                        correct = False
                        detail.append(
                            f"goal status is not complete (HTTP {goal_status})"
                        )
        toolset = {t for t in tools if t}
        for t in sc.get("want_tools") or []:
            if t not in toolset:
                correct = False
                detail.append(f"tool {t!r} not used")

        used_patch = "patch" in toolset
        path = "patch" if used_patch else "cat-only"
        if path == "cat-only" or errs or not (done and correct):
            detail.append(
                "tools=" + ",".join(f"{t}×{tools.count(t)}" for t in sorted(toolset))
            )
        evidence = []
        if type(output_limit) is int and output_limit >= 0:
            evidence.append(
                f"max-form-output={largest_form_output}/{output_limit} chars"
            )
        evidence.append(
            f"stdout={total_form_output} total chars; surfaced-errors={surfaced_errors}; activity-failures={len(failed_activities)} ({len(caught_failures)} without a form error)"
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
        if REASONING_EFFORT:
            evidence.append(f"reasoning-effort={REASONING_EFFORT}")
        if sc.get("want_requested_route") or REASONING_EFFORT:
            evidence.append(
                f"requested-route={PROVIDER}/{model} calls={len(provider_calls)}"
            )
        if sc.get("want_folded_prefix") and fold_forms:
            evidence.append(f"fold={fold_forms[0]['scope']}→prior-prefix")
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
        return {
            "id": sc["id"],
            "lang": sc["lang"],
            "provider": PROVIDER,
            "model": model,
            "repeat": sc.get("_repeat", 1),
            "converged": done,
            "correct": correct,
            "errors": len(errs),
            "err_msgs": errs[:2],
            "wall": round(wall, 1),
            "forms": len(forms),
            "provider_calls": len(provider_calls),
            "tokens": tokens,
            "token_errors": token_failures,
            "cache_usage": {
                key: cache_usage.get(key)
                for key in (
                    *CACHE_USAGE_FIELDS,
                    "output_tokens",
                    "output_reasoning_tokens",
                )
            }
            if isinstance(cache_usage, dict)
            else None,
            "max_form_output_chars": largest_form_output,
            "total_output_chars": total_form_output,
            "surfaced_errors": surfaced_errors,
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
        }
    finally:
        if local_gateway:
            stop_source_gateway(local_gateway)
        if not os.environ.get("VIS_E2E_KEEP"):
            shutil.rmtree(work, ignore_errors=True)


def main():
    pick = set(sys.argv[1:])
    scs = load_scenarios(pick)
    try:
        repeats = int(os.environ.get("VIS_E2E_REPEATS", "1"))
        if repeats < 1:
            raise ValueError
    except ValueError:
        print("VIS_E2E_REPEATS must be a positive integer", file=sys.stderr)
        sys.exit(2)
    if not scs:
        print(
            "no scenarios found under "
            + ", ".join(SCENARIO_ROOTS)
            + (f" matching {pick}" if pick else "")
        )
        sys.exit(2)
    try:
        gateway = start_source_gateway()
    except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
        print(f"could not start source gateway: {exc}", file=sys.stderr)
        sys.exit(2)
    jobs = [
        (
            {**sc, **({"_repeat": repeat} if repeats > 1 else {})},
            model,
            gateway["env"],
            gateway["port"],
        )
        for sc in scs
        for model in MODELS
        for repeat in range(1, repeats + 1)
    ]
    print(
        f"running {len(scs)} scenarios × {len(MODELS)} model(s) {MODELS} on {PROVIDER} "
        f"through {NATIVE_BIN or 'source JVM'} gateway 127.0.0.1:{gateway['port']} "
        f"(reasoning-effort={REASONING_EFFORT or 'default'}) "
        f"(repeats={repeats}, workers={WORKERS}, default timeout={TIMEOUT}s)\n"
    )
    results = []
    try:
        with concurrent.futures.ThreadPoolExecutor(max_workers=WORKERS) as ex:
            for result in ex.map(run_one, jobs):
                results.append(result)
    finally:
        stop_code = stop_source_gateway(gateway)
        if stop_code:
            print(
                f"warning: source gateway cleanup exited {stop_code}", file=sys.stderr
            )
    results.sort(key=lambda r: (r["id"], r["model"], r["repeat"]))
    summaries = summarize_results(results)
    os.makedirs(TRACES, exist_ok=True)
    with open(os.path.join(TRACES, "results.json"), "w") as stream:
        json.dump({"runs": results, "summaries": summaries}, stream, indent=2)
        stream.write("\n")

    mw = max(8, max((len(m) for m in MODELS), default=8))
    hdr = f"{'scenario':<18}{'model':<{mw}} {'lang':<11}{'conv':<5}{'ok':<4}{'err':<4}{'path':<14}{'forms':<6}{'sec':<6}"
    print(hdr)
    print("-" * len(hdr))
    nclean = nfast = 0
    for r in results:
        nfast += r["used_patch"]
        nclean += r["errors"] == 0
        print(
            f"{r['id']:<18}{r['model']:<{mw}} {r['lang']:<11}"
            f"{'✓' if r['converged'] else '✗':<5}{'✓' if r['correct'] else '✗':<4}"
            f"{r['errors']:<4}{r['edit_path']:<14}{r['forms']:<6}{r['wall']:<6}"
        )
        for d in r["detail"]:
            print(f"    ! {d}")
        for b in r["behavior"]:
            print(f"    ~ {b}")
        for item in r["evidence"]:
            print(f"    · {item}")
        for e in r["err_msgs"]:
            print(f"    err: {e[:140]}")
    n = len(results)
    # CROSS-VALIDATION GATE: a scenario passes only if EVERY model converged,
    # produced correct output, and had no loop/tool errors. `PATCH(fast)`
    # remains a performance/adherence metric because some scenarios legitimately
    # answer from the REPL instead of editing a file. A scenario marked
    # `measurement` reports its behavior rate instead of gating on it: a model's
    # habits vary between runs, while its answer, errors and edits do not.
    by_scn = {}
    for r in results:
        by_scn.setdefault(r["id"], []).append(
            r["converged"] and r["correct"] and r["errors"] == 0
        )
    gated = sum(1 for oks in by_scn.values() if all(oks))
    ok_clean = sum(
        1 for r in results if r["converged"] and r["correct"] and r["errors"] == 0
    )
    print("-" * len(hdr))
    print(
        f"RUNS converged+correct+clean {ok_clean}/{n} "
        f"| NO-ERROR {nclean}/{n} | PATCH(fast) {nfast}/{n}"
    )
    print(
        f"GATE (scenario passes iff ALL {len(MODELS)} model(s) pass cleanly): {gated}/{len(by_scn)}"
    )
    for summary in summaries:
        print(
            f"SUMMARY {summary['id']} {summary['provider']}/{summary['model']}: {summary['passed']}/{summary['runs']} passed; token samples={summary['token_samples']}"
        )
        if summary["measurement"]:
            print(
                f"    BEHAVIOR (measured, not gated): {summary['behavior_passed']}/{summary['runs']} runs met the behavior check"
            )
        if summary["token_samples"]:
            print(
                "    token medians="
                + ", ".join(
                    f"{key}:{summary[key]['median']} [{summary[key]['min']}..{summary[key]['max']}]"
                    for key in ("input", "cached", "uncached", "output")
                )
            )
            print(
                f"    aggregate cached-input share={summary['cached_input_percent']}%; wall median={summary['wall']['median']}s"
            )
    print(f"Full measurements: {os.path.join(TRACES, 'results.json')}")
    sys.exit(0 if gated == len(by_scn) else 1)


if __name__ == "__main__":
    main()
