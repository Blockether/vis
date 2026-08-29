#!/usr/bin/env python3
"""End-to-end editing harness — drives the REAL vis-agent CLI on a battery of editing
scenarios and checks, per scenario:

  - CONVERGED   the loop reached a final answer (no hang / crash)
  - CORRECT     resulting files satisfy want / wantnot, the answer contains
                want_answer, and want_tools were actually used
  - NO-ERROR    no form raised inside the loop
  - FAST PATH   the anchored `patch` wrote the edit, rather than the model
                wandering through the file with `cat` alone

Scenarios are SELF-CONTAINED FOLDERS, aggregated from the root `e2e/` and each
language pack's `e2e/` (the language-neutral editing set lives in the root; a pack
owns the scenarios that exercise its surface, beside its `test/` dir):

    e2e/scenarios/<id>/                                  # foundation, any language
    extensions/languages/<pack>/e2e/scenarios/<id>/      # that pack's surface
      scenario.json   {lang, prompt, want, wantnot, want_answer?,
                       want_tools?, want_forms?, want_requested_route?,
                       want_folded_prefix?, want_cache_read?}
      files/          real files seeded into a fresh git repo before the run

`want`/`wantnot` are {path: [substring, ...]} checks on the resulting files;
`want_answer` is substrings the final answer must contain (REPL / non-file
scenarios); `want_tools` are extension tools that MUST have fired (e.g.
repl_eval); `want_forms` are source substrings that MUST occur in a top-level
sandbox form (e.g. fold_session()). The three boolean benchmark guards pin the
requested route, the canonical oldest-prefix fold, and real provider cache reads.

Each scenario runs in its own throwaway git repo through one source-owned gateway on an
isolated temporary DB, so an already-running installed gateway cannot mask working-tree edits. Runs are parallel. Usage:

    VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.3-flash python3 run.py [scenario-id ...]
"""

import ast
import concurrent.futures
import json
import os
import re
import shutil
import socket
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))  # <repo>/e2e
REPO = os.path.dirname(HERE)
CLOJURE = os.environ.get("VIS_E2E_CLOJURE", "clojure")
PROVIDER = os.environ.get("VIS_PROVIDER", "zai-coding-plan")
MODEL = os.environ.get("VIS_MODEL", "glm-5.3-flash")
# Cross-validation gate: a scenario passes only if EVERY model passes it.
MODELS = [
    m.strip() for m in os.environ.get("VIS_MODELS", MODEL).split(",") if m.strip()
]
TIMEOUT = int(os.environ.get("VIS_E2E_TIMEOUT", "300"))
WORKERS = int(os.environ.get("VIS_E2E_WORKERS", "5"))
TRACES = os.environ.get("VIS_E2E_TRACES", "/tmp/vis_e2e/traces")


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


def gateway_eval(env, form, timeout):
    """Evaluate one canonical gateway-client form against the working tree."""
    return subprocess.run(
        [CLOJURE, "-M", "-e", form],
        cwd=REPO,
        env=env,
        capture_output=True,
        text=True,
        timeout=timeout,
    )


def stop_source_gateway(gateway):
    """Stop and remove the isolated source gateway; return its client exit code."""
    form = (
        "(require '[com.blockether.vis.internal.gateway.client :as gateway-client]) "
        "(gateway-client/stop-daemon!)"
    )
    try:
        return gateway_eval(gateway["env"], form, 60).returncode
    except (OSError, subprocess.SubprocessError):
        return -1
    finally:
        shutil.rmtree(gateway["runtime"], ignore_errors=True)


def start_source_gateway():
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
        result = gateway_eval(env, form, 120)
    except (OSError, subprocess.SubprocessError):
        stop_source_gateway(gateway)
        raise
    if result.returncode:
        detail = (result.stderr or result.stdout or "").strip().splitlines()
        stop_source_gateway(gateway)
        raise RuntimeError(
            f"source gateway failed (exit {result.returncode})"
            + (f": {detail[-1]}" if detail else "")
        )
    return gateway


# Scenario roots, aggregated like the test alias's `--dir` list: the foundation
# (language-neutral editing) set lives here in the root; each language pack
# owns the e2e scenarios that exercise ITS surface, alongside its `test/` dir.
SCENARIO_ROOTS = [os.path.join(HERE, "scenarios")]
LANG_ROOT = os.path.join(REPO, "extensions/languages")
if os.path.isdir(LANG_ROOT):
    SCENARIO_ROOTS.extend(
        os.path.join(LANG_ROOT, pack, "e2e/scenarios")
        for pack in sorted(os.listdir(LANG_ROOT))
    )


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
    fdir = os.path.join(sc["_dir"], "files")
    for root, _, names in os.walk(fdir):
        for n in names:
            src = os.path.join(root, n)
            dst = os.path.join(work, os.path.relpath(src, fdir))
            os.makedirs(os.path.dirname(dst), exist_ok=True)
            shutil.copyfile(src, dst)


def run_one(job):
    sc, model, run_env = job
    work = tempfile.mkdtemp(prefix=f"vis_e2e_{sc['id']}_")
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

        t0 = time.time()
        exit_code = None
        try:
            p = subprocess.run(
                [
                    CLOJURE,
                    f"-J-Duser.dir={work}",
                    "-M:vis",
                    "--full-trace-json-stream",
                    "--provider",
                    PROVIDER,
                    "--model",
                    model,
                    sc["prompt"],
                ],
                cwd=REPO,
                env=run_env,
                capture_output=True,
                text=True,
                timeout=TIMEOUT,
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
        with open(os.path.join(TRACES, tag + ".jsonl"), "w") as fh:
            fh.write(out)

        forms = []
        form_events = []
        form_results = {}
        provider_calls = []
        tools = []
        errs = []
        unparsed = []
        done = False
        answer = ""
        result_tokens = {}
        result_cost = {}
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
                result_cost = pl.get("cost") or {}
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
                if answer and not pl.get("error"):
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
            elif ph == "tool-start":
                te = pl.get("tool-event") or {}
                sym = te.get("symbol") or te.get("op")
                if sym:
                    tools.append(sym)
            elif ph == "form-result":
                form_results[pl.get("scope", "")] = str(pl.get("result") or "")
                if pl.get("error"):
                    e = pl.get("error")
                    errs.append(
                        e.get("message", "?") if isinstance(e, dict) else str(e)
                    )
            elif ph == "iteration-final" and pl.get("done?"):
                done = True

        if exit_code is None:
            errs.append(f"vis-agent timed out after {TIMEOUT}s")
        elif exit_code:
            suffix = f": {unparsed[0][:120]}" if unparsed else ""
            errs.append(f"vis-agent exited {exit_code}{suffix}")
        correct = True
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

        if sc.get("want_requested_route"):
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
                fold_receipt = form_results.get(fold_event["scope"], "")
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
                    expected_receipt = f"folded through t{turn}/i{iteration - 1}"
                    if iteration <= 1 or expected_receipt not in fold_receipt:
                        correct = False
                        detail.append(
                            f"fold receipt did not return to prior prefix: expected {expected_receipt!r}"
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

        cached_tokens = int(result_tokens.get("cached") or 0)
        if sc.get("want_cache_read") and cached_tokens <= 0:
            correct = False
            detail.append("provider reported zero prompt-cache read tokens")

        toolset = {t for t in tools if t}
        for t in sc.get("want_tools") or []:
            if t not in toolset:
                correct = False
                detail.append(f"tool {t!r} not used")

        used_patch = "patch" in toolset
        if used_patch:
            path = "patch"
        elif toolset & {"repl_eval", "repl_start"}:
            path = "repl"
        else:
            path = "cat-only"
        if path == "cat-only" or errs or not (done and correct):
            detail.append(
                "tools=" + ",".join(f"{t}×{tools.count(t)}" for t in sorted(toolset))
            )
        evidence = []
        if sc.get("want_requested_route"):
            evidence.append(f"route={PROVIDER}/{model} calls={len(provider_calls)}")
        if sc.get("want_folded_prefix") and fold_forms:
            evidence.append(f"fold={fold_forms[0]['scope']}→prior-prefix")
        if sc.get("want_cache_read"):
            evidence.append(
                f"cache-read={cached_tokens}/{int(result_tokens.get('input') or 0)} input tokens"
            )
        return {
            "id": sc["id"],
            "lang": sc["lang"],
            "model": model,
            "converged": done,
            "correct": correct,
            "errors": len(errs),
            "err_msgs": errs[:2],
            "wall": round(wall, 1),
            "forms": len(forms),
            "used_patch": used_patch,
            "edit_path": path,
            "detail": detail,
            "evidence": evidence,
        }
    finally:
        if not os.environ.get("VIS_E2E_KEEP"):
            shutil.rmtree(work, ignore_errors=True)


def main():
    pick = set(sys.argv[1:])
    scs = load_scenarios(pick)
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
    jobs = [(sc, model, gateway["env"]) for sc in scs for model in MODELS]
    print(
        f"running {len(scs)} scenarios × {len(MODELS)} model(s) {MODELS} on {PROVIDER} "
        f"through source gateway 127.0.0.1:{gateway['port']} "
        f"(workers={WORKERS}, timeout={TIMEOUT}s)\n"
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
    results.sort(key=lambda r: (r["id"], r["model"]))

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
        for item in r["evidence"]:
            print(f"    · {item}")
        for e in r["err_msgs"]:
            print(f"    err: {e[:140]}")
    n = len(results)
    # CROSS-VALIDATION GATE: a scenario passes only if EVERY model converged,
    # produced correct output, and had no loop/tool errors. `PATCH(fast)`
    # remains a performance/adherence metric because some scenarios legitimately
    # answer from the REPL instead of editing a file.
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
    sys.exit(0 if gated == len(by_scn) else 1)


if __name__ == "__main__":
    main()
