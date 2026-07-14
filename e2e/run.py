#!/usr/bin/env python3
"""End-to-end editing harness — drives the REAL vis CLI on a battery of editing
scenarios and checks, per scenario:

  - CONVERGED   the loop reached a final answer (no hang / crash)
  - CORRECT     resulting files satisfy want / wantnot, the answer contains
                want_answer, and want_tools were actually used
  - NO-ERROR    no form raised inside the loop
  - FAST PATH   the structural tools (struct_patch / sexpr) were used rather than
                the slower cat -> patch anchor dance

Scenarios are SELF-CONTAINED FOLDERS, aggregated from the root `e2e/` and each
language pack's `e2e/` (the foundation/struct_patch set lives in the root; a pack
owns the scenarios that exercise its surface, beside its `test/` dir):

    e2e/scenarios/<id>/                                  # foundation, any language
    extensions/languages/<pack>/e2e/scenarios/<id>/      # that pack's surface
      scenario.json   {lang, prompt, want, wantnot, want_answer?, want_tools?}
      files/          real files seeded into a fresh git repo before the run

`want`/`wantnot` are {path: [substring, ...]} checks on the resulting files;
`want_answer` is substrings the final answer must contain (REPL / non-file
scenarios); `want_tools` are tools that MUST have fired (e.g. repl_eval).

Each scenario runs in its own throwaway git repo with the live working-tree source
(`--source`). Runs are parallel. Usage:

    VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.2 python3 run.py [scenario-id ...]
"""
import json, os, subprocess, sys, tempfile, shutil, concurrent.futures, time

HERE     = os.path.dirname(os.path.abspath(__file__))   # <repo>/e2e
REPO     = os.path.dirname(HERE)
VIS      = os.path.join(REPO, "bin", "vis")
PROVIDER = os.environ.get("VIS_PROVIDER", "zai-coding-plan")
MODEL    = os.environ.get("VIS_MODEL", "glm-5.2")
# Cross-validation gate: a scenario passes only if EVERY model passes it.
MODELS   = [m.strip() for m in os.environ.get("VIS_MODELS", MODEL).split(",") if m.strip()]
TIMEOUT  = int(os.environ.get("VIS_E2E_TIMEOUT", "300"))
WORKERS  = int(os.environ.get("VIS_E2E_WORKERS", "5"))
TRACES   = os.environ.get("VIS_E2E_TRACES", "/tmp/vis_e2e/traces")
STRUCTURAL = {"struct_patch", "sexpr"}

# Scenario roots, aggregated like the test alias's `--dir` list: the foundation
# (language-neutral struct_patch) set lives here in the root; each language pack
# owns the e2e scenarios that exercise ITS surface, alongside its `test/` dir.
SCENARIO_ROOTS = [os.path.join(HERE, "scenarios")]
LANG_ROOT = os.path.join(REPO, "extensions/languages")
if os.path.isdir(LANG_ROOT):
    SCENARIO_ROOTS.extend(
        os.path.join(LANG_ROOT, pack, "e2e/scenarios")
        for pack in sorted(os.listdir(LANG_ROOT)))

def load_scenarios(pick):
    out = []
    for root in SCENARIO_ROOTS:
        if not os.path.isdir(root): continue
        for sid in sorted(os.listdir(root)):
            meta = os.path.join(root, sid, "scenario.json")
            if not os.path.isfile(meta): continue
            if pick and sid not in pick: continue
            sc = json.load(open(meta)); sc["id"] = sid; sc["_dir"] = os.path.join(root, sid)
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
    sc, model = job
    work = tempfile.mkdtemp(prefix=f"vis_e2e_{sc['id']}_")
    try:
        seed_files(sc, work)
        for cmd in (["git","init","-q","."], ["git","config","user.email","t@t.co"],
                    ["git","config","user.name","t"], ["git","add","-A"],
                    ["git","commit","-qm","init"]):
            subprocess.run(cmd, cwd=work, check=True)

        t0 = time.time()
        try:
            p = subprocess.run(
                [VIS,"--source","--full-trace-json-stream",
                 "--provider",PROVIDER,"--model",model, sc["prompt"]],
                cwd=work, capture_output=True, text=True, timeout=TIMEOUT)
            out = p.stdout
        except subprocess.TimeoutExpired as e:
            out = (e.stdout or b"").decode() if isinstance(e.stdout, bytes) else (e.stdout or "")
        wall = time.time() - t0

        os.makedirs(TRACES, exist_ok=True)
        tag = sc["id"] + ("__" + model if len(MODELS) > 1 else "")
        with open(os.path.join(TRACES, tag+".jsonl"), "w") as fh: fh.write(out)

        forms=[]; tools=[]; errs=[]; done=False; answer=""
        for line in out.splitlines():
            line=line.strip()
            if not line: continue
            try: o=json.loads(line)
            except: continue
            ev=o.get("event"); pl=o.get("payload",{})
            if ev=="result":
                a=pl.get("answer")
                answer=(a.get("answer","") if isinstance(a,dict) else str(a or ""))
                if a and not pl.get("error"): done=True
                continue
            ph=pl.get("phase")
            if ph=="form-start": forms.append(pl.get("code",""))
            elif ph=="tool-start":
                te=pl.get("tool-event") or {}
                sym=te.get("symbol") or te.get("op")
                if sym: tools.append(sym)
            elif ph=="form-result" and pl.get("error"):
                e=pl.get("error")
                errs.append(e.get("message","?") if isinstance(e,dict) else str(e))
            elif ph=="iteration-final" and pl.get("done?"): done=True

        correct=True; detail=[]
        for name, subs in (sc.get("want") or {}).items():
            try: txt=open(os.path.join(work,name)).read()
            except FileNotFoundError: txt=""
            for s in subs:
                if s not in txt: correct=False; detail.append(f"missing {name}:{s!r}")
        for name, subs in (sc.get("wantnot") or {}).items():
            try: txt=open(os.path.join(work,name)).read()
            except FileNotFoundError: txt=""
            for s in subs:
                if s in txt: correct=False; detail.append(f"still present {name}:{s!r}")
        for s in (sc.get("want_answer") or []):
            if s not in answer: correct=False; detail.append(f"answer missing {s!r}")
        toolset=set(t for t in tools if t)
        for t in (sc.get("want_tools") or []):
            if t not in toolset: correct=False; detail.append(f"tool {t!r} not used")

        used_structural=bool(toolset & STRUCTURAL)
        path=("struct_patch" if "struct_patch" in toolset
              else ("sexpr" if "sexpr" in toolset
                    else ("patch" if "patch" in toolset
                          else ("write" if "write" in toolset
                                else ("repl" if (toolset & {"repl_eval","repl_start"})
                                      else "cat-only")))))
        if path in ("cat-only","??") or errs or not (done and correct):
            detail.append("tools=" + ",".join(f"{t}×{tools.count(t)}" for t in sorted(toolset)))
        return dict(id=sc["id"], lang=sc["lang"], model=model, converged=done, correct=correct,
                    errors=len(errs), err_msgs=errs[:2], wall=round(wall,1),
                    forms=len(forms), used_structural=used_structural,
                    edit_path=path, detail=detail)
    finally:
        if not os.environ.get("VIS_E2E_KEEP"):
            shutil.rmtree(work, ignore_errors=True)

def main():
    pick=set(sys.argv[1:])
    scs=load_scenarios(pick)
    if not scs:
        print("no scenarios found under " + ", ".join(SCENARIO_ROOTS)
              + (f" matching {pick}" if pick else "")); sys.exit(2)
    jobs=[(sc, m) for sc in scs for m in MODELS]
    print(f"running {len(scs)} scenarios × {len(MODELS)} model(s) {MODELS} on {PROVIDER} "
          f"(workers={WORKERS}, timeout={TIMEOUT}s)\n")
    results=[]
    with concurrent.futures.ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for r in ex.map(run_one, jobs): results.append(r)
    results.sort(key=lambda r: (r["id"], r["model"]))

    mw=max(8, max((len(m) for m in MODELS), default=8))
    hdr=f"{'scenario':<18}{'model':<{mw}} {'lang':<11}{'conv':<5}{'ok':<4}{'err':<4}{'path':<14}{'forms':<6}{'sec':<6}"
    print(hdr); print("-"*len(hdr))
    nclean=nfast=0
    for r in results:
        nfast+=r["used_structural"]; nclean+=(r["errors"]==0)
        print(f"{r['id']:<18}{r['model']:<{mw}} {r['lang']:<11}"
              f"{'✓' if r['converged'] else '✗':<5}{'✓' if r['correct'] else '✗':<4}"
              f"{r['errors']:<4}{r['edit_path']:<14}{r['forms']:<6}{r['wall']:<6}")
        for d in r["detail"]: print(f"    ! {d}")
        for e in r["err_msgs"]: print(f"    err: {e[:140]}")
    n=len(results)
    # CROSS-VALIDATION GATE: a scenario passes only if EVERY model converged,
    # produced correct output, and had no loop/tool errors. `STRUCTURAL(fast)`
    # remains a performance/adherence metric because some language surfaces
    # legitimately fall back to patch/write.
    by_scn={}
    for r in results:
        by_scn.setdefault(r["id"], []).append(r["converged"] and r["correct"] and r["errors"]==0)
    gated=sum(1 for oks in by_scn.values() if all(oks))
    ok_clean=sum(1 for r in results if r['converged'] and r['correct'] and r['errors']==0)
    print("-"*len(hdr))
    print(f"RUNS converged+correct+clean {ok_clean}/{n} "
          f"| NO-ERROR {nclean}/{n} | STRUCTURAL(fast) {nfast}/{n}")
    print(f"GATE (scenario passes iff ALL {len(MODELS)} model(s) pass cleanly): {gated}/{len(by_scn)}")
    sys.exit(0 if gated==len(by_scn) else 1)

if __name__=="__main__":
    main()
