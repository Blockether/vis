#!/usr/bin/env python3
"""End-to-end editing harness: drive the REAL vis CLI on a battery of editing
tasks against a live model and verify, per scenario:

  - CONVERGED      the loop reached done() with a final answer (no hang / crash)
  - CORRECT        the resulting file(s) contain the expected change
  - NO-ERROR       no form raised inside the loop (ambiguous anchor, bad op, …)
  - FAST PATH      the structural tools (struct_edit / sexpr) were used rather
                   than the slower cat→patch anchor dance

Each scenario runs in its own throwaway git repo, with the live working-tree
source (`--source`) so it exercises the zipper you just edited. Runs are
parallel. Usage:

    VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.2 \
        python3 dev/benches/editing_e2e.py [scenario-id ...]
"""
import json, os, subprocess, sys, tempfile, shutil, concurrent.futures, time

VIS      = os.path.expanduser("~/vis/bin/vis")
PROVIDER = os.environ.get("VIS_PROVIDER", "zai-coding-plan")
MODEL    = os.environ.get("VIS_MODEL", "glm-5.2")
TIMEOUT  = int(os.environ.get("VIS_E2E_TIMEOUT", "300"))
WORKERS  = int(os.environ.get("VIS_E2E_WORKERS", "5"))

# ── 10 editing scenarios across the surface + languages ──────────────────────
SCENARIOS = [
    dict(id="py-fix-body", lang="python", files={"calc.py":
        "def add(a, b):\n    return a + b\n\n\ndef mul(a, b):\n    return a + b\n"},
        prompt="In calc.py the mul function is wrong: it returns a + b but must return a * b. Fix only mul.",
        want={"calc.py": ["return a * b", "def add(a, b):\n    return a + b"]}, wantnot={}),

    dict(id="clj-rename", lang="clojure", files={"core.clj":
        "(ns core)\n(defn greet [n] (str \"hi \" n))\n(defn run [] (greet \"x\"))\n"},
        prompt="In core.clj rename the function greet to salute everywhere it is used.",
        want={"core.clj": ["defn salute", "(salute \"x\")"]}, wantnot={"core.clj": ["greet"]}),

    dict(id="py-add-func", lang="python", files={"m.py":
        "def a():\n    return 1\n\n\ndef b():\n    return 2\n"},
        prompt="In m.py add a new function c that returns 3, placed after b. Keep a and b unchanged.",
        want={"m.py": ["def c(", "return 3", "def a(", "def b("]}, wantnot={}),

    dict(id="js-rename-var", lang="javascript", files={"app.js":
        "function area(w, h) {\n  const result = w * h;\n  return result;\n}\n"},
        prompt="In app.js rename the local variable result to area_value, both where it is assigned and returned.",
        want={"app.js": ["area_value", "return area_value"]}, wantnot={"app.js": ["result"]}),

    dict(id="clj-add-docstring", lang="clojure", files={"doc.clj":
        "(ns doc)\n(defn squared [x] (* x x))\n"},
        prompt="In doc.clj add a docstring \"Returns x times x.\" to the squared function.",
        want={"doc.clj": ["Returns x times x.", "defn squared"]}, wantnot={}),

    dict(id="py-add-param", lang="python", files={"g.py":
        "def greet(name):\n    return \"hi \" + name\n"},
        prompt="In g.py add a second parameter greeting (default \"hi\") to greet and use it instead of the literal \"hi\".",
        want={"g.py": ["greeting", "def greet(name"]}, wantnot={}),

    dict(id="clj-nested-edit", lang="clojure", files={"n.clj":
        "(ns n)\n(defn f [x]\n  (if (> x 0)\n    (+ x 1)\n    (- x 1)))\n"},
        prompt="In n.clj change ONLY the then-branch of the if inside f from (+ x 1) to (* x 2). Leave the else-branch alone.",
        want={"n.clj": ["(* x 2)", "(- x 1)"]}, wantnot={"n.clj": ["(+ x 1)"]}),

    dict(id="rs-fix-return", lang="rust", files={"lib.rs":
        "fn double(x: i32) -> i32 {\n    x + x\n}\n\nfn triple(x: i32) -> i32 {\n    x + x\n}\n"},
        prompt="In lib.rs triple is wrong (it returns x + x). Make triple return x * 3. Do not touch double.",
        want={"lib.rs": ["x * 3", "fn double"]}, wantnot={}),

    dict(id="go-add-func", lang="go", files={"main.go":
        "package main\n\nfunc one() int {\n\treturn 1\n}\n"},
        prompt="In main.go add a function two that returns 2, after one. Keep package and one unchanged.",
        want={"main.go": ["func two(", "return 2", "func one("]}, wantnot={}),

    dict(id="py-delete-func", lang="python", files={"d.py":
        "def keep():\n    return 1\n\n\ndef drop():\n    return 2\n\n\ndef alsokeep():\n    return 3\n"},
        prompt="In d.py delete the drop function entirely. Keep keep and alsokeep.",
        want={"d.py": ["def keep(", "def alsokeep("]}, wantnot={"d.py": ["def drop(", "return 2"]}),
]

STRUCTURAL = {"struct_edit", "sexpr"}

def run_one(sc):
    work = tempfile.mkdtemp(prefix=f"vis_e2e_{sc['id']}_")
    try:
        for name, content in sc["files"].items():
            with open(os.path.join(work, name), "w") as fh:
                fh.write(content)
        subprocess.run(["git","init","-q","."], cwd=work, check=True)
        subprocess.run(["git","config","user.email","t@t.co"], cwd=work, check=True)
        subprocess.run(["git","config","user.name","t"], cwd=work, check=True)
        subprocess.run(["git","add","-A"], cwd=work, check=True)
        subprocess.run(["git","commit","-qm","init"], cwd=work, check=True)

        t0 = time.time()
        try:
            p = subprocess.run(
                [VIS,"--source","--full-trace-json-stream",
                 "--provider",PROVIDER,"--model",MODEL, sc["prompt"]],
                cwd=work, capture_output=True, text=True, timeout=TIMEOUT)
            out = p.stdout
        except subprocess.TimeoutExpired as e:
            out = (e.stdout or b"").decode() if isinstance(e.stdout, bytes) else (e.stdout or "")
        wall = time.time() - t0

        tdir=os.path.join(os.environ.get("VIS_E2E_TRACES","/tmp/vis_e2e/traces"))
        os.makedirs(tdir, exist_ok=True)
        with open(os.path.join(tdir, sc["id"]+".jsonl"),"w") as fh: fh.write(out)

        forms=[]; tools=[]; errs=[]; done=False
        for line in out.splitlines():
            line=line.strip()
            if not line: continue
            try: o=json.loads(line)
            except: continue
            ev=o.get("event"); pl=o.get("payload",{})
            if ev=="result":
                if pl.get("answer") and not pl.get("error"): done=True
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

        # correctness check on the resulting files
        correct=True; detail=[]
        for name, subs in sc.get("want",{}).items():
            try: txt=open(os.path.join(work,name)).read()
            except FileNotFoundError: txt=""
            for s in subs:
                if s not in txt: correct=False; detail.append(f"missing {name}:{s!r}")
        for name, subs in sc.get("wantnot",{}).items():
            try: txt=open(os.path.join(work,name)).read()
            except FileNotFoundError: txt=""
            for s in subs:
                if s in txt: correct=False; detail.append(f"still present {name}:{s!r}")

        toolset=set(t for t in tools if t)
        used_structural=bool(toolset & STRUCTURAL)
        path=("struct_edit" if "struct_edit" in toolset
              else ("sexpr" if "sexpr" in toolset
                    else ("patch" if "patch" in toolset
                          else ("write" if "write" in toolset else "cat-only"))))
        if path in ("cat-only","??") or not (done and correct):
            detail.append("tools=" + ",".join(f"{t}×{tools.count(t)}" for t in sorted(toolset)))
        keep = os.environ.get("VIS_E2E_KEEP") and not (done and correct)
        return dict(id=sc["id"], lang=sc["lang"], converged=done, correct=correct,
                    errors=len(errs), err_msgs=errs[:2], wall=round(wall,1),
                    forms=len(forms), tools=tools, used_structural=used_structural,
                    edit_path=path, detail=detail, workdir=(work if keep else None))
    finally:
        if not os.environ.get("VIS_E2E_KEEP"):
            shutil.rmtree(work, ignore_errors=True)

def main():
    pick=set(sys.argv[1:])
    scs=[s for s in SCENARIOS if not pick or s["id"] in pick]
    print(f"running {len(scs)} editing scenarios on {PROVIDER}/{MODEL} "
          f"(workers={WORKERS}, timeout={TIMEOUT}s)\n")
    results=[]
    with concurrent.futures.ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for r in ex.map(run_one, scs): results.append(r)
    results.sort(key=lambda r: r["id"])

    hdr=f"{'scenario':<18}{'lang':<11}{'conv':<5}{'ok':<4}{'err':<4}{'path':<18}{'forms':<6}{'sec':<6}"
    print(hdr); print("-"*len(hdr))
    npass=nfast=nclean=0
    for r in results:
        ok = r["converged"] and r["correct"]
        npass += ok; nfast += r["used_structural"]; nclean += (r["errors"]==0)
        print(f"{r['id']:<18}{r['lang']:<11}"
              f"{'✓' if r['converged'] else '✗':<5}{'✓' if r['correct'] else '✗':<4}"
              f"{r['errors']:<4}{r['edit_path']:<18}{r['forms']:<6}{r['wall']:<6}")
        for d in r["detail"]: print(f"    ! {d}")
        for e in r["err_msgs"]: print(f"    err: {e[:140]}")
    n=len(results)
    print("-"*len(hdr))
    print(f"PASS(converged+correct) {npass}/{n} | NO-ERROR {nclean}/{n} | "
          f"STRUCTURAL(fast) {nfast}/{n}")
    sys.exit(0 if npass==n else 1)

if __name__=="__main__":
    main()
