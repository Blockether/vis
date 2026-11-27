"""Measure the default session read and Council text without a model."""

import argparse
import concurrent.futures
import json
import os
import runpy
import subprocess
import tempfile
from pathlib import Path

from bench_agent import HERE, TASKS, write_fallback
from blockether.vis.evals.agent import source_classpath, source_commit


def measure(text):
    return {"chars": len(text), "utf8_bytes": len(text.encode("utf-8"))}


def run_fixture(task, scratch, cp, render):
    kind = json.loads((task / "tests/fixture.json").read_text())["kind"]
    directory = scratch / task.name
    directory.mkdir()
    home = directory / "home"
    workspace = task / "environment/workspace"
    write_fallback(workspace, home, kind)
    env = dict(os.environ, VIS_HOME=str(home), VIS_BENCH_DB=str(directory / "vis.mdb"),
               VIS_BENCH_ROOT=str(workspace), VIS_BENCH_KIND=kind, VIS_BENCH_MEASURE="session")
    env.pop("VIS_BENCH_CHECK_SEED", None)
    form = f'(load-file {json.dumps(str(HERE / "seed.clj"))}) (load-file {json.dumps(str(HERE / "measure_static.clj"))})'
    command = ["clojure", "-Scp", cp, "-M", "-e", form]

    def run():
        proc = subprocess.run(command, cwd=directory, env=env, capture_output=True,
                              text=True, timeout=180)
        if proc.returncode:
            raise RuntimeError(proc.stderr)
        prefix = "VIS_BENCH_STATIC "
        return next(line[len(prefix):] for line in proc.stdout.splitlines() if line.startswith(prefix))

    if kind in {"handoff", "resume"}:
        env.update(VIS_BENCH_MEASURE="handoff", VIS_BENCH_REQUEST=(task / "instruction.md").read_text())
        command[-1] = f'(load-file {json.dumps(str(HERE / "measure_static.clj"))})'
        return task.name, json.loads(run())
    result = run()
    sizes = {"default_json": measure(result), "printed_repr": measure(render(json.loads(result)))}
    if kind == "batch":
        env["VIS_BENCH_MEASURE"] = "prompts"
        command[-1] = f'(load-file {json.dumps(str(HERE / "measure_static.clj"))})'
        sizes["prompt_measurements"] = json.loads(run())
    return task.name, sizes


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("checkout", type=Path)
    args = parser.parse_args()
    checkout = args.checkout.resolve(strict=True)
    cp = source_classpath("clojure", checkout)
    render = runpy.run_path(str(checkout / "resources/vis-host-python/vis_results.py"))["_result_repr"]
    tasks = sorted(path.parent for path in TASKS.glob("introspection-*/task.toml"))
    with tempfile.TemporaryDirectory(prefix="vis-introspection-static-") as tmp:
        with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
            cases = dict(pool.map(lambda task: run_fixture(task, Path(tmp), cp, render), tasks))
    prompts = cases["introspection-council-attribution"].pop("prompt_measurements")
    rows = prompts["council_input"]
    prompts["council_input"] = {"iterations": rows, "total_utf8_bytes": sum(row["utf8_bytes"] for row in rows)}
    print(json.dumps({"commit": source_commit(checkout), "cases": cases, "prompts": prompts}, indent=2))


if __name__ == "__main__":
    main()
