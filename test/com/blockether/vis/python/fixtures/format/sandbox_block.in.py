r = grep({"query": ["defn- src-dirs", "defn src-dirs"], "paths": [str(project_root_path/"build.clj")], "context": 12})
print(r)
sh = await shell("cd project && git status --porcelain=v1 | head -100 && echo --- && git log --oneline -5")
r = await sh.wait(30)
print(r["out"][-6000:])
results = await gather(*[fetch(url, timeout=10) for url in urls if url.startswith("https://")], return_exceptions=True)
for name, value in sorted(results.items()): print(f"{name:<20} {value!r}")
data = json.loads(Path("config.json").read_text()); print(data["servers"]["primary"]["host"], data["servers"]["primary"]["port"])
def helper(path, *, depth=1, pattern=None):
    """Return matching files."""
    return [p for p in Path(path).rglob(pattern or "*") if p.is_file() and len(p.relative_to(path).parts) <= depth]
