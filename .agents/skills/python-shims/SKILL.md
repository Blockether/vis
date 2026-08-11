---
name: python-shims
description: >
  How a Vis sandbox Python shim is built and linted: one lazy shim_*.clj per
  shim, Python in resources/vis-shims, in-process ruff instead of a subprocess,
  and the PIL font-resolution trap. Read before adding or changing a shim.
---

# Sandbox Python shims and ruff

- One lazy `shim_*.clj` per shim, one registered extension, and `builtin-extension-nses` inclusion. Python lives in `resources/vis-shims/<name>.py`, referenced as `:shim/source "vis-shims/<name>.py"` — never embedded as a Clojure string. Verify lazy import behavior and native resource inclusion.
- Python format/lint uses in-process `com.blockether/ruff`, never a subprocess or PyPI install. Honor ruff's nearest-config resolution; missing targets fail, and only syntax plus `E9xx`/`F6xx`/`F7xx`/`F82x` are errors. The shim supports `vis-agent python -m ruff check|format <paths>`; root `ruff.toml` configures this repo. Upgrade ruff in sibling `clj-ruff`, release it, then move the `deps.edn` pin.
- The PIL shim's `ImageFont.truetype` takes a family NAME or a file path, and the requested face must reach both the draw op and the measurement, or text is measured in one font and painted in another. Family comes from the file/name, weight and italic from the name stem (`…-Bold.ttf` → 700), and a variable font honors the `wght` axis — which is why one file can serve two weights.
