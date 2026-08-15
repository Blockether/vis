---
name: python-shims
description: >
  How a Vis sandbox Python shim is built and linted: one lazy shim_*.clj per
  shim, Python in resources/vis-shims, the runtime handle registry every shim
  that hands out a host id must use, in-process ruff instead of a subprocess,
  and the PIL font-resolution trap. Read before adding or changing a shim.
---

# Sandbox Python shims and ruff

- One lazy `shim_*.clj` per shim, one registered extension, and `builtin-extension-nses` inclusion. Python lives in `resources/vis-shims/<name>.py`, referenced as `:shim/source "vis-shims/<name>.py"` — never embedded as a Clojure string. Verify lazy import behavior and native resource inclusion.
- **A shim that hands the block a HOST handle never invents its own lifetime.** GraalPy does not refcount: dropping the Python wrapper runs no `__del__`, and the handle is a plain host id that outlives its owner, so the resource leaks for the life of the JVM (measured: 20 dropped `Image.new`s = 20 live rasters after `gc.collect()`; 15 dropped `sqlite3.connect()`s = 14 leaked descriptors). Declare the kind once at install — `__vis_handle_kind__(kind, free)` — and name every owner — `__vis_own__(obj, kind, key, nbytes)` — in the runtime's ONE registry (`resources/vis-python/async_runtime.py`), which frees a handle when its LAST owner becomes unreachable: at the block boundary, under allocation pressure (bytes, and count for kinds whose cost is a socket), and eagerly. `close()` calls `__vis_forget__` when the shim closes the resource itself (its error belongs to the caller) or `__vis_disown__` when several wrappers may share one handle (PIL's `exif_transpose(in_place=True)`). `__vis_handle_census__()` is the seam tests and diagnostics read. Never a private weak-ref table, a `__del__` or a per-shim reaper.

- Python format/lint uses in-process `com.blockether/ruff`, never a subprocess or PyPI install. Honor ruff's nearest-config resolution; missing targets fail, and only syntax plus `E9xx`/`F6xx`/`F7xx`/`F82x` are errors. The shim supports `vis-agent python -m ruff check|format <paths>`; root `ruff.toml` configures this repo. Upgrade ruff in sibling `clj-ruff`, release it, then move the `deps.edn` pin.
- The PIL shim's `ImageFont.truetype` takes a family NAME or a file path, and the requested face must reach both the draw op and the measurement, or text is measured in one font and painted in another. Family comes from the file/name, weight and italic from the name stem (`…-Bold.ttf` → 700), and a variable font honors the `wght` axis — which is why one file can serve two weights.
