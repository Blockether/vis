(ns com.blockether.vis.internal.foundation.shim-fonttools
  "Built-in sandbox SHIM: a `fontTools`/`brotli` pair for the model's Python
   sandbox, scoped to WOFF2 -> TTF conversion. The `fonttools` and `brotli`
   PyPI packages are not in GraalPy and cannot be pip-installed, so an agent
   that reaches for `from fontTools.ttLib.woff2 import decompress` (or
   `import brotli`) would otherwise hit ModuleNotFoundError. This extension
   contributes a `:ext/sandbox-shims` entry that env-python installs into every
   sandbox Context (main + every `sub_loop` fork).

   Why this exists: WOFF2 web fonts are Brotli-compressed sfnt data, so PIL
   (`ImageFont.truetype`) cannot load a `.woff2` directly - it needs a real
   `.ttf`/`.otf`. Converting WOFF2 -> TTF is exactly what fontTools' woff2 module
   does, and it needs a Brotli decoder. Both are absent, so brand text rendering
   with PIL dead-ends. This shim closes that gap fully in pure Python.

   Backing: the vendored `brotlidecpy` decoder (Sidney Markowitz, MIT) provides
   Brotli decompression and its 122784-byte dictionary; an inlined WOFF2 reader
   rebuilds the sfnt, undoing the glyf/loca transforms (composite glyphs copied
   verbatim, simple glyphs re-encoded). Verified against fontTools 4.63 on
   Inter, Roboto (hinted) and NotoSans - ~10800 glyphs, identical outlines; the
   reconstructed TTF renders pixel-identical to the vendor TTF under the PIL
   shim on GraalPy.

   Supported: `fontTools.ttLib.woff2.decompress(input, output)` and
   `brotli.decompress(bytes)`. NOT supported: `brotli.compress` (raises
   NotImplementedError), WOFF1, and the wider `fontTools.ttLib.TTFont` API -
   this is a correctness-focused WOFF2->TTF subset, not full fontTools.

   Like `shim-pil` there are NO `:shim/bindings`: a self-contained Python
   preamble with zero host callables. Publishes `fontTools`, `fontTools.ttLib`,
   `fontTools.ttLib.woff2` and `brotli` (plus `brotlidecpy`) into `sys.modules`
   and staples `fontTools`/`brotli` onto builtins. No pip, no native wheel, no
   host binary."
  (:require [com.blockether.vis.core :as vis]))


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-fonttools"
     :ext/description
     "Sandbox shim: WOFF2 -> TTF via `fontTools.ttLib.woff2.decompress` and `brotli.decompress` (import fontTools / import brotli). Backed by the vendored pure-Python brotlidecpy Brotli decoder (MIT) and an inlined WOFF2 reader; lets PIL render brand fonts shipped as .woff2. No pip, no native wheel, no host bridge. decompress-only (no brotli.compress), WOFF2-only, not the full fontTools API."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "fonttools"
       :shim/imports ["brotli" "fontTools"]
       :shim/description
       "WOFF2/Brotli subset: `fontTools.ttLib.woff2.decompress(input, output)` builds TTF and reverses glyf/loca transforms; `brotli.decompress(bytes)` uses vendored brotlidecpy. No brotli.compress, WOFF1, TTFont, or wider fontTools APIs."
       :shim/source "vis-shims/fonttools.py"}]}))

(vis/register-extension! vis-extension)
