(ns com.blockether.vis.internal.foundation.shim-pptx
  "Built-in sandbox SHIM: a `pptx` (python-pptx) compatible module backed by
   `com.blockether/imaging` (Rust OOXML writer) so `from pptx import Presentation`
   writes real .pptx files without the CPython package — and without Apache POI,
   which cannot follow the CLI into the native image.

   The Python side owns the whole presentation model and only crosses the
   boundary ONCE, at `save()`: one string-keyed spec map in, base64 bytes out."
  (:require [com.blockether.imaging :as im]
            [com.blockether.vis.core :as vis])
  (:import [java.util Base64]))

(defn- b64enc [^bytes ba] (.encodeToString (Base64/getEncoder) ba))

(defn- envelope [f] (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- op-build
  "Build a presentation from the shim's spec and hand it back as base64 bytes."
  [spec]
  (b64enc (im/pptx spec)))


(defn- pptx-bridge-bindings
  "Host callables (com.blockether/imaging, Rust) the pptx shim delegates to."
  []
  {"__vis_pptx_build__" (fn [spec]
                          (envelope #(op-build spec)))})

;; Python preamble: publishes a python-pptx-compatible module into sys.modules.


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pptx"
     :ext/description
     "Sandbox shim: a python-pptx-compatible module (Presentation, slides/slide_layouts, shapes with add_textbox/add_shape/add_picture/add_table/add_connector, placeholders, text frames with paragraphs/runs/fonts, fills, outlines, tables, notes, core properties, Emu/Pt/Inches/Cm units, RGBColor, PP_ALIGN/MSO_ANCHOR/MSO_SHAPE) backed by com.blockether/imaging's Rust OOXML writer. GraalPy can't install the CPython package; this makes `from pptx import Presentation` produce real .pptx files with no pip, no native wheel and no JVM document model."
     :ext/version "0.2.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "pptx"
       :shim/imports ["pptx"]
       :shim/description
       "python-pptx-compatible .pptx writer backed by Rust (the presentation is modelled in Python and built in one call on save). Not supported: opening/editing an existing .pptx, charts and SmartArt."
       :shim/bindings pptx-bridge-bindings
       :shim/source "vis-shims/pptx.py"}]}))

(vis/register-extension! vis-extension)
