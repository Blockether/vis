(ns com.blockether.vis.internal.foundation.shim-pptx
  "Built-in sandbox SHIM: a `pptx` (python-pptx) compatible module backed by
   `com.blockether/imaging`'s Rust OOXML reader/writer. It creates, opens, edits,
   and saves real presentations without Apache POI or a CPython wheel.

   Python owns the mutable presentation model. The boundary is crossed once when
   opening (bytes in, canonical model out) and once when saving (model in, bytes
   out)."
  (:require [com.blockether.imaging :as im]
            [com.blockether.vis.core :as vis])
  (:import [java.util Base64]))

(defn- b64enc [^bytes ba] (.encodeToString (Base64/getEncoder) ba))

(defn- envelope [f] (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- op-build
  "Build a presentation from the shim's spec and hand it back as base64 bytes."
  [spec]
  (b64enc (im/pptx spec)))

(defn- op-read
  "Read a presentation through the Rust OOXML reader into the shim's wire model."
  [encoded]
  (-> (im/read-office (.decode (Base64/getDecoder) ^String encoded) {:with-images true})
      vis/wire-canonical))

(defn- pptx-bridge-bindings
  "Host callables (com.blockether/imaging, Rust) the pptx shim delegates to."
  []
  {"__vis_pptx_build__" (fn [spec]
                          (envelope #(op-build spec)))
   "__vis_pptx_read__" (fn [encoded]
                         (envelope #(op-read encoded)))})

;; Python preamble: publishes a python-pptx-compatible module into sys.modules.


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-pptx"
     :ext/description
     (str "Sandbox python-pptx compatibility backed by com.blockether/imaging's Rust OOXML "
          "reader/writer: create, open, edit, and save presentations with layouts, placeholders, "
          "rich text, images, fills, shapes, tables, connectors, charts, notes, properties, "
          "units, colors, and common enums. No pip wheel or JVM document model.")
     :ext/version "0.3.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims [{:shim/name "pptx"
                          :shim/imports ["pptx"]
                          :shim/bindings pptx-bridge-bindings
                          :shim/source "vis-shims/pptx.py"}]}))

(vis/register-extension! vis-extension)
