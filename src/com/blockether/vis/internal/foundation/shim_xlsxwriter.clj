(ns com.blockether.vis.internal.foundation.shim-xlsxwriter
  "Built-in sandbox SHIM: an `xlsxwriter`-compatible module backed by
   `com.blockether/imaging` (Rust `rust_xlsxwriter`) so `import xlsxwriter`
   writes real .xlsx files without the CPython package — and without Apache POI,
   which cannot follow the CLI into the native image.

   The Python side owns the whole workbook model and only crosses the boundary
   ONCE, at `close()`: one string-keyed spec map in, base64 bytes out."
  (:require [com.blockether.imaging :as im]
            [com.blockether.vis.core :as vis])
  (:import [java.util Base64]))

(defn- b64enc [^bytes ba] (.encodeToString (Base64/getEncoder) ba))

(defn- envelope [f] (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- op-build
  "Build a workbook from the shim's spec and hand it back as base64 bytes."
  [spec]
  (b64enc (im/xlsx spec)))


(defn- xlsxwriter-bridge-bindings
  "Host callables (com.blockether/imaging, Rust) the xlsxwriter shim delegates to."
  []
  {"__vis_xlsx_build__" (fn [spec]
                          (envelope #(op-build spec)))})

;; Python preamble: publishes an xlsxwriter-compatible module into sys.modules.


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-xlsxwriter"
     :ext/description
     (str "Sandbox XlsxWriter subset backed by com.blockether/imaging's Rust writer: "
          "workbooks/worksheets, formats, cell/row/column writes, formulas, dates, URLs, rich "
          "strings, ranges, panes, filters, images, and utilities. "
          "Produces real `.xlsx` files without pip/native wheel/JVM document model; no "
          "streaming, VBA, charts, or data validation.")
     :ext/version "0.2.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "xlsxwriter"
       :shim/imports ["xlsxwriter"]
       :shim/description
       (str "Rust-backed `xlsxwriter` .xlsx writer: models workbooks in Python and builds on "
            "close. No `constant_memory` streaming, VBA, charts, or data validation.")
       :shim/bindings xlsxwriter-bridge-bindings
       :shim/source "vis-shims/xlsxwriter.py"}]}))

(vis/register-extension! vis-extension)
