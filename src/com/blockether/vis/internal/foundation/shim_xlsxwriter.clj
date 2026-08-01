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
     "Sandbox shim: an xlsxwriter-compatible module (Workbook/add_worksheet/add_format/write/write_string/number/formula/datetime/blank/url/rich_string/row/column, merge_range, set_column/set_row, freeze_panes, autofilter, insert_image, A1 notation, utility helpers) backed by com.blockether/imaging's Rust rust_xlsxwriter writer. GraalPy can't install the CPython package; this makes `import xlsxwriter` produce real .xlsx files with no pip, no native wheel and no JVM document model."
     :ext/version "0.2.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "xlsxwriter"
       :shim/imports ["xlsxwriter"]
       :shim/description
       "xlsxwriter-compatible .xlsx writer backed by Rust rust_xlsxwriter (the workbook is modelled in Python and built in one call on close). Not supported: streaming (`constant_memory`), VBA, charts and data validation."
       :shim/bindings xlsxwriter-bridge-bindings
       :shim/source "vis-shims/xlsxwriter.py"}]}))

(vis/register-extension! vis-extension)
