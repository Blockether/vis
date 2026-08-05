(ns com.blockether.vis.internal.foundation.shim-anydoc
  "Built-in sandbox SHIM: an `anydoc` module for the model's Python sandbox that
   turns ANY document a human sent — Word (`.doc` `.docx` `.odt` `.rtf`), PDF,
   EPUB, presentations (`.ppt` `.pptx` `.odp`) and spreadsheets (`.xls` `.xlsx`
   `.xlsm` `.xlsb` `.ods` `.csv`) — into GitHub-Flavored Markdown an LLM can
   actually read, headings, lists, tables and links intact.

   The parser is `com.blockether/imaging`'s cdylib (Rust `anydoc`), the same FFM
   stack behind the `PIL` shim: NOTHING here shells out to a headless office
   suite, unpacks a wheel, or touches `java.desktop`. Conversion is pure host
   work; only base64 blobs and small string-keyed maps cross the strings-only
   sandbox boundary.

   Reading a document is one call — `anydoc.to_markdown(path)` — and the richer
   `anydoc.to_document(...)` additionally returns the format that was detected,
   the evidence that identified it, and every embedded binary (`Asset`) as real
   Python `bytes`."
  (:require [clojure.string :as str]
            [com.blockether.imaging :as im]
            [com.blockether.vis.core :as vis])
  (:import [java.util Base64]))

(defn- decode64 ^bytes [^String encoded] (.decode (Base64/getDecoder) encoded))

(defn- encode64 ^String [^bytes raw] (.encodeToString (Base64/getEncoder) raw))

(defn- present
  "Trimmed `value`, or nil when Python passed nothing (`None` / empty string)."
  [value]
  (when (string? value)
    (let [v (str/trim value)]
      (when-not (str/blank? v) v))))

(defn- doc-opts
  "`com.blockether.imaging` options from the flat scalars Python sends. `format`
   forces the parser; `file-name` is the file name, the only route for CSV
   (which carries no signature at all)."
  [format file-name]
  (cond-> {}
    (present format)
    (assoc :format (keyword (str/lower-case (present format))))

    (present file-name)
    (assoc :name (present file-name))))

(defn- anydoc-envelope
  "Return `[true payload]`, or `[false message]` so Python can raise a catchable
   `anydoc.AnydocError` instead of leaking an uncatchable host exception."
  [f]
  (try [true (f)] (catch Throwable t [false (str (or (.getMessage t) t))])))

(defn- asset->wire
  "One embedded binary, with its payload base64'd for the strings-only boundary."
  [asset]
  {"id" (:id asset)
   "media_type" (:media-type asset)
   "origin_part" (:origin-part asset)
   "size" (:size asset)
   "bytes" (some-> ^bytes (:bytes asset)
                   encode64)})

(defn- read-document-wire
  [encoded format file-name with-assets max-assets]
  (let
    [opts
     (cond-> (doc-opts format file-name)
       with-assets
       (assoc :assets true)

       (pos? (long (or max-assets 0)))
       (assoc :max-assets (long max-assets)))

     {:keys [source chars markdown assets] doc-format :format}
     (im/read-document (decode64 encoded) opts)]

    {"format" (some-> doc-format
                      name)
     "source" source
     "chars" chars
     "markdown" markdown
     "assets" (mapv asset->wire assets)}))

(defn- detect-document-wire
  [encoded format file-name]
  (let
    [{:keys [source formats] doc-format :format} (im/document-format (decode64 encoded)
                                                                     (doc-opts format file-name))]
    {"format" (some-> doc-format
                      name)
     "source" source
     "formats" (vec formats)}))

(defn- anydoc-bridge-bindings
  "Host document callables. Bytes cross as base64 in both directions; everything
   else is a scalar or a string-keyed map. Both calls are total: a document the
   cdylib cannot parse comes back as `[false message]`, never as a host throw."
  []
  {"__vis_anydoc_markdown__" (fn [encoded format file-name with-assets max-assets]
                               (anydoc-envelope #(read-document-wire encoded
                                                                     format
                                                                     file-name
                                                                     (boolean with-assets)
                                                                     (long (or max-assets 0)))))
   "__vis_anydoc_detect__" (fn [encoded format file-name]
                             (anydoc-envelope #(detect-document-wire encoded format file-name)))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-anydoc"
     :ext/description
     "Sandbox `anydoc` module: Word/PDF/EPUB/presentation/spreadsheet bytes to GitHub-Flavored Markdown via com.blockether/imaging's Rust cdylib (no office suite, no wheel, no java.desktop)."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "anydoc"
       :shim/imports ["anydoc"]
       :shim/description
       "`anydoc.to_markdown(path)` / `to_markdown_bytes(data)` render .doc .docx .odt .rtf .pdf .epub .ppt .pptx .odp .xls .xlsx .xlsm .xlsb .ods .csv as GitHub-Flavored Markdown; `to_document` adds the detected format, the evidence that identified it and embedded assets as bytes; `format_from_bytes/extension/path` identify without converting. Signature-less CSV needs a name or an explicit format. Not supported: writing documents, OCR of scanned pages, layout coordinates."
       :shim/bindings anydoc-bridge-bindings
       :shim/source "vis-shims/anydoc.py"}]}))

(vis/register-extension! vis-extension)
