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

   Reading a document is one call — `anydoc.to_markdown(source)`, where a source
   is a path, raw bytes or an open file — and the richer
   `anydoc.to_document(...)` additionally returns the format that was detected,
   the evidence that identified it, every embedded binary (`Asset`) as real
   Python `bytes`, and — the part search is built on — the document's own
   STRUCTURE: `text` (the same content with none of Markdown's punctuation) plus
   one `block` per heading, paragraph, list item, table row, code block and note,
   each carrying its character span into `text`, its line, its heading
   breadcrumb, its table cells and, for a PDF, its PAGE. That is what lets a
   citation say `report.pdf p.7 › Revenue › row 4` instead of `report.md:812`.

   Matching is NOT done here. `anydoc.search(query, sources)` is plain Python in
   `resources/vis-shims/anydoc.py`: it folds the plain text (ligatures, accents,
   soft hyphens, line-wrapped phrases), parses a real query language, ranks with
   BM25 and cuts snippets — all over data the host already handed it, with no
   host call of its own. The walk that FINDS a corpus stays in Python too, on
   purpose: `internal/sandbox-fs` confines the sandbox's filesystem to the
   configured roots, so enumerating a directory host-side (through the pooled
   fff index, say) would hand the sandbox names it is not allowed to see.

   What this namespace does own is the CACHE. Converting a PDF costs orders of
   magnitude more than searching it, and a corpus gets asked more than one
   question, so every conversion is memoized on the CONTENT hash of the bytes
   plus the options - an LRU bounded three ways: how many documents stay
   resident, how large one payload may be, and how many characters the WHOLE
   cache may hold, the same shape as `internal/fff-index`'s pool. Two questions
   about the same 200-file corpus
   convert it once. The sandbox never sees that cache: there is no info door and
   no clear door in the Python surface - it evicts least-recently-used and keeps
   itself inside every budget it declares. That is what bounds the host's
   resident set: the cache outlives the session that filled it, so a bound on the
   ENTRY COUNT alone would not be a bound on memory at all."
  (:require [clojure.string :as str]
            [com.blockether.imaging :as im]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.util :as util])
  (:import [java.util Base64]))

(defn- decode64 ^bytes [^String encoded] (.decode (Base64/getDecoder) encoded))

(defn- encode64 ^String [^bytes raw] (.encodeToString (Base64/getEncoder) raw))

;; Conversion cache — LRU by content hash

(def ^:private cache-entries
  "How many converted documents stay resident. A corpus larger than this still
   works; it just re-converts the tail on the next question."
  32)

(def ^:private cache-entry-budget
  "Largest wire payload worth keeping, in characters - every string it holds:
   Markdown, plain text, the block structure and the base64 of each embedded
   binary. A 40 MB scanned PDF is converted and answered, never cached - one of
   those would evict the whole corpus around it."
  4000000)

(def ^:private cache-total-budget
  "How many characters stay resident across the WHOLE cache. The cache is a
   `defonce` that outlives every session feeding it, and one document's payload
   can be its embedded binaries rather than its prose, so entry COUNT is not a
   memory bound: thirty-two image-heavy PDFs would be gigabytes. Past this, the
   least-recently-used tail is dropped and re-converted on demand."
  32000000)

;; Nothing outside this namespace can see the cache: the sandbox has no door that
;; reports or empties it, so it carries no counters for anybody to read - only
;; what eviction itself needs.
(defonce ^:private conversion-cache (atom {:entries {} :order [] :chars {}}))

(defn- entry-chars
  "Approximate resident cost of one wire payload, in characters: every string it
   holds, so the block structure and the base64 of each embedded binary - the
   part that actually fills the heap - weigh what they cost."
  ^long [payload]
  (long (cond (string? payload) (count payload)
              (map? payload) (reduce + 0 (map entry-chars (vals payload)))
              (sequential? payload) (reduce + 0 (map entry-chars payload))
              :else 0)))

(defn- cache-take
  "Move `key` to the most-recently-used end."
  [state key]
  (assoc state :order (conj (vec (remove #(= % key) (:order state))) key)))

(defn- cache-put
  "Store `payload` under `key`, then evict least-recently-used entries until the
   cache is inside BOTH bounds: at most `cache-entries` documents, holding at
   most `cache-total-budget` characters between them."
  [state key payload]
  (loop [state (-> state
                   (assoc-in [:entries key] payload)
                   (assoc-in [:chars key] (entry-chars payload))
                   (cache-take key))]
    (if (and (<= (count (:order state)) (long cache-entries))
             (<= (long (reduce + 0 (vals (:chars state)))) (long cache-total-budget)))
      state
      (recur (let [oldest (first (:order state))]
               (-> state
                   (update :entries dissoc oldest)
                   (update :chars dissoc oldest)
                   (update :order #(vec (rest %)))))))))

(defn- cached
  "`(f)`, memoized on `key`. Misses that fit the budget become the new MRU entry."
  [key f]
  (if-let [hit (get-in (swap! conversion-cache #(cond-> % (contains? (:entries %) key) (cache-take
                                                                                         key)))
                       [:entries key])]
    hit
    (let [payload (f)]
      (when (<= (entry-chars payload) (long cache-entry-budget))
        (swap! conversion-cache cache-put key payload))
      payload)))

;; Conversion

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

(defn- ->wire
  "Engine data as strings-only wire data: kebab keywords become snake_case string
   keys, keyword values become their names, nested maps and vectors recurse. One
   rule, so a block key added in the cdylib reaches Python without a change here."
  [value]
  (cond (map? value) (reduce-kv (fn [m k v]
                                  (assoc m (str/replace (name k) "-" "_") (->wire v)))
                                {}
                                value)
        (sequential? value) (mapv ->wire value)
        (keyword? value) (name value)
        :else value))

(defn- asset->wire
  "One embedded binary, with its payload base64'd for the strings-only boundary."
  [asset]
  {"id" (:id asset)
   "media_type" (:media-type asset)
   "origin_part" (:origin-part asset)
   "size" (:size asset)
   "bytes" (some-> ^bytes (:bytes asset)
                   encode64)})

(defn- convert
  "Convert once. `with-blocks` additionally asks the cdylib for the document's
   plain text, its blocks and (for a PDF) its page count — the structure every
   citation is addressed in."
  [^bytes raw format file-name with-assets max-assets with-blocks]
  (let [opts
        (cond-> (doc-opts format file-name)
          with-assets
          (assoc :assets true)

          (pos? (long max-assets))
          (assoc :max-assets (long max-assets))

          with-blocks
          (assoc :blocks true))

        {:keys [source chars markdown text blocks pages assets] doc-format :format}
        (im/read-document raw opts)]

    {"format" (some-> doc-format
                      name)
     "source" source
     "chars" chars
     "markdown" markdown
     "text" text
     "blocks" (mapv ->wire blocks)
     "pages" pages
     "assets" (mapv asset->wire assets)}))

(defn- read-document-wire
  [encoded format file-name with-assets max-assets with-blocks]
  (let [raw
        (decode64 encoded)

        max-assets
        (long (or max-assets 0))]

    (cached
      [(util/sha256-hex raw) (str format) (str file-name) (boolean with-assets) max-assets
       (boolean with-blocks)]
      #(convert raw format file-name (boolean with-assets) max-assets (boolean with-blocks)))))

(defn- detect-document-wire
  [encoded format file-name]
  (let [{:keys [source formats] doc-format :format}
        (im/document-format (decode64 encoded) (doc-opts format file-name))]
    {"format" (some-> doc-format
                      name)
     "source" source
     "formats" (vec formats)}))

(defn- anydoc-bridge-bindings
  "Host document callables. Bytes cross as base64 in both directions; everything
   else is a scalar or a string-keyed map. Every call is total: a document the
   cdylib cannot parse comes back as `[false message]`, never as a host throw."
  []
  {"__vis_anydoc_markdown__"
   (fn [encoded format file-name with-assets max-assets with-blocks]
     (anydoc-envelope
       #(read-document-wire encoded format file-name with-assets max-assets with-blocks)))
   "__vis_anydoc_detect__" (fn [encoded format file-name]
                             (anydoc-envelope #(detect-document-wire encoded format file-name)))})


(def vis-extension
  (vis/extension
    {:ext/name "foundation-shim-anydoc"
     :ext/description
     (str "Sandbox `anydoc` module: Word/PDF/EPUB/presentation/spreadsheet bytes to "
          "GitHub-Flavored Markdown via com.blockether/imaging's Rust cdylib (no office suite, "
          "no wheel, no java.desktop), with the document's own structure (blocks, heading path, "
          "table cells, PDF page numbers) and a BM25-ranked search over one document or a whole "
          "corpus that cites page, section, line and a snippet. "
          "Conversions are cached on the content hash, so a second question about a corpus "
          "converts nothing.")
     :ext/version "0.3.1"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "anydoc"
       :shim/imports ["anydoc"]
       :shim/docs
       (str "`anydoc.to_markdown(source)` renders .doc .docx .odt .rtf "
            ".pdf .epub .ppt .pptx .odp .xls .xlsx .xlsm .xlsb .ods .csv as GitHub-Flavored "
            "Markdown; `to_document` adds the detected format, embedded assets as "
            "bytes (`max_assets`: None every one, `0` none, N capped), the plain `text`, "
            "the `blocks` (heading path, table cells, PDF `page`) and `pages`. A source is "
            "a path, raw bytes or an open binary file at both reading doors - `to_markdown` "
            "and `to_document` take whichever shape the caller happens to hold. "
            "`anydoc.search(query, sources)` searches ONE document, a list, a {id: "
            "doc} mapping or a whole directory and returns BM25-ranked `Citation`s with "
            "`.document_id .page .section .line .column .snippet .highlight .score .text .match "
            ".block_kind .cell`; `str(citation)` is `id p.N line L › Section: "
            "…snippet…`. Query language: bare terms (OR), \"a phrase\" (crosses line wraps AND "
            "table cells), +required, -excluded, `NEAR(a b, 5)`, prefix `rev*`, field scopes "
            "`heading: table: code: list: note: section: page:N`, `/regex/`. "
            "Exclusions and filters only NARROW: a query needs at least one thing to look FOR, "
            "so `-draft` or `page:3` alone is refused with that sentence. "
            "Matching folds ligatures, accents, curly quotes, soft/line-break hyphens and NBSP, "
            "and stems plurals, so `efficient` finds `eﬃcient` and `quarterly revenue` finds it "
            "wrapped over two lines. `results.explain()` says exactly how the query parsed and "
            "why each document scored; `results.suggestions` answers a typo; `results.skipped` "
            "names files that could not be read; `results.total_matches`/`.is_truncated` never "
            "lie about a capped search. Conversions are cached in the host on the content "
            "hash - a bounded LRU that evicts itself, with no door of its own - so "
            "`doc.search(...)` and a second corpus question cost no conversion. "
            "Errors are typed and catchable: `QueryError` (with the offending column), "
            "`DocumentError` (with `.document_id`), `SourceError`, all under `AnydocError`. "
            "Signature-less CSV needs a name or an explicit format. "
            "Not supported: writing documents, OCR of scanned pages, embeddings/semantic search.")
       :shim/bindings anydoc-bridge-bindings
       :shim/source "vis-shims/anydoc.py"}]}))

(defn register! [] (vis/register-extension! vis-extension))
