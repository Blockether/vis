(ns com.blockether.vis.internal.anydoc-compat-shim-test
  "The `anydoc` shim installed into every sandbox context via the generic
   sandbox-shim mechanism: an `anydoc` module published into `sys.modules` that
   converts real Word / spreadsheet / CSV bytes to GitHub-Flavored Markdown
   through `com.blockether/imaging`'s Rust cdylib, while keeping the
   strings-only boundary (documents and assets cross as base64) and turning a
   host refusal into a catchable `anydoc.AnydocError`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.imaging :as im]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.shim-anydoc :as shim-anydoc]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util Base64]
           [org.graalvm.polyglot Context]))

(defn- ev [^Context context code] (ep/->clj (.eval context "python" code)))

(defmacro ^:private with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (tpc/shared)]
     ~@body))

(defmacro ^:private with-fresh-python-context
  [& body]
  `(let
     [~(with-meta 'python-context {:tag `Context}) (:python-context (ep/create-python-context {}))]
     (try ~@body (finally (.close ~'python-context)))))

(defn- b64 [^bytes raw] (.encodeToString (Base64/getEncoder) raw))

(def ^:private docx-bytes
  (delay (im/docx {:blocks [{:type :heading :level 1 :text "Quarterly Report"}
                            {:type :paragraph :text "Revenue grew."}]})))

(def ^:private docx-fixture (delay (b64 @docx-bytes)))

(defn- png
  [color]
  (with-open [canvas (im/blank 2 2 color)]
    (im/encode canvas :png)))

(def ^:private png-fixture (delay (png "#ff0000")))

(defn- picture
  [data]
  {:kind :picture :image {:data data} :left 1.0 :top 1.0 :width 1.0 :height 1.0})

(def ^:private pptx-fixture
  "A deck carrying TWO distinct images, so `max_assets` has something to cap."
  (delay (im/pptx {:slides [{:shapes [(picture @png-fixture)]}
                            {:shapes [(picture (png "#00ff00"))]}]})))

(defn- tmp-dir
  "A directory the sandbox may open: Python's own `open` refuses anything outside
   the Context's roots, so a document on disk has to live under one."
  ^String []
  (str (Files/createTempDirectory "vis-anydoc-" (make-array FileAttribute 0))))

(defmacro ^:private with-fs-context
  [dir & body]
  `(let
     [~(with-meta 'python-context {:tag `Context})
      (:python-context (ep/create-python-context {} (constantly [~dir])))]
     (try ~@body (finally (.close ~'python-context)))))

(defn- py-bytes
  "Python expression rebuilding `encoded` as real `bytes` inside the sandbox."
  [encoded]
  (str "__import__('base64').b64decode('" encoded "')"))

(defdescribe
  anydoc-module-test
  (it "stays lazy and imports as a module"
      (with-fresh-python-context
        (expect
          (true?
            (ev python-context
                (str "import sys\n"
                     "before = 'anydoc' not in sys.modules\n" "import anydoc\n"
                     "before and anydoc is sys.modules['anydoc'] "
                     "and callable(anydoc.to_markdown) and callable(anydoc.to_markdown_bytes)"))))))
  (it "reports the formats it understands, fetched from the host on first touch"
      (with-python-context (expect (= [true true true]
                                      (ev python-context
                                          (str "import anydoc\n"
                                               "fs = anydoc.FORMATS\n"
                                               "['docx' in fs, 'pdf' in fs, 'csv' in fs]")))))))

(defdescribe anydoc-convert-test
             (it "renders a real .docx as GitHub-Flavored Markdown"
                 (with-python-context (let
                                        [markdown (ev python-context
                                                      (str "import anydoc\n"
                                                           "anydoc.to_markdown_bytes("
                                                           (py-bytes @docx-fixture)
                                                           ")"))]
                                        (expect (str/includes? markdown "# **Quarterly Report**"))
                                        (expect (str/includes? markdown "Revenue grew.")))))
             (it "renders signature-less CSV once a name says what it is"
                 (with-python-context
                   (let
                     [markdown (ev python-context
                                   (str "import anydoc\n"
                                        "anydoc.to_markdown_bytes(b'city,people\\nOslo,700000\\n', "
                                        "name='cities.csv')"))]
                     (expect (str/includes? markdown "| city | people |"))
                     (expect (str/includes? markdown "Oslo")))))
             (it "identifies a document without converting it"
                 (with-python-context
                   (expect (= ["docx" "content" "csv" nil]
                              (ev python-context
                                  (str "import anydoc\n"
                                       "found = anydoc.detect(" (py-bytes @docx-fixture)
                                       ")\n" "[found['format'], found['source'],\n"
                                       " anydoc.format_from_extension('cities.csv'),\n"
                                       " anydoc.format_from_bytes(b'city,people\\n')]")))))))

(defdescribe anydoc-document-test
             (it "returns a Document carrying the format, its evidence and its assets"
                 (with-python-context
                   (expect (= ["docx" "content" true true 0 true]
                              (ev python-context
                                  (str "import anydoc\n"
                                       "doc = anydoc.to_document(" (py-bytes @docx-fixture)
                                       ")\n" "[doc.format, doc.source, doc.chars > 0,\n"
                                       " str(doc) == doc.markdown, len(doc.assets),\n"
                                       " isinstance(doc.assets, list)]"))))))
             (it "hands embedded binaries back as real Python bytes"
                 (with-python-context
                   (expect (= [true true true]
                              (ev python-context
                                  (str "import anydoc\n"
                                       "doc = anydoc.to_document("
                                       (py-bytes (b64 @pptx-fixture))
                                       ")\n"
                                       "asset = doc.assets[0]\n" "[doc.format == 'pptx',\n"
                                       " isinstance(asset.bytes, bytes) and asset.bytes[:8] == "
                                       (py-bytes (b64 (byte-array (take 8 @png-fixture))))
                                       ",\n" " len(asset.bytes) == len(asset)]"))))))
             (it "keeps its asset promises: none when refused, capped when limited"
                 (with-python-context
                   (expect (= [0 1 2]
                              (ev python-context
                                  (str "import anydoc\n"
                                       "deck = " (py-bytes (b64 @pptx-fixture))
                                       "\n" "[len(anydoc.to_document(deck, assets=False).assets),\n"
                                       " len(anydoc.to_document(deck, max_assets=1).assets),\n"
                                       " len(anydoc.to_document(deck).assets)]")))))))

(defdescribe
  anydoc-error-test
  (it "raises a catchable AnydocError for input nothing recognises"
      (with-python-context
        (expect (= [true true]
                   (ev python-context
                       (str "import anydoc\n"
                            "try:\n" "    anydoc.to_markdown_bytes(b'\\x00\\x01not a document')\n"
                            "    out = [False, False]\n" "except anydoc.AnydocError as err:\n"
                            "    out = [True, bool(str(err))]\n" "out"))))))
  (it "refuses text where bytes belong instead of guessing an encoding"
      (with-python-context
        (expect (true? (ev python-context
                           (str "import anydoc\n"
                                "try:\n" "    anydoc.to_markdown_bytes('already a string')\n"
                                "    out = False\n" "except TypeError:\n"
                                "    out = True\n" "out")))))))

(defdescribe anydoc-disk-test
             (it "reads a document on disk in one call"
                 (let [dir (tmp-dir)]
                   (io/copy @docx-bytes (io/file dir "report.docx"))
                   (with-fs-context dir
                                    (expect (= [true true "docx" true]
                                               (ev python-context
                                                   (str "import anydoc\n"
                                                        "path = '"
                                                        dir
                                                        "/report.docx'\n"
                                                        "markdown = anydoc.to_markdown(path)\n"
                                                        "doc = anydoc.read(path)\n"
                                                        "['# **Quarterly Report**' in markdown,\n"
                                                        " doc.markdown == markdown,\n"
                                                        " anydoc.format_from_path(path),\n"
                                                        " doc.format == 'docx']")))))))
             (it "identifies a signature-less .csv on disk by its own file name"
                 (let [dir (tmp-dir)]
                   (spit (io/file dir "cities.csv") "city,people\nOslo,700000\n")
                   (with-fs-context
                     dir
                     (expect (= ["csv" true]
                                (ev python-context
                                    (str "import anydoc\n"
                                         "path = '"
                                         dir
                                         "/cities.csv'\n"
                                         "[anydoc.format_from_path(path),\n"
                                         " '| city | people |' in anydoc.to_markdown(path)]"))))))))

;; --- search: one document, a few, or a directory of many, with citations ---

(defn- py
  "Python source from lines — the sandbox programs below read like Python."
  [& lines]
  (str/join "\n" lines))

(def ^:private report-docx-bytes
  "One document carrying every shape a citation has to address, and every trap
   the flat line-grep used to fall into: a word wearing emphasis, emphasis
   INSIDE a word, a ligature, a decomposed accent, a curly apostrophe, a
   non-breaking space, an eszett and a table."
  (delay (im/docx {:blocks
                   [{:type :heading :level 1 :text "Quarterly Report"}
                    {:type :heading :level 2 :text "Revenue"}
                    {:type :paragraph
                     :runs [{:text "Revenue in "} {:text "March" :bold true} {:text " 2024 rose."}]}
                    {:type :paragraph
                     :runs [{:text "The "} {:text "mar" :italic true} {:text "ket closed early."}]}
                    {:type :paragraph :text "It was the most e\ufb03cient month in Zu\u0308rich."}
                    {:type :paragraph :text "We don\u2019t expect payments before March\u00a02024."}
                    {:type :heading :level 2 :text "Regions"}
                    {:type :paragraph :text "Marching orders arrived from Hauptstra\u00dfe."}
                    {:type :table
                     :header true
                     :rows [{:cells [{:text "Month"} {:text "Revenue"}]}
                            {:cells [{:text "March"} {:text "12.4"}]}
                            {:cells [{:text "April"} {:text "9.1"}]}]}]})))

(def ^:private march-csv-bytes (delay (.getBytes "month,revenue\nMarch,1200\nApril,900\n" "UTF-8")))

(defn- pdf-bytes
  "The smallest real PDF that carries text: one uncompressed content stream per
   page, Helvetica, a correct xref. Handwritten because nothing here WRITES
   PDFs and page provenance can only be proved against a document with pages."
  ^bytes [pages]
  (let
    [n
     (count pages)

     page-ids
     (mapv #(+ 3 (* 2 (long %))) (range n))

     cont-ids
     (mapv #(+ 4 (* 2 (long %))) (range n))

     font-id
     (+ 3 (* 2 n))

     stream
     (fn [lines]
       (str "BT /F1 12 Tf 72 720 Td 16 TL\n"
            (str/join "\n"
                      (for [l lines]
                        (str "(" (str/escape l {\\ "\\\\" \( "\\(" \) "\\)"}) ") Tj T*")))
            "\nET"))

     objs
     (into (sorted-map)
           (concat [[1 "<< /Type /Catalog /Pages 2 0 R >>"]
                    [2
                     (str "<< /Type /Pages /Kids ["
                          (str/join " " (map #(str % " 0 R") page-ids))
                          "] /Count "
                          n
                          " >>")]
                    [font-id "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"]]
                   (for [i (range n)]
                     [(page-ids i)
                      (str "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents "
                           (cont-ids i)
                           " 0 R /Resources << /Font << /F1 "
                           font-id
                           " 0 R >> >> >>")])
                   (for
                     [i
                      (range n)

                      :let [s
                            (stream (nth pages i))]]

                     [(cont-ids i) (str "<< /Length " (count s) " >>\nstream\n" s "\nendstream")])))

     out
     (java.io.ByteArrayOutputStream.)

     put!
     (fn [^String s]
       (.write out (.getBytes s "ISO-8859-1")))

     _
     (put! "%PDF-1.4\n")

     offsets
     (reduce (fn [acc [num body]]
               (let [at (.size out)]
                 (put! (str num " 0 obj\n" body "\nendobj\n"))
                 (assoc acc num at)))
             {}
             objs)

     xref-at
     (.size out)

     size
     (inc (long (apply max (keys objs))))]

    (put! (str "xref\n0 " size "\n0000000000 65535 f \n"))
    (doseq [num (range 1 size)]
      (put! (if-let [at (offsets num)]
              (format "%010d 00000 n \n" at)
              "0000000000 65535 f \n")))
    (put! (str "trailer\n<< /Size " size " /Root 1 0 R >>\nstartxref\n" xref-at "\n%%EOF\n"))
    (.toByteArray out)))

(def ^:private report-pdf-bytes
  (delay (pdf-bytes [["Quarterly Report" "Total quarterly" "revenue rose sharply."]
                     ["Regional Detail" "April was quieter than March."]])))

(defn- corpus
  "A directory of real documents on disk that the sandbox is allowed to open."
  ^String [entries]
  (let [dir (tmp-dir)]
    (doseq [[file-name ^bytes raw] entries]
      (io/copy raw (io/file dir file-name)))
    dir))

(defdescribe
  anydoc-citation-test
  (it
    "addresses a hit the way a human quotes it: section, line, column, passage"
    (let [dir (corpus {"report.docx" @report-docx-bytes})]
      (with-fs-context
        dir
        (expect
          (= [true "Revenue" "Revenue" true "Revenue" ["Quarterly Report" "Revenue"] "paragraph"
              "**Revenue** in March 2024 rose." true true]
             (ev
               python-context
               (py
                 "import anydoc"
                 (str "doc = anydoc.read('" dir "/report.docx')")
                 ;; The best hit is the passage that answers the WHOLE
                 ;; query, not the boosted heading that answers half.
                 "hit = doc.search('March revenue')[0]"
                 "line = doc.text.split('\\n')[hit.line - 1]" "[hit.document_id == doc.id,"
                 " hit.match," " doc.text[hit.offset:hit.end],"
                 " line[hit.column - 1:].startswith(hit.match)," " hit.section,"
                 " list(hit.path)," " hit.block_kind,"
                 " hit.highlight," " hit.snippet in line,"
                 " str(hit).startswith(doc.id + ' line %d \\u203a Quarterly Report \\u203a Revenue'"
                 "                     % hit.line)]")))))))
  (it "cites the PAGE a PDF hit is printed on"
      (let [dir (corpus {"report.pdf" @report-pdf-bytes})]
        (with-fs-context dir
                         (expect (= [2 2 true "pdf"]
                                    (ev python-context
                                        (py "import anydoc"
                                            (str "doc = anydoc.read('" dir "/report.pdf')")
                                            "hit = doc.search('April')[0]"
                                            "[doc.pages," " hit.page,"
                                            " hit.location.startswith('p.2')," " hit.format]")))))))
  (it "quotes a table hit by its own column"
      (let [dir (corpus {"report.docx" @report-docx-bytes})]
        (with-fs-context dir
                         (expect (= ["table-row" "Month" "March"]
                                    (ev python-context
                                        (py "import anydoc"
                                            (str "doc = anydoc.read('" dir "/report.docx')")
                                            "hit = doc.search('table:March')[0]"
                                            "[hit.block_kind, hit.cell.name, hit.cell.text]"))))))))

(defdescribe
  anydoc-matching-test
  (it "finds what a human would call a hit, not what a regex would"
      (let [dir (corpus {"report.docx" @report-docx-bytes})]
        (with-fs-context
          dir
          (expect
            (= [2 1 1 1 1 1 1 1 1]
               (ev python-context
                   (py "import anydoc" (str "doc = anydoc.read('" dir "/report.docx')")
                       "n = lambda q, **kw: len(doc.search(q, **kw))" "[n('\\\"March 2024\\\"'),"
                       " n('market')," " n('efficient'),"
                       " n('Z\u00fcrich')," " n(\"don't\"),"
                       " n('HAUPTSTRASSE')," " n('payment'),"
                       " n('Marching', whole_word=True)," " n('\\\"QUARTERLY REPORT\\\"')]")))))))
  (it "still refuses what is genuinely not there, and says what it looked for"
      (let [dir (corpus {"report.docx" @report-docx-bytes})]
        (with-fs-context dir
                         (expect (= [0 0 ["march"] 0 true]
                                    (ev python-context
                                        (py "import anydoc"
                                            (str "doc = anydoc.read('" dir "/report.docx')")
                                            "typo = doc.search('Marhc')" "[len(typo),"
                                            " len(doc.search('march', ignore_case=False)),"
                                            " typo.suggestions.get('marhc'),"
                                            " len(doc.search('\\\"nothing at all in here\\\"')),"
                                            " 'no hit for' in typo.explain()]"))))))))

(defdescribe
  anydoc-query-language-test
  (it "reads a real query: phrases, +/-, NEAR, prefixes, kinds, regex, sections"
      (let [dir (corpus {"report.docx" @report-docx-bytes})]
        (with-fs-context
          dir
          (expect (= [1 0 5 0 0 1 1 1 0]
                     (ev python-context
                         (py "import anydoc" (str "doc = anydoc.read('" dir "/report.docx')")
                             "n = lambda q, **kw: len(doc.search(q, **kw))"
                             ;; `+`/`-` judge the DOCUMENT, the way Lucene does: a
                             ;; document missing a required term is not searched at
                             ;; all, and one carrying an excluded term is dropped
                             ;; whole - `March -orders` finds nothing HERE because
                             ;; this document also says `Marching orders`.
                             "[n('\\\"market closed\\\"'),"
                             " n('\\\"closed market\\\"')," " n('+March +2024'),"
                             " n('+March +nowhere')," " n('March -orders'),"
                             " n('NEAR(revenue rose, 4)')," " n('heading:Revenue'),"
                             " n('/Marc\\\\w+ orders/')," " n('section:Revenue orders')]")))))))
  (it "explains a query before a single document is read"
      (with-fresh-python-context
        (expect (= [true true true]
                   (ev python-context
                       (py "import anydoc"
                           "text = anydoc.explain_query('\\\"total revenue\\\" +march -draft')"
                           "[ 'phrase' in text, 'required' in text, 'excluded' in text]"))))))
  (it "points at the character it choked on and suggests the spelling"
      (with-fresh-python-context
        (expect (= [true true true true true]
                   (ev python-context
                       (py "import anydoc" "out = []"
                           "probes = ('', '\\\"unclosed', 'NEAR(a b, x)', '-only', 'page:x foo')"
                           "for probe in probes:"
                           "    try:" "        anydoc.explain_query(probe)"
                           "        out.append(False)" "    except anydoc.QueryError as err:"
                           "        out.append(isinstance(err, ValueError) and bool(err.message))"
                           "out")))))))

(defdescribe
  anydoc-corpus-test
  (it "searches a directory, ranks the documents and keeps a broken file out of the way"
      (let
        [dir (corpus {"report.docx" @report-docx-bytes
                      "sales.csv" @march-csv-bytes
                      "broken.docx" (.getBytes "not a document at all" "UTF-8")
                      "notes.txt" (.getBytes "March again" "UTF-8")})]
        (with-fs-context
          dir
          (expect (= [["report.docx" "sales.csv"] ["broken.docx"] true ["csv" "docx"] true 2 true]
                     (ev python-context
                         (py
                           "import anydoc"
                           (str "found = anydoc.search('March', '" dir "')")
                           "names = lambda ids: sorted(i.rsplit('/', 1)[-1] for i in ids)"
                           "[names(found.documents),"
                           " names(s.id for s in found.skipped),"
                           " bool(found.skipped[0].reason),"
                           " sorted({c.format for c in found}),"
                           " [score for _, score in found.ranking] == sorted("
                           "     (score for _, score in found.ranking), reverse=True),"
                           (str " len(anydoc.search('March', '" dir "', per_document=1)),")
                           " all(c.line >= 1 and 'march' in c.match.lower() for c in found)]")))))))
  (it "drops a whole document a NOT term rules out, and keeps the rest"
      (let [dir (corpus {"report.docx" @report-docx-bytes "sales.csv" @march-csv-bytes})]
        (with-fs-context
          dir
          (expect (= [["sales.csv"] 2]
                     (ev python-context
                         (py "import anydoc"
                             (str "kept = anydoc.search('March -orders', '" dir "')")
                             (str "both = anydoc.search('March', '" dir "')")
                             "names = lambda ids: sorted(i.rsplit('/', 1)[-1] for i in ids)"
                             ;; `documents` is everything READ; `ranking` is
                             ;; what survived the query.
                             "[names(d for d, _ in kept.ranking), len(both.ranking)]")))))))
  (it "says so when a limit hid something"
      (let [dir (corpus {"report.docx" @report-docx-bytes "sales.csv" @march-csv-bytes})]
        (with-fs-context dir
                         (expect
                           (= [1 true false true]
                              (ev python-context
                                  (py "import anydoc"
                                      (str "capped = anydoc.search('March', '" dir "', limit=1)")
                                      (str "whole = anydoc.search('March', '" dir "')")
                                      "[len(capped)," " capped.is_truncated,"
                                      " whole.is_truncated,"
                                      " capped.total_matches == whole.total_matches > 1]")))))))
  (it "takes the ids the caller chose, a plain list, or bytes the caller named"
      (let [dir (corpus {"report.docx" @report-docx-bytes "sales.csv" @march-csv-bytes})]
        (with-fs-context
          dir
          (expect (= [["ledger" "report"] ["ledger" "report"] "ledger.csv" true]
                     (ev python-context
                         (py "import anydoc"
                             (str "docx = '" dir "/report.docx'")
                             (str "csv = '" dir "/sales.csv'")
                             "named = anydoc.search('March', {'report': docx, 'ledger': csv})"
                             "listed = anydoc.search('March', [docx, csv])"
                             ;; Bytes have no name of their own, so a mapping key
                             ;; lends them one: `ledger.csv` says what they are.
                             (str "raw = anydoc.search('March', {'ledger.csv': "
                                  (py-bytes (b64 @march-csv-bytes))
                                  "})")
                             "[sorted(named.documents)," " sorted({c.document_id for c in named}),"
                             " raw.citations[0].document_id,"
                             " listed.documents[csv].format == 'csv']")))))))
  (it "converts a corpus once, however many questions it is asked"
      (let [dir (corpus {"report.docx" @report-docx-bytes "sales.csv" @march-csv-bytes})]
        (with-fs-context
          dir
          (expect (= [2 0 1 1 0]
                     (ev python-context
                         (py "import anydoc"
                             "anydoc.clear_cache()"
                             (str "first = anydoc.search('March', '" dir "')")
                             (str "again = anydoc.search('April', '" dir "')")
                             (str "raw = {'ledger.csv': " (py-bytes (b64 @march-csv-bytes)) "}")
                             "anydoc.clear_cache()" "anydoc.search('March', raw)"
                             "one = anydoc.cache_info()" "anydoc.search('April', raw)"
                             "two = anydoc.cache_info()" "[first.stats['converted'],"
                             " again.stats['converted']," " one['misses'],"
                             " two['hits'] - one['hits']," " two['misses'] - one['misses']]"))))))))

(defdescribe anydoc-refusal-test
             (it "names the document, the source and the format when a document cannot be read"
                 (let [dir (corpus {"report.docx" @report-docx-bytes})]
                   (with-fs-context
                     dir
                     (expect
                       (= [true true true "SourceError" true]
                          (ev python-context
                              (py "import anydoc" "out = []"
                                  "try:" (str "    anydoc.search('March', '" dir "/missing.docx')")
                                  "    out.append(False)" "except OSError as err:"
                                  "    out.append('missing.docx' in str(err))" "try:"
                                  "    anydoc.search('March', b'not a document at all')"
                                  "    out.append(False)"
                                  "except anydoc.DocumentError as err:"
                                  "    out.append(bool(err.document_id) and bool(err.message))"
                                  "try:" "    anydoc.search('', b'x')"
                                  "    out.append(False)" "except anydoc.QueryError as err:"
                                  "    out.append(bool(str(err)))" "try:"
                                  "    anydoc.search('March', 42)" "    out.append('never')"
                                  "except TypeError as err:" "    out.append(type(err).__name__)"
                                  "    out.append('not int' in str(err) or 'not 42' in str(err))"
                                  "out"))))))))

;; --- the prose is CHECKED: what vis SAYS anydoc does, anydoc does -----------

(def ^:private docs-anydoc-section
  "The `anydoc` part of the docs page a human reads — heading to the next rule."
  (delay (or (re-find #"(?s)#### Asking a document a question.*?(?=\n---|\n#)"
                      (slurp (io/resource "vis-docs/extending.md")))
             (throw (ex-info "extending.md no longer documents anydoc" {})))))

(defn- py-list
  "Python list literal of `values` — identifiers and queries, none of which
   carries a single quote."
  [values]
  (str "[" (str/join ", " (map #(str "'" % "'") values)) "]"))

(defn- claimed-members
  "Every name the prose spells with a dot in front of it: `anydoc.search`,
   `hit.block_kind`, `.total_matches`, `.docx`."
  [text]
  (into (sorted-set) (map second) (re-seq #"\.([a-z_][a-z0-9_]{2,})\b" text)))

(defn- prose-surfaces
  "Every place vis DESCRIBES anydoc to somebody who will not read the source: the
   `:shim/description` the registry advertises (one bullet of the system prompt's
   sandbox-shims block, and the sandbox's `doc`/`apropos` gist), the module
   docstring the sandbox hands the model, and the docs page.
   All three are prose nobody runs."
  [^Context python-context]
  {":shim/description" (-> shim-anydoc/vis-extension
                           :ext/sandbox-shims
                           first
                           :shim/description)
   "anydoc.__doc__" (ev python-context (py "import anydoc" "anydoc.__doc__"))
   "extending.md" @docs-anydoc-section})

(defn- unknown-members
  "The claimed names that are NEITHER a member of one of anydoc's own objects NOR
   a file extension anydoc really reads."
  [^Context python-context dir claimed]
  (ev python-context
      (py "import anydoc"
          (str "doc = anydoc.read('" dir "/report.docx')")
          "hits = doc.search('March revenue')"
          "cell_hit = doc.search('table:March')[0]" (str "walk = anydoc.search('March', '" dir "')")
          "known = set()"
          "for obj in (anydoc, doc, hits, hits[0], cell_hit, cell_hit.cell, doc.blocks[0],"
          "            walk, walk.skipped[0], anydoc.Asset, anydoc.Document, anydoc.Citation,"
          "            anydoc.SearchResults, anydoc.Skipped, anydoc.Block, anydoc.Cell):"
          "    known |= set(dir(obj))" (str "claimed = " (py-list claimed))
          "missing = [name for name in claimed if name not in known]"
          "[name for name in missing if not anydoc.format_from_extension('.' + name)]")))

(defn- documented-queries
  "Every query vis SHOWS somebody: the first cell of each row of the docs table,
   and each line of the module docstring's own query block. An example that is
   printed has to parse and to run."
  [^Context python-context]
  (let
    [doc-string
     (ev python-context (py "import anydoc" "anydoc.__doc__"))

     table
     (->> (re-seq #"(?m)^\|([^|]*)\|" @docs-anydoc-section)
          (mapcat (fn [[_ cell]]
                    (map second (re-seq #"`([^`]+)`" cell)))))

     listed
     (some->> (second (str/split doc-string #"(?s)Query language[^\n]*\n" 2))
              (#(str/split % #"\n\n"))
              (filter #(str/starts-with? % "    "))
              first
              str/split-lines
              (map #(first (str/split (str/trim %) #"\s{2,}"))))]

    (into (sorted-set) (concat table listed))))

(defn- query-report
  "Per documented query: how `explain_query` parsed it, and whether a real search
   over `dir` ran instead of refusing."
  [^Context python-context dir queries]
  (ev python-context
      (py "import anydoc" (str "queries = " (py-list queries))
          "out = []" "for query in queries:"
          "    try:" "        head = anydoc.explain_query(query).splitlines()[0]"
          "    except anydoc.AnydocError as err:" "        head = 'REFUSED: %s' % err"
          "    try:" (str "        ran = bool(anydoc.search(query, '" dir "').documents)")
          "    except anydoc.AnydocError as err:" "        ran = 'REFUSED: %s' % err"
          "    out.append([query, head, ran])" "out")))

(defdescribe
  anydoc-prose-test
  (it "keeps every promise it prints — the prompt, the docstring and the docs"
      (let
        [dir (corpus {"report.docx" @report-docx-bytes
                      "sales.csv" @march-csv-bytes
                      "broken.pdf" (.getBytes "not a pdf at all" "UTF-8")})]
        (with-fs-context
          dir
          (expect (= {}
                     (into {}
                           (keep (fn [[label text]]
                                   (let
                                     [unknown
                                      (unknown-members python-context dir (claimed-members text))]
                                     (when (seq unknown) [label (vec unknown)]))))
                           (prose-surfaces python-context)))))))
  (it "parses AND runs every query it shows, from both surfaces"
      (let [dir (corpus {"report.docx" @report-docx-bytes "report.pdf" @report-pdf-bytes})]
        (with-fs-context dir
                         (let [queries (documented-queries python-context)]
                           (expect (<= 12 (count queries)))
                           (expect (= []
                                      (vec (remove (fn [[_ head ran]]
                                                     (and (str/starts-with? head "query:")
                                                          (true? ran)))
                                             (query-report python-context dir queries)))))))))
  (it "runs the very example the docs print, against real documents"
      (let
        [dir
         (corpus {"q1.pdf" @report-pdf-bytes "march.docx" @report-docx-bytes})

         example
         (-> (re-find #"(?s)```python\n(.*?)```" @docs-anydoc-section)
             second
             (str/replace "\"/data/reports\"" (str "'" dir "'")))]

        (with-fs-context dir
                         (expect
                           (= [true true true]
                              (ev python-context
                                  (py "import contextlib, io"
                                      "with contextlib.redirect_stdout(io.StringIO()) as shown:"
                                      (->> (str/split-lines example)
                                           (map #(str "    " %))
                                           (str/join "\n"))
                                      "printed = shown.getvalue().strip()"
                                      "lines = printed.splitlines()" "[bool(lines),"
                                      " any('q1.pdf' in line for line in lines),"
                                      " any(line.startswith('query:') for line in lines)]"))))))))
