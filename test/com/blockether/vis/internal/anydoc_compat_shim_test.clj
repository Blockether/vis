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
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util Base64]
           [org.graalvm.polyglot Context]))

(defn- ev [^Context context code] (ep/->clj (.eval context "python" code)))

(defonce ^:private python-context* (delay (ep/create-python-context {})))

(defmacro ^:private with-python-context
  [& body]
  `(let [~(with-meta 'python-context {:tag `Context}) (:python-context @python-context*)]
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

(def ^:private march-docx-bytes
  "Three lines mention March; one of them only inside the word `Marching`."
  (delay (im/docx {:blocks [{:type :heading :level 1 :text "Quarterly Report"}
                            {:type :paragraph :text "January was quiet."}
                            {:type :paragraph :text "March broke the record."}
                            {:type :paragraph :text "Marching orders arrived."}
                            {:type :paragraph :text "April cooled off, but March still leads."}]})))

(def ^:private march-csv-bytes (delay (.getBytes "month,revenue\nMarch,1200\nApril,900\n" "UTF-8")))

(defn- corpus
  "A directory of real documents on disk that the sandbox is allowed to open."
  ^String [entries]
  (let [dir (tmp-dir)]
    (doseq [[file-name ^bytes raw] entries]
      (io/copy raw (io/file dir file-name)))
    dir))

(defdescribe
  anydoc-search-one-test
  (it "cites the document id, the line a hit starts on and that line's own text"
      (let [dir (corpus {"report.docx" @march-docx-bytes})]
        (with-fs-context
          dir
          (expect (= [3 true "March" "docx" true true "March" true true true]
                     (ev python-context
                         (str "import anydoc\n"
                              "path = '" dir
                              "/report.docx'\n" "doc = anydoc.read(path)\n"
                              "hits = doc.search('March')\n" "first = hits[0]\n"
                              "lines = doc.markdown.split('\\n')\n" "[len(hits),\n"
                              " first.document_id == doc.id == path,\n" " first.query,\n"
                              " first.format,\n" " lines[first.line - 1] == first.text,\n"
                              " first.text[first.column - 1:].startswith('March'),\n"
                              " doc.markdown[first.offset:first.offset + 5],\n"
                              " 'March broke the record.' in first.text,\n"
                              " [c.line for c in hits] == sorted(c.line for c in hits),\n"
                              " str(first) == '%s:%d: %s' % (first.document_id, first.line,"
                              " first.text)]")))))))
  (it "answers the question it was asked: whole words, patterns, caps, context"
      (let [dir (corpus {"report.docx" @march-docx-bytes})]
        (with-fs-context dir
                         (expect (= [3 2 3 0 1 2 1 [1 1] false]
                                    (ev python-context
                                        (str "import anydoc\n"
                                             "doc = anydoc.read('"
                                             dir
                                             "/report.docx')\n"
                                             "near = doc.search('March', context=1)[0]\n"
                                             "[len(doc.search('March')),\n"
                                             " len(doc.search('March', whole_word=True)),\n"
                                             " len(doc.search('march')),\n"
                                             " len(doc.search('march', ignore_case=False)),\n"
                                             " len(doc.search(r'Marc\\w+ orders', regex=True)),\n"
                                             " len(doc.search(['January', 'April'])),\n"
                                             " len(doc.search('March', limit=1)),\n"
                                             " [len(near.before), len(near.after)],\n"
                                             " bool(doc.search('nothing at all in here'))]"))))))))

(defdescribe
  anydoc-search-many-test
  (it "searches a whole directory and keeps a corpus that has one broken file in it"
      (let
        [dir (corpus {"report.docx" @march-docx-bytes
                      "sales.csv" @march-csv-bytes
                      "broken.docx" (.getBytes "not a document at all" "UTF-8")
                      "notes.txt" (.getBytes "March again" "UTF-8")})]
        (with-fs-context
          dir
          (expect (= [["report.docx" "sales.csv"] ["report.docx" "sales.csv"] ["broken.docx"]
                      ["csv" "docx"] 4 2 true]
                     (ev python-context
                         (str "import anydoc\n"
                              "found = anydoc.search('March', '"
                              dir
                              "')\n"
                              "names = lambda ids: sorted(i.rsplit('/', 1)[-1] for i in ids)\n"
                              "[names(found.documents),\n"
                              " names(found.by_document()),\n"
                              " names(s['id'] for s in found.skipped),\n"
                              " sorted({c.format for c in found}),\n" " len(found),\n"
                              " len(anydoc.search('March', '" dir
                              "', per_document=1)),\n"
                              " all(c.line >= 1 and c.match == 'March' for c in found)]")))))))
  (it "takes the ids the caller chose, a plain list, or raw bytes"
      (let [dir (corpus {"report.docx" @march-docx-bytes "sales.csv" @march-csv-bytes})]
        (with-fs-context
          dir
          (expect
            (= [["ledger" "report"] ["ledger" "report"] 4 4 "document" true]
               (ev python-context
                   (str "import anydoc\n"
                        "docx = '"
                        dir
                        "/report.docx'\n"
                        "csv = '"
                        dir
                        "/sales.csv'\n"
                        "named = anydoc.search('March', {'report': docx, 'ledger': csv})\n"
                        "listed = anydoc.search('March', [docx, csv])\n"
                        "raw = anydoc.search('March', "
                        (py-bytes (b64 @march-docx-bytes))
                        ")\n"
                        "[sorted(named.documents),\n" " sorted({c.document_id for c in named}),\n"
                        " len(named),\n" " len(listed),\n"
                        " raw.citations[0].document_id,\n"
                        " listed.documents[csv].format == 'csv']")))))))
  (it "refuses a document it was told to read, an empty query and a bogus source"
      (let [dir (corpus {"report.docx" @march-docx-bytes})]
        (with-fs-context dir
                         (expect (= [true true true]
                                    (ev python-context
                                        (str "import anydoc\n" "out = []\n"
                                             "probes = (lambda: anydoc.search('March', '" dir
                                             "/missing.docx'),\n"
                                             "          lambda: anydoc.search('', b'x'),\n"
                                             "          lambda: anydoc.search('March', 42))\n"
                                             "for probe in probes:\n"
                                             "    try:\n" "        probe()\n"
                                             "        out.append(False)\n"
                                             "    except (OSError, ValueError, TypeError) as err:\n"
                                             "        out.append(bool(str(err)))\n" "out"))))))))
