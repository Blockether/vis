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
