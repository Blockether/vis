(ns com.blockether.vis.internal.anydoc-compat-shim-test
  "The `anydoc` shim installed into every sandbox context via the generic
   sandbox-shim mechanism: an `anydoc` module published into `sys.modules` that
   converts real Word / spreadsheet / CSV bytes to GitHub-Flavored Markdown
   through `com.blockether/imaging`'s Rust cdylib, while keeping the
   strings-only boundary (documents and assets cross as base64) and turning a
   host refusal into a catchable `anydoc.AnydocError`."
  (:require [clojure.string :as str]
            [com.blockether.imaging :as im]
            [com.blockether.vis.internal.env-python :as ep]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.util Base64]
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

(def ^:private docx-fixture
  (delay (b64 (im/docx {:blocks [{:type :heading :level 1 :text "Quarterly Report"}
                                 {:type :paragraph :text "Revenue grew."}]}))))

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

(defdescribe
  anydoc-document-test
  (it "returns a Document carrying the format, its evidence and its assets"
      (with-python-context (expect (= ["docx" "content" true true 0 true]
                                      (ev python-context
                                          (str "import anydoc\n"
                                               "doc = anydoc.to_document(" (py-bytes @docx-fixture)
                                               ")\n" "[doc.format, doc.source, doc.chars > 0,\n"
                                               " str(doc) == doc.markdown, len(doc.assets),\n"
                                               " isinstance(doc.assets, list)]"))))))
  (it
    "hands embedded binaries back as real Python bytes"
    (with-python-context
      (let
        [png
         (with-open [canvas (im/blank 2 2 "#ff0000")]
           (im/encode canvas :png))

         deck
         (im/pptx
           {:slides
            [{:shapes
              [{:kind :picture :image {:data png} :left 1.0 :top 1.0 :width 1.0 :height 1.0}]}]})]

        (expect (= [true true true]
                   (ev python-context
                       (str "import anydoc\n"
                            "doc = anydoc.to_document("
                            (py-bytes (b64 deck))
                            ")\n"
                            "asset = doc.assets[0]\n" "[doc.format == 'pptx',\n"
                            " isinstance(asset.bytes, bytes) and asset.bytes[:8] == "
                            (py-bytes (b64 (byte-array (take 8 png))))
                            ",\n" " len(asset.bytes) == len(asset)]"))))))))

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
