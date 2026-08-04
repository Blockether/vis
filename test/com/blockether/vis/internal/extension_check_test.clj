(ns com.blockether.vis.internal.extension-check-test
  "`vis-agent extension check` -- the static check of a Python extension.

   The whole point is what it does NOT do: no line of the extension runs, so the
   proof below includes a file whose top level would write to disk and notify a
   human, and the check still comes back as a report. What it DOES do is answer
   the three questions an author cannot answer by reading: does it parse, does it
   only touch `vis` names that exist, and would the forms it asks for be accepted
   by the very seam the running dialog uses."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension-check :as check]
            [com.blockether.vis.internal.python-extensions :as pyx]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (org.graalvm.polyglot Context)))

(def ^:private valid-py
  (str
    "import vis\n" "\n"
    "FIELDS = [\n" "    vis.heading(\"Deploy\"),\n"
    "    vis.paragraph(\"Staging pages nobody.\"),\n"
    "    vis.select(\"env\", [vis.option(\"staging\"), vis.option(\"prod\")], is_required=True),\n"
    "    vis.slider(\"canary\", min=0, max=100, step=5, default=10),\n"
    "    vis.password(\"token\", is_required=True),\n"
    "]\n" "\n"
    "\n" "def deploy(args):\n"
    "    return vis.ask(\"Deploy\", FIELDS, submit_label=\"Ship it\")\n" "\n"
    "\n" "vis.extension(name=\"demo\", tools=[])\n"))

(def ^:private bad-select-py
  (str "import vis\n" "\n"
       "\n" "def deploy(args):\n"
       "    return vis.ask(\"Deploy\", [vis.select(\"env\", [])])\n" "\n"
       "\n" "vis.extension(name=\"demo\")\n"))

(def ^:private typo-py
  (str "import vis\n" "\n"
       "\n" "def deploy(args):\n"
       "    return vis.ask(\"Deploy\", [vis.plaintxt(\"who\")])\n" "\n"
       "\n" "vis.extension(name=\"demo\")\n"))

(def ^:private duplicate-names-py
  (str "import vis\n" "\n"
       "\n" "def deploy(args):\n"
       "    return vis.check(\"Deploy\", [vis.plaintext(\"who\"), vis.password(\"who\")])\n" "\n"
       "\n" "vis.extension(name=\"demo\")\n"))

(def ^:private unknowable-py
  (str "import vis\n" "\n"
       "\n" "def deploy(fields):\n"
       "    return vis.ask(\"Deploy\", fields)\n" "\n"
       "\n" "vis.extension(name=\"demo\")\n"))

(def ^:private broken-py "import vis\n\ndef deploy(:\n")

(def ^:private library-py "import vis\n\nENV = \"prod\"\n")

(def ^:private ^java.io.File proof-file
  (io/file (System/getProperty "java.io.tmpdir") "vis-extension-check-must-not-run.txt"))

(def ^:private side-effect-py
  (str "import vis\n"
       "\n"
       "with open("
       (pr-str (.getPath proof-file))
       ", \"w\") as fh:\n"
       "    fh.write(\"the checker ran the extension\")\n" "\n"
       "vis.notify(\"loaded\")\n" "vis.shell(\"touch /tmp/vis-extension-check-shelled\")\n"
       "\n" "vis.extension(name=\"demo\")\n"))

(def ^:private reports
  ;; ONE checker context for the whole suite: building the `vis` module is the
  ;; expensive part, and every case below is judged in the same one.
  (delay (into {}
               (map (juxt :path identity))
               (check/check-sources [["valid.py" valid-py] ["bad-select.py" bad-select-py]
                                     ["typo.py" typo-py] ["duplicate-names.py" duplicate-names-py]
                                     ["unknowable.py" unknowable-py] ["broken.py" broken-py]
                                     ["library.py" library-py]
                                     ["side-effect.py" side-effect-py]]))))

(defn- report [path] (get @reports path))

(defn- kinds [path] (set (map :kind (:problems (report path)))))

(defn- reason [path] (:message (first (:problems (report path)))))

(defdescribe
  extension-check-test
  (describe "a file that is fine"
            (it "reports valid, with the forms it actually judged counted"
                (expect (:is-valid (report "valid.py")))
                (expect (= [] (:problems (report "valid.py"))))
                (expect (= 1 (:checked (report "valid.py"))))
                (expect (= 0 (:skipped (report "valid.py")))))
            (it "reads the builders out of the real vis module"
                ;; `vis.heading`, `vis.option`, `vis.slider` are the shipped builders --
                ;; an unknown-attribute problem here would mean the check invented a name.
                (expect (= #{} (kinds "valid.py")))))
  (describe "a form the engine would refuse"
            (it "answers with the engine's own one-line reason, at the ask's line"
                (expect (= #{"invalid-request"} (kinds "bad-select.py")))
                (expect (str/includes? (reason "bad-select.py") "select needs at least one option"))
                (expect (= 5 (:line (first (:problems (report "bad-select.py"))))))
                (expect (= 1 (:checked (report "bad-select.py")))))
            (it "judges a vis.check call exactly like a vis.ask call"
                (expect (= #{"invalid-request"} (kinds "duplicate-names.py")))
                (expect (str/includes? (reason "duplicate-names.py")
                                       "field names must be distinct"))))
  (describe "a vis name that does not exist"
            (it "is caught before it raises in front of a human"
                (expect (contains? (kinds "typo.py") "unknown-attribute"))
                (expect (str/includes? (str (some #(when (= "unknown-attribute" (:kind %))
                                                     (:message %))
                                                  (:problems (report "typo.py"))))
                                       "plaintxt")))
            (it "is a syntax problem when the file does not even parse"
                (expect (= #{"syntax"} (kinds "broken.py")))
                (expect (= 3 (:line (first (:problems (report "broken.py"))))))))
  (describe "what only running the file would tell"
            (it "is counted as skipped rather than guessed at"
                (expect (:is-valid (report "unknowable.py")))
                (expect (= 0 (:checked (report "unknowable.py"))))
                (expect (= 1 (:skipped (report "unknowable.py"))))))
  (describe "a file that registers nothing"
            (it "says so instead of passing quietly"
                (expect (= #{"no-extension"} (kinds "library.py")))))
  (describe "nothing in the extension runs"
            (it "checks a file whose top level would write a file and notify a human"
                (.delete proof-file)
                ;; Re-checked in its own context so the assertion cannot pass just because
                ;; the shared run happened to skip this source.
                (let [r (check/check-source side-effect-py "side-effect.py")]
                  (expect (map? r))
                  (expect (not (.exists proof-file)))
                  (expect (not (.exists (io/file "/tmp/vis-extension-check-shelled"))))
                  ;; `vis.notify` / `vis.shell` exist and the file registers itself, so a
                  ;; file whose top level is nothing but side effects checks CLEAN: every
                  ;; call was READ, none was made.
                  (expect (= [] (:problems r)))
                  (expect (:is-valid r)))))
  (describe "a run is data, not a throw"
            (it "reports an unreadable path instead of aborting the run"
                (let
                  [rs (check/check-files [(io/file (System/getProperty "java.io.tmpdir")
                                                   "vis-extension-check-absent.py")])]
                  (expect (= 1 (count rs)))
                  (expect (= #{"unreadable"} (set (map :kind (:problems (first rs))))))
                  (expect (not (check/ok? rs))))))
  (describe "the printed report"
            (it "carries one line per file, one per problem, and a tally"
                (let
                  [text
                   (check/report-text [(report "valid.py") (report "bad-select.py")])

                   lines
                   (str/split-lines text)]

                  (expect (str/starts-with? (first lines) "ok   valid.py"))
                  (expect (some #(str/starts-with? % "FAIL bad-select.py") lines))
                  (expect (some #(str/includes? % "bad-select.py:5:") lines))
                  (expect (= "2 files, 2 forms checked, 1 problem" (last lines))))))
  (describe "expand-paths"
            (it "turns a directory into its own *.py files, in name order"
                (let
                  [dir (io/file (System/getProperty "java.io.tmpdir")
                                (str "vis-extension-check-" (System/nanoTime)))]
                  (.mkdirs dir)
                  (try (spit (io/file dir "b.py") valid-py)
                       (spit (io/file dir "a.py") valid-py)
                       (spit (io/file dir "notes.txt") "not python")
                       (expect (= ["a.py" "b.py"]
                                  (mapv (fn [f]
                                          (.getName (io/file f)))
                                        (check/expand-paths [(.getPath dir)]))))
                       (finally (run! io/delete-file (reverse (file-seq dir))))))
                (it "keeps an explicitly named file even when it is not a directory"
                    (expect (= ["deploy.py"]
                               (mapv (fn [f]
                                       (.getName (io/file f)))
                                     (check/expand-paths ["deploy.py"]))))))))

;; The checker reads the REAL `vis` module, and the bootstrap builds its `_host`
;; dict at module level: a callback nobody bound is a `NameError` before the
;; module exists, so a new host call silently breaks `extension check` unless it
;; is listed. That list is the contract, and this is the test that holds it.
(defdescribe
  inert-host-test
  (describe "every host callback the bootstrap reads"
            (it "is bound as a refusal by the checker's binder"
                (expect (= (set (re-seq #"__vis_host_\w+__" pyx/bootstrap-python))
                           (set pyx/host-member-names))))
            (it "cannot run, so a checked form is judged by the engine and nobody else"
                (let [^Context ctx (pyx/build-context)]
                  (try (pyx/bind-inert-host! ctx nil)
                       (.eval ctx "python" ^String pyx/bootstrap-python)
                       (expect (str/includes?
                                 (str (try (.eval ctx "python" "import vis\nvis.notify(\"nope\")")
                                           (catch Exception e (ex-message e))))
                                 "not available while checking"))
                       (finally (.close ctx)))))))
