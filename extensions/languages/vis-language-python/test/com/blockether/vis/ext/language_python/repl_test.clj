(ns com.blockether.vis.ext.language-python.repl-test
  "Managed Python REPL: interpreter detection, subprocess lifecycle + persistent
   eval, and the language-facade wiring. The live-subprocess tests SKIP when no
   Python is on PATH so CI without Python stays green."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.language-python.core :as core]
            [com.blockether.vis.ext.language-python.interpreter :as interp]
            [com.blockether.vis.ext.language-python.repl-manager :as repl]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- tmp-dir
  ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-py-ext-" (into-array FileAttribute []))))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(def ^:private on-path? @#'interp/on-path?)
(def ^:private test-session-id "python-pack-test")

(process-jail/register-session-jail!
  test-session-id
  (constantly {:roots-fn (constantly [(System/getProperty "java.io.tmpdir")]) :net-enabled? true}))

(defn- has-python? [] (boolean (or (on-path? "python3") (on-path? "python"))))

;; ── interpreter detection (no subprocess) ────────────────────────────────────
(defdescribe interpreter-test
             (it "prefers a project-local .venv interpreter when present"
                 (let
                   [root
                    (tmp-dir)

                    py
                    (io/file root ".venv" "bin" "python")]

                   (try (.mkdirs (.getParentFile py))
                        (spit py "#!/bin/sh\n")
                        (.setExecutable py true)
                        (expect (= [(.getCanonicalPath py)]
                                   (interp/resolve-command (.getPath root))))
                        (finally (cleanup root)))))
             (it "falls back to a system interpreter with no project env"
                 (let [root (tmp-dir)]
                   (try (let [cmd (interp/resolve-command (.getPath root))]
                          (expect (= 1 (count cmd)))
                          (expect (#{"python3" "python" "python.exe"} (first cmd))))
                        (finally (cleanup root))))))

;; ── live REPL subprocess ─────────────────────────────────────────────────────
(defdescribe repl-lifecycle-test
             (it "starts, evaluates, persists globals across evals, captures output + errors, stops"
                 (when (has-python?)
                   (let [dir (.getPath (tmp-dir))]
                     (try (expect
                            (= "up" (get (repl/start! dir {:session-id test-session-id}) "status")))
                          ;; last expression's value is captured (REPL semantics)
                          (expect (= "2" (get (repl/eval! dir "1+1" 10000) "value")))
                          ;; globals PERSIST across separate evals — a real session
                          (repl/eval! dir "x = 21" 10000)
                          (expect (= "42" (get (repl/eval! dir "x*2" 10000) "value")))
                          ;; stdout is captured, not leaked
                          (let [r (repl/eval! dir "print('hi')" 10000)]
                            (expect (= "hi\n" (get r "out")))
                            (expect (get r "ok")))
                          ;; an exception is captured, not thrown into Clojure
                          (let [r (repl/eval! dir "1/0" 10000)]
                            (expect (false? (get r "ok")))
                            (expect (re-find #"ZeroDivisionError" (str (get r "exc")))))
                          (expect (= "up" (get (repl/status dir) "status")))
                          (repl/stop! dir)
                          (expect (= "down" (get (repl/status dir) "status")))
                          (finally (repl/stop! dir))))))
             (it "eval before start fails closed with a clear error"
                 (let [dir (str (.getPath (tmp-dir)) "-never-started")]
                   (expect (= :py/no-repl
                              (try (repl/eval! dir "1" 1000)
                                   nil
                                   (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

;; ── language-facade wiring ───────────────────────────────────────────────────
(defdescribe
  facade-test
  (it "repl_eval requires explicit repl_start and then returns the value"
      (when (has-python?)
        (let
          [root
           (tmp-dir)

           dir
           (.getCanonicalPath root)

           env
           {:workspace/root (.getPath root) :session-id test-session-id}]

          (try (expect (= :py/no-repl
                          (try (core/py-repl-eval-fn env "3 * 7")
                               nil
                               (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
               (core/py-start-repl-fn env "start" nil)
               (let [r (core/py-repl-eval-fn env "3 * 7")]
                 (expect (:success? r))
                 (expect (= "21" (get-in r [:result "value"]))))
               (finally (repl/stop! dir))))))
  (it "repl_start status/stop lifecycle ops route through the manager"
      (when (has-python?)
        (let
          [root
           (tmp-dir)

           dir
           (.getCanonicalPath root)

           env
           {:workspace/root (.getPath root) :session-id test-session-id}]

          (try
            (expect (:success? (core/py-start-repl-fn env "start" nil)))
            (expect (= "up" (get-in (core/py-start-repl-fn env "status" nil) [:result "status"])))
            (core/py-start-repl-fn env "stop" nil)
            (expect (= "down" (get-in (core/py-start-repl-fn env "status" nil) [:result "status"])))
            (finally (repl/stop! dir)))))))

;; ── activation ───────────────────────────────────────────────────────────────
(def ^:private activation-fn @#'core/activation-fn)

(defdescribe activation-test
             (it "activates on a pyproject.toml workspace"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "pyproject.toml") "[project]\nname = \"x\"\n")
                        (expect (true? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "activates on a loose .py file"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "script.py") "print(1)\n")
                        (expect (true? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "stays dark on a non-Python workspace"
                 (let [root (tmp-dir)]
                   (try (spit (io/file root "README.md") "# nope\n")
                        (expect (false? (activation-fn {:workspace/root (.getAbsolutePath root)})))
                        (finally (cleanup root)))))
             (it "stays dark with no :workspace/root" (expect (false? (activation-fn {})))))

(defdescribe
  value-representation-test
  "Real Python objects come back as JSON-safe STRUCTURED data, not just a repr;
   objects that can't be serialized stay LIVE in the REPL and are described."
  (it "represents dicts / lists / sets as nested data"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! dir {:session-id test-session-id})
               (expect (= {"a" 1 "b" [2 3]}
                          (get (repl/eval! dir "{'a': 1, 'b': [2,3]}" 10000) "data")))
               (expect (= [1 2 3] (sort (get (repl/eval! dir "{3,1,2}" 10000) "data"))))
               (expect (= "dict" (get (repl/eval! dir "{}" 10000) "type")))
               (finally (repl/stop! dir))))))
  (it "represents a dataclass / custom object as a field map tagged with __type__"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! dir {:session-id test-session-id})
               (repl/eval!
                 dir
                 "from dataclasses import dataclass\n@dataclass\nclass P:\n    x: int\n    y: int"
                 10000)
               (expect (= {"x" 3 "y" 4 "__type__" "P"}
                          (get (repl/eval! dir "P(3,4)" 10000) "data")))
               (finally (repl/stop! dir))))))
  (it "an OPAQUE object stays LIVE + is described (type/repr/attrs), not lost"
      (when (has-python?)
        (let [dir (.getPath (tmp-dir))]
          (try (repl/start! dir {:session-id test-session-id})
               (let [d (get (repl/eval! dir "(i for i in range(3))" 10000) "data")]
                 (expect (get d "__opaque__"))
                 (expect (= "generator" (get d "__type__")))
                 (expect (string? (get d "__repr__"))))
               ;; bind it, then keep using it across evals — globals persist
               (repl/eval! dir "g = (i*i for i in range(4))" 10000)
               (expect (= "0" (get (repl/eval! dir "next(g)" 10000) "value")))
               (expect (= "1" (get (repl/eval! dir "next(g)" 10000) "value")))
               (expect (= "4" (get (repl/eval! dir "next(g)" 10000) "value")))
               (finally (repl/stop! dir)))))))
