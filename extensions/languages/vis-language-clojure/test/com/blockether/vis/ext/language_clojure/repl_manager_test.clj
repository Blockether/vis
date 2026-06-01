(ns com.blockether.vis.ext.language-clojure.repl-manager-test
  "Hermetic tests for the flag-gated REPL manager. The actual subprocess
   self-start is exercised in REPL-driven verification, not here, so these
   stay fast and side-effect-free."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.language-clojure.core :as core]
   [com.blockether.vis.ext.language-clojure.repl-manager :as rm]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^String []
  (.getAbsolutePath
    (.toFile (Files/createTempDirectory "vis-rm-" (into-array FileAttribute [])))))

(defn- with-file [^String dir name content]
  (spit (io/file dir name) content)
  dir)

(defdescribe flag-enabled?-test
  (it "is on by default and only off for an explicit falsy value (case-insensitive)"
    (doseq [v ["1" "true" "TRUE" "yes" "On" "maybe" "anything"]]
      (with-redefs [vis/extension-env-value (fn [_] v)]
        (expect (true? (rm/flag-enabled?)))))
    (doseq [v ["0" "false" "FALSE" "no" "off"]]
      (with-redefs [vis/extension-env-value (fn [_] v)]
        (expect (false? (rm/flag-enabled?))))))

  (it "is on when unset / blank (no config, no env)"
    (with-redefs [vis/extension-env-value (fn [_] nil)]
      ;; relies on VIS_CLJ_REPL_AUTOSTART being unset in the test env
      (expect (true? (rm/flag-enabled?))))
    (with-redefs [vis/extension-env-value (fn [_] "   ")]
      (expect (true? (rm/flag-enabled?))))))

(defdescribe launcher-for-test
  (it "selects clojure for deps.edn"
    (expect (= :clj (:tool (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}"))))))
  (it "selects lein for project.clj"
    (expect (= :lein (:tool (rm/launcher-for (with-file (tmp-dir) "project.clj" "(defproject x)"))))))
  (it "selects bb for bb.edn"
    (expect (= :bb (:tool (rm/launcher-for (with-file (tmp-dir) "bb.edn" "{}"))))))
  (it "returns nil when no known build file is present"
    (expect (nil? (rm/launcher-for (tmp-dir)))))
  (it "the clojure launcher injects the nrepl dep and runs nrepl.cmdline"
    (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}")))]
      (expect (= "clojure" (first cmd)))
      (expect (some #(= "nrepl.cmdline" %) cmd))
      ;; bare -M with no aliases
      (expect (some #(= "-M" %) cmd))))

  (it "threads deps.edn aliases into the clojure -M flag"
    (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}") [:dev :test]))]
      (expect (some #(= "-M:dev:test" %) cmd))))

  (it "threads lein profiles via with-profile"
    (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "project.clj" "(defproject x)") [:dev :test]))]
      (expect (some #(= "with-profile" %) cmd))
      (expect (some #(= "+dev,+test" %) cmd)))))

(defdescribe status+stop-test
  (it "status reports no managed process and a port vector for a fresh root"
    (let [s (rm/status (tmp-dir))]
      (expect (= :status (:result s)))
      (expect (false? (:running (:managed s))))
      (expect (vector? (:ports s)))))

  (it "stop is a safe no-op when nothing is managed"
    (expect (= :not-managed (:result (rm/stop! (tmp-dir)))))))

(defdescribe clj-repl-tool-gating-test
  (it ":start throws :clj/repl-disabled when the flag is off"
    (with-redefs [rm/flag-enabled? (fn [] false)]
      (let [t (try (core/clj-repl-fn {:workspace/root (tmp-dir)} :start)
                :no-throw
                (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
        (expect (= :clj/repl-disabled t)))))

  (it ":status succeeds regardless of the flag"
    (with-redefs [rm/flag-enabled? (fn [] false)]
      (expect (:success? (core/clj-repl-fn {:workspace/root (tmp-dir)} :status)))))

  (it "rejects an unknown op"
    (let [t (try (core/clj-repl-fn {:workspace/root (tmp-dir)} :frobnicate)
              :no-throw
              (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
      (expect (= :clj/bad-args t)))))

(defdescribe resolve-repl-dir-test
  ;; resolve-repl-dir returns canonical paths (stable process-map keys), so
  ;; expectations canonicalize too.
  (let [resolve  #'core/resolve-repl-dir
        canon    (fn [p] (.getCanonicalPath (io/file p)))]
    (it "blank/nil dir resolves to the workspace root"
      (let [root (tmp-dir)]
        (expect (= (canon root) (resolve root nil)))
        (expect (= (canon root) (resolve root "")))))
    (it "a relative dir resolves under the workspace root"
      (let [root (tmp-dir)
            _    (.mkdirs (io/file root "a" "b"))]
        (expect (= (canon (io/file root "a" "b"))
                  (resolve root "a/b")))))
    (it "an absolute dir is used as-is"
      (let [root (tmp-dir)
            abs  (tmp-dir)]
        (expect (= (canon abs) (resolve root abs)))))))
