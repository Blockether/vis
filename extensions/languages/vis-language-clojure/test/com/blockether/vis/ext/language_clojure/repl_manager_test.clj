(ns com.blockether.vis.ext.language-clojure.repl-manager-test
  "Hermetic tests for the REPL manager. The actual subprocess
   self-start is exercised in REPL-driven verification, not here, so these
   stay fast and side-effect-free."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
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
      ;; nrepl.cmdline now rides the synthetic `:vis/nrepl-launch` alias's
      ;; :main-opts inside the -Sdeps EDN, not as a bare cmd element.
      (expect (some #(str/includes? (str %) "nrepl.cmdline") cmd))
      ;; -M carries only the synthetic launch alias when no user aliases
      (expect (some #(= "-M:vis/nrepl-launch" %) cmd))))

  (it "threads deps.edn aliases into the clojure -M flag"
    (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}") [:dev :test]))]
      ;; user aliases come first, then the synthetic launch alias (last-wins)
      (expect (some #(= "-M:dev:test:vis/nrepl-launch" %) cmd))))

  (it "threads lein profiles via with-profile"
    (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "project.clj" "(defproject x)") [:dev :test]))]
      (expect (some #(= "with-profile" %) cmd))
      (expect (some #(= "+dev,+test" %) cmd)))))

(defdescribe status+stop-test
  ;; status/stop return STRING-keyed lifecycle maps (they cross the strings-only
  ;; boundary as tool `:result`s).
  (it "status reports no managed process and a port vector for a fresh root"
    (let [s (rm/status (tmp-dir))]
      (expect (= "status" (get s "result")))
      (expect (false? (get-in s ["managed" "running"])))
      (expect (vector? (get s "ports")))))

  (it "stop is a safe no-op when nothing is managed"
    (expect (= "not-managed" (get (rm/stop! (tmp-dir)) "result")))))

(defdescribe clj-repl-tool-gating-test

  (it "\"status\" always succeeds (start/stop are never flag-gated)"
    (expect (:success? (core/clj-repl-fn {:workspace/root (tmp-dir)} "status"))))

  (it "rejects an unknown op"
    (let [t (try (core/clj-repl-fn {:workspace/root (tmp-dir)} "frobnicate")
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

(defdescribe registry-reattach-test
  ;; The persistent registry is what lets a managed REPL survive a vis restart:
  ;; managed-ports re-attaches to the recorded PID via ProcessHandle. We stub
  ;; the registry IO so the user's real ~/.vis file is never touched, and use
  ;; the current JVM's PID as a guaranteed-alive process.
  (it "re-attaches a registry entry by live PID (survives restart)"
    (let [dir    (tmp-dir)
          my-pid (.pid (java.lang.ProcessHandle/current))]
      (spit (io/file dir ".nrepl-port") "54321")
      (with-redefs [rm/read-registry   (fn [] {dir {:pid my-pid :tool :clj :aliases [:dev]}})
                    rm/write-registry! (fn [_] nil)]
        (expect (= {:managed true :dir dir :tool :clj :aliases [:dev] :pid my-pid}
                  (get (rm/managed-ports) 54321))))))

  (it "drops a dead PID and prunes it from the registry"
    (let [dir   (tmp-dir)
          wrote (atom :unset)]
      (spit (io/file dir ".nrepl-port") "55555")
      (with-redefs [rm/read-registry   (fn [] {dir {:pid 2147483646 :tool :clj}}) ; not a live pid
                    rm/write-registry! (fn [m] (reset! wrote m))]
        (expect (empty? (rm/managed-ports)))
        (expect (= {} @wrote))))))           ; dead entry pruned
