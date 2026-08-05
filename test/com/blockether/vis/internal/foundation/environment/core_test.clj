(ns com.blockether.vis.internal.foundation.environment.core-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.env-python :as env-python]
            [com.blockether.vis.internal.foundation.environment.core :as env-core]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- make-tmp-dir
  ^java.io.File []
  (let [path (Files/createTempDirectory "vis-env-core-" (into-array FileAttribute []))]
    (.toFile path)))

(defn- spit-rel
  [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (when-let [parent (.getParentFile f)]
      (.mkdirs parent))
    (spit f content)))

(defn- cleanup
  [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(defn- git!
  [^java.io.File root & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
    (.directory pb root)
    (.redirectErrorStream pb true)
    (let [p (.start pb)]
      (slurp (.getInputStream p))
      (.waitFor p))))

(defn- init-repo-on-branch!
  [^java.io.File root branch]
  (git! root "init" "-q")
  (git! root "config" "user.name" "test")
  (git! root "config" "user.email" "test@example.com")
  (git! root "config" "commit.gpgsign" "false")
  (spit-rel root "README.md" "# initial")
  (git! root "add" "README.md")
  (git! root "commit" "-q" "-m" "init")
  (git! root "checkout" "-q" "-b" branch))

(defdescribe environment-core-test
             (it "exports the expected environment symbol surface"
                 (let [syms (set (map :ext.symbol/symbol env-core/environment-symbols))]
                   (expect (not (contains? syms 'snapshot)))
                   (expect (not (contains? syms 'git)))
                   (expect (contains? syms 'repositories))
                   (expect (contains? syms 'languages))
                   (expect (contains? syms 'monorepo))
                   (expect (contains? syms 'refresh!))
                   (expect (not (contains? syms 'render)))
                   (expect (contains? syms 'main-agent-instructions))
                   (expect (not (contains? syms 'load-skill!)))
                   (expect (not (contains? syms 'load-skill)))
                   (expect (not (contains? syms 'reload-skills!)))
                   (expect (not (contains? syms 'scan-warnings)))
                   (expect (not (contains? syms 'reload-instructions!)))
                   (expect (not (contains? syms 'reload-extensions!)))))
             (it "provides foundation environment info through ctx"
                 (let [ctx (env-core/environment-ctx {})]
                   (expect (contains? ctx :project))
                   (expect (contains? (:project ctx) :host))
                   (expect (contains? (:project ctx) :root))))
             (it "uses active workspace root instead of JVM cwd for internal git facts"
                 (let
                   [root
                    (make-tmp-dir)

                    branch
                    "feature/ws"]

                   (try (init-repo-on-branch! root branch)
                        (binding [workspace/*workspace-root* (.getCanonicalPath root)]
                          (let [git (:git (env-core/refresh!))]
                            (expect (= branch (:branch git)))
                            (expect (= (.getCanonicalPath root) (:root git)))))
                        (finally (binding [workspace/*workspace-root* nil]
                                   (env-core/refresh!))
                                 (cleanup root))))))

(defn- symbol-view
  "Call the environment symbol named `sym` and return what Python would hold."
  [sym]
  (let
    [{:ext.symbol/keys [fn]} (first (filter #(= sym (:ext.symbol/symbol %))
                                            env-core/environment-symbols))]
    (env-python/boundary-view (:result (fn)))))

;; Regression, issue #115: every environment symbol handed Python its RAW
;; keyword-keyed snapshot, so `await refresh()` died with "STRINGS-ONLY
;; boundary violation: non-string-key :host at the TOP-LEVEL map key" — and so
;; did repositories(), languages(), monorepo() and main_agent_instructions().
(defdescribe environment-symbols-boundary-test
             (it "hands Python string-keyed payloads for every environment symbol"
                 (doseq [sym-map env-core/environment-symbols]
                   (let
                     [sym (:ext.symbol/symbol sym-map)
                      envelope ((:ext.symbol/fn sym-map))
                      ;; `boundary-view` is the no-context mirror of the real
                      ;; Clojure->Python boundary: it THROWS on a keyword key or
                      ;; value at any depth, exactly like `->py` does in GraalPy.
                      view (env-python/boundary-view (:result envelope))]

                     (expect (:success? envelope) (str sym " envelope must succeed"))
                     (expect (map? view) (str sym " must return a dict"))
                     (expect (every? string? (keys view)) (str sym " top-level keys")))))
             (it "spells refresh() keys the way its docstring promises"
                 (let [view (symbol-view 'refresh!)]
                   (expect (= #{"host" "git" "languages" "monorepo" "repositories"}
                              (set (keys view))))
                   (expect (string? (get-in view ["host" "os_name"])))
                   (expect (contains? (get view "languages") "total_files"))
                   (expect (contains? (get view "languages") "is_truncated"))
                   (expect (contains? (get view "repositories") "count")))))
