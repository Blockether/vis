(ns com.blockether.vis.ext.language-clojure.core-test
  "Activation-gate test for the language-clojure extension. Confirms
   the extension activates on Clojure workspaces and stays dark on
   plain ones."
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.language-clojure.core :as core]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-clj-ext-act-" (into-array FileAttribute []))))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))] (.delete f))))

(defn- activation-fn []
  ;; private — reach into ns directly so the manifest stays the
  ;; public contract.
  @#'core/activation-fn)

(defdescribe activation-test
  (it "activates when deps.edn is at the workspace root"
    (let [root (tmp-dir)]
      (try
        (spit (io/file root "deps.edn") "{:paths [\"src\"]}")
        (expect (true? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
        (finally (cleanup root)))))

  (it "activates when only a .nrepl-port is present"
    (let [root (tmp-dir)]
      (try
        (spit (io/file root ".nrepl-port") "7888")
        (expect (true? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
        (finally (cleanup root)))))

  (it "activates when .clj sources exist without any manifest"
    (let [root (tmp-dir)]
      (try
        (let [src (io/file root "src" "x.clj")]
          (.mkdirs (.getParentFile src))
          (spit src "(ns x)"))
        (expect (true? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
        (finally (cleanup root)))))

  (it "stays dark on a non-Clojure workspace"
    (let [root (tmp-dir)]
      (try
        (spit (io/file root "README.md") "# nope\n")
        (let [f (io/file root "src" "x.py")]
          (.mkdirs (.getParentFile f))
          (spit f "print('hi')"))
        (expect (false? ((activation-fn) {:workspace/root (.getAbsolutePath root)})))
        (finally (cleanup root)))))

  (it "stays dark when :workspace/root is missing"
    (expect (false? ((activation-fn) {})))))

(defn- classpath-manifests
  "Return every parsed `META-INF/vis-extension/vis.edn` on the classpath.
   `io/resource` only yields the FIRST match (whichever jar loads first),
   but the scanner walks `getResources` — mirror that here so the test
   sees this extension's manifest even when another extension is also
   on the test classpath."
  []
  (let [cl (.getContextClassLoader (Thread/currentThread))
        urls (enumeration-seq (.getResources cl "META-INF/vis-extension/vis.edn"))]
    (mapv (fn [u] (read-string (slurp u))) urls)))

(defdescribe manifest-discovery-test
  ;; Regression: the extension was invisible because
  ;; `resources/META-INF/vis-extension/vis.edn` did not exist. With no
  ;; manifest the classpath scanner skips the namespace, the ns is
  ;; never `require`d, `(vis/register-extension! …)` never runs, and
  ;; `clj/` shows up nowhere — see conversation
  ;; 11d4f817-fbd1-43ab-a6b4-052c8557af0a issue #3
  ;; (\"Dlaczego CLOJURE extension nie jest widoczny?!\"). The manifest
  ;; is the public discovery contract; keep it pinned by a test so
  ;; nobody silently deletes it again.
  (it "ships a vis-extension manifest with the language-clojure id on the classpath"
    (let [manifests (classpath-manifests)
          merged    (reduce merge {} manifests)]
      (expect (seq manifests))
      (expect (contains? merged 'language-clojure))))

  (it "manifest registers the core namespace under the language-clojure id"
    (let [manifests (classpath-manifests)
          merged    (reduce merge {} manifests)]
      (expect (some #{'com.blockether.vis.ext.language-clojure.core}
                (get-in merged ['language-clojure :nses]))))))

(defdescribe symbol-surface-test
  (it "exposes only Clojure-specific helpers, not generic language tools"
    (let [syms (set (map :ext.symbol/symbol @#'core/clj-symbols))]
      (expect (contains? syms 'edit))
      (expect (contains? syms 'paren-repair))
      (expect (not (contains? syms 'repl)))
      (expect (not (contains? syms 'eval)))
      (expect (not (contains? syms 'test)))
      (expect (not (contains? syms 'format))))))
