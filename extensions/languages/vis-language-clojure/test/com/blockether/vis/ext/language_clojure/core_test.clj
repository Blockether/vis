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
