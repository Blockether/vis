(ns com.blockether.vis.ext.lang-clojure.core-test
  (:require
   [babashka.fs :as fs]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.lang-clojure.core :as clj-ext]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]))

(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.lang-clojure.core" name))))

(defn- temp-root [name]
  (let [root (fs/file "target/lang-clojure-test" name)]
    (fs/delete-tree root)
    (fs/create-dirs root)
    (.getCanonicalFile root)))

(defdescribe clojure-extension-activation-test
  (it "activates for Clojure marker files"
    (let [root (temp-root "marker")
          clojure-project? (private-fn "clojure-project?")]
      (spit (fs/file root "deps.edn") "{}")
      (expect (true? (clojure-project? root)))))

  (it "activates for nested Clojure source files"
    (let [root (temp-root "source")
          clojure-project? (private-fn "clojure-project?")]
      (fs/create-dirs (fs/file root "src" "demo"))
      (spit (fs/file root "src" "demo" "core.clj") "(ns demo.core)\n")
      (expect (true? (clojure-project? root)))))

  (it "stays inactive when no Clojure marker or source exists"
    (let [root (temp-root "plain")
          clojure-project? (private-fn "clojure-project?")]
      (spit (fs/file root "README.md") "plain\n")
      (expect (false? (clojure-project? root)))))

  (it "registers language-gated z extension metadata"
    (expect (= 'com.blockether.vis.ext.lang-clojure.core
              (:ext/namespace clj-ext/clojure-extension)))
    (expect (= "languages" (:ext/kind clj-ext/clojure-extension)))
    (expect (fn? (:ext/activation-fn clj-ext/clojure-extension))))

  (it "exports z/patch helpers and rewrite-clj zipper API"
    (let [symbols (set (map :ext.symbol/sym (:ext/symbols clj-ext/clojure-extension)))]
      (expect (contains? symbols 'patch))
      (expect (contains? symbols 'locators))
      (expect (contains? symbols 'symbols))
      (expect (contains? symbols 'find-value))
      (expect (contains? symbols 'replace))
      (expect (contains? symbols 'subedit->))))

  (it "makes rewrite-clj threading macros callable inside SCI"
    (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]} (vis/create-sci-context nil)
          env {:sci-ctx sci-ctx
               :sandbox-ns sandbox-ns
               :initial-ns-keys initial-ns-keys
               :extensions (atom [])}
          eval* (fn [s] (:val (sci/eval-string+ sci-ctx s {:ns sandbox-ns})))]
      (vis/install-extension! env clj-ext/clojure-extension)
      (expect (= "(x b)"
                (eval* "(z/root-string (z/subedit-> (z/of-string \"(a b)\") z/down (z/replace (quote x))))")))
      (expect (= "(x b)"
                (eval* "(z/root-string (z/subedit->> (z/of-string \"(a b)\") (#(z/down %)) (#(z/replace % (quote x)))))")))
      (expect (= "(x b)"
                (eval* "(z/root-string (z/edit-> (z/of-string \"(a b)\") z/down (z/replace (quote x))))")))
      (expect (= "(x b)"
                (eval* "(z/root-string (z/edit->> (z/of-string \"(a b)\") (#(z/down %)) (#(z/replace % (quote x)))))"))))))
