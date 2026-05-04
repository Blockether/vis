(ns com.blockether.vis.ext.lang-clojure.core-test
  (:require
   [babashka.fs :as fs]
   [com.blockether.vis.ext.lang-clojure.core :as clj-ext]
   [lazytest.core :refer [defdescribe expect it]]))

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
    (expect (fn? (:ext/activation-fn clj-ext/clojure-extension)))))
