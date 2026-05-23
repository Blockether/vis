(ns com.blockether.vis.ext.language-clojure.find-test
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.language-clojure.find :as cfind]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-find-" (into-array FileAttribute []))))

(defn- cleanup [^java.io.File root]
  (when (.exists root)
    (doseq [^java.io.File f (reverse (file-seq root))] (.delete f))))

(defn- spit-rel [^java.io.File root rel content]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f content)))

(defdescribe find-defs-test
  (it "returns entries matching the regex"
    (let [root (tmp-dir)]
      (try
        (spit-rel root "src/a.clj" "(ns a)\n(defn make-foo [] 1)\n(defn other [] 2)\n")
        (spit-rel root "src/b.clj" "(ns b)\n(defn make-bar [] 1)\n")
        (let [{:keys [matches]} (cfind/find-defs (.getAbsolutePath root)
                                  {:name "^make-"})]
          (expect (= 2 (count matches)))
          (expect (every? #(re-find #"^make-" (:name %)) matches)))
        (finally (cleanup root)))))

  (it "filters by :kind"
    (let [root (tmp-dir)]
      (try
        (spit-rel root "src/a.clj"
          "(ns a)\n(def x 1)\n(defn y [] 2)\n(defmacro z [] 3)\n")
        (let [{:keys [matches]} (cfind/find-defs (.getAbsolutePath root)
                                  {:kind :defmacro})]
          (expect (= 1 (count matches)))
          (expect (= "z" (:name (first matches))))
          (expect (= :defmacro (:kind (first matches)))))
        (finally (cleanup root)))))

  (it "skips ignored directories"
    (let [root (tmp-dir)]
      (try
        (spit-rel root "src/keep.clj" "(ns k) (defn keepme [] 1)")
        (spit-rel root "target/skip.clj" "(ns s) (defn skipme [] 1)")
        (spit-rel root "node_modules/x/skip.clj" "(ns s) (defn skipme [] 1)")
        (let [{:keys [matches]} (cfind/find-defs (.getAbsolutePath root)
                                  {:name "(?i).me"})]
          (expect (some #(= "keepme" (:name %)) matches))
          (expect (not-any? #(= "skipme" (:name %)) matches)))
        (finally (cleanup root))))))
