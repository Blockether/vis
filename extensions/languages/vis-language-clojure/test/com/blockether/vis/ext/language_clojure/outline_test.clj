(ns com.blockether.vis.ext.language-clojure.outline-test
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.language-clojure.outline :as outline]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(def sample
  "(ns demo.core
  \"Sample for outline test.\"
  (:require [clojure.string :as str]))

(def ^:private secret 1)

(defn add
  \"Add two numbers.

   Multi-line doc.\"
  [a b]
  (+ a b))

(defn- helper [x] x)

(defmulti area :shape)
(defmethod area :rectangle [{:keys [w h]}] (* w h))
(defmethod area :circle [{:keys [r]}] (* Math/PI r r))
")

(defdescribe outline-string-test
  (it "captures ns, def, defn, defn-, defmulti, and defmethod entries"
    (let [o (outline/outline-string sample)
          forms (:forms o)]
      (expect (= "demo.core" (get-in o [:ns :name])))
      (expect (contains? (set (map :name forms)) "secret"))
      (expect (some #(and (= "add" (:name %)) (= :defn (:kind %))) forms))
      (expect (some #(and (= "helper" (:name %)) (= :defn (:kind %)) (:private? %)) forms))
      (expect (some #(and (= "area" (:name %)) (= :defmulti (:kind %))) forms))
      (let [methods (filter #(= :defmethod (:kind %)) forms)]
        (expect (= 2 (count methods)))
        (expect (every? :dispatch methods)))))

  (it "extracts arglists and first docstring line"
    (let [o     (outline/outline-string sample)
          add   (first (filter #(= "add" (:name %)) (:forms o)))]
      (expect (= [["a" "b"]] (:arglists add)))
      (expect (= "Add two numbers." (:doc add)))))

  (it "returns counts and total"
    (let [o (outline/outline-string sample)]
      (expect (= (:total o) (count (:forms o))))
      (expect (pos? (get-in o [:counts :defn]))))))

(defdescribe outline-file-test
  (it "reads from disk and surfaces :path :bytes"
    (let [dir (.toFile (Files/createTempDirectory "vis-outline-" (into-array FileAttribute [])))
          f   (io/file dir "x.clj")]
      (try
        (spit f sample)
        (let [o (outline/outline-file (.getAbsolutePath dir) "x.clj")]
          (expect (= (.getAbsolutePath f) (:path o)))
          (expect (pos? (:bytes o)))
          (expect (= "demo.core" (get-in o [:ns :name]))))
        (finally
          (when (.exists f) (.delete f))
          (.delete dir))))))
