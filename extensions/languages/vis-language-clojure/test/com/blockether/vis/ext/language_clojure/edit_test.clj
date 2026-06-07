(ns com.blockether.vis.ext.language-clojure.edit-test
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.ext.language-clojure.edit :as edit]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (java.nio.file Files)
   (java.nio.file.attribute FileAttribute)))

(defn- tmp-dir ^java.io.File []
  (.toFile (Files/createTempDirectory "vis-clj-edit-" (into-array FileAttribute []))))

(defn- cleanup [^java.io.File root]
  (when (and root (.exists root))
    (doseq [^java.io.File f (reverse (file-seq root))]
      (.delete f))))

(def base-src
  "(ns demo.core)

(defn foo [x] (* x 2))

(defmulti area :shape)
(defmethod area :rectangle [{:keys [w h]}] (* w h))
")

(defdescribe replace-defn
  (it "swaps a defn by name and keeps file parsable"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op   :replace
                     :target "foo"
                     :code "(defn foo [x] (+ x 1))"
                     :is_format false})]
          (expect (= :ok (:status res)))
          (expect (re-find #"\(\+ x 1\)" (slurp f)))
          (expect (re-find #"defmulti area" (slurp f))))
        (finally (cleanup root))))))

(defdescribe replace-defmethod
  (it "swaps a defmethod by [name dispatch]"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :replace
                     :target ["area" :rectangle]
                     :code "(defmethod area :rectangle [{:keys [w h]}] (+ w h))"
                     :is_format false})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            (expect (re-find #"\(\+ w h\)" s))
            ;; ensure we didn't blow away the defmulti
            (expect (re-find #"defmulti area :shape" s))))
        (finally (cleanup root))))))

(defdescribe insert-after
  (it "inserts a new top-level form after the target"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :insert-after
                     :target "foo"
                     :code "(defn bar [] :bar)"
                     :is_format false})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            (expect (re-find #"defn foo" s))
            (expect (re-find #"defn bar" s))
            (expect (< (.indexOf s "defn foo") (.indexOf s "defn bar")))))
        (finally (cleanup root))))))

(defdescribe add-op
  (it "appends a new top-level form at EOF when no :target is given"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :add
                     :code "(defn appended [] :end)"})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            (expect (re-find #"defn appended" s))
            ;; after the LAST original form (the defmethod)
            (expect (< (.indexOf s "defmethod area") (.indexOf s "defn appended")))))
        (finally (cleanup root)))))

  (it "inserts after :target when one is given (insert-after alias)"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :add
                     :target "foo"
                     :code "(defn bar [] :bar)"})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            (expect (< (.indexOf s "defn foo") (.indexOf s "defn bar")))
            (expect (< (.indexOf s "defn bar") (.indexOf s "defmulti area")))))
        (finally (cleanup root))))))

(defdescribe replace-doc-op
  (it "swaps an existing docstring, leaving the body intact"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f "(ns demo.core)\n\n(defn foo \"old doc\" [x] (* x 2))\n")
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj" :op :replace-doc :target "foo" :code "Doubles x."})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            (expect (re-find #"\"Doubles x\.\"" s))
            (expect (not (re-find #"old doc" s)))
            (expect (re-find #"\(\* x 2\)" s))))
        (finally (cleanup root)))))

  (it "inserts a docstring when the def has none"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f "(ns demo.core)\n\n(defn bar [x] x)\n")
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj" :op :replace-doc :target "bar" :code "Identity."})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            (expect (re-find #"(?s)defn bar\s+\"Identity\.\"\s+\[x\]" s))))
        (finally (cleanup root)))))

  (it "treats a lone trailing string as a def VALUE, not a docstring"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f "(ns demo.core)\n\n(def greeting \"hello\")\n")
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj" :op :replace-doc :target "greeting" :code "A greeting."})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            ;; value preserved, docstring inserted BEFORE it
            (expect (re-find #"(?s)def greeting\s+\"A greeting\.\"\s+\"hello\"" s))))
        (finally (cleanup root))))))

(defdescribe missing-target
  (it "returns :error when target def is absent"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :replace
                     :target "nope"
                     :code "(defn nope [] 1)"
                     :is_format false})]
          (expect (= :error (:status res)))
          (expect (re-find #"target not found" (:error res))))
        (finally (cleanup root))))))

(defdescribe invalid-op
  (it "rejects unknown :op values"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :replace-all-the-things
                     :target "foo"
                     :code "(defn foo [] 1)"})]
          (expect (= :error (:status res)))
          (expect (re-find #"invalid :op" (:error res))))
        (finally (cleanup root))))))

(defdescribe missing-file
  (it "errors cleanly when the file does not exist"
    (let [root (tmp-dir)]
      (try
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "ghost.clj"
                     :op :replace
                     :target "foo"
                     :code "(defn foo [] 1)"})]
          (expect (= :error (:status res)))
          (expect (re-find #"file not found" (:error res))))
        (finally (cleanup root))))))
