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
          (expect (re-find #"defmulti area" (slurp f)))
          ;; result carries a unified diff (write evidence + the channel's
          ;; "regular patch view"): old line removed, new line added.
          (expect (string? (:diff res)))
          (expect (re-find #"(?m)^-.*\(\* x 2\)" (:diff res)))
          (expect (re-find #"(?m)^\+.*\(\+ x 1\)" (:diff res))))
        (finally (cleanup root))))))

(defdescribe op-as-python-snake-string
  ;; The Python caller sends op as a snake_case STRING (e.g. "insert_after"),
  ;; not a Clojure keyword. apply-edit! must normalize it to the kebab op
  ;; (:insert-after) the dispatch expects — else every clj_edit fails with
  ;; "invalid op". Regression guard for the GraalPy migration.
  (it "accepts a snake_case string op and dispatches like the keyword"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op   "insert_after"
                     :target "foo"
                     :code "(defn bar [] 1)"
                     :is_format false})]
          (expect (= :ok (:status res)))
          (expect (re-find #"defn bar" (slurp f))))
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

(def nested-src
  "(ns demo.core)

(defn pick [r]
  (let [x 1]
    (cond
      (:a r) (when (:b r) (+ 1 1))
      :else  0)))
")

(defdescribe insert-after-own-line
  (it "places the inserted form on its own line, not jammed onto the target"
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
            ;; bar lands on its OWN line (optionally indented), NOT jammed
            ;; onto foo's closing-paren line.
            (expect (re-find #"(?m)^\s*\(defn bar" s))
            (expect (not (re-find #"(?m)defn foo.*defn bar" s)))))
        (finally (cleanup root))))))

(defdescribe replace-sexp-deep
  (it "replaces a sexp nested deeper than two levels inside the target form"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f nested-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :replace-sexp
                     :target "pick"
                     :match "(+ 1 1)"
                     :code "(+ 2 2)"
                     :is_format false})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            (expect (re-find #"\(\+ 2 2\)" s))
            (expect (not (re-find #"\(\+ 1 1\)" s)))))
        (finally (cleanup root))))))

(defdescribe replacement-code-repair
  (it "repairs a delimiter slip in replacement code before parsing"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :replace
                     :target "foo"
                     :code "(defn foo [x]
  (let [y (inc x)]
    (+ y 1))"})]
          (expect (= :ok (:status res)))
          (expect (:repaired? res))
          (let [s (slurp f)]
            (expect (re-find #"\(\+ y 1\)\)\)" s))
            (expect (re-find #"defmulti area" s))))
        (finally (cleanup root))))))

(defdescribe broken-source-file
  ;; A file with unbalanced delimiters used to make apply-edit! THROW
  ;; (ex-info "source did not parse") -> raw exception bubble in the
  ;; channel. It must instead return a clean :error result that points
  ;; at clj_paren_repair, so the TUI/Web show one EDIT FAILED badge.
  (it "returns :error (no throw) when the source file does not parse"
    (let [root (tmp-dir)
          f    (io/file root "broken.clj")]
      (try
        (spit f "(defn a [x] (")
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "broken.clj"
                     :op :replace
                     :target "a"
                     :code "(defn a [x] x)"})]
          (expect (= :error (:status res)))
          (expect (re-find #"does not parse" (:error res)))
          (expect (re-find #"clj_paren_repair" (:error res)))
          ;; refused: the broken file is untouched
          (expect (= "(defn a [x] (" (slurp f))))
        (finally (cleanup root))))))

(defdescribe multi-form-code-refused
  ;; :code with TWO top-level forms used to be silently truncated to the
  ;; FIRST form (the zipper points at form 1; the rest was dropped with an
  ;; OK result - lost code). It must now refuse with a clear error.
  (it "errors on multi-form :code instead of dropping trailing forms"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")]
      (try
        (spit f base-src)
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj"
                     :op :replace
                     :target "foo"
                     :code "(def b 1)\n(def c 2)"
                     :is_format false})]
          (expect (= :error (:status res)))
          (expect (re-find #"2 top-level forms" (:error res)))
          ;; refused: target untouched
          (expect (re-find #"\(\* x 2\)" (slurp f))))
        (finally (cleanup root))))))

(defdescribe replace-doc-multiline
  (it "writes real newlines into the source, never literal backslash-n"
    (let [root (tmp-dir)
          f    (io/file root "a.clj")
          doc  "Line one.\nLine two with \"quotes\" and a back\\slash."]
      (try
        (spit f "(ns demo.core)\n\n(defn foo \"old\" [x] x)\n")
        (let [res (edit/apply-edit! (.getAbsolutePath root)
                    {:path "a.clj" :op :replace-doc :target "foo" :code doc})]
          (expect (= :ok (:status res)))
          (let [s (slurp f)]
            ;; the doc spans REAL source lines - no literal backslash-n pair
            (expect (re-find #"(?s)\"Line one\.\nLine two" s))
            (expect (not (re-find #"Line one\.\\n" s)))
            ;; quotes/backslash escaped so the file reads back to the same value
            (let [form (read-string (subs s (clojure.string/index-of s "(defn")))]
              (expect (= doc (nth form 2))))))
        (finally (cleanup root))))))






