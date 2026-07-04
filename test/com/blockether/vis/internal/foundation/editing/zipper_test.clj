(ns com.blockether.vis.internal.foundation.editing.zipper-test
  "Language-neutral structural zipper over tree-sitter: parse → navigate by
   named-child path → splice-edit with syntax refusal."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.core :as editing]
            [com.blockether.vis.internal.foundation.editing.zipper :as zip]
            [com.blockether.vis.internal.extension :as ext]
            ;; Side-effecting: registers the foundation editing extension at load
            ;; so the op->tag index covers sexpr/sexpr_edit (tool-success reads it).
            [com.blockether.vis.internal.foundation.core]
            [lazytest.core :refer [defdescribe it expect]]))

(def ^:private clj-src "(ns foo)\n(defn bar [x] (+ x 1))\n")
(def ^:private py-src "def foo(x):\n    return x + 1\n")

(defn- child-idx-containing [info needle]
  (some (fn [{:keys [idx head]}] (when (str/includes? (str head) needle) idx))
    (:children info)))

(defdescribe zipper-test
  (it "parses, navigates by path, and edits a Clojure tree"
    (let [root (zip/inspect "clojure" clj-src [])]
      (expect (:ok? root))
      (expect (>= (:named-child-count root) 2))
      (let [i    (child-idx-containing root "defn")
            node (zip/inspect "clojure" clj-src [i])]
        (expect (some? i))
        (expect (str/includes? (:text node) "defn bar"))
        ;; replace that whole form structurally
        (let [r (zip/edit "clojure" clj-src [i] :replace "(defn bar [x] (* x 2))")]
          (expect (:ok? r))
          (expect (str/includes? (:new-source r) "(* x 2)"))
          (expect (not (str/includes? (:new-source r) "(+ x 1)"))))
        ;; a syntax-breaking replace is refused
        (let [r (zip/edit "clojure" clj-src [i] :replace "(defn bar [x]")]
          (expect (= :syntax-broken (get-in r [:error :reason])))))))

  (it "works language-neutrally on a Python tree"
    (let [root (zip/inspect "python" py-src [])]
      (expect (:ok? root))
      (let [i    (child-idx-containing root "def foo")
            node (zip/inspect "python" py-src [i])]
        (expect (some? i))
        (expect (str/includes? (:text node) "def foo"))
        (let [r (zip/edit "python" py-src [i] :replace "def foo(x):\n    return x * 2")]
          (expect (:ok? r))
          (expect (str/includes? (:new-source r) "x * 2"))))))

  (it "descends a deeper named-child path (the cursor going down)"
    (let [i      (child-idx-containing (zip/inspect "clojure" clj-src []) "defn")
          deeper (zip/inspect "clojure" clj-src [i 0])]
      (expect (:ok? deeper))
      (expect (string? (:kind deeper)))))

  (it "errors cleanly on a bad path"
    (let [r (zip/inspect "clojure" clj-src [99])]
      (expect (= :bad-path (get-in r [:error :reason]))))))

(defn- write-temp! [name content]
  (fs/create-dirs "target/editing-test")
  (let [rel (str "target/editing-test/" name)]
    (spit (fs/file rel) content)
    rel))

(defdescribe sexpr-verbs-test
  (it "sexpr navigates + struct_patch splices the SAME path (unified surface)"
    (let [sexpr       @#'editing/sexpr-tool
          struct-patch @#'editing/struct-patch-tool
          path (write-temp! "zip.clj" "(ns z)\n(defn g [x] (+ x 1))\n")
          root (:result (sexpr path))]
      (expect (>= (get root "named_child_count") 2))
      (let [i (some (fn [c] (when (str/includes? (str (get c "head")) "defn") (get c "idx")))
                (get root "children"))]
        (expect (some? i))
        (expect (str/includes? (get (:result (sexpr path {"at" [i]})) "text") "defn g"))
        ;; relative move sugar: at=[i], nav=["down"] resolves to [i 0]
        (expect (:success? (sexpr path {"at" [i] "nav" ["down"]})))
        ;; struct_patch takes the zipper PATH (sexpr_edit folded into it)
        (let [ed (struct-patch "path" path "at" [i] "op" "replace" "code" "(defn g [x] (* x 9))")]
          (expect (:success? ed))
          (expect (str/includes? (slurp (fs/file path)) "(* x 9)")))
        ;; syntax-breaking edit refused
        (expect (try (struct-patch "path" path "at" [i] "op" "replace" "code" "(defn g [x]")
                  false (catch clojure.lang.ExceptionInfo _ true)))))))

(defdescribe op-keyword-regression-test
  (it "every structural op emits an op-keyword that resolves its registered tag"
    ;; struct_patch / project_references were long broken: their tools emitted a
    ;; DASH op (:struct-patch) while the registry key derived from the underscore
    ;; symbol (:struct_patch), so op-tag threw on every real invocation. Guard it.
    (doseq [op [:sexpr :struct_patch :occurrences
                :create-dirs :delete-if-exists :patch :write]]
      (expect (#{:observation :mutation} (ext/op-tag op))))))
