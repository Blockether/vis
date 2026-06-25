(ns com.blockether.vis.internal.foundation.editing.zipper-test
  "Language-neutral structural zipper over tree-sitter: parse → navigate by
   named-child path → splice-edit with syntax refusal."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.zipper :as zip]
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
