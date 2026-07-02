(ns com.blockether.vis.internal.foundation.editing.zipper-api-test
  "FULL clojure.zip / rewrite-clj cursor vocabulary, exercised LANGUAGE-NEUTRALLY
   (the other zipper suites are mostly Clojure-only). Three layers:
     1. depth-first `next`/`prev` — pre-order traversal + the inverse laws, as
        PROPERTIES over every node of several real grammars.
     2. `find` / `find_kind` search — find-NEXT semantics, across languages.
     3. `append_child` / `prepend_child` + lefts/rights (index/siblings), and the
        same moves driven through the sexpr / struct_patch TOOLS (the boundary)."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.core :as editing]
            [com.blockether.vis.internal.foundation.editing.zipper :as z]
            ;; Side-effecting: registers the editing extension so tool-success's
            ;; op->tag lookup resolves for sexpr / struct_patch.
            [com.blockether.vis.internal.foundation.core]
            [lazytest.core :refer [defdescribe it expect]]))

;; ── shared multi-language corpus (each parses clean, has real nesting) ───────
(def ^:private cases
  [{:lang "clojure"
    :src "(ns x)\n(defn a [p q] (+ p q))\n(defn b [] (if p 1 2))\n"}
   {:lang "python"
    :src "def a(x):\n    if x:\n        return x + 1\n    return 0\n\ndef b():\n    return 2\n"}
   {:lang "javascript"
    :src "function a(x) { if (x) { return x + 1; } return 0; }\nfunction b() { return 2; }\n"}
   {:lang "rust"
    :src "fn a(x: i32) -> i32 { if x > 0 { x + 1 } else { 0 } }\nfn b() -> i32 { 2 }\n"}
   {:lang "go"
    :src "package m\nfunc a(x int) int {\n\tif x > 0 {\n\t\treturn x + 1\n\t}\n\treturn 0\n}\n"}])

(defn- all-paths
  "Pre-order DFS of every named node as named-child-index paths (root first)."
  [lang src]
  (letfn [(walk [p] (let [info (z/inspect lang src p)]
                      (cons p (mapcat #(walk (conj p %))
                                (range (:named-child-count info))))))]
    (walk [])))

(defn- nav [lang src at moves] (z/navigate lang src at moves))
(defn- pathOf [r] (when (:ok? r) (:path r)))

;; ── 1. depth-first next / prev — as PROPERTIES over every node ────────────────
(defdescribe dfs-traversal-test
  (it "`next` reproduces the EXACT pre-order traversal — every node, once, in order"
    (doseq [{:keys [lang src]} cases]
      (let [walk (loop [p [] acc [[]]]
                   (let [r (nav lang src p ["next"])]
                     (if (:ok? r) (recur (:path r) (conj acc (:path r))) acc)))]
        ;; identical to a hand-rolled pre-order DFS of the same tree
        (expect (= (all-paths lang src) walk)
          (str lang ": dfs `next` walk diverged from pre-order")))))

  (it "`prev` is the inverse of `next` for every node (prev∘next = id)"
    (doseq [{:keys [lang src]} cases]
      (doseq [p (all-paths lang src)]
        (when-let [nx (pathOf (nav lang src p ["next"]))]
          (expect (= p (pathOf (nav lang src nx ["prev"])))
            (str lang " @" p ": prev(next) ≠ id"))))))

  (it "`next` is the inverse of `prev` for every node (next∘prev = id)"
    (doseq [{:keys [lang src]} cases]
      (doseq [p (all-paths lang src)]
        (when-let [pv (pathOf (nav lang src p ["prev"]))]
          (expect (= p (pathOf (nav lang src pv ["next"])))
            (str lang " @" p ": next(prev) ≠ id"))))))

  (it "`next` from the LAST node and `prev` from the root FAIL CLOSED"
    (doseq [{:keys [lang src]} cases]
      (let [last-node (last (all-paths lang src))]
        (expect (= :bad-move (get-in (nav lang src last-node ["next"]) [:error :reason]))
          (str lang ": next past the end should fail"))
        (expect (= :bad-move (get-in (nav lang src [] ["prev"]) [:error :reason]))
          (str lang ": prev at root should fail"))))))

;; ── 2. find / find_kind — rewrite-clj search, language-neutral ────────────────
(defdescribe find-search-test
  (it "{find: text} jumps to the next node whose text contains it (every language)"
    (doseq [{:keys [lang src needle]}
            [{:lang "clojure"    :needle "defn b"}
             {:lang "python"     :needle "def b"}
             {:lang "javascript" :needle "function b"}
             {:lang "rust"       :needle "fn b"}
             {:lang "go"         :needle "return x"}]]
      (let [src (:src (first (filter #(= lang (:lang %)) cases)))
            r   (nav lang src [] [{:find needle}])]
        (expect (:ok? r) (str lang ": find " (pr-str needle) " not found"))
        (expect (str/includes? (:text (z/inspect lang src (:path r))) needle)))))

  (it "{find_kind: k} lands on the FIRST node of that kind in DFS order (any grammar)"
    (doseq [{:keys [lang src]} cases]
      (let [paths   (all-paths lang src)
            k       (:kind (z/inspect lang src (second paths)))   ; some non-root kind
            first-k (first (filter #(= k (:kind (z/inspect lang src %))) (rest paths)))
            r       (nav lang src [] [{:find_kind k}])]
        (expect (:ok? r) (str lang ": no node of kind " k))
        (expect (= k (:kind (z/inspect lang src (:path r)))))
        (expect (= first-k (:path r)) (str lang ": find_kind " k " not the first")))))

  (it "find has find-NEXT semantics — it starts AFTER the cursor, never returns it"
    (let [src "(a)\n(b)\n(a)\n"]                ; two identical (a) forms
      ;; from the FIRST (a) at [0], finding \"(a)\" must skip to the SECOND at [2]
      (expect (= [2] (pathOf (nav "clojure" src [0] [{:find "(a)"}]))))))

  (it "a find with no match FAILS CLOSED (not a silent no-op)"
    (doseq [{:keys [lang src]} cases]
      (expect (= :bad-move (get-in (nav lang src [] [{:find "zzz_no_such_token"}])
                             [:error :reason]))))))

;; ── 3. append_child / prepend_child + lefts/rights ───────────────────────────
(defdescribe child-insert-test
  (it "append_child / prepend_child insert at the right edge and stay valid (clj lists)"
    (let [src "(ns x)\n(defn a [p q] (+ p q))\n"]
      (let [r (z/edit "clojure" src [1 3] :append-child " r")]      ; into (+ p q)
        (expect (:ok? r))
        (expect (str/includes? (:new-source r) "(+ p q r)"))
        (expect (not (:has-error? (z/inspect "clojure" (:new-source r) [])))))
      (let [r (z/edit "clojure" src [1 2] :prepend-child "z ")]     ; into [p q]
        (expect (:ok? r))
        (expect (str/includes? (:new-source r) "[z p q]")))
      (let [r (z/edit "clojure" src [] :append-child "\n(defn b [] 2)")] ; into file root
        (expect (:ok? r))
        (expect (str/includes? (:new-source r) "(defn b [] 2)")))
      (expect (= :no-children
                (get-in (z/edit "clojure" "(ns x)\n7\n" [1] :append-child "9")
                  [:error :reason])))))

  (it "append_child is equivalent to insert_after the last child / prepend before first"
    (let [src "(ns x)\n(defn a [p q] (+ p q))\n"]
      (expect (= (:new-source (z/edit "clojure" src [1 3] :append-child " r"))
                (:new-source (z/edit "clojure" src [1 3 2] :insert-after " r"))))
      (expect (= (:new-source (z/edit "clojure" src [1 2] :prepend-child "z "))
                (:new-source (z/edit "clojure" src [1 2 0] :insert-before "z ")))))))

(defdescribe lefts-rights-test
  (it "moves-available reports index/siblings consistently for EVERY node (lefts/rights)"
    (doseq [{:keys [lang src]} cases]
      (doseq [p (all-paths lang src)]
        (let [m (z/moves-available lang src p)]
          (if (seq p)
            (let [i  (peek p)
                  pc (:named-child-count (z/inspect lang src (vec (butlast p))))]
              (expect (= i (:index m)) (str lang " @" p ": index"))
              (expect (= pc (:siblings m)) (str lang " @" p ": siblings"))
              ;; lefts = index, rights = siblings-1-index — and they agree with `can`
              (expect (= (pos? i) (:left m)))
              (expect (= (< (inc i) pc) (:right m))))
            (do (expect (nil? (:index m)))
              (expect (nil? (:siblings m))))))))))

;; ── 4. the same vocabulary through the sexpr / struct_patch TOOLS ─────────────
(defn- write-temp! [name content]
  (fs/create-dirs "target/editing-test")
  (let [rel (str "target/editing-test/" name)]
    (spit (fs/file rel) content)
    rel))

(defdescribe tool-navigation-test
  (it "sexpr resolves a {find:...} move (navigate by content, not index)"
    (let [sexpr @#'editing/sexpr-tool
          path  (write-temp! "apifind.clj"
                  "(ns z)\n(defn target [x] (+ x 1))\n(defn other [] 9)\n")
          r     (:result (sexpr path {:nav [{:find "defn other"}]}))]
      (expect (str/includes? (:text r) "defn other"))
      ;; the `can` map now carries next/prev + index/siblings
      (expect (contains? (:can r) :next))
      (expect (contains? (:can r) :siblings))))

  (it "struct_patch edits the node reached by at + nav (find then replace)"
    (let [struct-patch @#'editing/struct-patch-tool
          path (write-temp! "apiedit.clj"
                 "(ns z)\n(defn target [x] (+ x 1))\n(defn other [] 9)\n")
          ed   (struct-patch :path path :at [] :nav [{:find "defn target"}]
                 :op "replace" :code "(defn target [x] (* x 5))")]
      (expect (:success? ed))
      (expect (str/includes? (slurp (fs/file path)) "(* x 5)"))
      (expect (not (str/includes? (slurp (fs/file path)) "(+ x 1)")))))

  (it "struct_patch append_child via a path adds a child node through the tool"
    (let [struct-patch @#'editing/struct-patch-tool
          path (write-temp! "apiappend.clj" "(ns z)\n(defn g [a b] (+ a b))\n")
          ed   (struct-patch :path path :at [1 3] :op "append_child" :code " c")]
      (expect (:success? ed))
      (expect (str/includes? (slurp (fs/file path)) "(+ a b c)"))))

  (it "a struct_patch nav that can't resolve fails with a nav error (no write)"
    (let [struct-patch @#'editing/struct-patch-tool
          path (write-temp! "apibad.clj" "(ns z)\n(defn g [] 1)\n")]
      (expect (try (struct-patch :path path :at [] :nav [{:find "nope_xyz"}]
                     :op "replace" :code "X")
                false (catch clojure.lang.ExceptionInfo _ true)))
      ;; file untouched
      (expect (str/includes? (slurp (fs/file path)) "(defn g [] 1)")))))

;; ── 5. DEEPLY NESTED edits — a node buried ~9 levels down ────────────────────
(def ^:private deep-src
  (str "(ns deep)\n(defn handler [req]\n  (let [body (get req :body)]\n"
    "    (when (valid? body)\n"
    "      (process {:data {:items [{:id 1 :score (* base 2)}]}}))))\n"))

(defdescribe deep-nesting-test
  (it "find reaches a node buried deep in nested maps/vectors; edit changes ONLY it"
    (let [r (z/navigate "clojure" deep-src [] [{:find "(* base 2)"}])
          p (:path r)]
      (expect (:ok? r))
      (expect (= "(* base 2)" (:text (z/inspect "clojure" deep-src p))))
      (expect (>= (count p) 6) (str "expected a deep path, got " p))   ; genuinely nested
      (let [e (z/edit "clojure" deep-src p :replace "(* base 100)")]
        (expect (:ok? e))
        (expect (str/includes? (:new-source e) "(* base 100)"))
        (expect (not (str/includes? (:new-source e) "(* base 2)")))
        ;; the surrounding structure at every level is untouched
        (expect (str/includes? (:new-source e) "(get req :body)"))
        (expect (str/includes? (:new-source e) "(when (valid? body)"))
        (expect (str/includes? (:new-source e) "{:data {:items"))
        (expect (not (:has-error? (z/inspect "clojure" (:new-source e) [])))))))

  (it "the cursor descends arbitrarily deep by stepping, matching the find path"
    (let [byfind (:path (z/navigate "clojure" deep-src [] [{:find "(* base 2)"}]))
          ;; independently re-derive the path by descending child-by-child,
          ;; always following the child whose text still contains the target
          stepwise (loop [p []]
                     (if (= "(* base 2)" (:text (z/inspect "clojure" deep-src p)))
                       p
                       (let [n (:named-child-count (z/inspect "clojure" deep-src p))
                             c (some (fn [i]
                                       (when (str/includes?
                                               (:text (z/inspect "clojure" deep-src (conj p i)))
                                               "(* base 2)") i))
                                 (range n))]
                         (if c (recur (conj p c)) p))))]
      (expect (= byfind stepwise))
      (expect (>= (count stepwise) 6))
      ;; every ancestor prefix is itself a valid, inspectable node (no gaps)
      (expect (every? #(:ok? (z/inspect "clojure" deep-src (subvec stepwise 0 %)))
                (range (inc (count stepwise))))))))
