(ns com.blockether.vis.internal.foundation.editing.zipper-edge-test
  "Edge cases for the tree-sitter zipper that the model WILL hit:
     1. UTF-8 / multibyte  — byte-range splicing across emoji/CJK/accents.
     2. insert_before/after — position correctness, distinct from replace.
     3. path-move arithmetic — down/up/next/prev/{child} boundaries (pure fns).
     4. large files         — what cat truncated; the zipper must not.
     5. empty / already-broken inputs — documented behavior."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.zipper :as z]
            [com.blockether.vis.internal.foundation.editing.core :as editing]
            [lazytest.core :refer [defdescribe it expect]]))

(defn- all-paths [lang src]
  (letfn [(walk [p] (let [info (z/inspect lang src p)]
                      (cons p (mapcat #(walk (conj p %)) (range (:named-child-count info))))))]
    (walk [])))

(defn- roundtrip-every-node? [lang src]
  (every? (fn [p] (let [n (z/inspect lang src p)
                        r (z/edit lang src p :replace (:text n))]
                    (and (:ok? r) (= src (:new-source r)))))
    (all-paths lang src)))

(defn- child-idx [lang src needle]
  (let [root (z/inspect lang src [])]
    (some (fn [{:keys [idx]}]
            (when (str/includes? (:text (z/inspect lang src [idx])) needle) idx))
      (:children root))))

;; ── 1. UTF-8 / multibyte ────────────────────────────────────────────────────
;; Each carries multibyte (emoji 🚀 / CJK 日本語 / accent café ☕) that sits
;; BEFORE the edit target, so the target's byte offsets only line up if the
;; engine counts UTF-8 bytes — not UTF-16 chars.
(def ^:private utf8-cases
  [{:ext "py"  :lang "python"
    :src "# 日本語コメント café ☕\ndef add(a, b):\n    return a + b\n"
    :needle "add" :code "def plus(a, b):\n    return a * b"
    :want "def plus" :keep "日本語コメント café ☕"}
   {:ext "py"  :lang "python"     ; multibyte IDENTIFIER (python allows it)
    :src "def café(x):\n    return x\n"
    :needle "café" :code "def café(x):\n    return x + 1"
    :want "return x + 1" :keep "café"}
   {:ext "clj" :lang "clojure"    ; emoji + CJK inside a string literal
    :src "(def msg \"hi 🚀 café 日本語\")\n(defn add [a b] (+ a b))\n"
    :needle "add" :code "(defn plus [a b] (* a b))"
    :want "plus" :keep "🚀 café 日本語"}
   {:ext "rs"  :lang "rust"
    :src "// café ☕ 日本語 🚀\nfn add(a: i32) -> i32 { a }\n"
    :needle "add" :code "fn plus(a: i32) -> i32 { a + 1 }"
    :want "plus" :keep "café ☕ 日本語 🚀"}
   {:ext "js"  :lang "javascript"
    :src "const e = \"🎉 日本語\";\n// café ☕\nfunction add(a) { return a; }\n"
    :needle "add" :code "function plus(a) { return a; }"
    :want "plus" :keep "🎉 日本語"}])

(defdescribe utf8-multibyte-test
  (doseq [{:keys [ext lang src needle code want keep]} utf8-cases]
    (it (str ext " splices correctly past multibyte content")
      (let [i (child-idx lang src needle)
            r (z/edit lang src [i] :replace code)]
        (expect (some? i))
        (expect (:ok? r))
        (expect (str/includes? (:new-source r) want))
        ;; the multibyte content (before the target) survives byte-exact
        (expect (str/includes? (:new-source r) keep))
        (expect (not (:has-error? (z/inspect lang (:new-source r) []))))))
    (it (str ext " round-trips EVERY node byte-for-byte with multibyte present")
      (expect (true? (roundtrip-every-node? lang src))))
    (it (str ext " node text reads multibyte uncorrupted (no mojibake)")
      (let [i (child-idx lang src needle)
            t (:text (z/inspect lang src [i]))]
        ;; the located node's own text is a real substring of the source
        (expect (str/includes? src t))))))

;; ── 2. insert_before / insert_after ─────────────────────────────────────────
(defdescribe insert-ops-test
  (it "insert_before places a node just before the target (clj)"
    (let [src "(defn a [] 1)\n(defn b [] 2)\n"
          ib  (child-idx "clojure" src "defn b")
          r   (z/edit "clojure" src [ib] :insert-before "(def M 0)\n")]
      (expect (:ok? r))
      (expect (str/includes? (:new-source r) "(def M 0)\n(defn b"))
      (expect (str/includes? (:new-source r) "(defn a [] 1)"))   ; untouched
      (expect (not (:has-error? (z/inspect "clojure" (:new-source r) []))))))
  (it "insert_after places a node just after the target (clj)"
    (let [src "(defn a [] 1)\n(defn b [] 2)\n"
          r   (z/edit "clojure" src [0] :insert-after "\n(def N 9)")]
      (expect (:ok? r))
      (expect (str/includes? (:new-source r) "(defn a [] 1)\n(def N 9)"))
      (expect (not (:has-error? (z/inspect "clojure" (:new-source r) []))))))
  (it "insert works language-neutrally (python)"
    (let [src "def a():\n    return 1\n\ndef b():\n    return 2\n"
          ib  (child-idx "python" src "def b")
          r   (z/edit "python" src [ib] :insert-before "M = 0\n\n")]
      (expect (:ok? r))
      (expect (str/includes? (:new-source r) "M = 0"))
      (expect (not (:has-error? (z/inspect "python" (:new-source r) []))))))
  (it "an insert that NEWLY breaks syntax is refused"
    (let [r (z/edit "clojure" "(defn a [] 1)\n" [0] :insert-before "(((")]
      (expect (= :syntax-broken (get-in r [:error :reason]))))))

;; ── 3. path-move arithmetic (pure; the cursor the model drives) ──────────────
(defdescribe path-move-arithmetic-test
  (it "down/up/next/prev/{child} resolve at and past boundaries"
    (let [rp @#'editing/zip-resolve-path]
      (expect (= [2 0] (rp [2] ["down"])))
      (expect (= [3]   (rp [2] ["next"])))
      (expect (= [1]   (rp [2] ["prev"])))
      (expect (= [0]   (rp [0] ["prev"])))          ; clamped at 0, never negative
      (expect (= [2]   (rp [2 1] ["up"])))
      (expect (= []    (rp [] ["up"])))             ; up at root is a no-op
      (expect (= []    (rp [] ["next"])))           ; next with no current index: no-op
      (expect (= [2 5] (rp [2] [{:child 5}])))
      (expect (= [1 2] (rp [1] ["down" "next" "next"])))  ; chained moves
      (expect (= [2]   (rp [2] ["bogus"])))         ; unknown move ignored
      (expect (= [4]   (rp [2] ["next" "next"]))))))

;; ── 4. large file — the motivating case cat TRUNCATED ──────────────────────
(defdescribe large-file-test
  (it "parses + edits a 1500-def file with no window truncation"
    (let [src  (apply str (for [i (range 1500)] (str "(defn f" i " [x] (+ x " i "))\n")))
          root (z/inspect "clojure" src [])]
      (expect (:ok? root))
      (expect (= 1500 (:named-child-count root)))
      ;; edit a def NEAR THE END (idx 1400) — well past where cat's 50KB window
      ;; would have chopped (~970 lines)
      (let [node (z/inspect "clojure" src [1400])]
        (expect (str/includes? (:text node) "f1400"))
        (let [r (z/edit "clojure" src [1400] :replace "(defn f1400 [x] (* x 2))")]
          (expect (:ok? r))
          (expect (str/includes? (:new-source r) "(* x 2)"))
          ;; both ends intact — nothing dropped
          (expect (str/includes? (:new-source r) "(defn f0 [x]"))
          (expect (str/includes? (:new-source r) "(defn f1499 [x]"))
          (expect (not (:has-error? (z/inspect "clojure" (:new-source r) [])))))))))

;; ── 5. empty / already-broken inputs ───────────────────────────────────────
(defdescribe edge-input-test
  (it "empty / whitespace-only files inspect cleanly with zero named children"
    (let [r (z/inspect "clojure" "" [])]
      (expect (:ok? r))
      (expect (= 0 (:named-child-count r))))
    (expect (:ok? (z/inspect "clojure" "   \n\n  " []))))
  (it "editing an ALREADY-broken file is allowed — only NEWLY-introduced breakage is refused"
    (let [broken "(defn a [] 1) (oops [\n"           ; valid first form, broken tail
          root   (z/inspect "clojure" broken [])]
      (expect (:has-error? root))
      ;; replace the valid first node; the file stays broken, but because it was
      ;; ALREADY broken the edit is applied (not refused as :syntax-broken)
      (let [r (z/edit "clojure" broken [0] :replace "(defn a [] 2)")]
        (expect (not= :syntax-broken (get-in r [:error :reason])))
        (expect (str/includes? (:new-source r) "(defn a [] 2)"))))))
