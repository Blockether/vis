(ns com.blockether.vis-loop.loop.code-block-doc-test
  "Optional `:doc` field on code blocks. Tests cover three things:

   1. `extract-defining-name` recognizes def / defn / defn- / defmacro
      shapes and pulls the var name; rejects anything else.
   2. End-to-end: a `:code-block` with `:expr` `(def x 1)` and `:doc \"…\"`
      attaches the docstring to the var's :doc meta after eval.
   3. The new docstring shows up in render-data-form output (the
      var-index path that powers `<var_index>`)."
  (:require
   [clojure.string :as str]
   [com.blockether.vis-loop.loop.runtime.conversation.environment.core :as env-core]
   [com.blockether.vis-loop.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

;; -----------------------------------------------------------------------------
;; extract-defining-name
;; -----------------------------------------------------------------------------

(defdescribe extract-defining-name-test
  (it "extracts var name from (def NAME val)"
    (expect (= 'foo (iterate/extract-defining-name "(def foo 42)"))))

  (it "extracts var name from (defn NAME [args] body)"
    (expect (= 'my-fn (iterate/extract-defining-name "(defn my-fn [x] (inc x))"))))

  (it "extracts var name from (defn- NAME [args] body)"
    (expect (= 'private-fn (iterate/extract-defining-name "(defn- private-fn [] 1)"))))

  (it "extracts var name from (defmacro NAME [args] body)"
    (expect (= 'my-macro (iterate/extract-defining-name "(defmacro my-macro [x] `(inc ~x))"))))

  (it "returns nil for non-def expressions"
    (expect (nil? (iterate/extract-defining-name "(+ 1 2)")))
    (expect (nil? (iterate/extract-defining-name "(println :hi)")))
    (expect (nil? (iterate/extract-defining-name "42"))))

  (it "returns nil for multi-form code blocks"
    ;; A block with two top-level forms shouldn't be doc-attached
    ;; ambiguously — only single-form (def…) shapes qualify.
    (expect (nil? (iterate/extract-defining-name "(def a 1) (def b 2)"))))

  (it "returns nil for parse errors"
    (expect (nil? (iterate/extract-defining-name "(def foo")))
    (expect (nil? (iterate/extract-defining-name "this is not clojure")))))

;; -----------------------------------------------------------------------------
;; End-to-end via execute-code (the private helper) — round-trip through SCI
;; -----------------------------------------------------------------------------

(defn- fresh-environment []
  (env-core/create-sci-context nil))

(defn- def-doc [{:keys [sci-ctx]} sym]
  (let [doc-form (str "(:doc (meta (resolve '" sym ")))")]
    (:val (sci/eval-string+ sci-ctx doc-form
            {:ns (sci/find-ns sci-ctx 'sandbox)}))))

(defn- exec [environment expression doc]
  ;; execute-code is private; reach via the var directly so the test
  ;; doesn't depend on a public re-export.
  (let [execute-code-var (resolve 'com.blockether.vis-loop.loop.runtime.conversation.environment.query.iteration.core/execute-code)]
    (apply (deref execute-code-var) environment expression
      [:doc doc])))

(defdescribe doc-attach-test
  (it "attaches :doc meta to the var named in (def NAME val)"
    (let [environment (fresh-environment)]
      (exec environment "(def width 1024)" "Pixel width of the canvas.")
      (expect (= "Pixel width of the canvas." (def-doc environment 'width)))))

  (it "attaches :doc meta to the var named in (defn NAME [args] body)"
    (let [environment (fresh-environment)]
      (exec environment "(defn double-it [x] (* 2 x))" "Doubles its input.")
      (expect (= "Doubles its input." (def-doc environment 'double-it)))))

  (it "no-op when :doc is blank or nil"
    (let [environment (fresh-environment)]
      (exec environment "(def x 1)" nil)
      (expect (nil? (def-doc environment 'x)))
      (exec environment "(def y 2)" "")
      (expect (nil? (def-doc environment 'y)))
      (exec environment "(def z 3)" "   ")
      (expect (nil? (def-doc environment 'z)))))

  (it "no-op when :expr is not a def-shape"
    ;; Doc was supplied but expr does nothing var-creating; the eval still
    ;; succeeds, the doc is just dropped.
    (let [environment (fresh-environment)
          result (exec environment "(+ 1 2)" "An addition, surely.")]
      (expect (= 3 (:result result)))))

  (it "doc does not leak into siblings — only the targeted var receives it"
    (let [environment (fresh-environment)]
      (exec environment "(def alpha 1)" "First var.")
      (exec environment "(def beta 2)" nil)
      (expect (= "First var." (def-doc environment 'alpha)))
      (expect (nil? (def-doc environment 'beta))))))

;; -----------------------------------------------------------------------------
;; Render path — `<var_index>` shows the docstring for data vars too
;; -----------------------------------------------------------------------------

(defdescribe render-with-doc-test
  (it "render-data-form embeds first docstring line for documented data vars"
    (let [environment (fresh-environment)]
      (exec environment "(def width 1024)" "Pixel width of the canvas.\nSecond line ignored.")
      (let [sandbox (get-in @(:env (:sci-ctx environment)) [:namespaces 'sandbox])
            initial (:initial-ns-keys environment)
            out (env-core/build-var-index (:sci-ctx environment) initial sandbox nil nil nil)]
        (expect (re-find #"\(def width \"Pixel width of the canvas\.\" 1024\)" out))
        (expect (not (re-find #"Second line" out))))))

  (it "render-fn-form embeds first docstring line for documented fns"
    (let [environment (fresh-environment)]
      (exec environment "(defn doubler [x] (* 2 x))" "Doubles its argument.")
      (let [sandbox (get-in @(:env (:sci-ctx environment)) [:namespaces 'sandbox])
            initial (:initial-ns-keys environment)
            out (env-core/build-var-index (:sci-ctx environment) initial sandbox nil nil nil)]
        (expect (re-find #"\(defn doubler \[x\] \"Doubles its argument\." out))))))

;; -----------------------------------------------------------------------------
;; safe-pr-str — bound-then-format, never format-then-bound. The whole
;; reason this helper exists is to keep `pr-str` from materializing
;; unbounded user/model data into the JVM heap before truncation.
;; -----------------------------------------------------------------------------

(defdescribe safe-pr-str-test
  (it "caps element count via *print-length*"
    (let [v   (vec (range 200))
          out (iterate/safe-pr-str v {:print-length 5 :max-chars 1000})]
      ;; First 5 elements rendered, rest collapsed to `...` per Clojure's
      ;; *print-length* convention.
      (expect (re-find #"\[0 1 2 3 4 \.\.\.\]" out))))

  (it "caps nesting via *print-level*"
    (let [deep {:a {:b {:c {:d {:e :leaf}}}}}
          out  (iterate/safe-pr-str deep {:print-level 2 :max-chars 1000})]
      ;; At depth 2 Clojure replaces deeper structure with `#`.
      (expect (re-find #"#" out))))

  (it "caps the final char count and appends a clip marker"
    (let [s   (apply str (repeat 5000 "a"))
          out (iterate/safe-pr-str s {:max-chars 100 :print-length 1000 :print-level 10})]
      (expect (<= (count out) 200))                  ;; bounded prefix + suffix
      (expect (re-find #" …<\+\d+ chars>$" out))))

  (it "does not clip when input fits within max-chars"
    (let [out (iterate/safe-pr-str {:hello "world"} {:max-chars 1000})]
      (expect (= "{:hello \"world\"}" out))
      (expect (not (re-find #"…" out)))))

  (it "never materializes more than print-length elements during pr"
    ;; If pr-str were applied to the full value first, this test would
    ;; OOM or stall on a billion-element lazy seq. With *print-length*
    ;; bound, pr stops after N elements and returns instantly.
    (let [billion (range 1000000000)
          out     (iterate/safe-pr-str billion {:print-length 3 :max-chars 200})]
      (expect (re-find #"\(0 1 2 \.\.\.\)" out)))))
