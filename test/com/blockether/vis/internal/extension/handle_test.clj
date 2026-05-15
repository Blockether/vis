(ns com.blockether.vis.internal.extension.handle-test
  (:require
   [clojure.string :as string]
   [com.blockether.vis.internal.extension.handle :as handle]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (com.blockether.vis.internal.extension.handle CatHandle)))

(defn- fresh-cat
  "Build a CatHandle from `lines`, isolating the store fixture per test."
  [path lines]
  (handle/clear-store!)
  (handle/make-cat {:path path :lines lines}))

;; -----------------------------------------------------------------------------
;; Construction + identity
;; -----------------------------------------------------------------------------

(defdescribe handle-make-cat-test
  (it "make-cat returns a CatHandle with info map populated"
    (let [h (fresh-cat "x" ["alpha" "beta" "gamma"])
          i (:info h)]
      (expect (instance? CatHandle h))
      (expect (= "x" (:path i)))
      (expect (= 3 (:line-count i)))
      (expect (= "alpha" (:first-line i)))
      (expect (= "gamma" (:last-line i)))
      (expect (string? (:sha i)))
      (expect (= 8 (count (:sha i))))
      (expect (string/starts-with? (:store-key h) "h_"))))
  (it "make-cat with empty lines stores an empty payload and nil sha"
    (let [h (fresh-cat "empty" [])]
      (expect (= 0 (:line-count (:info h))))
      (expect (nil? (:sha (:info h))))
      (expect (= [] @h)))))

(defdescribe handle-protocol-test
  (it "kind returns the kind tag"
    (let [h (fresh-cat "x" ["a"])]
      (expect (= :v.cat (handle/kind h)))))
  (it "summary contains :kind, :path, :line-count, :views"
    (let [h (fresh-cat "x" ["a" "b"])
          s (handle/summary h)]
      (expect (= :v.cat (:kind s)))
      (expect (= "x" (:path s)))
      (expect (= 2 (:line-count s)))
      (expect (vector? (:views s)))
      (expect (every? (set (:views s)) [:peek :lines :at]))))
  (it "handle? recognises records implementing PHandle and rejects non-handles"
    (let [h (fresh-cat "x" ["a"])]
      (expect (handle/handle? h))
      (expect (not (handle/handle? {:kind :v.cat})))
      (expect (not (handle/handle? nil)))
      (expect (not (handle/handle? 42)))
      (expect (not (handle/handle? "string")))
      (expect (not (handle/handle? [1 2 3]))))))

(defdescribe handle-protocol-fallback-test
  (it "kind on non-handles returns :not-a-handle"
    (expect (= :not-a-handle (handle/kind nil)))
    (expect (= :not-a-handle (handle/kind 42)))
    (expect (= :not-a-handle (handle/kind "string")))
    (expect (= :not-a-handle (handle/kind {:some :map}))))
  (it "summary on non-handles returns a structured :not-a-handle map with a hint"
    (let [s (handle/summary 42)]
      (expect (= :not-a-handle (:kind s)))
      (expect (= 42 (:value s)))
      (expect (string? (:hint s)))
      (expect (string/includes? (:hint s) "Handle"))))
  (it "summary on nil reports the nil value clearly"
    (let [s (handle/summary nil)]
      (expect (= :not-a-handle (:kind s)))
      (expect (nil? (:value s)))
      (expect (string? (:hint s)))))
  (it "view on non-handles returns :not-a-handle with the op and args echoed back"
    (let [s0 (handle/view 42 :peek)
          s1 (handle/view 42 :at 3)
          s2 (handle/view 42 :lines 0 10)]
      (expect (= :not-a-handle (:kind s0)))
      (expect (= :peek (:op s0)))
      (expect (= :not-a-handle (:kind s1)))
      (expect (= :at (:op s1)))
      (expect (= [3] (:args s1)))
      (expect (= :not-a-handle (:kind s2)))
      (expect (= :lines (:op s2)))
      (expect (= [0 10] (:args s2)))))
  (it "calls do not throw — fallback is structured data, not an exception"
    (expect (map? (handle/view "a string" :peek)))
    (expect (map? (handle/view nil :anything)))
    (expect (map? (handle/view {} :whatever 1 2)))))

;; -----------------------------------------------------------------------------
;; Deref
;; -----------------------------------------------------------------------------

(defdescribe handle-deref-test
  (it "@h returns the original payload vector"
    (let [payload ["alpha" "beta" "gamma"]
          h       (fresh-cat "x" payload)]
      (expect (= payload @h))
      (expect (= payload (deref h)))))
  (it "deref throws :vis.handle/evicted when the store entry is gone"
    (let [h (fresh-cat "x" ["payload"])]
      (handle/clear-store!)
      (try
        (deref h)
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (= :vis.handle/evicted (:type (ex-data e)))))))))

;; -----------------------------------------------------------------------------
;; View ops
;; -----------------------------------------------------------------------------

(defdescribe handle-view-cat-test
  (it ":lines op returns a bounded subvec"
    (let [h (fresh-cat "x" ["l0" "l1" "l2" "l3" "l4"])]
      (expect (= ["l1" "l2"] (handle/view h :lines 1 3)))
      (expect (= ["l0" "l1" "l2" "l3" "l4"] (handle/view h :lines 0 100)))
      (expect (= [] (handle/view h :lines 10 20)))))
  (it ":at op returns the 1-based line"
    (let [h (fresh-cat "x" ["l0" "l1" "l2"])]
      (expect (= "l0" (handle/view h :at 1)))
      (expect (= "l2" (handle/view h :at 3)))
      (expect (nil? (handle/view h :at 99)))))
  (it ":peek op returns the first window"
    (let [h (fresh-cat "x" (mapv #(str "line-" %) (range 100)))]
      (expect (= 50 (count (handle/view h :peek))))
      (expect (= "line-0" (first (handle/view h :peek))))
      (expect (= "line-49" (last (handle/view h :peek))))))
  (it "unsupported view op throws :vis.handle/unsupported-view"
    (let [h (fresh-cat "x" ["a"])]
      (try
        (handle/view h :unknown-op)
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (= :vis.handle/unsupported-view (:type (ex-data e)))))))))

;; -----------------------------------------------------------------------------
;; print-method
;; -----------------------------------------------------------------------------

(defdescribe handle-print-method-test
  (it "renders as a single-line #vis/handle form with summary keys"
    (let [h (fresh-cat "x" ["alpha"])
          s (pr-str h)]
      (expect (string/starts-with? s "#vis/handle "))
      (expect (string/includes? s ":kind :v.cat"))
      (expect (string/includes? s ":line-count 1"))
      (expect (string/includes? s ":path \"x\""))
      (expect (string/includes? s ":views"))
      (expect (not (string/includes? s "\n")))))
  (it "never inlines payload bytes outside the first/last line preview"
    ;; first-line / last-line are intentional in summary. The leak
    ;; guarantee covers payload content NOT exposed there — the middle
    ;; of a multi-line file must remain hidden until the model derefs.
    (let [secret "do-not-leak-into-print"
          h      (fresh-cat "y" ["header-line" secret "footer-line"])
          s      (pr-str h)]
      (expect (not (string/includes? s secret)))
      (expect (string/includes? s "header-line"))
      (expect (string/includes? s "footer-line")))))

;; -----------------------------------------------------------------------------
;; Store eviction + diagnostics
;; -----------------------------------------------------------------------------

(defdescribe handle-store-eviction-test
  (it "evicts oldest entries when total bytes exceed MAX_STORE_BYTES"
    (handle/clear-store!)
    (let [chunk (vec (repeat 1 (apply str (repeat (* 8 1024 1024) "a"))))
          h1    (handle/make-cat {:path "p1" :lines chunk})
          _h2   (handle/make-cat {:path "p2" :lines chunk})
          h3    (handle/make-cat {:path "p3" :lines chunk})]
      (expect (= chunk @h3))
      (try
        (deref h1)
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (= :vis.handle/evicted (:type (ex-data e))))))))
  (it "store-stats reports current entry count and total bytes"
    (handle/clear-store!)
    (handle/make-cat {:path "a" :lines ["a" "b"]})
    (handle/make-cat {:path "b" :lines ["c" "d"]})
    (let [{:keys [entry-count total-bytes max-bytes]} (handle/store-stats)]
      (expect (= 2 entry-count))
      (expect (pos? total-bytes))
      (expect (= handle/MAX_STORE_BYTES max-bytes)))))
