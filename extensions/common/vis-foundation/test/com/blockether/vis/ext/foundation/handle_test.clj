(ns com.blockether.vis.ext.foundation.handle-test
  (:require
   [clojure.string :as string]
   [com.blockether.vis.ext.foundation.handle :as handle]
   [lazytest.core :refer [defdescribe expect it]])
  (:import
   (com.blockether.vis.ext.foundation.handle CatHandle)))

(defn- fresh-cat
  "Build a CatHandle from `lines`, isolating the store fixture per test."
  [path lines]
  (handle/clear-store!)
  (handle/make-cat {:path path :lines lines}))

;; -----------------------------------------------------------------------------
;; Construction + identity
;; -----------------------------------------------------------------------------

(defdescribe handle-make-cat-test
  (it "make-cat returns a CatHandle with summary fields populated"
    (let [h (fresh-cat "x" ["alpha" "beta" "gamma"])]
      (expect (instance? CatHandle h))
      (expect (= "x" (:path h)))
      (expect (= 3 (:line-count h)))
      (expect (= "alpha" (:first-line h)))
      (expect (= "gamma" (:last-line h)))
      (expect (string? (:sha h)))
      (expect (= 8 (count (:sha h))))
      (expect (string/starts-with? (:store-key h) "h_"))))
  (it "make-cat with empty lines stores an empty payload and nil sha"
    (let [h (fresh-cat "empty" [])]
      (expect (= 0 (:line-count h)))
      (expect (nil? (:sha h)))
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
  (it "handle? recognises records implementing PHandle"
    (let [h (fresh-cat "x" ["a"])]
      (expect (handle/handle? h))
      (expect (not (handle/handle? {:kind :v.cat})))
      (expect (not (handle/handle? nil))))))

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
