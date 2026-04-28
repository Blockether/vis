(ns com.blockether.vis-loop.loop.var-index-render-test
  "Type-aware var-index rendering. Each test feeds a synthetic sandbox
   directly into `build-var-index` and asserts the rendered string
   exposes the expected literal / schema preview, without touching the
   live SCI environment or the DB var-registry."
  (:require
   [clojure.string :as str]
   [com.blockether.vis-loop.loop.runtime.conversation.environment.core :as env-core]
   [lazytest.core :refer [defdescribe it expect]]))

;; -----------------------------------------------------------------------------
;; Helpers — build a minimal sandbox map without a real SCI context.
;; `build-var-index` accepts an explicit sandbox-map override (the third
;; arity), so we can hand it any `{sym → val}` map and skip SCI entirely.
;; -----------------------------------------------------------------------------

(defn- index
  "Render the index for the given sandbox map. `initial-ns-keys` defaults
   to empty, so every key in the sandbox is treated as a user var."
  ([sandbox] (index sandbox #{}))
  ([sandbox initial-ns-keys]
   (env-core/build-var-index nil initial-ns-keys sandbox nil nil nil)))

;; -----------------------------------------------------------------------------
;; Inline scalars
;; -----------------------------------------------------------------------------

(defdescribe scalar-rendering-test
  (it "renders nil"
    (expect (re-find #"\(def x nil\)" (index {'x nil}))))

  (it "renders booleans inline"
    (expect (re-find #"\(def y true\)" (index {'y true}))))

  (it "renders integers inline"
    (expect (re-find #"\(def n 42\)" (index {'n 42}))))

  (it "renders keywords inline"
    (expect (re-find #"\(def k :foo\)" (index {'k :foo})))))

;; -----------------------------------------------------------------------------
;; String values
;; -----------------------------------------------------------------------------

(defdescribe string-rendering-test
  (it "inlines strings ≤200 chars"
    (let [out (index {'s "hello world"})]
      (expect (re-find #"\(def s \"hello world\"\)" out))))

  (it "previews strings >200 chars with size + 80-char head"
    (let [big (apply str (repeat 500 "a"))
          out (index {'big big})]
      (expect (re-find #":string-size 500" out))
      (expect (re-find #":head \"a{80}\"" out))
      (expect (not (re-find #"a{81}" out)))))

  (it "stats comment carries the live scope"
    (let [out (index {'s "hi"})]
      (expect (re-find #";; v=1 scope=live n=2" out)))))

;; -----------------------------------------------------------------------------
;; Maps
;; -----------------------------------------------------------------------------

(defdescribe map-rendering-test
  (it "inlines maps with ≤8 keys as {:keys [...]}"
    (let [m   {:a 1 :b 2 :c 3}
          out (index {'m m})]
      (expect (re-find #"\{:keys \[" out))
      ;; clojure maps don't guarantee order, so just verify membership.
      (doseq [k [:a :b :c]]
        (expect (re-find (re-pattern (str ":" (name k))) out)))))

  (it "samples maps with >8 keys via {:n :keys-sample}"
    (let [m   (zipmap (map #(keyword (str "k" %)) (range 12)) (range 12))
          out (index {'m m})]
      (expect (re-find #":n 12" out))
      (expect (re-find #":keys-sample" out)))))

;; -----------------------------------------------------------------------------
;; Vectors / sets / sequences
;; -----------------------------------------------------------------------------

(defdescribe sequential-rendering-test
  (it "inlines vectors with ≤5 elements"
    (let [out (index {'v [1 2 3 4 5]})]
      (expect (re-find #"\(def v \[1 2 3 4 5\]\)" out))))

  (it "samples vectors with >5 elements via {:n :head}"
    (let [v   (vec (range 100))
          out (index {'v v})]
      (expect (re-find #":n 100" out))
      (expect (re-find #":head \[0 1 2\]" out))))

  (it "inlines small sets"
    (let [out (index {'s #{1 2}})]
      ;; sets render through `(vec val)` for inline → may be ordered as
      ;; vector seq; assert the body is a vector literal of size 2.
      (expect (re-find #"\(def s \[" out)))))

;; -----------------------------------------------------------------------------
;; Functions
;; -----------------------------------------------------------------------------

(defdescribe fn-rendering-test
  (it "renders fn with arglists when present"
    (let [f (with-meta (fn []) {:arglists '([x] [x opts])})
          out (index {'my-fn f})]
      (expect (re-find #"\(defn my-fn \[x\]" out))
      (expect (re-find #"\[x opts\]" out))))

  (it "renders fn with [& args] when no arglists meta"
    (let [out (index {'opaque (fn [])})]
      (expect (re-find #"\(defn opaque \[& args\]" out))))

  (it "embeds first docstring line when present"
    (let [f (with-meta (fn []) {:arglists '([x])
                                :doc "First line of the docstring.\nSecond line."})
          out (index {'documented f})]
      (expect (re-find #"First line of the docstring\." out))
      (expect (not (re-find #"Second line\." out))))))

;; -----------------------------------------------------------------------------
;; Stats comment shape — `;; v=N scope=...`
;; -----------------------------------------------------------------------------

(defdescribe stats-comment-test
  (it "renders scope=live for sandbox-bound vars"
    (let [out (index {'x 42})]
      (expect (re-find #";; v=1 scope=live" out))))

  (it "drops `^{...}` reader-macro metadata entirely"
    ;; The old format injected `^{:v 3 :s :l :t :map :n 12}` onto the
    ;; symbol — invalid Clojure that confused parser priors. The new
    ;; format puts stats in a comment line and emits a real `(def …)`.
    (let [out (index {'foo {:a 1}})]
      (expect (not (re-find #"\^\{:v" out)))
      (expect (not (re-find #"\^\{:s" out))))))

;; -----------------------------------------------------------------------------
;; SYSTEM vars are excluded
;; -----------------------------------------------------------------------------

(defdescribe system-var-exclusion-test
  (it "does not render QUERY/ANSWER/REASONING in the live block"
    (let [out (index {'QUERY     "user query"
                      'ANSWER    "prior answer"
                      'REASONING "thinking"
                      'user-var  42})]
      (expect (re-find #"\(def user-var 42\)" out))
      (expect (not (re-find #"\(def QUERY" out)))
      (expect (not (re-find #"\(def ANSWER" out)))
      (expect (not (re-find #"\(def REASONING" out)))))

  (it "does not render initial-ns-keys (tools / helpers)"
    (let [out (index {'read-file (fn []) 'user-var 42}
                #{'read-file})]
      (expect (re-find #"\(def user-var 42\)" out))
      (expect (not (re-find #"read-file" out))))))

;; -----------------------------------------------------------------------------
;; Empty-state path
;; -----------------------------------------------------------------------------

(defdescribe empty-index-test
  (it "returns nil when sandbox has no user vars"
    (expect (nil? (index {} #{}))))

  (it "returns nil when sandbox has only initial-ns-keys"
    (expect (nil? (index {'read-file (fn [])} #{'read-file}))))

  (it "returns nil when sandbox has only SYSTEM vars"
    (expect (nil? (index {'QUERY "x" 'ANSWER "y" 'REASONING "z"})))))

;; -----------------------------------------------------------------------------
;; Sort order — newest-touched first by recency-of (no DB → all tied at
;; Long/MAX_VALUE → falls back to alphabetical ordering by sym name).
;; -----------------------------------------------------------------------------

(defdescribe sort-order-test
  (it "tied recency falls back to alphabetical ordering"
    ;; All vars share the same recency (no DB var-registry passed), so
    ;; the secondary sort key — `(str sym)` — kicks in.
    (let [out (index {'zoo 1 'apple 2 'mango 3})
          apple-pos (str/index-of out "apple")
          mango-pos (str/index-of out "mango")
          zoo-pos   (str/index-of out "zoo")]
      (expect (< apple-pos mango-pos))
      (expect (< mango-pos zoo-pos)))))
