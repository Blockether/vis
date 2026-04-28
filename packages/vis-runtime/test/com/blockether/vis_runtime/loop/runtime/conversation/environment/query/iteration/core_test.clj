(ns com.blockether.vis-runtime.loop.runtime.conversation.environment.query.iteration.core-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis-runtime.loop.runtime.conversation.environment.core :as env-core]
   [com.blockether.vis-runtime.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]
   [com.blockether.vis-runtime.loop.core :as loop]))

;; ─── from code_block_doc_test.clj ───

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
  (let [execute-code-var (resolve 'com.blockether.vis-runtime.loop.runtime.conversation.environment.query.iteration.core/execute-code)]
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

;; ─── from auto_forget_test.clj ───

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- sci-var
  "Create a SCI var with optional :doc metadata."
  ([sym val]
   (sci/new-var sym val))
  ([sym val doc]
   (sci/new-var sym val {:doc doc})))

(defn- make-sandbox
  "Build a sandbox-map {symbol -> sci-var} from a seq of
   [sym val] or [sym val doc] triples."
  [entries]
  (into {}
    (map (fn [[sym val & [doc]]]
           [sym (if doc (sci-var sym val doc) (sci-var sym val))]))
    entries))

(defn- make-registry
  "Build a var-registry {symbol -> {:query-id uuid ...}} from a seq of
   [sym query-id] pairs."
  [entries]
  (into {}
    (map (fn [[sym qid]]
           [sym {:query-id qid :value nil :code "" :version 0}]))
    entries))

;; ---------------------------------------------------------------------------
;; auto-forget-candidates (pure)
;; ---------------------------------------------------------------------------

(def q1 (random-uuid))
(def q2 (random-uuid))
(def q3 (random-uuid))
(def q4 (random-uuid))

(defdescribe auto-forget-candidates-test

  (it "🫙 empty sandbox → nothing to forget, move along"
    (expect (= #{} (loop/auto-forget-candidates {} #{} {} #{q1}))))

  (it "🛡️ built-ins are untouchable — hands off initial-ns-keys"
    (let [sandbox   (make-sandbox [['fetch 42]])
          initials  #{'fetch}
          registry  (make-registry [['fetch q1]])
          recent    #{q1}]
      (expect (= #{} (loop/auto-forget-candidates sandbox initials registry recent)))))

  (it "🎧 SYSTEM vars (QUERY/ANSWER/REASONING) are sacred — never forgotten"
    (let [sandbox   (make-sandbox [['QUERY "hello"]])
          registry  (make-registry [['QUERY q1]])
          recent    #{q2}]  ;; q1 is NOT recent — would be forgotten if not in SYSTEM_VAR_NAMES
      (expect (= #{} (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "📝 documented vars survive any purge — docstrings are armor"
    (let [sandbox   (make-sandbox [['important 99 "This var is documented"]])
          registry  (make-registry [['important q1]])
          recent    #{q2}]  ;; q1 is NOT recent
      (expect (= #{} (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🕐 recently-touched vars stay alive within the recency window"
    (let [sandbox   (make-sandbox [['scratch 1]])
          registry  (make-registry [['scratch q2]])
          recent    #{q1 q2 q3}]
      (expect (= #{} (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🗑️ stale undocumented scratch vars get swept without mercy"
    (let [sandbox   (make-sandbox [['scratch 1] ['tmp 2]])
          registry  (make-registry [['scratch q1] ['tmp q1]])
          recent    #{q3 q4}]
      (expect (= #{'scratch 'tmp}
                (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "🎯 full gauntlet: stale→gone, documented→safe, recent→safe, system→safe, builtin→safe"
    (let [sandbox   (make-sandbox [['stale-a 1]
                                   ['stale-b 2]
                                   ['documented 3 "keep me"]
                                   ['recent-var 4]
                                   ['REASONING 5]            ;; SYSTEM_VAR_NAMES — protected
                                   ['builtin 6]])
          initials  #{'builtin}
          registry  (make-registry [['stale-a q1]
                                    ['stale-b q1]
                                    ['documented q1]
                                    ['recent-var q3]
                                    ['REASONING q1]
                                    ['builtin q1]])
          recent    #{q3 q4}]
      (expect (= #{'stale-a 'stale-b}
                (loop/auto-forget-candidates sandbox initials registry recent)))))

  (it "👻 ephemeral vars with no DB footprint are invisible to the janitor"
    (let [sandbox   (make-sandbox [['ephemeral 99]])
          registry  {}
          recent    #{q1}]
      (expect (= #{} (loop/auto-forget-candidates sandbox #{} registry recent)))))

  (it "⚡ a non-registered uppercase var (e.g. CONFIG) gets forgotten like any mortal var"
    ;; SYSTEM_VAR_NAMES is a fixed set #{QUERY ANSWER REASONING};
    ;; user-defined uppercase names (CONFIG, MAX_FOO, ...) are NOT system
    ;; vars and get the normal stale-sweep treatment.
    (let [sandbox   (make-sandbox [['CONFIG 42]])
          registry  (make-registry [['CONFIG q1]])
          recent    #{q2}]
      (expect (= #{'CONFIG} (loop/auto-forget-candidates sandbox #{} registry recent))))))
