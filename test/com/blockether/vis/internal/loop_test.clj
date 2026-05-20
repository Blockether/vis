(ns com.blockether.vis.internal.loop-test
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.env.sci-patches :as sp]
   [com.blockether.vis.internal.loop :as lp]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]))

(defn- captured-ask-code-opts
  [opts]
  (let [seen (atom nil)]
    (with-redefs-fn {#'lp/get-router (fn [] ::router)
                     #'svar/ask-code! (fn [router opts]
                                        (reset! seen {:router router :opts opts})
                                        {:blocks [] :raw ""})}
      #(lp/ask-code! opts))
    @seen))

(def ^:private provider-error-explanation
  (deref #'lp/provider-error-explanation))

(def ^:private collect-iteration-start-hints
  (deref #'lp/collect-iteration-start-hints))

(def ^:private ask-result->api-usage
  (deref #'lp/ask-result->api-usage))

(def ^:private ask-code-block-observation
  (deref #'lp/ask-code-block-observation))

(def ^:private parse-top-level-forms
  (deref #'lp/parse-top-level-forms))

(def ^:private code-entries-preflight
  (deref #'lp/code-entries-preflight))

(def ^:private empty-code-error-with-observation
  (deref #'lp/empty-code-error-with-observation))

(def ^:private multi-fence-hint     (deref #'lp/multi-fence-hint))
(def ^:private attach-multi-fence-hint (deref #'lp/attach-multi-fence-hint))

(defdescribe multi-fence-hint-test
  (it "is nil for a single-fence entry"
    (expect (nil? (multi-fence-hint {})))
    (expect (nil? (multi-fence-hint {:multi-fence-merged? false :multi-fence-count 1}))))

  (it "names the rule and the count when several fences were merged"
    (let [h (multi-fence-hint {:multi-fence-merged? true :multi-fence-count 5})]
      (expect (string? h))
      (expect (str/includes? h "5"))
      (expect (str/includes? h "```clojure```"))
      (expect (str/includes? h "exactly ONE"))))

  (it "falls back to 'multiple' when the count is missing"
    (let [h (multi-fence-hint {:multi-fence-merged? true})]
      (expect (str/includes? h "multiple")))))

(defdescribe attach-multi-fence-hint-test
  (it "is identity on single-fence entries"
    (let [err {:message "boom"}]
      (expect (= err (attach-multi-fence-hint err {})))
      (expect (= err (attach-multi-fence-hint err {:multi-fence-merged? false})))))

  (it "adds :hint when the entry was a multi-fence merge and no hint existed"
    (let [out (attach-multi-fence-hint {:message "parse error"}
                {:multi-fence-merged? true :multi-fence-count 3})]
      (expect (string? (:hint out)))
      (expect (str/includes? (:hint out) "3"))))

  (it "appends to an existing upstream hint instead of clobbering it"
    (let [out (attach-multi-fence-hint {:message "parse error" :hint "close brace at line 12"}
                {:multi-fence-merged? true :multi-fence-count 2})]
      (expect (str/starts-with? (:hint out) "close brace at line 12"))
      (expect (str/includes? (:hint out) "2"))))

  (it "treats a blank upstream hint as absent"
    (let [out (attach-multi-fence-hint {:message "x" :hint "   "}
                {:multi-fence-merged? true :multi-fence-count 4})]
      (expect (not (str/starts-with? (:hint out) " "))))))

(defdescribe token-usage-normalization-test
  (it "preserves Anthropic cache write tokens from svar token maps"
    (expect (= {:prompt_tokens 112
                :completion_tokens 69
                :completion_tokens_details {:reasoning_tokens 0}
                :prompt_tokens_details {:cached_tokens 0
                                        :cache_creation_tokens 8777}}
              (ask-result->api-usage {:tokens {:input 112
                                               :output 69
                                               :cached 0
                                               :cache-created 8777}})))))

(defdescribe ask-code-block-observation-test
  (it "summarizes svar 0.5.5 fence observations"
    (expect (= {:block-count 1
                :all-block-count 3
                :dropped-block-count 2
                :saw-fence? true
                :malformed? true}
              (ask-code-block-observation
                {:blocks [{:source "(def x 1)" :lang "clojure"}]
                 :all-blocks [{:source "(def x 1)" :lang "clojure"}
                              {:source "console.log(1)" :lang "javascript"}
                              {:source "oops" :lang nil}]
                 :saw-fence? true
                 :malformed? true}))))

  (it "keeps pre-0.5.5 shape compatible"
    (expect (= {:block-count 1
                :all-block-count 1
                :dropped-block-count 0
                :saw-fence? false
                :malformed? false}
              (ask-code-block-observation
                {:blocks [{:source "(def x 1)" :lang "clojure"}]})))))

(defdescribe empty-code-error-with-observation-test
  (it "reports malformed fences before generic no-code"
    (let [msg (empty-code-error-with-observation "generic" {:malformed? true})]
      (expect (str/includes? msg "malformed Markdown code fence"))))

  (it "reports dropped non-clojure fences with counts"
    (let [msg (empty-code-error-with-observation
                "generic"
                {:blocks []
                 :all-blocks [{:source "x" :lang "python"}
                              {:source "y" :lang nil}]
                 :saw-fence? true})]
      (expect (str/includes? msg "none survived Clojure selection"))
      (expect (str/includes? msg "Dropped blocks: 2 of 2"))))

  (it "keeps generic message for fenceless responses"
    (expect (= "generic" (empty-code-error-with-observation "generic" {})))))

(defdescribe iteration-start-hook-test
  (it "collects active :turn.iteration/start hook hints and ignores other phases"
    (let [seen (atom nil)
          ext {:ext/name "test.hints"
               :ext/hooks [{:id :test/title
                            :doc "title"
                            :phase :turn.iteration/start
                            :fn (fn [ctx]
                                  (reset! seen ctx)
                                  {:text "set title" :importance :high})}
                           {:id :test/answer
                            :doc "answer"
                            :phase :turn.answer/validate
                            :fn (fn [_] {:reject true})}
                           {:id :test/bad
                            :doc "bad"
                            :phase :turn.iteration/start
                            :fn (fn [_] {:importance :high})}]}
          ctx {:session-title nil
               :title-refresh? true
               :turn-position 1}
          hints (collect-iteration-start-hints {} [ext] ctx)]
      (expect (= [{:id :test/title
                   :text "set title"
                   :satisfy-with '(satisfy-hint! :test/title)
                   :importance :high}]
                hints))
      (expect (= ctx @seen))))

  (it "filters satisfied hint ids from the next ctx hint set"
    (let [ext {:ext/name "test.hints"
               :ext/hooks [{:id :test/title
                            :doc "title"
                            :phase :turn.iteration/start
                            :fn (fn [_] {:text "set title"})}]}
          env {:satisfied-hints-atom (atom #{:test/title})}]
      (expect (= [] (collect-iteration-start-hints env [ext] {})))))

  (it "satisfy-hint! records keyword ids and returns :vis/silent"
    (let [env (lp/create-environment ::router {:db :memory})]
      (try
        (let [r (sci/eval-string+ (:sci-ctx env)
                  "(satisfy-hint! :test/title)"
                  {:ns (:sandbox-ns env)})]
          (expect (= :vis/silent (:val r)))
          (expect (= #{:test/title} @(:satisfied-hints-atom env))))
        (finally
          (lp/dispose-environment! env)))))

  (it "satisfy-hint! rejects non-keyword ids"
    (let [env (lp/create-environment ::router {:db :memory})]
      (try
        (try
          (sci/eval-string+ (:sci-ctx env) "(satisfy-hint! \"bad\")" {:ns (:sandbox-ns env)})
          (expect false)
          (catch Throwable e
            (expect (str/includes? (ex-message e) "satisfy-hint! requires a keyword hint id"))))
        (finally
          (lp/dispose-environment! env)))))

  (it "set-session-title! is a bare engine symbol, not a v/ tool"
    (let [env (lp/create-environment ::router {:db :memory})]
      (try
        (expect (= :vis/silent
                  (:val (sci/eval-string+ (:sci-ctx env)
                          "(set-session-title! \"Liveness check\")"
                          {:ns (:sandbox-ns env)}))))
        (try
          (sci/eval-string+ (:sci-ctx env)
            "(v/set-session-title! \"Liveness check\")"
            {:ns (:sandbox-ns env)})
          (expect false)
          (catch Throwable e
            (expect (str/includes? (ex-message e)
                      "Unable to resolve symbol: v/set-session-title!"))))
        (finally
          (lp/dispose-environment! env))))))

(defdescribe provider-error-explanation-test
  (it "tells users to re-authenticate or update keys on provider auth failures"
    (let [text (provider-error-explanation
                 {:message "API authentication failed. Check your API key. (Original: Exceptional status code: 401)"
                  :data {:status 401
                         :body "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"Invalid authentication credentials\"}}"}})]
      (expect (str/includes? text "provider rejected credentials"))
      (expect (str/includes? text "Provider message: Invalid authentication credentials"))
      (expect (str/includes? text "NEXT STEP: re-authenticate this provider or update its API key"))
      (expect (str/includes? text "Ctrl+K -> Model / Providers"))
      (expect (str/includes? text "vis providers auth")))))

(defdescribe ask-code-idle-timeout-test
  (it "uses a five-minute ask-code idle timeout by default"
    (expect (= (* 5 60 1000) lp/ASK_CODE_IDLE_TIMEOUT_MS))
    (let [{:keys [router opts]} (captured-ask-code-opts {:lang "clojure" :messages []})]
      (expect (= ::router router))
      (expect (= lp/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts)))
      (expect (not (contains? opts :semantic-timeout-ms)))))

  (it "preserves explicit ask-code idle timeout overrides"
    (expect (= 42 (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms 42})))))
    (expect (contains? (:opts (captured-ask-code-opts {:idle-timeout-ms nil})) :idle-timeout-ms))
    (expect (nil? (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms nil}))))))

  (it "keeps semantic timeout opt-in"
    (expect (nil? lp/ASK_CODE_SEMANTIC_TIMEOUT_MS))
    (let [opts (:opts (captured-ask-code-opts {:semantic-timeout-ms 180000}))]
      (expect (= 180000 (:semantic-timeout-ms opts)))
      (expect (= lp/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts))))))

(defdescribe parse-top-level-forms-test
  (it "keeps quote reader macro attached when streaming top-level forms"
    (let [{:keys [forms parse-error]} (parse-top-level-forms
                                        "(require '[clojure.string :as str])")
          form (:form (first forms))]
      (expect (nil? parse-error))
      (expect (= 1 (count forms)))
      (expect (= '(require (quote [clojure.string :as str])) form))
      (expect (= "(require (quote [clojure.string :as str]))"
                (:source (first forms))))))

  (it "keeps a blank line between merged clojure fences"
    (let [{:keys [code-entries normalized-code]}
          (code-entries-preflight
            1
            [{:lang "clojure" :source "(def a 1)"}
             {:lang "clojure" :source "(def b 2)"}])
          entry (first code-entries)]
      (expect (= "(def a 1)\n\n(def b 2)" normalized-code))
      (expect (= normalized-code (:expr entry)))
      (expect (true? (:multi-fence-merged? entry)))
      (expect (= 2 (:multi-fence-count entry))))))

(defdescribe malformed-direct-answer-repair-test
  (it "auto-repairs delimiter slips in direct answer blocks"
    (let [preflight (var-get #'lp/code-entries-preflight)
          parse-forms (var-get #'lp/parse-top-level-forms)
          src "(done [:ir [:p \"Hotword biasing - add \" [:c \"setHotwordsFile\") \"/\" [:c \"setHotwordsScore\"]]])"
          entry (first (:code-entries (preflight 1 [{:source src :lang "clojure"}])))
          parsed (parse-forms (:expr entry))]
      (expect (nil? (:repaired? entry)))
      (expect (string? (:repaired-source parsed)))
      (expect (nil? (:parse-error parsed)))
      (expect (true? (:vis/structurally-silent? entry)))
      (expect (= src (:expr entry)))
      (expect (str/includes? (:repaired-source parsed) "\"setHotwordsFile\" \"/\""))))

  (it "auto-repairs stray close delimiters before eval"
    (let [parsed ((var-get #'lp/parse-top-level-forms) "(def x 1))")]
      (expect (nil? (:parse-error parsed)))
      (expect (= "(def x 1)" (:repaired-source parsed)))
      (expect (= '(def x 1) (:form (first (:forms parsed)))))))

  (it "executes repaired source instead of failing pre-eval validators"
    (let [env (lp/create-environment ::router {:db :memory})]
      (try
        (let [result ((var-get #'lp/execute-code) env "(def x 1))")]
          (expect (nil? (:error result)))
          (expect (true? (:repaired? result)))
          (expect (= "(def x 1)" (:repaired-source result)))
          (expect (= 1 (:val (sci/eval-string+ (:sci-ctx env) "x" {:ns (:sandbox-ns env)})))))
        (finally
          (lp/dispose-environment! env)))))

  (it "does not rewrite malformed non-answer code when quotes are unsafe"
    (let [preflight (var-get #'lp/code-entries-preflight)
          parse-forms (var-get #'lp/parse-top-level-forms)
          src "(println \"a\" broken \"b)"
          entry (first (:code-entries (preflight 1 [{:source src :lang "clojure"}])))
          parsed (parse-forms (:expr entry))]
      (expect (nil? (:repaired? entry)))
      (expect (nil? (:repaired-source parsed)))
      (expect (some? (:parse-error parsed)))
      (expect (= src (:expr entry)))))

  (it "streams fn shorthand and regex literals through parser instead of rejecting them"
    (let [parse-forms (var-get #'lp/parse-top-level-forms)
          regex-src (str "(re-find #" "\"v\" \"voice\")")]
      (expect (nil? (:parse-error (parse-forms "(filter #(= % 1) [1 2])"))))
      (expect (nil? (:parse-error (parse-forms regex-src)))))))

(defdescribe top-level-do-unwrapping-test
  (it "unwraps legacy top-level do before display and eval"
    (let [preflight (var-get #'lp/code-entries-preflight)
          src "(do (set-session-title! \"Triage render noise\") (def x \"doc\" 1))"
          entry (first (:code-entries (preflight 1 [{:source src :lang "clojure"}])))]
      (expect (= "(set-session-title! \"Triage render noise\")\n(def x \"doc\" 1)"
                (:expr entry)))
      (expect (true? (:vis/unwrapped-do? entry)))
      (expect (= [{:kind :title :value "Triage render noise"}
                  {:kind :code :source "(def x \"doc\" 1)"}]
                (:render-segments entry))))))

;; ---------------------------------------------------------------------------
;; def-sink -> vars-snapshot (per-var precise source extraction)
;; ---------------------------------------------------------------------------

(defn- mock-var
  "Stub IDeref so def-sink->vars-snapshot can produce a value without
   needing a live SCI context."
  [v]
  (reify clojure.lang.IDeref (deref [_] v)))

(defdescribe def-sink-vars-snapshot-test
  (it "single top-level def: per-var :code is the def form itself"
    (let [snap (lp/def-sink->vars-snapshot
                 [{:name 'x :var (mock-var 42)}]
                 "(def x \"doc\" 42)"
                 nil)]
      (expect (= 1 (count snap)))
      (expect (= "x" (:name (first snap))))
      (expect (= 42 (:value (first snap))))
      (expect (= "(def x \"doc\" 42)" (:code (first snap))))))

  (it "(do (def a) (def b)): each var carries its own precise source"
    (let [code "(do (def a \"doc\" 1) (def b \"doc\" 2))"
          snap (lp/def-sink->vars-snapshot
                 [{:name 'a :var (mock-var 1)}
                  {:name 'b :var (mock-var 2)}]
                 code
                 nil)
          by-name (into {} (map (juxt :name identity)) snap)]
      (expect (= "(def a \"doc\" 1)" (:code (by-name "a"))))
      (expect (= "(def b \"doc\" 2)" (:code (by-name "b"))))
      ;; Restore guarantee: re-eval of (by-name "a") :code MUST NOT
      ;; mention 'b — otherwise restoring a re-introduces b's side
      ;; effects.
      (expect (not (re-find #"\bb\b" (:code (by-name "a")))))
      (expect (not (re-find #"\ba\b" (:code (by-name "b")))))))

  (it "multi-top-level (no (do …) wrap): each var still carries its own precise source"
    ;; The block has three top-level forms; only the two defs should
    ;; produce snapshot rows, and each row's :code must be the
    ;; matching def form, not the whole block.
    (let [code "(def a \"doc\" 1)\n(def b \"doc\" 2)\n(+ a b)"
          snap (lp/def-sink->vars-snapshot
                 [{:name 'a :var (mock-var 1)}
                  {:name 'b :var (mock-var 2)}]
                 code
                 nil)
          by-name (into {} (map (juxt :name identity)) snap)]
      (expect (= "(def a \"doc\" 1)" (:code (by-name "a"))))
      (expect (= "(def b \"doc\" 2)" (:code (by-name "b"))))
      (expect (not (re-find #"\bb\b" (:code (by-name "a")))))
      (expect (not (re-find #"\ba\b" (:code (by-name "b")))))))

  (it "defn round-trips: precise (defn NAME doc args body) lands as :code"
    (let [code "(do (def base \"doc\" 10)\n     (defn add \"adds base\" [x] (+ x base)))"
          snap (lp/def-sink->vars-snapshot
                 [{:name 'base :var (mock-var 10)}
                  {:name 'add  :var (mock-var (fn [_] :stub))}]
                 code
                 nil)
          by-name (into {} (map (juxt :name identity)) snap)]
      (expect (re-find #"defn add" (:code (by-name "add"))))
      (expect (not (re-find #"defn add" (:code (by-name "base")))))
      (expect (re-find #"\bbase\b" (:code (by-name "base"))))))

  (it "sink entry without a matching parsed form falls back to whole block"
    ;; Simulates a macro-expanded def (e.g. `(s/def ::kw …)` — sink
    ;; captures the var name but parsed source contains no
    ;; `(def kw ...)` shape).
    (let [code "(s/def ::kw int?)"
          snap (lp/def-sink->vars-snapshot
                 [{:name 'kw :var (mock-var :stub)}]
                 code
                 nil)]
      (expect (= code (:code (first snap))))))

  (it "iteration-time-ms attaches to every var when positive"
    (let [snap (lp/def-sink->vars-snapshot
                 [{:name 'x :var (mock-var 1)}
                  {:name 'y :var (mock-var 2)}]
                 "(do (def x \"d\" 1) (def y \"d\" 2))"
                 17)]
      (expect (every? #(= 17 (:time-ms %)) snap))))

  (it "sink entry with nil :var is skipped"
    (let [snap (lp/def-sink->vars-snapshot
                 [{:name 'x :var nil}
                  {:name 'y :var (mock-var 2)}]
                 "(do (def x \"d\" 1) (def y \"d\" 2))"
                 nil)]
      (expect (= ["y"] (mapv :name snap))))))

;; ---------------------------------------------------------------------------
;; END-TO-END function round-trip: eval → sink → snapshot → re-eval → call
;;
;; This is the integration proof that defn round-trips: not only does
;; the persistence machinery record `{:vis/ref :expr}` for fn values,
;; the `:expr` source we store actually rebuilds a working function on
;; re-eval. Every layer covered:
;;
;;   1. SCI eval with `*def-sink-atom*` bound  → sink captures defn
;;   2. `def-sink->vars-snapshot`              → per-var :code
;;   3. FRESH SCI context                      → simulates a restart
;;   4. Re-eval the persisted :code            → fn redefined
;;   5. Call the restored fn                   → same behavior
;; ---------------------------------------------------------------------------

(defn- eval-with-sink
  "Run `code` in `ctx` with a fresh def-sink bound; return the sink contents.
   Mirrors how the engine binds `*def-sink-atom*` around iteration eval."
  [ctx code]
  (let [sink (sp/fresh-sink-atom)]
    (binding [sp/*def-sink-atom* sink]
      (sci/eval-string+ ctx code {:ns (sci/find-ns ctx 'user)}))
    @sink))

(defdescribe defn-round-trip-end-to-end-test
  (it "plain defn: source captured by sink rebuilds a working fn in a fresh SCI context"
    (let [src  "(defn add \"adds two numbers\" [a b] (+ a b))"
          ;; Iteration 1: model evaluates the defn in some SCI context.
          ctx1 (sci/init {:namespaces {'user {}}})
          sink (eval-with-sink ctx1 src)
          snap (lp/def-sink->vars-snapshot sink src nil)
          stored-code (:code (first snap))]
      ;; The snapshot row has the canonical defn source as its :code
      ;; field — this is what `db-store-iteration!` would write into
      ;; definition_state.expression.
      (expect (= 1 (count snap)))
      (expect (= "add" (:name (first snap))))
      (expect (re-find #"\(defn add" stored-code))

      ;; Simulate `vis db reset` / next-process-boot: a brand new SCI
      ;; context with no `add` binding.
      (let [ctx2 (sci/init {:namespaces {'user {}}})
            ;; db-restore-blocks gives us {:expr stored-code :result
            ;; {:vis/ref :expr}}; the caller re-evals :expr to
            ;; reconstitute the fn. Use eval-with-sink so a fresh
            ;; sink absorbs the (def add …) the restore writes.
            _    (eval-with-sink ctx2 stored-code)
            ;; Call the restored fn from inside SCI — if it's a real,
            ;; working function the inner eval returns 7.
            r    (sci/eval-string+ ctx2 "(add 3 4)" {:ns (sci/find-ns ctx2 'user)})]
        (expect (= 7 (:val r))))))

  (it "defn that closes over another (def …): both vars restore in dependency order"
    ;; The classic 'function closes over data' case. Iteration code:
    ;;   (def base "d" 10)
    ;;   (defn add-base "d" [x] (+ x base))
    ;; Two top-level forms; no (do …). After restore, calling
    ;; `(add-base 5)` must return 15 — which requires `base` to be
    ;; restored BEFORE `add-base` (since the fn body refers to it).
    (let [src  "(def base \"d\" 10)\n(defn add-base \"d\" [x] (+ x base))"
          ctx1 (sci/init {:namespaces {'user {}}})
          sink (eval-with-sink ctx1 src)
          snap (lp/def-sink->vars-snapshot sink src nil)
          by-name (into {} (map (juxt :name identity)) snap)]
      (expect (= #{"base" "add-base"} (set (map :name snap))))
      ;; Each row carries its OWN form, not the whole 2-form block.
      (expect (= "(def base \"d\" 10)" (:code (by-name "base"))))
      (expect (re-find #"\(defn add-base" (:code (by-name "add-base"))))
      ;; Restore in dependency order (base first, then add-base — db-
      ;; restore-blocks topo-sorts on definition_dependency; without
      ;; deps it sorts by created_at, but `base` was sink-captured
      ;; first so it lands first).
      (let [ctx2 (sci/init {:namespaces {'user {}}})]
        (doseq [entry snap]
          (eval-with-sink ctx2 (:code entry)))
        (let [r (sci/eval-string+ ctx2 "(add-base 5)" {:ns (sci/find-ns ctx2 'user)})]
          (expect (= 15 (:val r)))))))

  (it "two independent defns in the SAME multi-top-level block: each restores in isolation"
    ;; Critical isolation property: restoring fn A must NOT redefine
    ;; sibling fn B. With per-var precise :code this Just Works; with
    ;; the old whole-block fallback it would not.
    (let [src  "(defn doubler \"d\" [x] (* 2 x))\n(defn tripler \"d\" [x] (* 3 x))"
          ctx1 (sci/init {:namespaces {'user {}}})
          sink (eval-with-sink ctx1 src)
          snap (lp/def-sink->vars-snapshot sink src nil)
          by-name (into {} (map (juxt :name identity)) snap)
          ctx2 (sci/init {:namespaces {'user {}}})]
      ;; Restore ONLY `doubler` into ctx2. `tripler` should remain
      ;; unbound — proving the precise per-var :code does not leak
      ;; the sibling defn into the same eval.
      (eval-with-sink ctx2 (:code (by-name "doubler")))
      (let [r (sci/eval-string+ ctx2 "(doubler 21)" {:ns (sci/find-ns ctx2 'user)})]
        (expect (= 42 (:val r))))
      (try
        (sci/eval-string+ ctx2 "(tripler 1)" {:ns (sci/find-ns ctx2 'user)})
        (expect false)
        (catch Throwable e
          ;; SCI throws on unbound symbol — exact shape varies; the
          ;; important thing is that it threw, meaning the doubler
          ;; re-eval did NOT silently rebind tripler.
          (expect (some? (ex-message e))))))))
