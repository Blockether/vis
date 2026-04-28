(ns com.blockether.vis-runtime.loop.runtime.conversation.environment.query.core-test
  (:require
   [com.blockether.vis-runtime.loop.mustache :as mustache]
   [com.blockether.vis-runtime.loop.runtime.conversation.environment.core :as env-core]
   [lazytest.core :refer [defdescribe it expect]]
   [sci.core :as sci]
   [com.blockether.vis-runtime.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [com.blockether.svar.internal.llm :as llm]
   [clojure.string :as str]
   [com.blockether.vis-sdk.core :as ext]
   [com.blockether.vis-runtime.loop.runtime.conversation.environment.query.iteration.core :as iter]))

;; ─── from answer_render_test.clj ───

(defn- fresh-environment []
  (env-core/create-sci-context nil))

(defn- eval-in [{:keys [sci-ctx]} source]
  (:val (sci/eval-string+ sci-ctx source
          {:ns (sci/find-ns sci-ctx 'sandbox)})))

(defn- get-locals [{:keys [sci-ctx initial-ns-keys]}]
  (let [sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox])]
    (persistent!
      (reduce-kv (fn [acc k v]
                   (if (or (contains? initial-ns-keys k) (keyword? k))
                     acc
                     (assoc! acc k (if (instance? clojure.lang.IDeref v) @v v))))
        (transient {}) sandbox))))

(defn- render [environment raw-answer]
  (mustache/render raw-answer (get-locals environment)))

(defdescribe answer-render-test

  (it "passes plain prose through unchanged"
    (let [environment (fresh-environment)]
      (expect (= "Done. The cache is now warm."
                (render environment "Done. The cache is now warm.")))))

  (it "passes markdown through unchanged"
    (let [environment (fresh-environment)
          markdown    "## Summary\n- Patched 3 files\n- All tests green"]
      (expect (= markdown (render environment markdown)))))

  (it "interpolates sandbox vars via {{var}}"
    (let [environment (fresh-environment)]
      (eval-in environment "(def hits 12)")
      (eval-in environment "(def files 3)")
      (expect (= "Found 12 hits across 3 files."
                (render environment "Found {{hits}} hits across {{files}} files.")))))

  (it "supports computed answers via def + {{var}}"
    ;; The model defs the computed value in :code, then references
    ;; it from :answer. No SCI eval at finalize time; just Mustache.
    (let [environment (fresh-environment)]
      (eval-in environment "(def summary (clojure.string/join \"\\n\" [\"a\" \"b\" \"c\"]))")
      (expect (= "a\nb\nc" (render environment "{{summary}}")))))

  (it "iterates collections via Mustache sections"
    (let [environment (fresh-environment)]
      (eval-in environment "(def rows [\"r1\" \"r2\" \"r3\"])")
      (expect (= "r1\nr2\nr3\n"
                (render environment "{{#rows}}{{.}}\n{{/rows}}")))))

  (it "raises on a missing var so the iteration handler can surface it"
    (let [environment (fresh-environment)]
      (expect (try
                (render environment "Hi {{undefined-var}}")
                false
                (catch Exception _ true))))))

;; ─── from redundancy_metric_test.clj ───

;; -----------------------------------------------------------------------------
;; canonical-expression-hash
;; -----------------------------------------------------------------------------

(defdescribe canonical-expression-hash-test
  (it "collapses whitespace differences"
    (expect (= (iterate/canonical-expression-hash "(grep \"X\")")
              (iterate/canonical-expression-hash "(grep   \"X\")"))))

  (it "collapses leading/trailing whitespace"
    (expect (= (iterate/canonical-expression-hash "(grep \"X\")")
              (iterate/canonical-expression-hash "  (grep \"X\")\n"))))

  (it "produces different hashes for different forms"
    (expect (not= (iterate/canonical-expression-hash "(grep \"X\")")
              (iterate/canonical-expression-hash "(grep \"Y\")"))))

  (it "produces different hashes for forms with different head sym"
    (expect (not= (iterate/canonical-expression-hash "(grep \"X\")")
              (iterate/canonical-expression-hash "(read-file \"X\")"))))

  (it "falls back to raw-string hash on parse failure (never throws)"
    ;; A truncated form like \"(def\" is unparseable; the helper must
    ;; still return a stable hash.
    (let [a (iterate/canonical-expression-hash "(def")
          b (iterate/canonical-expression-hash "(def")
          c (iterate/canonical-expression-hash "(defn")]
      (expect (= a b))
      (expect (not= a c))))

  (it "treats nil / empty string as a stable hash, not a throw"
    (expect (string? (iterate/canonical-expression-hash "")))
    (expect (string? (iterate/canonical-expression-hash nil)))))

;; -----------------------------------------------------------------------------
;; count-duplicates
;; -----------------------------------------------------------------------------

(defdescribe count-duplicates-test
  (it "first iter has zero duplicates and seeds the seen-set"
    (let [seen (atom #{})
          [duplicates total] (iterate/count-duplicates seen
                               [{:code "(+ 1 2)"}
                                {:code "(grep \"X\")"}])]
      (expect (= 0 duplicates))
      (expect (= 2 total))
      (expect (= 2 (count @seen)))))

  (it "subsequent iter reports duplicates and grows the seen-set with new hashes only"
    (let [seen (atom #{})]
      (iterate/count-duplicates seen
        [{:code "(grep \"X\")"}
         {:code "(read-file \"a\")"}])
      (let [[duplicates total] (iterate/count-duplicates seen
                                 [{:code "(grep \"X\")"}            ;; duplicate
                                  {:code "(grep \"Y\")"}             ;; new
                                  {:code "(read-file \"a\")"}])]      ;; duplicate
        (expect (= 2 duplicates))
        (expect (= 3 total))
        ;; Seen-set now has 3 distinct hashes: grep X, grep Y, read-file a.
        (expect (= 3 (count @seen))))))

  (it "errors are NOT recorded — retrying after failure is legitimate"
    ;; Iter 1 errored out; iter 2 retries the same call; iter 2's call
    ;; must NOT count as a duplicate.
    (let [seen (atom #{})]
      (iterate/count-duplicates seen
        [{:code "(grep \"X\")" :error "regex broken"}])
      (let [[duplicates total] (iterate/count-duplicates seen
                                 [{:code "(grep \"X\")"}])]
        (expect (= 0 duplicates))
        (expect (= 1 total)))))

  (it "whitespace-equivalent retries dedup correctly"
    (let [seen (atom #{})]
      (iterate/count-duplicates seen [{:code "(+ 1 2)"}])
      (let [[duplicates total] (iterate/count-duplicates seen
                                 [{:code "(+   1   2)"}])]
        (expect (= 1 duplicates))
        (expect (= 1 total)))))

  (it "intra-iter duplicates count: identical calls in the SAME iter"
    ;; The seen-set rolls forward as we walk the iter's expressions,
    ;; so the SECOND occurrence of `(grep \"X\")` within one iter
    ;; counts as a duplicate. Without this, calls like 'three
    ;; identical greps in one :code array' would report 0
    ;; duplicates even though the dedup short-circuit fires for
    ;; calls 2 and 3.
    (let [seen (atom #{})
          [duplicates total] (iterate/count-duplicates seen
                               [{:code "(grep \"X\")"}
                                {:code "(grep \"X\")"}
                                {:code "(grep \"X\")"}
                                {:code "(read-file \"a\")"}])]
      (expect (= 2 duplicates))                                       ;; calls #2 and #3
      (expect (= 4 total))
      (expect (= 2 (count @seen)))))                                  ;; only 2 distinct hashes seeded

  (it "handles an empty expressions vec gracefully"
    (let [seen (atom #{})
          [duplicates total] (iterate/count-duplicates seen [])]
      (expect (= 0 duplicates))
      (expect (= 0 total)))))

;; -----------------------------------------------------------------------------
;; dedup-cache short-circuit — the actual Phase 2 mechanism.
;; -----------------------------------------------------------------------------

(defdescribe dedup-cache-test
  (it "lookup returns nil when the cache is empty"
    (let [cache (atom {})]
      (expect (nil? (iterate/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "lookup returns nil when expression is nil"
    (let [cache (atom {})]
      (expect (nil? (iterate/dedup-cache-lookup cache nil)))))

  (it "record! stores successful results, lookup hits afterwards"
    (let [cache (atom {})
          successful {:result :ok :stdout "" :stderr "" :execution-time-ms 7}]
      (iterate/dedup-cache-record! cache "(grep \"X\")" successful "i3.1")
      (let [hit (iterate/dedup-cache-lookup cache "(grep \"X\")")]
        (expect (some? hit))
        (expect (= :ok (:result hit)))
        (expect (= "i3.1" (:cached-from hit)))
        (expect (true? (:cached? hit)))
        (expect (= 0 (:execution-time-ms hit))))))

  (it "record! is a no-op for error results"
    (let [cache (atom {})
          err-result {:result nil :error "boom" :stdout "" :stderr ""}]
      (iterate/dedup-cache-record! cache "(grep \"X\")" err-result "i3.1")
      (expect (nil? (iterate/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "record! is a no-op for timeouts"
    (let [cache (atom {})
          timeout-result {:result nil :timeout? true :stdout "" :stderr ""}]
      (iterate/dedup-cache-record! cache "(grep \"X\")" timeout-result "i3.1")
      (expect (nil? (iterate/dedup-cache-lookup cache "(grep \"X\")")))))

  (it "record! preserves the FIRST writer when racing"
    (let [cache (atom {})
          first-result  {:result :first :execution-time-ms 1}
          second-result {:result :second :execution-time-ms 2}]
      (iterate/dedup-cache-record! cache "(grep \"X\")" first-result "i1.1")
      (iterate/dedup-cache-record! cache "(grep \"X\")" second-result "i5.2")
      (let [hit (iterate/dedup-cache-lookup cache "(grep \"X\")")]
        (expect (= :first (:result hit)))
        (expect (= "i1.1" (:cached-from hit))))))

  (it "whitespace-equivalent forms hit the same cache entry"
    (let [cache (atom {})]
      (iterate/dedup-cache-record! cache "(grep \"X\")" {:result :ok} "i1.1")
      (expect (some? (iterate/dedup-cache-lookup cache "(grep    \"X\")"))))))

;; ─── from schema_reject_retry_test.clj ───

(defn- schema-reject-ex
  "Build the same exception svar's `internal.spec/str->data-with-spec`
   throws when the provider returns a bare JSON-string."
  [preview]
  (ex-info
    (str "Your response did not match the JSON schema contract. "
      "PRODUCE valid JSON/EDN matching the schema fields. NO prose "
      "outside the structure.")
    {:type :svar.spec/schema-rejected
     :reason :not-a-map
     :received-type "String"
     :raw-data preview
     :raw-data-preview (pr-str preview)}))

(defn- ok-result
  "Minimal shape of what `llm/ask!` returns when parsing succeeds."
  []
  {:result      {:thinking "ok" :code []}
   :tokens      {:input 10 :output 5 :reasoning 0 :total 15}
   :duration-ms 1.0})

(defn- with-stubbed-ask!
  "Run `f` with `llm/ask!` replaced by a stub that pops one outcome per
   call from `outcomes` and either returns it (`:ok`) or throws it
   (instance of Throwable). Returns
     {:result <f result OR :threw> :calls <vec of :messages> :exception <or nil>}."
  [outcomes f]
  (let [calls (atom [])
        remaining (atom (vec outcomes))
        stub (fn [_router opts]
               (swap! calls conj (:messages opts))
               (let [next-out (first @remaining)]
                 (swap! remaining subvec 1)
                 (if (instance? Throwable next-out)
                   (throw next-out)
                   next-out)))]
    (with-redefs [llm/ask! stub]
      (try
        {:result    (f) :calls @calls :exception nil}
        (catch Throwable t
          {:result    :threw :calls @calls :exception t})))))

(defn- run-helper
  "Invoke `ask-with-schema-retry!` with sensible defaults. `chunks` is
   an atom that the helper appends to via `:on-chunk`."
  [chunks & {:keys [max-retries]
             :or   {max-retries 2}}]
  (iter/ask-with-schema-retry!
    ::router-stub
    {:spec ::iteration-spec-stub
     :messages [{:role "user" :content "Q"}]
     :routing {}
     :check-context? false}
    {:iteration   3
     :on-chunk    (fn [chunk] (swap! chunks conj chunk))
     :max-retries max-retries}))

(defdescribe ask-with-schema-retry-test
  (it "first-call success: returns immediately, no retry, no reminder"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask! [(ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 1 (count calls)))
      ;; No reminder was appended.
      (expect (= [{:role "user" :content "Q"}] (first calls)))
      ;; No on-chunk schema-reject events.
      (expect (empty? (filter :schema-reject-retry @chunks)))))

  (it "transient rejection: retry succeeds; ONE reminder, iteration loop never sees the failure"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "Looking at what I have so far")
             (ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 2 (count calls)))
      ;; First call: original messages.
      (expect (= 1 (count (first calls))))
      ;; Second call: original messages + ONE reminder.
      (expect (= 2 (count (second calls))))
      (let [reminder (last (second calls))]
        (expect (= "user" (:role reminder)))
        (expect (re-find #"\[svar/schema-reject 1/2\]" (:content reminder)))
        (expect (re-find #"top-level value MUST be a JSON/EDN map"
                  (:content reminder)))
        ;; The reminder includes the literal raw-data preview so the
        ;; model sees what it sent.
        (expect (re-find #"Looking at what I have so far"
                  (:content reminder))))
      ;; on-chunk was notified exactly once with the retry counter.
      (let [retry-chunks (filter :schema-reject-retry @chunks)]
        (expect (= 1 (count retry-chunks)))
        (expect (= 1 (:schema-reject-retry (first retry-chunks))))
        (expect (= 2 (:schema-reject-max (first retry-chunks))))
        (expect (= 3 (:iteration (first retry-chunks)))))))

  (it "two transient rejections: second retry succeeds; reminder is replaced, not accumulated"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "first prose")
             (schema-reject-ex "second prose")
             (ok-result)]
            #(run-helper chunks))]
      (expect (nil? exception))
      (expect (= (:result (ok-result)) (:result result)))
      (expect (= 3 (count calls)))
      ;; Each retry replaces the reminder, never accumulates -> messages
      ;; on attempt 2 have exactly 2 entries (original + 1 reminder),
      ;; not 3.
      (expect (= 1 (count (nth calls 0))))
      (expect (= 2 (count (nth calls 1))))
      (expect (= 2 (count (nth calls 2))))
      ;; Reminders carry the CURRENT attempt counter, not a stale one.
      (expect (re-find #"\[svar/schema-reject 1/2\]"
                (:content (last (nth calls 1)))))
      (expect (re-find #"\[svar/schema-reject 2/2\]"
                (:content (last (nth calls 2)))))
      ;; The second reminder cites the second prose preview, not the
      ;; first -- the helper inspects the current rejection.
      (expect (re-find #"second prose"
                (:content (last (nth calls 2)))))))

  (it "rejection budget exhausted: bubbles out to the iteration loop with the original ex-data"
    (let [chunks (atom [])
          {:keys [result calls exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "p1")
             (schema-reject-ex "p2")
             (schema-reject-ex "p3")]
            #(run-helper chunks :max-retries 2))]
      (expect (= :threw result))
      (expect (some? exception))
      (expect (= :svar.spec/schema-rejected (:type (ex-data exception))))
      (expect (= "String" (:received-type (ex-data exception))))
      ;; 1 + 2 retries = 3 attempts.
      (expect (= 3 (count calls)))
      ;; Two retry chunks fired for the two retries (the final
      ;; bubble-out is NOT a retry chunk -- it's a real failure).
      (expect (= 2 (count (filter :schema-reject-retry @chunks))))))

  (it "non-schema rejection bubbles immediately, no retry"
    (let [chunks (atom [])
          other-ex (ex-info "boom" {:type :something/else})
          {:keys [result calls exception]}
          (with-stubbed-ask! [other-ex]
            #(run-helper chunks))]
      (expect (= :threw result))
      (expect (= other-ex exception))
      (expect (= 1 (count calls)))
      (expect (empty? (filter :schema-reject-retry @chunks)))))

  (it "max-retries=0 disables the retry layer (parity with calling llm/ask! directly)"
    (let [chunks (atom [])
          {:keys [result exception]}
          (with-stubbed-ask!
            [(schema-reject-ex "first prose")]
            #(run-helper chunks :max-retries 0))]
      (expect (= :threw result))
      (expect (= :svar.spec/schema-rejected (:type (ex-data exception))))
      (expect (empty? (filter :schema-reject-retry @chunks))))))

;; ─── from parse_rescue_loop_test.clj ───

(def ^:private try-extension-parse-rescue
  #'iter/try-extension-parse-rescue)

(defn- preceding-backslash-count
  [code index]
  (loop [position (dec index)
         count    0]
    (if (and (<= 0 position) (= \\ (.charAt ^String code position)))
      (recur (dec position) (inc count))
      count)))

(defn- rescue-one-unsupported-escape
  "Test fixture hook: doubles exactly one unsupported regex escape.
   The loop driver must call it repeatedly when a source string has
   multiple bad sites. This intentionally lives in core tests so core
   does not depend on the common-editing extension test classpath."
  [{:keys [code error]}]
  (when (and (string? code) (str/includes? (str error) "Unsupported escape character"))
    (let [targets #{\| \. \( \) \$ \* \+ \? \[ \] \{ \}}
          length  (count code)]
      (loop [index 0]
        (cond
          (>= index (dec length)) nil
          (and (= \\ (.charAt ^String code index))
            (targets (.charAt ^String code (inc index)))
            (even? (preceding-backslash-count code index)))
          (str (subs code 0 index) "\\" (subs code index))
          :else (recur (inc index)))))))

(def ^:private rg-symbol
  (ext/symbol 'rg (fn [& _] nil)
    {:doc               "fixture"
     :arglists          '([pattern])
     :on-parse-error-fn rescue-one-unsupported-escape}))

(defn- minimal-environment
  "Build the smallest possible environment shape that
   `try-extension-parse-rescue` reads from. Only `:extensions`
   (a deref-able holder of an extension vec) is required."
  []
  (let [ext (ext/extension
              {:ext/namespace 'com.blockether.vis.test.parse-rescue
               :ext/doc       "Loop test fixture."
               :ext/group     "filesystem"
               :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
               :ext/prompt    (constantly "placeholder")
               :ext/symbols   [rg-symbol]})]
    {:extensions (atom [ext])
     :sci-ctx    (sci/init {})}))

(defn- parses? [^String code]
  (try
    (require '[edamame.core :as eda])
    ((resolve 'eda/parse-string-all) code {:all true})
    true
    (catch Throwable _ false)))

(defn- parse-error-msg [^String code]
  (try
    (require '[edamame.core :as eda])
    ((resolve 'eda/parse-string-all) code {:all true})
    nil
    (catch Throwable t (ex-message t))))

(defdescribe try-extension-parse-rescue-loop-test

  (it "repairs a single `\\|` site (baseline; pre-fix already passed)"
    (let [env  (minimal-environment)
          code "(vis/rg \"a\\|b\")"
          err  (parse-error-msg code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (= "(vis/rg \"a\\\\|b\")" out))
      (expect (parses? out))))

  (it "loops the rescue across THREE `\\|` sites until the source parses (Bug 2.A.1)"
    ;; Pre-fix: returns nil (single-shot rescue gives up on 2+ sites).
    ;; Post-fix: returns a fully repaired string that parses cleanly.
    (let [env  (minimal-environment)
          code "(vis/rg \"foo\\|bar\\|baz\\|qux\")"
          err  (parse-error-msg code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (string? out))
      (expect (parses? out))
      ;; Every original `\|` is now `\\|`.
      (expect (str/includes? out "\\\\|"))))

  (it "loops across `\\|` AND `\\.` AND `\\(` mixed escapes"
    (let [env  (minimal-environment)
          code "(vis/rg \"a\\|b\\.c\\(d\")"
          err  (parse-error-msg code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (string? out))
      (expect (parses? out))))

  (it "still returns nil when the rescue has nothing to repair"
    ;; A real broken form the rescue can't fix: single-quoted string
    ;; literal the reader rejects. The hook's `rescue-parse-error`
    ;; only handles Unsupported-escape errors; other shapes return
    ;; nil from every iteration of the loop.
    (let [env  (minimal-environment)
          code "(vis/rg 'unterminated"
          err  (parse-error-msg code)
          out  (try-extension-parse-rescue env code err)]
      (expect (some? err))
      (expect (nil? out))))

  (it "bounded: a pathological hook that returns a non-shrinking rewrite must not loop forever"
    ;; If a hook keeps returning the same error shape (or makes no
    ;; progress), the driver MUST give up. We simulate that with an
    ;; extension whose hook trivially returns its input wrapped in
    ;; a no-op transformation that re-raises the same parse error.
    (let [pathological-hook (fn [{:keys [code]}]
                              ;; Return code unchanged — should be
                              ;; detected as no-progress and stop.
                              code)
          rg (ext/symbol 'rg (fn [& _] nil)
               {:doc      "fixture"
                :arglists '([pattern])
                :on-parse-error-fn pathological-hook})
          ext (ext/extension
                {:ext/namespace 'com.blockether.vis.test.pathological
                 :ext/doc       "pathological"
                 :ext/group     "filesystem"
                 :ext/ns-alias  {:ns 'vis.ext.tools :alias 'vis}
                 :ext/prompt    (constantly "x")
                 :ext/symbols   [rg]})
          env {:extensions (atom [ext]) :sci-ctx (sci/init {})}
          code "(vis/rg \"a\\|b\")"
          err  (parse-error-msg code)
          start-ms (System/currentTimeMillis)
          out  (try-extension-parse-rescue env code err)
          elapsed (- (System/currentTimeMillis) start-ms)]
      (expect (nil? out))
      ;; Sanity: bailout must be sub-second.
      (expect (< elapsed 1000)))))
