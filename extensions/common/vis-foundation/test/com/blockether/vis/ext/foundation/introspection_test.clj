(ns com.blockether.vis.ext.foundation.introspection-test
  "Tests for the meta extension's consolidated API. Seven
   functions, each returning a map or vector. Each test bootstraps
   synthetic conversation + turn + iteration rows in an in-memory
   SQLite DB, then invokes the impl fns directly with a fake env map."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
   [lazytest.core :refer [defdescribe it expect]]))

;; Populate the classpath docs registry once for the whole namespace.
;; The (v/extensions ...) / (v/extension-docs ...) / etc. tests
;; read from the registry that `discover-extensions!` produces by
;; merging every `META-INF/vis-extension/vis.edn` on the classpath.
;; Idempotent across runs (memoized inside the loader).
(vis/discover-extensions!)

(h/use-mem-store!)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- bootstrap [store]
  (let [conversation-id (vis/db-store-conversation! store
                          {:channel :tui :title "meta test"})
        conversation-turn-id (vis/db-store-conversation-turn! store
                               {:parent-conversation-id conversation-id
                                :user-request "what's the plan?"
                                :status :running})]
    {:conversation-id conversation-id :conversation-turn-id conversation-turn-id}))

(defn- db-store-iteration!
  [store conversation-turn-id {:keys [blocks thinking error]
                               :or {blocks []}}]
  (vis/db-store-iteration! store
    (cond-> {:conversation-turn-id    conversation-turn-id
             :blocks blocks
             :duration-ms 100
             :llm-model   "test-model"
             :metadata    {}}
      thinking   (assoc :thinking thinking)
      error      (assoc :error error))))

(defn- env [store conversation-id]
  {:db-info store
   :conversation-id conversation-id
   :current-iteration-atom (atom 3)})

(defn- eval-provenance [iteration form-position form-count]
  {:op :sci/eval
   :status :done
   :iteration iteration
   :form-position form-position
   :form-count form-count
   :ref (str "i" iteration "." form-position)
   :started-at-ms 10
   :finished-at-ms 11
   :duration-ms 1})

(defn- tool-result [op _markdown]
  {:success? true
   :result {:value :ok}
   :provenance {:op op
                :status :done
                :started-at-ms 10
                :finished-at-ms 15
                :duration-ms 5}
   :error nil})

;; The impl fns are private — reach via the var registry so tests
;; stay decoupled from any public re-export.
(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.foundation.introspection" name))))

;; -----------------------------------------------------------------------------
;; current-turn helper — single rich snapshot of the current turn
;; -----------------------------------------------------------------------------

(defdescribe foundation-turn-test
  (it "returns nil when DB is unreachable"
    (expect (nil? ((private-fn "foundation-turn") {:conversation-id "x"}))))

  (it "returns a snapshot map with user request / status / iteration / cost / elapsed-ms"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          turn ((private-fn "foundation-turn") (env s conversation-id))]
      (expect (= "what's the plan?" (:user-request turn)))
      (expect (= :running (:status turn)))
      (expect (map? (:iteration turn)))
      (expect (= 3 (:current (:iteration turn))))
      (expect (= [:current] (vec (keys (:iteration turn)))))
      (expect (map? (:cost turn)))
      (expect (vector? (:attempts turn)))
      (expect (vector? (:errors turn)))
      (expect (vector? (:failures turn)))
      (expect (not (contains? turn :plan)))
      (expect (not (contains? turn :breadcrumbs)))))

  (it "splits attempts and errors so callers consume each list directly (no second filter pass)"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                  {:id 1 :code "(boom)"  :error "boom" :execution-time-ms 1}]})
      (let [turn ((private-fn "foundation-turn") (env s conversation-id))]
        (expect (= 2 (count (:attempts turn))))
        (expect (= 1 (count (:errors turn))))
        (expect (= "boom" (-> turn :errors first :error))))))

  (it "omits the dropped :redundancy key from the snapshot"
    ;; The dedup cache + redundancy metric were removed; current-turn snapshots
    ;; no longer surfaces a `:redundancy` key. Asserting absence here
    ;; pins the contract so a future addition is a deliberate decision,
    ;; not a silent regression.
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (vis/db-store-iteration! s
        {:conversation-turn-id    conversation-turn-id
         :blocks      [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]
         :duration-ms 100
         :llm-model   "test-model"})
      (let [turn ((private-fn "foundation-turn") (env s conversation-id))]
        (expect (not (contains? turn :redundancy)))))))

;; -----------------------------------------------------------------------------
;; conversation helper — current or specific conversation
;; -----------------------------------------------------------------------------

(defdescribe foundation-conversation-test
  (it "no-arg form returns the current conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          conversation ((private-fn "foundation-conversation") (env s conversation-id))]
      (expect (= conversation-id (:id conversation)))
      (expect (= :tui (:channel conversation)))
      (expect (vector? (:turns conversation)))
      (expect (= 1 (:turn-count conversation)))))

  (it "arg form fetches any conversation by id"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          other (vis/db-store-conversation! s {:channel :telegram :title "other"})
          conversation ((private-fn "foundation-conversation") (env s conversation-id) other)]
      (expect (= other (:id conversation)))
      (expect (= :telegram (:channel conversation)))
      (expect (= 0 (:turn-count conversation)))))

  (it "turns include user-request/outcome/answer when present"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (vis/db-update-conversation-turn! s conversation-turn-id
        {:answer "42" :iteration-count 1 :duration-ms 50 :status :done
         :prior-outcome :complete})
      (let [conversation ((private-fn "foundation-conversation") (env s conversation-id))
            turn (first (:turns conversation))]
        (expect (= "what's the plan?" (:user-request turn)))
        (expect (= "42" (:answer turn)))
        (expect (= :complete (:outcome turn))))))

  (it "auto-excludes the in-flight turn (= TURN_CONVERSATION_TURN_ID) from the current conversation"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          env-with-in-flight (assoc (env s conversation-id)
                               :current-conversation-turn-id-atom (atom conversation-turn-id))
          conversation ((private-fn "foundation-conversation") env-with-in-flight)]
      (expect (= 0 (:turn-count conversation))) ; bootstrap creates exactly 1 turn, the in-flight one
      (expect (empty? (:turns conversation)))
      (expect (= conversation-turn-id (:in-flight-turn-id conversation)))))

  (it "does NOT filter when inspecting a foreign conversation"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          other (vis/db-store-conversation! s {:channel :telegram :title "other"})
          env-with-in-flight (assoc (env s conversation-id)
                               :current-conversation-turn-id-atom (atom conversation-turn-id))
          conversation ((private-fn "foundation-conversation") env-with-in-flight other)]
      ;; Foreign conversation untouched: no in-flight-turn-id, original turns kept.
      (expect (= other (:id conversation)))
      (expect (= 0 (:turn-count conversation)))
      (expect (nil? (:in-flight-turn-id conversation)))))

  (it "does NOT filter when no turn is in flight (current-conversation-turn-id-atom = nil)"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          conversation ((private-fn "foundation-conversation") (env s conversation-id))]
      (expect (= 1 (:turn-count conversation)))
      (expect (nil? (:in-flight-turn-id conversation))))))

;; -----------------------------------------------------------------------------
;; conversations helper — list across one or all channels
;; -----------------------------------------------------------------------------

(defdescribe foundation-conversations-test
  (it "no-arg form scans every known channel"
    (let [s (h/store)
          a (vis/db-store-conversation! s {:channel :tui :title "vis-a"})
          b (vis/db-store-conversation! s {:channel :telegram :title "tg-b"})
          all ((private-fn "foundation-conversations") (env s a))
          ids (set (map :id all))]
      (expect (contains? ids a))
      (expect (contains? ids b))))

  (it "channel-arg form filters to one channel"
    (let [s (h/store)
          a (vis/db-store-conversation! s {:channel :tui :title "vis-a"})
          _ (vis/db-store-conversation! s {:channel :telegram :title "tg-b"})
          tui-list ((private-fn "foundation-conversations") (env s a) :telegram)]
      (expect (= 1 (count tui-list)))
      (expect (= :telegram (:channel (first tui-list))))))

  (it "every entry carries id, channel, title, turn-count"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          all ((private-fn "foundation-conversations") (env s conversation-id))
          this (first (filter #(= conversation-id (:id %)) all))]
      (expect (some? this))
      (expect (= "meta test" (:title this)))
      (expect (= 1 (:turn-count this))))))

;; -----------------------------------------------------------------------------
;; conversation-forks helper — fork tree introspection.
;; -----------------------------------------------------------------------------

(defdescribe foundation-conversation-forks-test
  (it "returns the trunk row for an unforked conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          rows ((private-fn "foundation-conversation-forks") (env s conversation-id))]
      (expect (vector? rows))
      (expect (= 1 (count rows)))
      (expect (= 0 (:version (first rows))))
      (expect (nil? (:parent-state-id (first rows))))))

  (it "surfaces every fork with parent links and per-state turn counts"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (vis/db-fork-conversation! s conversation-id {:title "Branch A"})
      (vis/db-fork-conversation! s conversation-id {:title "Branch B"})
      (let [rows ((private-fn "foundation-conversation-forks") (env s conversation-id))]
        (expect (= 3 (count rows)))
        (expect (= [0 1 2] (mapv :version rows)))
        (expect (nil? (:parent-state-id (first rows))))
        (expect (every? :parent-state-id (drop 1 rows))))))

  (it "explicit conversation-id form scans a different conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          other (vis/db-store-conversation! s {:channel :tui :title "other"})]
      (vis/db-fork-conversation! s other {:title "Other branch"})
      (let [rows ((private-fn "foundation-conversation-forks") (env s conversation-id) other)]
        (expect (= 2 (count rows))))))

  (it "returns [] (vector, never nil) when env is missing handles"
    (let [rows ((private-fn "foundation-conversation-forks") {})]
      (expect (vector? rows))
      (expect (= [] rows)))
    (let [rows ((private-fn "foundation-conversation-forks") {} (random-uuid))]
      (expect (vector? rows))
      (expect (= [] rows)))))

;; -----------------------------------------------------------------------------
;; turn-retries helper — retry history introspection.
;; -----------------------------------------------------------------------------

(defdescribe meta-turn-retries-test
  (it "returns the v0 row for a turn with no retries"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          rows ((private-fn "meta-turn-retries") (env s conversation-id) conversation-turn-id)]
      (expect (vector? rows))
      (expect (= 1 (count rows)))
      (expect (= 0 (:version (first rows))))
      (expect (nil? (:forked-from-conversation-turn-state-id (first rows))))))

  (it "surfaces every retry in version order with forked-from links"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (vis/db-retry-conversation-turn! s conversation-turn-id {:status :running :model "claude-4"})
      (vis/db-retry-conversation-turn! s conversation-turn-id {:status :done    :model "gpt-4o"})
      (let [rows ((private-fn "meta-turn-retries") (env s conversation-id) conversation-turn-id)]
        (expect (= 3 (count rows)))
        (expect (= [0 1 2] (mapv :version rows)))
        (expect (every? :forked-from-conversation-turn-state-id (drop 1 rows))))))

  (it "returns [] (vector, never nil) for an unknown conversation-turn-id"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          rows ((private-fn "meta-turn-retries") (env s conversation-id) (random-uuid))]
      (expect (vector? rows))
      (expect (= [] rows))))

  (it "returns [] (vector, never nil) when env or conversation-turn-id missing"
    (expect (= [] ((private-fn "meta-turn-retries") {} (random-uuid))))
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "meta-turn-retries") (env s conversation-id) nil))))))

;; -----------------------------------------------------------------------------
;; failures and diagnosis helpers — no raw SQLite needed for triage
;; -----------------------------------------------------------------------------

(defdescribe meta-failure-diagnostics-test
  (it "normalizes provider schema rejections with raw previews"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (db-store-iteration! s conversation-turn-id
        {:error {:message "Your response did not match the JSON schema contract."
                 :data {:type :svar.spec/schema-rejected
                        :reason :not-a-map
                        :received-type "String"
                        :raw-data "Looking at what I have so far"}}})
      (let [failures ((private-fn "foundation-failures") (env s conversation-id))
            first-failure (first failures)]
        (expect (= 1 (count failures)))
        (expect (= :provider (:source first-failure)))
        (expect (= :provider-schema-rejected (:classification first-failure)))
        (expect (= :not-a-map (:reason first-failure)))
        (expect (= "Looking at what I have so far" (:raw-preview first-failure))))))

  (it "classifies regex escaping and patch no-match tool failures"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(v/rg \"foo\\|bar\\|baz\" \"x\")"
                   :error "Unsupported escape character: \\|"
                   :execution-time-ms 1}
                  {:id 1
                   :code "(v/patch [{:path \"render.clj\" :search \"x\" :replace \"y\"}])"
                   :error "SEARCH block 1 not found in render.clj"
                   :execution-time-ms 1}]})
      (let [diagnosis ((private-fn "foundation-diagnose") (env s conversation-id))]
        (expect (= 2 (:failure-count diagnosis)))
        (expect (= 1 (get-in diagnosis [:by-classification :regex-unsupported-escape])))
        (expect (= 1 (get-in diagnosis [:by-classification :patch-no-match])))
        (expect (seq (:next-actions diagnosis)))
        ;; Two distinct one-off failures — no cluster meets the
        ;; repetition threshold.
        (expect (false? (:repetition-loop? diagnosis)))
        (expect (= [] (:repetition-clusters diagnosis))))))

  (it "flags a same-error repetition loop when one signature dominates"
    ;; Mirrors the worst case the report flagged: 148 'Path/File not
    ;; found' failures inside a single iteration. Test seeds a smaller
    ;; cluster (6 + 2) just past `REPETITION_THRESHOLD` so the bucket
    ;; with 6 fires and the bucket with 2 stays below the floor.
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (db-store-iteration! s conversation-turn-id
        {:blocks
         (into
           (mapv (fn [i]
                   {:id i
                    :code (str "(v/cat \"src/tui/file" i ".clj\")")
                    :error (str "File not found: /Users/x/vis/src/tui/file" i ".clj")
                    :execution-time-ms 1})
             (range 6))
           [{:id 6 :code "(v/cat \"src/tui/render.clj\")"
             :error "Path not found: /Users/x/vis/src/tui"
             :execution-time-ms 1}
            {:id 7 :code "(v/cat \"src/tui\")"
             :error "Path not found: /Users/x/vis/src/tui"
             :execution-time-ms 1}])})
      (let [diag ((private-fn "foundation-diagnose") (env s conversation-id))
            clusters (:repetition-clusters diag)]
        (expect (= 8 (:failure-count diag)))
        (expect (true? (:repetition-loop? diag)))
        ;; Only the 6-strong 'File not found' cluster meets the floor.
        (expect (= 1 (count clusters)))
        (expect (= 6 (-> clusters first :count)))
        (expect (= "File not found" (-> clusters first :signature last)))
        ;; First next-action calls out the loop and steers off it.
        (expect (some #(re-find #"Same error repeated" %)
                  (:next-actions diag))))))

  (it "conversation-id form scans every turn"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          second-conversation-turn-id (vis/db-store-conversation-turn! s
                                        {:parent-conversation-id conversation-id
                                         :user-request "second turn"
                                         :status :running})]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0 :code "(v/rg \"x\\|y\" \"z\")"
                   :error "Unsupported escape character: \\|"
                   :execution-time-ms 1}]})
      (db-store-iteration! s second-conversation-turn-id
        {:blocks [{:id 0 :code "(v/patch [{:path \"x\"}])"
                   :error "SEARCH block 1 not found in x"
                   :execution-time-ms 1}]})
      (let [failures ((private-fn "foundation-failures") (env s conversation-id) conversation-id)]
        (expect (= 2 (count failures)))
        (expect (every? :turn-id failures))
        (expect (every? :user-request failures)))))

  (it "returns [] (vector, never nil) when the current turn has no failures"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]})
      (let [failures ((private-fn "foundation-failures") (env s conversation-id))]
        (expect (vector? failures))
        (expect (= [] failures))))))

;; -----------------------------------------------------------------------------
;; (v/inspect) and (v/report) — one data surface + one Markdown renderer
;; -----------------------------------------------------------------------------

(defdescribe foundation-inspect-report-test
  (it "returns the canonical single-map introspection surface"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]})
      (let [data ((private-fn "foundation-inspect") (env s conversation-id))]
        (expect (= 1 (:schema-version data)))
        (expect (= :conversation (:scope data)))
        (expect (= conversation-id (:conversation-id data)))
        (expect (= conversation-id (get-in data [:conversation :id])))
        (expect (= conversation-id (get-in data [:transcript :conversation :id])))
        (expect (vector? (:conversation-index data)))
        (expect (map? (:current-turn data)))
        (expect (vector? (:failures data)))
        (expect (map? (:diagnosis data)))
        (expect (vector? (:conversation-forks data)))
        (expect (map? (:turn-retries data))))))

  (it "exposes raw LLM response diagnostics as a top-level inspect view"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          raw-response "provider prelude\n```clojure\n(+ 1 2)\n```"]
      (vis/db-store-iteration! s
        {:conversation-turn-id conversation-turn-id
         :blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]
         :duration-ms 100
         :llm-provider :test-provider
         :llm-model "test-model"
         :llm-raw-response raw-response
         :llm-executable-code "(+ 1 2)"
         :llm-executable-blocks [{:lang "clojure" :source "(+ 1 2)"}]})
      (let [data ((private-fn "foundation-inspect") (env s conversation-id))
            diagnostic (first (:llm-diagnostics data))
            transcript-iter (get-in data [:transcript :turns 0 :iterations 0])]
        (expect (= 1 (count (:llm-diagnostics data))))
        (expect (= conversation-turn-id (:turn-id diagnostic)))
        (expect (= "what's the plan?" (:user-request diagnostic)))
        (expect (= (:id transcript-iter) (:iteration-id diagnostic)))
        (expect (= 1 (:iteration diagnostic)))
        (expect (= :done (:status diagnostic)))
        (expect (= :test-provider (:provider diagnostic)))
        (expect (= "test-model" (:model diagnostic)))
        (expect (= raw-response (get-in diagnostic [:raw-response :preview])))
        (expect (= (count raw-response) (get-in diagnostic [:raw-response :length])))
        (expect (= (:llm-raw-response-sha256 transcript-iter)
                  (get-in diagnostic [:raw-response :sha256])))
        (expect (= "(+ 1 2)" (get-in diagnostic [:raw-response :executable-code])))
        (expect (= [{:lang "clojure" :source "(+ 1 2)"}]
                  (get-in diagnostic [:raw-response :executable-blocks])))
        (expect (= 1 (get-in diagnostic [:raw-response :block-count])))
        (expect (= ["clojure"] (get-in diagnostic [:raw-response :block-langs]))))))

  (it "renders raw LLM response diagnostics in the Markdown report"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          raw-response "provider prelude\n```clojure\n(+ 1 2)\n```"]
      (vis/db-store-iteration! s
        {:conversation-turn-id conversation-turn-id
         :blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]
         :duration-ms 100
         :llm-provider :test-provider
         :llm-model "test-model"
         :llm-raw-response raw-response
         :llm-executable-code "(+ 1 2)"
         :llm-executable-blocks [{:lang "clojure" :source "(+ 1 2)"}]})
      (let [out ((private-fn "foundation-report") (env s conversation-id))]
        (expect (string? out))
        (expect (str/includes? out (str "conversation `" conversation-id "`")))
        (expect (str/includes? out "User request:** what's the plan?"))
        (expect (str/includes? out "Raw LLM response diagnostics"))
        (expect (str/includes? out "Raw chars"))
        (expect (str/includes? out (str "| `" conversation-turn-id "` | 1 | done |")))
        (expect (str/includes? out (str "| " (count raw-response) " |")))
        (expect (str/includes? out "| 1 | clojure |"))
        (expect (str/includes? out raw-response)))))

  (it "renders Markdown from the same inspected transcript data"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]})
      (let [out ((private-fn "foundation-report") (env s conversation-id))]
        (expect (string? out))
        (expect (str/includes? out (str "conversation `" conversation-id "`")))
        (expect (str/includes? out "User request:** what's the plan?")))))

  (it "renders proof audit violations in the audit report"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          _iteration (db-store-iteration! s conversation-turn-id
                       {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]})
          ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/1")
          intent (vis/db-store-intent! s {:conversation-turn-id conversation-turn-id
                                          :title "Legacy audit"
                                          :rationale "Exercise report."})
          plan (vis/db-store-plan! s {:intent-id (:id intent)
                                      :summary "Legacy plan"})
          gate (vis/db-store-gate! s {:plan-id (:id plan)
                                      :question "Legacy proof?"})]
      (vis/db-prove-gate! s {:gate-id (:id gate)
                             :summary "Legacy proven."
                             :refs [ref]})
      (vis/db-fulfill-intent! s (:id intent) {:summary "Legacy fulfilled."
                                              :refs [ref]})
      (let [audit ((private-fn "foundation-audit") (env s conversation-id))
            out ((private-fn "foundation-audit-report") (env s conversation-id))]
        (expect (= false (:success? audit)))
        (expect (contains? (set (map :type (:violations audit)))
                  :missing-intent-closure-attestation))
        (expect (str/includes? out "## Proof audit"))
        (expect (str/includes? out "Proof audit: needs work"))
        (expect (str/includes? out ":missing-intent-closure-attestation")))))

  (it "exports inspect/report, provenance helpers, plus extension discovery symbols for introspection"
    (let [symbols (set (map :ext.symbol/sym com.blockether.vis.ext.foundation.introspection/all-symbols))]
      (expect (contains? symbols 'inspect))
      (expect (contains? symbols 'report))
      (expect (contains? symbols 'provenance-timeline))
      (expect (contains? symbols 'provenance-stats))
      (expect (contains? symbols 'provenance-guards))
      (expect (contains? symbols 'latest-provenance-refs))
      (expect (contains? symbols 'provenance-report))
      (expect (not (contains? symbols 'proof-checks)))
      (expect (not (contains? symbols 'proofs)))
      (expect (contains? symbols 'await-proof!))
      (expect (contains? symbols 'issue-intent!))
      (expect (contains? symbols 'focus-intent!))
      (expect (contains? symbols 'relate-intents!))
      (expect (contains? symbols 'proof-slot))
      (expect (contains? symbols 'plan))
      (expect (contains? symbols 'issue-plan!))
      (expect (contains? symbols 'issue-gate!))
      (expect (contains? symbols 'offer-proof!))
      (expect (contains? symbols 'attest-gate!))
      (expect (not (contains? symbols 'prove-gate!)))
      (expect (contains? symbols 'impede-gate!))
      (expect (contains? symbols 'block-gate!))
      (expect (contains? symbols 'attest-intent!))
      (expect (not (contains? symbols 'fulfill-intent!)))
      (expect (contains? symbols 'abandon-intent!))
      (expect (contains? symbols 'intents))
      (expect (not (contains? symbols 'intent!)))
      (expect (not (contains? symbols 'contract)))
      (expect (contains? symbols 'audit))
      (expect (contains? symbols 'audit-report))
      (expect (contains? symbols 'namespace-docs))
      (expect (contains? symbols 'symbol-doc))
      (expect (not (contains? symbols 'symbol-docs)))
      (doseq [removed ['turn 'conversation 'conversations 'conversation-forks
                       'turn-retries 'var-history 'find-attempts
                       'find-attempts-everywhere 'failures
                       'failures-everywhere 'diagnose]]
        (expect (not (contains? symbols removed)))))))

;; -----------------------------------------------------------------------------
;; Await proof — canonical Future/blocking deref observation.
;; -----------------------------------------------------------------------------

(defdescribe foundation-await-proof-test
  (it "returns terminal :done provenance for completed futures"
    (let [out ((private-fn "foundation-await-proof!") (future 42) {:timeout-ms 1000})]
      (expect (true? (:success? out)))
      (expect (= 42 (:result out)))
      (expect (= :future/await (get-in out [:provenance :op])))
      (expect (= :done (get-in out [:provenance :status])))))

  (it "returns terminal :timeout provenance for blocking deref timeouts"
    (let [p (promise)
          out ((private-fn "foundation-await-proof!") p {:timeout-ms 1})]
      (expect (false? (:success? out)))
      (expect (= :future/await (get-in out [:provenance :op])))
      (expect (= :timeout (get-in out [:provenance :status])))
      (expect (= "java.util.concurrent.TimeoutException" (get-in out [:error :type]))))))

;; -----------------------------------------------------------------------------
;; Provenance helpers — timeline, stats, guards, compact report.
;; -----------------------------------------------------------------------------

(defdescribe foundation-provenance-test
  (it "builds an ordered eval/tool timeline with canonical refs"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          base-ref (fn [block]
                     (str "turn/" (subs (str conversation-turn-id) 0 8)
                       "/iteration/1/block/" block))]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(+ 1 2)"
                   :result 3
                   :stdout ""
                   :stderr ""
                   :error nil
                   :execution-time-ms 1
                   :provenance (eval-provenance 1 1 2)}
                  {:id 1
                   :code "(v/bash \"pwd\")"
                   :result (tool-result :v/bash "Ran bash in `.` — exit `0`, 5 ms.")
                   :stdout ""
                   :stderr ""
                   :error nil
                   :execution-time-ms 1
                   :provenance (eval-provenance 1 2 2)}]})
      (let [timeline ((private-fn "foundation-provenance-timeline") (env s conversation-id))]
        (expect (= [(base-ref 1) (base-ref 2) (str (base-ref 2) "/tool/v.bash")]
                  (mapv :ref timeline)))
        (expect (= [:eval :eval :tool] (mapv :kind timeline)))
        (expect (= [:sci/eval :sci/eval :v/bash] (mapv :op timeline)))
        (expect (= (base-ref 2) (:parent-ref (last timeline)))))))

  (it "rolls up provenance stats and failure slices"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/1")]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(missing)"
                   :result nil
                   :stdout ""
                   :stderr ""
                   :error "Unable to resolve symbol: missing"
                   :execution-time-ms 1
                   :provenance (eval-provenance 1 1 1)}]})
      (let [stats ((private-fn "foundation-provenance-stats") (env s conversation-id))]
        (expect (= 1 (:event-count stats)))
        (expect (= {:eval 1} (:by-kind stats)))
        (expect (= {:error 1} (:by-status stats)))
        (expect (= ref (:ref (first (:failures stats))))))))

  (it "returns latest observed refs grouped by evidence role"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          base-ref (fn [iteration block]
                     (str "turn/" (subs (str conversation-turn-id) 0 8)
                       "/iteration/" iteration "/block/" block))
          done-ref (base-ref 1 1)
          error-ref (base-ref 2 1)]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(+ 1 2)"
                   :result 3
                   :stdout ""
                   :stderr ""
                   :error nil
                   :execution-time-ms 1
                   :provenance (eval-provenance 1 1 1)}]})
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(missing)"
                   :result nil
                   :stdout ""
                   :stderr ""
                   :error "Unable to resolve symbol: missing"
                   :execution-time-ms 1
                   :provenance (eval-provenance 2 1 1)}]})
      (let [refs ((private-fn "foundation-latest-provenance-refs") (env s conversation-id))]
        (expect (= error-ref (:latest-ref refs)))
        (expect (= done-ref (:latest-done-ref refs)))
        (expect (= done-ref (:latest-proof-ref refs)))
        (expect (= error-ref (:latest-terminal-ref refs)))
        (expect (= error-ref (:latest-error-ref refs)))
        (expect (= error-ref (:latest-blocker-ref refs)))
        (expect (= [done-ref] (:proof-refs refs)))
        (expect (= [error-ref] (:blocker-refs refs)))
        (expect (= [done-ref error-ref] (:all-valid-refs refs))))))

  (it "guards injected block provenance as valid"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(+ 1 2)"
                   :result 3
                   :stdout ""
                   :stderr ""
                   :error nil
                   :execution-time-ms 1}]})
      (let [guards ((private-fn "foundation-provenance-guards") (env s conversation-id))]
        (expect (true? (:success? guards)))
        (expect (= [] (:violations guards))))))

  (it "renders a compact Markdown provenance report"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/1")]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(+ 1 2)"
                   :result 3
                   :stdout ""
                   :stderr ""
                   :error nil
                   :execution-time-ms 1
                   :provenance (eval-provenance 1 1 1)}]})
      (let [out ((private-fn "foundation-provenance-report") (env s conversation-id))]
        (expect (str/includes? out "## Provenance"))
        (expect (str/includes? out ref))
        (expect (str/includes? out "Guards: ok"))))))

;; -----------------------------------------------------------------------------
;; Conversation intents — current conversation focus only.
;; -----------------------------------------------------------------------------

(defdescribe foundation-gate-test
  (it "creates focused intent state and reports an open required gate"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          e (assoc (env s conversation-id)
              :current-conversation-turn-id-atom (atom conversation-turn-id))
          intent ((private-fn "foundation-issue-intent!") e {:title "Ship it" :rationale "User asked."})
          slot   ((private-fn "foundation-proof-slot") e intent :verification)
          graph  ((private-fn "foundation-plan") e intent {:requires [slot]})
          plan   ((private-fn "foundation-issue-plan!") e {:intent-id (:id intent)
                                                           :summary "Plan"
                                                           :plan graph})
          gate   ((private-fn "foundation-issue-gate!") e {:plan-id (:id plan)
                                                           :proposition "Verification passes."
                                                           :expected-proof {:slots {slot {:required? true}}
                                                                            :guard [:exists [:slot slot :ref]]}})
          checks ((private-fn "foundation-intents") e)]
      (expect (= [(:id intent) :verification] slot))
      (expect (= [[:intent (:id intent)] :requires [:slot slot]] (first (:edges graph))))
      (expect (= "Verification passes." (:proposition gate)))
      (expect (false? (:success? checks)))
      (expect (str/includes? (:report checks) "Intent"))
      (expect (str/includes? (:report checks) "Plan"))
      (expect (str/includes? (:report checks) "Gate"))
      (expect (str/includes? (:report checks) "Verification passes."))
      (expect (some #(= :required-open-gate (:type %)) (:violations checks)))))

  (it "accepts a fulfilled intent only when canonical refs resolve"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          e (assoc (env s conversation-id)
              :current-conversation-turn-id-atom (atom conversation-turn-id))
          ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/1")]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(+ 1 2)"
                   :result 3
                   :execution-time-ms 1
                   :provenance (eval-provenance 1 1 1)}]})
      (let [intent ((private-fn "foundation-issue-intent!") e {:title "Ship it" :rationale "User asked."})
            plan   ((private-fn "foundation-issue-plan!") e {:intent-id (:id intent)
                                                             :summary "Plan"})
            gate   ((private-fn "foundation-issue-gate!") e {:plan-id (:id plan)
                                                             :proposition "Verification passes."
                                                             :expected-proof {:slots {}}})]
        ((private-fn "foundation-prove-gate!") e (:id gate)
                                               {:summary "Observed verification."
                                                :refs [ref]})
        ((private-fn "foundation-fulfill-intent!") e (:id intent)
                                                   {:summary "Done."
                                                    :refs [ref]})
        (let [checks ((private-fn "foundation-intents") e)]
          (expect (true? (:success? checks)))
          (expect (str/includes? (:report checks) ref))))))

  (it "attests gate and intent through evidence bundles instead of legacy proof blobs"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          proof-ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/1")
          close-ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/2")
          e (env s conversation-id)
          intent (vis/db-store-intent! s {:conversation-turn-id conversation-turn-id
                                          :title "Attested closure"
                                          :rationale "Exercise attestation helpers."})
          plan (vis/db-store-plan! s {:intent-id (:id intent)
                                      :summary "Attested plan"})
          gate (vis/db-store-gate! s {:plan-id (:id plan)
                                      :question "Verified?"})
          slot-owner (random-uuid)]
      (doseq [ref [proof-ref close-ref]]
        (vis/db-store-provenance-event! s {:conversation-id conversation-id
                                           :conversation-turn-id conversation-turn-id
                                           :ref ref
                                           :kind :tool
                                           :op :v/bash
                                           :status :done
                                           :payload {:result {:exit 0}}}))
      ((private-fn "foundation-attest-gate!") e (:id gate)
                                              {:kind :gate/proven
                                               :reason "Gate accepted."
                                               :requirements [{:evidence/slot [slot-owner :proof]
                                                               :evidence/from-ref proof-ref
                                                               :evidence/extract [:result :exit]
                                                               :evidence/guard [:= [:value] 0]
                                                               :event/kind :tool
                                                               :event/op :v/bash}]})
      ((private-fn "foundation-attest-intent!") e (:id intent)
                                                {:kind :intent/fulfilled
                                                 :summary "Done."
                                                 :requirements [{:evidence/slot [slot-owner :closure]
                                                                 :evidence/from-ref close-ref
                                                                 :evidence/extract [:result :exit]
                                                                 :evidence/guard [:= [:value] 0]
                                                                 :event/kind :tool
                                                                 :event/op :v/bash}]})
      (let [state (vis/db-intents s {:conversation-turn-id conversation-turn-id})
            audit ((private-fn "foundation-audit") e)]
        (expect (= :fulfilled (-> state :intents first :status)))
        (expect (= true (:success? audit)))
        (expect (= 2 (get-in audit [:counts :attestations]))))))

  (it "proof-checks reports what each gate asked for, what proof was given, and renders <proofs> meat"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          e (assoc (env s conversation-id)
              :current-conversation-turn-id-atom (atom conversation-turn-id))
          ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/1")]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(= 0 (:exit result))"
                   :result true
                   :execution-time-ms 1
                   :provenance (eval-provenance 1 1 1)}]})
      (let [intent ((private-fn "foundation-issue-intent!") e {:title "Ship it" :rationale "User asked."})
            slot   ((private-fn "foundation-proof-slot") e intent :verification)
            plan   ((private-fn "foundation-issue-plan!") e {:intent-id (:id intent)
                                                             :summary "Run deterministic verification."})
            gate   ((private-fn "foundation-issue-gate!") e {:plan-id (:id plan)
                                                             :proposition "Verification passes."
                                                             :expected-proof {:slots {slot {:required? true
                                                                                            :description "Verification observation."}}
                                                                              :guard [:exists [:slot slot :ref]]}})]
        ((private-fn "foundation-prove-gate!") e (:id gate)
                                               {:summary "Observed verification."
                                                :refs [ref]
                                                :slots {slot {:ref ref :exit-code 0}}})
        ((private-fn "foundation-fulfill-intent!") e (:id intent)
                                                   {:summary "Done."
                                                    :refs [ref]})
        (let [checks ((private-fn "foundation-proof-checks") e)
              gate-check (first (:gates checks))
              out ((private-fn "foundation-proofs") e checks)
              event (introspection/foundation-provenance-event e ref)]
          (expect (= ref (:ref event)))
          (expect (true? (:success? checks)))
          (expect (= "Verification passes." (:asked gate-check)))
          (expect (= {slot {:ref ref :exit-code 0}} (get-in gate-check [:given :slots])))
          (expect (= [] (:violations checks)))
          (expect (str/includes? out "<proofs>"))
          (expect (str/includes? out "Proofs · OK"))
          (expect (str/includes? out "- Intent: `"))
          (expect (str/includes? out "> Ship it"))
          (expect (str/includes? out "- Asked: Verification passes."))
          (expect (str/includes? out "- Expected slots:"))
          (expect (str/includes? out "- Given refs:"))
          (expect (str/includes? out (str "[`" ref "`](vis-provenance://" ref ")")))
          (expect (str/includes? out "## What happened"))
          (expect (str/includes? out ref))))))

  (it "proof-checks rejects stale candidate refs instead of trusting proof-looking prose"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          e (assoc (env s conversation-id)
              :current-conversation-turn-id-atom (atom conversation-turn-id))
          stale-ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/99/block/1")
          intent ((private-fn "foundation-issue-intent!") e {:title "Ship it" :rationale "User asked."})
          plan   ((private-fn "foundation-issue-plan!") e {:intent-id (:id intent)
                                                           :summary "Plan"})
          gate   ((private-fn "foundation-issue-gate!") e {:plan-id (:id plan)
                                                           :proposition "Verification passes."
                                                           :expected-proof {:slots {}}})]
      ((private-fn "foundation-offer-proof!") e {:gate-id (:id gate) :refs [stale-ref]})
      (let [checks ((private-fn "foundation-proof-checks") e)
            gate-check (first (:gates checks))
            out ((private-fn "foundation-proofs") e checks)]
        (expect (false? (:success? checks)))
        (expect (some #(= :unobserved-ref (:type %)) (:violations checks)))
        (expect (false? (:observed? (first (:ref-checks gate-check)))))
        (expect (str/includes? out "Proofs · NEEDS WORK"))
        (expect (str/includes? out stale-ref)))))

  (it "proof-checks rejects refs that only observe intent/proof bookkeeping"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          e (assoc (env s conversation-id)
              :current-conversation-turn-id-atom (atom conversation-turn-id))
          ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/1")]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(v/intents)"
                   :result {:success? true}
                   :execution-time-ms 1
                   :provenance (eval-provenance 1 1 1)}]})
      (let [intent ((private-fn "foundation-issue-intent!") e {:title "Explain TUI auth bug"
                                                               :rationale "User asked."})
            plan   ((private-fn "foundation-issue-plan!") e {:intent-id (:id intent)
                                                             :summary "Plan"})
            gate   ((private-fn "foundation-issue-gate!") e {:plan-id (:id plan)
                                                             :proposition "Source has been inspected."
                                                             :expected-proof {:slots {}}})]
        ((private-fn "foundation-prove-gate!") e (:id gate)
                                               {:summary "Bookkeeping observed."
                                                :refs [ref]})
        ((private-fn "foundation-fulfill-intent!") e (:id intent)
                                                   {:summary "Done."
                                                    :refs [ref]})
        (let [checks ((private-fn "foundation-proof-checks") e)
              gate-check (first (:gates checks))]
          (expect (false? (:success? checks)))
          (expect (some #(= :non-evidence-ref (:type %)) (:violations checks)))
          (expect (false? (:success? (first (:ref-checks gate-check)))))
          (expect (false? (:evidence? (first (:ref-checks gate-check)))))))))

  (it "accepts :summary as an abandon-intent! reason alias at the foundation boundary"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          e (assoc (env s conversation-id)
              :current-conversation-turn-id-atom (atom conversation-turn-id))
          ref (str "turn/" (subs (str conversation-turn-id) 0 8) "/iteration/1/block/1")]
      (db-store-iteration! s conversation-turn-id
        {:blocks [{:id 0
                   :code "(v/needs-input \"Paste the ideas to review.\")"
                   :result :vis/system
                   :execution-time-ms 1
                   :rendering-kind :vis/system}]})
      (let [intent ((private-fn "foundation-issue-intent!") e {:title "Review ideas"
                                                               :rationale "User asked."})
            abandoned ((private-fn "foundation-abandon-intent!") e (:id intent)
                                                                 {:summary "Cannot review ideas until user provides them."
                                                                  :refs [ref]})]
        (expect (= :abandoned (:status abandoned)))
        (expect (= "Cannot review ideas until user provides them."
                  (:abandonment-reason abandoned))))))

  (it "rejects compact refs for gate proof"
    (let [s (h/store)
          {:keys [conversation-id conversation-turn-id]} (bootstrap s)
          e (assoc (env s conversation-id)
              :current-conversation-turn-id-atom (atom conversation-turn-id))
          intent ((private-fn "foundation-issue-intent!") e {:title "Ship it" :rationale "User asked."})
          plan   ((private-fn "foundation-issue-plan!") e {:intent-id (:id intent)
                                                           :summary "Plan"})
          gate   ((private-fn "foundation-issue-gate!") e {:plan-id (:id plan)
                                                           :proposition "Verification passes."
                                                           :expected-proof {:slots {}}})
          thrown (try
                   ((private-fn "foundation-prove-gate!") e (:id gate)
                                                          {:summary "Wrong ref."
                                                           :refs ["i1.1"]})
                   nil
                   (catch Exception ex ex))]
      (expect (some? thrown))
      (expect (str/includes? (ex-message thrown) "canonical")))))

;; -----------------------------------------------------------------------------
;; (v/extensions), (v/extension-docs ...), (v/extension-doc ...),
;; and (v/extension-readme ...) — catalog + abstracts + bodies.
;; -----------------------------------------------------------------------------

(defdescribe foundation-extensions-catalog-test
  (it "includes the unified `vis-foundation` extension itself with its declared docs and abstracts"
    (let [extensions ((private-fn "foundation-extensions") {})
          this       (some #(when (= 'com.blockether.vis.ext.foundation.core (:namespace %)) %)
                       extensions)]
      (expect (some? this))
      (expect (= 'v (:alias this)))
      (expect (= 'v (:registry-id this)))
      (expect (= "vis" (:owner this)))
      (expect (string? (:license this)))
      (expect (vector? (:source-paths this)))
      (expect (vector? (:symbols this)))
      ;; Unified vis ext bundles introspection + editing + environment
      ;; under one alias. Spot-check one symbol from each area.
      (expect (contains? (set (:symbols this)) 'inspect))
      (expect (contains? (set (:symbols this)) 'report))
      (expect (contains? (set (:symbols this)) 'extensions))
      (expect (contains? (set (:symbols this)) 'cat))
      (expect (contains? (set (:symbols this)) 'snapshot))
      ;; :docs is a vector of summary maps. Each summary carries the
      ;; structured descriptor fields except :content; :content lives
      ;; on the full descriptor returned by (v/extension-doc ...).
      (let [docs (:docs this)
            readme (some #(when (= "README.md" (:name %)) %) docs)]
        (expect (vector? docs))
        (expect (some? readme))
        (expect (string? (:description readme)))
        (expect (pos? (count (:description readme))))
        (expect (vector? (:links readme)))
        (expect (vector? (:reflinks readme)))
        ;; Summaries omit :content -- catalog stays small.
        (expect (not (contains? readme :content))))))

  (it "every entry carries :namespace, source markers, :symbols, and :docs"
    (let [extensions ((private-fn "foundation-extensions") {})]
      (expect (every? :namespace extensions))
      (expect (every? #(contains? % :source-paths) extensions))
      (expect (every? #(contains? % :source-mtime-max) extensions))
      (expect (every? #(contains? % :source-hash-sha256) extensions))
      (expect (every? #(contains? % :symbols) extensions))
      (expect (every? #(contains? % :docs) extensions)))))

(defdescribe foundation-extension-docs-test
  (it "single-arg form returns summaries for a registered extension"
    (let [docs ((private-fn "foundation-extension-docs") {} 'v)]
      (expect (vector? docs))
      (expect (= #{"README.md"} (set (map :name docs))))
      (expect (every? #(string? (:description %)) docs))
      (expect (every? #(vector? (:links %)) docs))
      (expect (every? #(vector? (:reflinks %)) docs))))

  (it "summaries do NOT include :content (catalog stays small)"
    (let [docs ((private-fn "foundation-extension-docs") {} 'v)]
      (expect (every? #(not (contains? % :content)) docs))))

  (it "reflinks vec is present (potentially empty in the unified-vis world)"
    ;; Pre-merge `vis-common-foundation` had cross-ext links to
    ;; `vis-common-editing`'s README and vice-versa. After the merge
    ;; everything lives under one ext id (`vis`), so cross-ext
    ;; reflinks for THIS package are empty by construction. The
    ;; field still has to be a vec, never nil.
    (let [readme (first ((private-fn "foundation-extension-docs") {} 'v))]
      (expect (vector? (:reflinks readme)))))

  (it "no-arg form returns the full registry keyed by id symbol"
    (let [registry ((private-fn "foundation-extension-docs") {})]
      (expect (map? registry))
      (expect (contains? registry 'v))))

  (it "unknown reference returns nil"
    (expect (nil? ((private-fn "foundation-extension-docs") {} 'no.such.extension)))))

(defdescribe foundation-extension-doc-test
  (it "returns the canonical README descriptor by id symbol"
    (let [doc ((private-fn "foundation-extension-doc") {} 'v)]
      (expect (map? doc))
      (expect (= "README.md" (:name doc)))
      (expect (string? (:description doc)))
      (expect (string? (:content doc)))
      (expect (str/includes? (:content doc) "# vis-foundation"))
      (expect (vector? (:links doc)))
      (expect (pos? (count (:links doc))))
      (expect (vector? (:reflinks doc)))))

  (it "resolves extension docs by keyword, string, full namespace, and alias namespace"
    (doseq [reference [:v "v" 'com.blockether.vis.ext.foundation.core 'vis.ext.v]]
      (let [doc ((private-fn "foundation-extension-doc") {} reference)]
        (expect (= "README.md" (:name doc)))
        (expect (str/includes? (:content doc) "# vis-foundation")))))

  (it "links carry author-declared targets and contexts"
    (let [doc   ((private-fn "foundation-extension-doc") {} 'v)
          links (:links doc)]
      ;; The unified vis-foundation README links to the RLM paper
      ;; (URL) and the prompt-assembler source file (file). No
      ;; cross-ext doc links anymore (everything is one ext now).
      (expect (some #(some? (:url %)) links))
      (expect (some #(some? (:file %)) links))))

  (it "returns nil for an unknown extension reference"
    (expect (nil? ((private-fn "foundation-extension-doc") {} 'no.such.ext)))))

(defdescribe foundation-namespace-docs-test
  (it "single-arg form returns sandbox symbol docs for a registered extension namespace"
    (let [docs ((private-fn "foundation-namespace-docs") {} 'v)
          file-link (some #(when (= 'file-link (:name %)) %) docs)]
      (expect (vector? docs))
      (expect (some? file-link))
      (expect (= 'v (:extension-id file-link)))
      (expect (= 'v (:extension-alias file-link)))
      (expect (= 'com.blockether.vis.ext.foundation.core (:extension-namespace file-link)))
      (expect (= 'v/file-link (:symbol file-link)))
      (expect (= :fn (:kind file-link)))
      (expect (str/includes? (:doc file-link) "Workspace file link"))
      (expect (= '([path] [path line]) (:arglists file-link)))
      (expect (some #(str/includes? % "v/file-link") (:examples file-link)))))

  (it "no-arg form returns the namespace-doc registry keyed by extension id"
    (let [registry ((private-fn "foundation-namespace-docs") {})]
      (expect (map? registry))
      (expect (contains? registry 'v))
      (expect (some #(= 'inspect (:name %)) (get registry 'v)))))

  (it "unknown extension reference returns nil"
    (expect (nil? ((private-fn "foundation-namespace-docs") {} 'no.such.extension)))))

(defdescribe foundation-symbol-doc-test
  (it "returns one sandbox symbol descriptor by extension ref and symbol name"
    (let [doc ((private-fn "foundation-symbol-doc") {} 'v 'file-link)]
      (expect (= 'file-link (:name doc)))
      (expect (= 'v/file-link (:symbol doc)))
      (expect (= 'v (:extension-id doc)))
      (expect (= 'v (:extension-alias doc)))
      (expect (str/includes? (:doc doc) "Workspace file link"))
      (expect (= '([path] [path line]) (:arglists doc)))))

  (it "accepts a single qualified alias/symbol reference"
    (let [doc ((private-fn "foundation-symbol-doc") {} 'v/file-link)]
      (expect (= 'file-link (:name doc)))
      (expect (= 'v/file-link (:symbol doc)))))

  (it "accepts keyword and string references"
    (let [doc ((private-fn "foundation-symbol-doc") {} :v "link")]
      (expect (= 'link (:name doc)))
      (expect (= 'v/link (:symbol doc)))))

  (it "keeps manifest extension docs and sandbox symbol docs separate"
    (let [extension-doc ((private-fn "foundation-extension-doc") {} 'v)
          symbol-doc    ((private-fn "foundation-symbol-doc") {} 'v 'file-link)]
      (expect (= "README.md" (:name extension-doc)))
      (expect (str/includes? (:content extension-doc) "# vis-foundation"))
      (expect (= 'file-link (:name symbol-doc)))
      (expect (str/includes? (:doc symbol-doc) "Workspace file link"))))

  (it "documents arglists for both extension-doc and symbol-doc"
    (let [extension-doc-doc ((private-fn "foundation-symbol-doc") {} 'v 'extension-doc)
          symbol-doc-doc    ((private-fn "foundation-symbol-doc") {} 'v 'symbol-doc)]
      (expect (= '([extension-ref]) (:arglists extension-doc-doc)))
      (expect (= '([qualified-symbol] [extension-ref symbol-name])
                (:arglists symbol-doc-doc)))
      (expect (some #(str/includes? % "v/extension-doc") (:examples extension-doc-doc)))
      (expect (some #(str/includes? % "v/symbol-doc") (:examples symbol-doc-doc)))))

  (it "returns nil for unknown extension or symbol"
    (expect (nil? ((private-fn "foundation-symbol-doc") {} 'no.such.extension 'file-link)))
    (expect (nil? ((private-fn "foundation-symbol-doc") {} 'v 'no-such-symbol)))
    (expect (nil? ((private-fn "foundation-symbol-doc") {} 'not-qualified)))))

(defdescribe foundation-extension-readme-test
  (it "resolves by id symbol"
    (let [text ((private-fn "foundation-extension-readme") {} 'v)]
      (expect (string? text))
      (expect (clojure.string/includes? text "v/inspect"))))

  (it "resolves by id keyword"
    (let [text ((private-fn "foundation-extension-readme") {} :v)]
      (expect (string? text))
      (expect (clojure.string/includes? text "v/report"))))

  (it "resolves by full extension namespace"
    (let [text ((private-fn "foundation-extension-readme") {} 'com.blockether.vis.ext.foundation.core)]
      (expect (string? text))
      (expect (clojure.string/includes? text "# vis-foundation"))))

  (it "resolves by alias-ns symbol"
    (let [text ((private-fn "foundation-extension-readme") {} 'vis.ext.v)]
      (expect (string? text))))

  (it "returns nil for an unknown extension reference"
    (expect (nil? ((private-fn "foundation-extension-readme") {} 'no.such.extension)))
    (expect (nil? ((private-fn "foundation-extension-readme") {} :nope)))
    (expect (nil? ((private-fn "foundation-extension-readme") {} nil)))))

;; -----------------------------------------------------------------------------
;; Failure modes — every fn must return nil/[], NEVER throw
;; -----------------------------------------------------------------------------

(defdescribe failure-mode-test
  (let [empty-result? #(or (nil? %) (and (coll? %) (empty? %)))]
    (it "returns nil-or-empty when DB is unreachable"
      (let [environment {:conversation-id "x"}]
        (expect (empty-result? ((private-fn "foundation-turn") environment)))
        (expect (empty-result? ((private-fn "foundation-conversation") environment)))
        (expect (empty-result? ((private-fn "foundation-conversations") environment)))
        (expect (empty-result? ((private-fn "foundation-conversation-forks") environment)))
        (expect (vector? ((private-fn "foundation-conversation-forks") environment)))
        (expect (empty-result? ((private-fn "foundation-conversation-forks") environment (random-uuid))))
        (expect (empty-result? ((private-fn "meta-turn-retries") environment (random-uuid))))
        (expect (vector? ((private-fn "meta-turn-retries") environment (random-uuid))))
        (expect (empty-result? ((private-fn "foundation-failures") environment)))
        (expect (vector? ((private-fn "foundation-failures") environment)))
        (expect (= 0 (:failure-count ((private-fn "foundation-diagnose") environment))))
        (expect (map? ((private-fn "foundation-inspect") environment)))
        (expect (string? ((private-fn "foundation-report") environment)))
        (expect (vector? ((private-fn "foundation-extensions") environment)))
        (expect (map? ((private-fn "foundation-extension-docs") environment)))
        (expect (nil? ((private-fn "foundation-extension-docs") environment 'no.such.ext)))
        (expect (nil? ((private-fn "foundation-extension-doc") environment 'no.such.ext)))
        (expect (nil? ((private-fn "foundation-extension-readme") environment 'no.such.ext)))))

    (it "returns nil-or-empty when conversation-id is missing"
      (let [s (h/store)
            environment {:db-info s}]
        (expect (empty-result? ((private-fn "foundation-turn") environment)))
        (expect (empty-result? ((private-fn "foundation-conversation") environment)))))))
