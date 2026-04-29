(ns com.blockether.vis.ext.foundation.introspection-test
  "Tests for the meta extension's consolidated API. Seven
   functions, each returning a map or vector. Each test bootstraps
   synthetic conversation + query + iteration rows in an in-memory
   SQLite DB, then invokes the impl fns directly with a fake env map."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as sdk]
   [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
   [lazytest.core :refer [defdescribe it expect]]))

;; The extension's core ns calls `register-global!` at load time;
;; required eagerly so the impl fns are interned before tests run.
(require '[com.blockether.vis.ext.foundation.introspection])

;; Populate the classpath docs registry once for the whole namespace.
;; The (foundation/extensions ...) / (foundation/extension-docs ...) / etc. tests
;; read from the registry that `discover-extensions!` produces by
;; merging every `META-INF/vis-extension/vis.edn` on the classpath.
;; Idempotent across runs (memoized inside the loader).
(sdk/discover-extensions!)

(h/use-mem-store!)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- bootstrap [store]
  (let [conversation-id (sdk/db-store-conversation! store
                          {:channel :tui :title "meta test"})
        query-id (sdk/db-store-query! store
                   {:parent-conversation-id conversation-id
                    :query "what's the plan?"
                    :status :running})]
    {:conversation-id conversation-id :query-id query-id}))

(defn- db-store-iteration!
  [store query-id {:keys [blocks thinking error]
                   :or {blocks []}}]
  (sdk/db-store-iteration! store
    (cond-> {:query-id    query-id
             :blocks blocks
             :duration-ms 100
             :llm-model   "test-model"
             :metadata    {}}
      thinking   (assoc :thinking thinking)
      error      (assoc :error error))))

(defn- env [store conversation-id]
  {:db-info store
   :conversation-id conversation-id
   :current-iteration-atom (atom 2)})

;; The impl fns are private — reach via the var registry so tests
;; don't depend on a public re-export.
(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.foundation.introspection" name))))

;; -----------------------------------------------------------------------------
;; (foundation/turn) — single rich snapshot of the current turn
;; -----------------------------------------------------------------------------

(defdescribe foundation-turn-test
  (it "returns nil when DB is unreachable"
    (expect (nil? ((private-fn "foundation-turn") {:conversation-id "x"}))))

  (it "returns a snapshot map with goal / status / iteration / cost / elapsed-ms"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          turn ((private-fn "foundation-turn") (env s conversation-id))]
      (expect (= "what's the plan?" (:goal turn)))
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

  (it "splits attempts and errors so callers don't have to filter twice"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                  {:id 1 :code "(boom)"  :error "boom" :execution-time-ms 1}]})
      (let [turn ((private-fn "foundation-turn") (env s conversation-id))]
        (expect (= 2 (count (:attempts turn))))
        (expect (= 1 (count (:errors turn))))
        (expect (= "boom" (-> turn :errors first :error))))))

  (it "surfaces the redundancy summary aggregated across iteration metadata"
    ;; The Phase 2-m metric lands in iteration.metadata as :dedup-saves
    ;; per iteration; (foundation/turn).:redundancy aggregates across iterations.
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (sdk/db-store-iteration! s
        {:query-id    query-id
         :blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                  {:id 1 :code "(grep \"X\")" :result [] :execution-time-ms 1}]
         :duration-ms 100
         :llm-model   "test-model"
         :metadata    {:dedup-saves 0
                       :expression-redundancy-fraction 0.0}})
      (sdk/db-store-iteration! s
        {:query-id    query-id
         :blocks [{:id 0 :code "(grep \"X\")" :result [] :execution-time-ms 1}]
         :duration-ms 100
         :llm-model   "test-model"
         :metadata    {:dedup-saves 1
                       :expression-redundancy-fraction 1.0}})
      (let [turn ((private-fn "foundation-turn") (env s conversation-id))
            redundancy (:redundancy turn)]
        (expect (= 1 (:duplicate-count redundancy)))
        (expect (= 3 (:total-count redundancy)))
        (expect (< (Math/abs (- (/ 1.0 3.0) (:fraction redundancy))) 1e-9))))))

;; -----------------------------------------------------------------------------
;; (foundation/conversation [id]) — current or specific conversation
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
          other (sdk/db-store-conversation! s {:channel :telegram :title "other"})
          conversation ((private-fn "foundation-conversation") (env s conversation-id) other)]
      (expect (= other (:id conversation)))
      (expect (= :telegram (:channel conversation)))
      (expect (= 0 (:turn-count conversation)))))

  (it "turns include goal/outcome/answer when present"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (sdk/db-update-query! s query-id
        {:answer "42" :iteration-count 1 :duration-ms 50 :status :done
         :prior-outcome :complete})
      (let [conversation ((private-fn "foundation-conversation") (env s conversation-id))
            turn (first (:turns conversation))]
        (expect (= "what's the plan?" (:goal turn)))
        (expect (= "42" (:answer turn)))
        (expect (= :complete (:outcome turn))))))

  (it "auto-excludes the in-flight turn (= CURRENT_QUERY_ID) from the current conversation"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          env-with-in-flight (assoc (env s conversation-id)
                               :current-query-id-atom (atom query-id))
          conversation ((private-fn "foundation-conversation") env-with-in-flight)]
      (expect (= 0 (:turn-count conversation))) ; bootstrap creates exactly 1 turn, the in-flight one
      (expect (empty? (:turns conversation)))
      (expect (= query-id (:in-flight-turn-id conversation)))))

  (it "does NOT filter when inspecting a foreign conversation"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          other (sdk/db-store-conversation! s {:channel :telegram :title "other"})
          env-with-in-flight (assoc (env s conversation-id)
                               :current-query-id-atom (atom query-id))
          conversation ((private-fn "foundation-conversation") env-with-in-flight other)]
      ;; Foreign conversation untouched: no in-flight-turn-id, original turns kept.
      (expect (= other (:id conversation)))
      (expect (= 0 (:turn-count conversation)))
      (expect (nil? (:in-flight-turn-id conversation)))))

  (it "does NOT filter when no turn is in flight (current-query-id-atom = nil)"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          conversation ((private-fn "foundation-conversation") (env s conversation-id))]
      (expect (= 1 (:turn-count conversation)))
      (expect (nil? (:in-flight-turn-id conversation))))))

;; -----------------------------------------------------------------------------
;; (foundation/conversations [channel]) — list across one or all channels
;; -----------------------------------------------------------------------------

(defdescribe foundation-conversations-test
  (it "no-arg form scans every known channel"
    (let [s (h/store)
          a (sdk/db-store-conversation! s {:channel :tui :title "vis-a"})
          b (sdk/db-store-conversation! s {:channel :telegram :title "tg-b"})
          all ((private-fn "foundation-conversations") (env s a))
          ids (set (map :id all))]
      (expect (contains? ids a))
      (expect (contains? ids b))))

  (it "channel-arg form filters to one channel"
    (let [s (h/store)
          a (sdk/db-store-conversation! s {:channel :tui :title "vis-a"})
          _ (sdk/db-store-conversation! s {:channel :telegram :title "tg-b"})
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
;; (foundation/var-history sym [conversation-id])
;; -----------------------------------------------------------------------------

(defdescribe foundation-var-history-test
  (it "returns [] for an unknown sym"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "foundation-var-history") (env s conversation-id) 'never-defined)))))

  (it "tolerates string sym names"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "foundation-var-history") (env s conversation-id) "still-undefined")))))

  (it "explicit conversation-id form queries a different conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          other (sdk/db-store-conversation! s {:channel :tui :title "other"})]
      (expect (= [] ((private-fn "foundation-var-history") (env s conversation-id) 'foo other))))))

;; -----------------------------------------------------------------------------
;; (foundation/conversation-forks [conversation-id]) — fork tree introspection.
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

  (it "surfaces every fork with parent links and per-state query counts"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (sdk/db-fork-conversation! s conversation-id {:title "Branch A"})
      (sdk/db-fork-conversation! s conversation-id {:title "Branch B"})
      (let [rows ((private-fn "foundation-conversation-forks") (env s conversation-id))]
        (expect (= 3 (count rows)))
        (expect (= [0 1 2] (mapv :version rows)))
        (expect (nil? (:parent-state-id (first rows))))
        (expect (every? :parent-state-id (drop 1 rows))))))

  (it "explicit conversation-id form scans a different conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          other (sdk/db-store-conversation! s {:channel :tui :title "other"})]
      (sdk/db-fork-conversation! s other {:title "Other branch"})
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
;; (foundation/query-retries query-id) — retry history introspection.
;; -----------------------------------------------------------------------------

(defdescribe meta-query-retries-test
  (it "returns the v0 row for a query with no retries"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          rows ((private-fn "meta-query-retries") (env s conversation-id) query-id)]
      (expect (vector? rows))
      (expect (= 1 (count rows)))
      (expect (= 0 (:version (first rows))))
      (expect (nil? (:forked-from-query-state-id (first rows))))))

  (it "surfaces every retry in version order with forked-from links"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (sdk/db-retry-query! s query-id {:status :running :model "claude-4"})
      (sdk/db-retry-query! s query-id {:status :done    :model "gpt-4o"})
      (let [rows ((private-fn "meta-query-retries") (env s conversation-id) query-id)]
        (expect (= 3 (count rows)))
        (expect (= [0 1 2] (mapv :version rows)))
        (expect (every? :forked-from-query-state-id (drop 1 rows))))))

  (it "returns [] (vector, never nil) for an unknown query-id"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          rows ((private-fn "meta-query-retries") (env s conversation-id) (random-uuid))]
      (expect (vector? rows))
      (expect (= [] rows))))

  (it "returns [] (vector, never nil) when env or query-id missing"
    (expect (= [] ((private-fn "meta-query-retries") {} (random-uuid))))
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "meta-query-retries") (env s conversation-id) nil))))))

;; -----------------------------------------------------------------------------
;; (foundation/find-attempts pattern [conversation-id])
;; -----------------------------------------------------------------------------

(defdescribe foundation-find-attempts-test
  (it "one-arg form regex-searches the current turn"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(+ 1 2)"        :result 3 :execution-time-ms 1}
                  {:id 1 :code "(grep \"FOO\")" :result [] :execution-time-ms 1}
                  {:id 2 :code "(grep \"BAR\")" :result [] :execution-time-ms 1}]})
      (let [hits ((private-fn "foundation-find-attempts") (env s conversation-id) "grep")]
        (expect (= 2 (count hits)))
        (expect (every? #(re-find #"grep" (:code %)) hits))
        ;; :turn-id resolves to the latest query of the conversation — a
        ;; UUID identifying the turn the attempt belongs to.
        (expect (= query-id (-> hits first :turn-id)))
        (expect (every? #(= query-id (:turn-id %)) hits)))))

  (it "accepts a Pattern object directly"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(defn foo [x] x)" :result nil :execution-time-ms 1}]})
      (let [hits ((private-fn "foundation-find-attempts") (env s conversation-id) #"\bdefn\b")]
        (expect (= 1 (count hits))))))

  (it "two-arg form scans every turn of the given conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          q2 (sdk/db-store-query! s
               {:parent-conversation-id conversation-id
                :query "second turn" :status :running})]
      ;; Leave q1 empty; fill q2.
      (db-store-iteration! s q2
        {:blocks [{:id 0 :code "(grep \"target\")" :result [] :execution-time-ms 1}]})
      (let [hits ((private-fn "foundation-find-attempts") (env s conversation-id) "grep" conversation-id)]
        (expect (= 1 (count hits))))))

  (it "returns [] (vector, never nil) when the current turn has no matches"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]})
      (let [hits ((private-fn "foundation-find-attempts") (env s conversation-id) #"Unmatched delimiter")]
        ;; Defensive default: agents commonly do `(first hits)` /
        ;; `(:code (first hits))`. Returning [] keeps that path nil-safe
        ;; instead of crashing improvised helpers downstream.
        (expect (vector? hits))
        (expect (= [] hits)))))

  (it "returns [] (vector, never nil) when the env is missing handles"
    (let [hits ((private-fn "foundation-find-attempts") {} "anything")]
      (expect (vector? hits))
      (expect (= [] hits)))
    (let [hits ((private-fn "foundation-find-attempts") {} "anything" "some-uuid")]
      (expect (vector? hits))
      (expect (= [] hits)))))

;; -----------------------------------------------------------------------------
;; (foundation/find-attempts-everywhere pattern) — cross-conversation regex search.
;; -----------------------------------------------------------------------------

(defdescribe foundation-find-attempts-everywhere-test
  (it "scans every conversation in the DB"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          other-conversation-id (sdk/db-store-conversation! s
                                  {:channel :tui :title "other turn"})
          other-query-id (sdk/db-store-query! s
                           {:parent-conversation-id other-conversation-id
                            :query "other goal" :status :running})]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(grep \"alpha\")" :result [] :execution-time-ms 1}]})
      (db-store-iteration! s other-query-id
        {:blocks [{:id 0 :code "(grep \"beta\")" :result [] :execution-time-ms 1}]})
      (let [hits ((private-fn "foundation-find-attempts-everywhere") (env s conversation-id) #"grep")]
        (expect (= 2 (count hits)))
        ;; Each hit carries the originating :conversation-id so callers
        ;; can drill in without a second meta call.
        (expect (every? :conversation-id hits))
        (expect (= #{conversation-id other-conversation-id}
                  (set (map :conversation-id hits)))))))

  (it "returns [] (vector, never nil) when nothing matches"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]})
      ;; Reproduces the exact diagnose-loop failure: agent searches for
      ;; an error that doesn't exist in the DB, expects an empty vec
      ;; back, must NOT receive nil.
      (let [hits ((private-fn "foundation-find-attempts-everywhere")
                  (env s conversation-id) #"Unmatched delimiter")]
        (expect (vector? hits))
        (expect (= [] hits)))))

  (it "returns [] (vector, never nil) when the env has no :db-info"
    (let [hits ((private-fn "foundation-find-attempts-everywhere") {} #"x")]
      (expect (vector? hits))
      (expect (= [] hits)))))

;; -----------------------------------------------------------------------------
;; (foundation/failures) and (foundation/diagnose) — no raw SQLite needed for triage
;; -----------------------------------------------------------------------------

(defdescribe meta-failure-diagnostics-test
  (it "normalizes provider schema rejections with raw previews"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
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
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks [{:id 0
                   :code "(vis/rg \"foo\\|bar\\|baz\" \"x\")"
                   :error "Unsupported escape character: \\|"
                   :execution-time-ms 1}
                  {:id 1
                   :code "(vis/patch [{:path \"render.clj\" :search \"x\" :replace \"y\"}])"
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
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks
         (into
           (mapv (fn [i]
                   {:id i
                    :code (str "(vis/cat \"src/tui/file" i ".clj\")")
                    :error (str "File not found: /Users/x/vis/src/tui/file" i ".clj")
                    :execution-time-ms 1})
             (range 6))
           [{:id 6 :code "(vis/cat \"src/tui/render.clj\")"
             :error "Path not found: /Users/x/vis/src/tui"
             :execution-time-ms 1}
            {:id 7 :code "(vis/cat \"src/tui\")"
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
          {:keys [conversation-id query-id]} (bootstrap s)
          second-query-id (sdk/db-store-query! s
                            {:parent-conversation-id conversation-id
                             :query "second turn"
                             :status :running})]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(vis/rg \"x\\|y\" \"z\")"
                   :error "Unsupported escape character: \\|"
                   :execution-time-ms 1}]})
      (db-store-iteration! s second-query-id
        {:blocks [{:id 0 :code "(vis/patch [{:path \"x\"}])"
                   :error "SEARCH block 1 not found in x"
                   :execution-time-ms 1}]})
      (let [failures ((private-fn "foundation-failures") (env s conversation-id) conversation-id)]
        (expect (= 2 (count failures)))
        (expect (every? :turn-id failures))
        (expect (every? :goal failures)))))

  (it "returns [] (vector, never nil) when the current turn has no failures"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]})
      (let [failures ((private-fn "foundation-failures") (env s conversation-id))]
        (expect (vector? failures))
        (expect (= [] failures))))))

;; -----------------------------------------------------------------------------
;; (foundation/failures-everywhere) — cross-conversation failure scan.
;; -----------------------------------------------------------------------------

(defdescribe foundation-failures-everywhere-test
  (it "aggregates failures across every conversation in the DB"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          other-conversation-id (sdk/db-store-conversation! s
                                  {:channel :tui :title "other turn"})
          other-query-id (sdk/db-store-query! s
                           {:parent-conversation-id other-conversation-id
                            :query "other goal" :status :running})]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(vis/rg \"x\\|y\" \"z\")"
                   :error "Unsupported escape character: \\|"
                   :execution-time-ms 1}]})
      (db-store-iteration! s other-query-id
        {:blocks [{:id 0 :code "(vis/patch [{:path \"x\"}])"
                   :error "SEARCH block 1 not found in x"
                   :execution-time-ms 1}]})
      (let [failures ((private-fn "foundation-failures-everywhere") (env s conversation-id))]
        (expect (= 2 (count failures)))
        (expect (every? :conversation-id failures))
        (expect (every? :turn-id failures))
        (expect (every? :goal failures))
        (expect (= #{conversation-id other-conversation-id}
                  (set (map :conversation-id failures)))))))

  (it "returns [] (vector, never nil) when no failures exist"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:blocks [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}]})
      (let [failures ((private-fn "foundation-failures-everywhere") (env s conversation-id))]
        (expect (vector? failures))
        (expect (= [] failures)))))

  (it "returns [] (vector, never nil) when the env has no :db-info"
    (let [failures ((private-fn "foundation-failures-everywhere") {})]
      (expect (vector? failures))
      (expect (= [] failures)))))

;; -----------------------------------------------------------------------------
;; (foundation/extensions), (foundation/extension-docs ...), (foundation/extension-doc ...),
;; and (foundation/extension-readme ...) — catalog + abstracts + bodies.
;; -----------------------------------------------------------------------------

(defdescribe foundation-extensions-catalog-test
  (it "includes the meta extension itself with its declared docs and abstracts"
    (let [extensions ((private-fn "foundation-extensions") {})
          this       (some #(when (= 'com.blockether.vis.ext.foundation.introspection (:namespace %)) %)
                       extensions)]
      (expect (some? this))
      (expect (= 'foundation (:alias this)))
      (expect (vector? (:symbols this)))
      (expect (contains? (set (:symbols this)) 'extensions))
      (expect (contains? (set (:symbols this)) 'extension-docs))
      (expect (contains? (set (:symbols this)) 'extension-doc))
      (expect (contains? (set (:symbols this)) 'extension-readme))
      ;; :docs is a vector of summary maps. Each summary carries the
      ;; structured descriptor fields except :content; :content lives
      ;; on the full descriptor returned by (foundation/extension-doc ...).
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

  (it "every entry carries :namespace, :symbols, and :docs"
    (let [extensions ((private-fn "foundation-extensions") {})]
      (expect (every? :namespace extensions))
      (expect (every? #(contains? % :symbols) extensions))
      (expect (every? #(contains? % :docs) extensions)))))

(defdescribe foundation-extension-docs-test
  (it "single-arg form returns summaries for a registered extension"
    (let [docs ((private-fn "foundation-extension-docs") {} 'foundation)]
      (expect (vector? docs))
      (expect (= #{"README.md"} (set (map :name docs))))
      (expect (every? #(string? (:description %)) docs))
      (expect (every? #(vector? (:links %)) docs))
      (expect (every? #(vector? (:reflinks %)) docs))))

  (it "summaries do NOT include :content (catalog stays small)"
    (let [docs ((private-fn "foundation-extension-docs") {} 'foundation)]
      (expect (every? #(not (contains? % :content)) docs))))

  (it "reflinks reflect cross-extension authored links"
    ;; vis-foundation links to vis (companion) and vis-foundation
    ;; links back to meta -- so each gets one inbound reflink.
    (let [meta-readme (first ((private-fn "foundation-extension-docs") {} 'foundation))
          vis-readme  (first ((private-fn "foundation-extension-docs") {} 'vis))]
      (expect (some #(= 'vis  (:from-id %)) (:reflinks meta-readme)))
      (expect (some #(= 'foundation (:from-id %)) (:reflinks vis-readme)))))

  (it "no-arg form returns the full registry keyed by id symbol"
    (let [registry ((private-fn "foundation-extension-docs") {})]
      (expect (map? registry))
      (expect (contains? registry 'foundation))))

  (it "unknown reference returns nil"
    (expect (nil? ((private-fn "foundation-extension-docs") {} 'no.such.extension)))))

(defdescribe foundation-extension-doc-test
  (it "returns the full descriptor map for a declared doc"
    (let [doc ((private-fn "foundation-extension-doc") {} 'foundation "README.md")]
      (expect (map? doc))
      (expect (= "README.md" (:name doc)))
      (expect (string? (:description doc)))
      (expect (string? (:content doc)))
      (expect (str/includes? (:content doc) "# Meta extension"))
      (expect (vector? (:links doc)))
      (expect (pos? (count (:links doc))))
      (expect (vector? (:reflinks doc)))))

  (it "links carry author-declared targets and contexts"
    (let [doc   ((private-fn "foundation-extension-doc") {} 'foundation "README.md")
          links (:links doc)]
      ;; meta links to vis README (cross-ext doc), the RLM paper
      ;; (URL), and the loader source file (file).
      (expect (some #(= ['vis "README.md"] [(:to-id %) (:to-doc %)]) links))
      (expect (some #(some? (:url %)) links))
      (expect (some #(some? (:file %)) links))))

  (it "returns nil for an unknown doc name"
    (expect (nil? ((private-fn "foundation-extension-doc") {} 'foundation "NOPE.md"))))

  (it "returns nil for an unknown extension reference"
    (expect (nil? ((private-fn "foundation-extension-doc") {} 'no.such.ext "README.md")))))

(defdescribe foundation-extension-readme-test
  (it "resolves by id symbol"
    (let [text ((private-fn "foundation-extension-readme") {} 'foundation)]
      (expect (string? text))
      (expect (clojure.string/includes? text "foundation/extensions"))))

  (it "resolves by id keyword"
    (let [text ((private-fn "foundation-extension-readme") {} :foundation)]
      (expect (string? text))
      (expect (clojure.string/includes? text "foundation/extension-readme"))))

  (it "resolves by full extension namespace"
    (let [text ((private-fn "foundation-extension-readme") {} 'com.blockether.vis.ext.foundation.introspection)]
      (expect (string? text))
      (expect (clojure.string/includes? text "# Meta extension"))))

  (it "resolves by alias-ns symbol"
    (let [text ((private-fn "foundation-extension-readme") {} 'vis.ext.foundation)]
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
        (expect (empty-result? ((private-fn "foundation-var-history") environment 'foo)))
        (expect (empty-result? ((private-fn "foundation-conversation-forks") environment)))
        (expect (vector? ((private-fn "foundation-conversation-forks") environment)))
        (expect (empty-result? ((private-fn "foundation-conversation-forks") environment (random-uuid))))
        (expect (empty-result? ((private-fn "meta-query-retries") environment (random-uuid))))
        (expect (vector? ((private-fn "meta-query-retries") environment (random-uuid))))
        (expect (empty-result? ((private-fn "foundation-find-attempts") environment "grep")))
        (expect (vector? ((private-fn "foundation-find-attempts") environment "grep")))
        (expect (empty-result? ((private-fn "foundation-find-attempts-everywhere") environment "grep")))
        (expect (vector? ((private-fn "foundation-find-attempts-everywhere") environment "grep")))
        (expect (empty-result? ((private-fn "foundation-failures") environment)))
        (expect (vector? ((private-fn "foundation-failures") environment)))
        (expect (empty-result? ((private-fn "foundation-failures-everywhere") environment)))
        (expect (vector? ((private-fn "foundation-failures-everywhere") environment)))
        (expect (= 0 (:failure-count ((private-fn "foundation-diagnose") environment))))
        (expect (vector? ((private-fn "foundation-extensions") environment)))
        (expect (map? ((private-fn "foundation-extension-docs") environment)))
        (expect (nil? ((private-fn "foundation-extension-docs") environment 'no.such.ext)))
        (expect (nil? ((private-fn "foundation-extension-doc") environment 'no.such.ext "README.md")))
        (expect (nil? ((private-fn "foundation-extension-readme") environment 'no.such.ext)))))

    (it "returns nil-or-empty when conversation-id is missing"
      (let [s (h/store)
            environment {:db-info s}]
        (expect (empty-result? ((private-fn "foundation-turn") environment)))
        (expect (empty-result? ((private-fn "foundation-conversation") environment)))))))
