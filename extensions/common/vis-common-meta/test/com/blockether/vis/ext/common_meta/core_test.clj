(ns com.blockether.vis.ext.common-meta.core-test
  "Tests for the meta extension's consolidated API. Seven
   functions, each returning a map or vector. Each test bootstraps
   synthetic conversation + query + iteration rows in an in-memory
   SQLite DB, then invokes the impl fns directly with a fake env map."
  (:require
   [com.blockether.vis.core :as sdk]
   [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
   [lazytest.core :refer [defdescribe it expect]]))

;; The extension's core ns calls `register-global!` at load time;
;; required eagerly so the impl fns are interned before tests run.
(require '[com.blockether.vis.ext.common-meta.core])

;; Populate the classpath docs registry once for the whole namespace.
;; The (meta/extensions ...) / (meta/extension-docs ...) / etc. tests
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
  [store query-id {:keys [plan-state breadcrumb expressions thinking error]
                   :or {expressions []}}]
  (sdk/db-store-iteration! store
    (cond-> {:query-id    query-id
             :expressions expressions
             :duration-ms 100
             :llm-model   "test-model"
             :metadata    {}}
      plan-state (assoc :plan-state plan-state)
      breadcrumb (assoc :breadcrumb breadcrumb)
      thinking   (assoc :thinking thinking)
      error      (assoc :error error))))

(defn- env [store conversation-id]
  {:db-info store
   :conversation-id conversation-id
   :current-iteration-atom (atom 2)})

;; The impl fns are private — reach via the var registry so tests
;; don't depend on a public re-export.
(defn- private-fn [name]
  (deref (resolve (symbol "com.blockether.vis.ext.common-meta.core" name))))

;; -----------------------------------------------------------------------------
;; (meta/turn) — single rich snapshot of the current turn
;; -----------------------------------------------------------------------------

(defdescribe meta-turn-test
  (it "returns nil when DB is unreachable"
    (expect (nil? ((private-fn "meta-turn") {:conversation-id "x"}))))

  (it "returns a snapshot map with goal / status / iteration / cost / elapsed-ms / empty plan"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          turn ((private-fn "meta-turn") (env s conversation-id))]
      (expect (= "what's the plan?" (:goal turn)))
      (expect (= :running (:status turn)))
      (expect (map? (:iteration turn)))
      (expect (= 3 (:current (:iteration turn))))
      (expect (= [:current] (vec (keys (:iteration turn)))))
      (expect (map? (:cost turn)))
      (expect (vector? (:breadcrumbs turn)))
      (expect (vector? (:attempts turn)))
      (expect (vector? (:errors turn)))
      (expect (vector? (:failures turn)))
      (expect (nil? (:plan turn)))))

  (it "carries the sticky plan and the breadcrumb chain in the snapshot"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          plan {:goal "g" :items [{:id 1 :content "a" :status :in-progress}]}]
      (db-store-iteration! s query-id {:plan-state plan :breadcrumb "i0 first"})
      (db-store-iteration! s query-id {:breadcrumb "i1 second"})
      (let [turn ((private-fn "meta-turn") (env s conversation-id))]
        (expect (= "g" (-> turn :plan :goal)))
        (expect (= 2 (count (:breadcrumbs turn))))
        (expect (= "i0 first" (-> turn :breadcrumbs first :breadcrumb))))))

  (it "splits attempts and errors so callers don't have to filter twice"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:expressions [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                       {:id 1 :code "(boom)"  :error "boom" :execution-time-ms 1}]})
      (let [turn ((private-fn "meta-turn") (env s conversation-id))]
        (expect (= 2 (count (:attempts turn))))
        (expect (= 1 (count (:errors turn))))
        (expect (= "boom" (-> turn :errors first :error))))))

  (it "surfaces the redundancy summary aggregated across iteration metadata"
    ;; The Phase 2-m metric lands in iteration.metadata as :dedup-saves
    ;; per iteration; (meta/turn).:redundancy aggregates across iterations.
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (sdk/db-store-iteration! s
        {:query-id    query-id
         :expressions [{:id 0 :code "(+ 1 2)" :result 3 :execution-time-ms 1}
                       {:id 1 :code "(grep \"X\")" :result [] :execution-time-ms 1}]
         :duration-ms 100
         :llm-model   "test-model"
         :metadata    {:dedup-saves 0
                       :expression-redundancy-fraction 0.0}})
      (sdk/db-store-iteration! s
        {:query-id    query-id
         :expressions [{:id 0 :code "(grep \"X\")" :result [] :execution-time-ms 1}]
         :duration-ms 100
         :llm-model   "test-model"
         :metadata    {:dedup-saves 1
                       :expression-redundancy-fraction 1.0}})
      (let [turn ((private-fn "meta-turn") (env s conversation-id))
            redundancy (:redundancy turn)]
        (expect (= 1 (:duplicate-count redundancy)))
        (expect (= 3 (:total-count redundancy)))
        (expect (< (Math/abs (- (/ 1.0 3.0) (:fraction redundancy))) 1e-9))))))

;; -----------------------------------------------------------------------------
;; (meta/conversation [id]) — current or specific conversation
;; -----------------------------------------------------------------------------

(defdescribe meta-conversation-test
  (it "no-arg form returns the current conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          conversation ((private-fn "meta-conversation") (env s conversation-id))]
      (expect (= conversation-id (:id conversation)))
      (expect (= :tui (:channel conversation)))
      (expect (vector? (:turns conversation)))
      (expect (= 1 (:turn-count conversation)))))

  (it "arg form fetches any conversation by id"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          other (sdk/db-store-conversation! s {:channel :telegram :title "other"})]
      (let [conversation ((private-fn "meta-conversation") (env s conversation-id) other)]
        (expect (= other (:id conversation)))
        (expect (= :telegram (:channel conversation)))
        (expect (= 0 (:turn-count conversation))))))

  (it "turns include goal/outcome/answer when present"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (sdk/db-update-query! s query-id
        {:answer "42" :iterations 1 :duration-ms 50 :status :done
         :prior-outcome :complete})
      (let [conversation ((private-fn "meta-conversation") (env s conversation-id))
            turn (first (:turns conversation))]
        (expect (= "what's the plan?" (:goal turn)))
        (expect (= "42" (:answer turn)))
        (expect (= :complete (:outcome turn))))))

  (it "auto-excludes the in-flight turn (= CURRENT_QUERY_ID) from the current conversation"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          env-with-in-flight (assoc (env s conversation-id)
                               :current-query-id-atom (atom query-id))
          conversation ((private-fn "meta-conversation") env-with-in-flight)]
      (expect (= 0 (:turn-count conversation))) ; bootstrap creates exactly 1 turn, the in-flight one
      (expect (empty? (:turns conversation)))
      (expect (= query-id (:in-flight-turn-id conversation)))))

  (it "does NOT filter when inspecting a foreign conversation"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          other (sdk/db-store-conversation! s {:channel :telegram :title "other"})
          env-with-in-flight (assoc (env s conversation-id)
                               :current-query-id-atom (atom query-id))
          conversation ((private-fn "meta-conversation") env-with-in-flight other)]
      ;; Foreign conversation untouched: no in-flight-turn-id, original turns kept.
      (expect (= other (:id conversation)))
      (expect (= 0 (:turn-count conversation)))
      (expect (nil? (:in-flight-turn-id conversation)))))

  (it "does NOT filter when no turn is in flight (current-query-id-atom = nil)"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          conversation ((private-fn "meta-conversation") (env s conversation-id))]
      (expect (= 1 (:turn-count conversation)))
      (expect (nil? (:in-flight-turn-id conversation))))))

;; -----------------------------------------------------------------------------
;; (meta/conversations [channel]) — list across one or all channels
;; -----------------------------------------------------------------------------

(defdescribe meta-conversations-test
  (it "no-arg form scans every known channel"
    (let [s (h/store)
          a (sdk/db-store-conversation! s {:channel :tui :title "vis-a"})
          b (sdk/db-store-conversation! s {:channel :telegram :title "tg-b"})
          all ((private-fn "meta-conversations") (env s a))
          ids (set (map :id all))]
      (expect (contains? ids a))
      (expect (contains? ids b))))

  (it "channel-arg form filters to one channel"
    (let [s (h/store)
          a (sdk/db-store-conversation! s {:channel :tui :title "vis-a"})
          _ (sdk/db-store-conversation! s {:channel :telegram :title "tg-b"})
          tui-list ((private-fn "meta-conversations") (env s a) :telegram)]
      (expect (= 1 (count tui-list)))
      (expect (= :telegram (:channel (first tui-list))))))

  (it "every entry carries id, channel, title, turn-count"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          all ((private-fn "meta-conversations") (env s conversation-id))
          this (first (filter #(= conversation-id (:id %)) all))]
      (expect (some? this))
      (expect (= "meta test" (:title this)))
      (expect (= 1 (:turn-count this))))))

;; -----------------------------------------------------------------------------
;; (meta/var-history sym [conversation-id])
;; -----------------------------------------------------------------------------

(defdescribe meta-var-history-test
  (it "returns [] for an unknown sym"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "meta-var-history") (env s conversation-id) 'never-defined)))))

  (it "tolerates string sym names"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)]
      (expect (= [] ((private-fn "meta-var-history") (env s conversation-id) "still-undefined")))))

  (it "explicit conversation-id form queries a different conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          other (sdk/db-store-conversation! s {:channel :tui :title "other"})]
      (expect (= [] ((private-fn "meta-var-history") (env s conversation-id) 'foo other))))))

;; -----------------------------------------------------------------------------
;; (meta/find-attempts pattern [conversation-id])
;; -----------------------------------------------------------------------------

(defdescribe meta-find-attempts-test
  (it "one-arg form regex-searches the current turn"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)]
      (db-store-iteration! s query-id
        {:expressions [{:id 0 :code "(+ 1 2)"        :result 3 :execution-time-ms 1}
                       {:id 1 :code "(grep \"FOO\")" :result [] :execution-time-ms 1}
                       {:id 2 :code "(grep \"BAR\")" :result [] :execution-time-ms 1}]})
      (let [hits ((private-fn "meta-find-attempts") (env s conversation-id) "grep")]
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
        {:expressions [{:id 0 :code "(defn foo [x] x)" :result nil :execution-time-ms 1}]})
      (let [hits ((private-fn "meta-find-attempts") (env s conversation-id) #"\bdefn\b")]
        (expect (= 1 (count hits))))))

  (it "two-arg form scans every turn of the given conversation"
    (let [s (h/store)
          {:keys [conversation-id]} (bootstrap s)
          q2 (sdk/db-store-query! s
               {:parent-conversation-id conversation-id
                :query "second turn" :status :running})]
      ;; Leave q1 empty; fill q2.
      (db-store-iteration! s q2
        {:expressions [{:id 0 :code "(grep \"target\")" :result [] :execution-time-ms 1}]})
      (let [hits ((private-fn "meta-find-attempts") (env s conversation-id) "grep" conversation-id)]
        (expect (= 1 (count hits)))))))

;; -----------------------------------------------------------------------------
;; (meta/failures) and (meta/diagnose) — no raw SQLite needed for triage
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
      (let [failures ((private-fn "meta-failures") (env s conversation-id))
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
        {:expressions [{:id 0
                        :code "(vis/rg \"foo\\|bar\\|baz\" \"x\")"
                        :error "Unsupported escape character: \\|"
                        :execution-time-ms 1}
                       {:id 1
                        :code "(vis/patch [{:path \"render.clj\" :search \"x\" :replace \"y\"}])"
                        :error "SEARCH block 1 not found in render.clj"
                        :execution-time-ms 1}]})
      (let [diagnosis ((private-fn "meta-diagnose") (env s conversation-id))]
        (expect (= 2 (:failure-count diagnosis)))
        (expect (= 1 (get-in diagnosis [:by-classification :regex-unsupported-escape])))
        (expect (= 1 (get-in diagnosis [:by-classification :patch-no-match])))
        (expect (seq (:next-actions diagnosis))))))

  (it "conversation-id form scans every turn"
    (let [s (h/store)
          {:keys [conversation-id query-id]} (bootstrap s)
          second-query-id (sdk/db-store-query! s
                            {:parent-conversation-id conversation-id
                             :query "second turn"
                             :status :running})]
      (db-store-iteration! s query-id
        {:expressions [{:id 0 :code "(vis/rg \"x\\|y\" \"z\")"
                        :error "Unsupported escape character: \\|"
                        :execution-time-ms 1}]})
      (db-store-iteration! s second-query-id
        {:expressions [{:id 0 :code "(vis/patch [{:path \"x\"}])"
                        :error "SEARCH block 1 not found in x"
                        :execution-time-ms 1}]})
      (let [failures ((private-fn "meta-failures") (env s conversation-id) conversation-id)]
        (expect (= 2 (count failures)))
        (expect (every? :turn-id failures))
        (expect (every? :goal failures))))))

;; -----------------------------------------------------------------------------
;; (meta/extensions), (meta/extension-docs ...), (meta/extension-doc ...),
;; and (meta/extension-readme ...) — catalog + abstracts + bodies.
;; -----------------------------------------------------------------------------

(defdescribe meta-extensions-catalog-test
  (it "includes the meta extension itself with its declared docs and abstracts"
    (let [extensions ((private-fn "meta-extensions") {})
          this       (some #(when (= 'com.blockether.vis.ext.common-meta.core (:namespace %)) %)
                       extensions)]
      (expect (some? this))
      (expect (= 'meta (:alias this)))
      (expect (vector? (:symbols this)))
      (expect (contains? (set (:symbols this)) 'extensions))
      (expect (contains? (set (:symbols this)) 'extension-docs))
      (expect (contains? (set (:symbols this)) 'extension-doc))
      (expect (contains? (set (:symbols this)) 'extension-readme))
      ;; :docs is a vector of summary maps. Each summary carries the
      ;; structured descriptor fields except :content; :content lives
      ;; on the full descriptor returned by (meta/extension-doc ...).
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
    (let [extensions ((private-fn "meta-extensions") {})]
      (expect (every? :namespace extensions))
      (expect (every? #(contains? % :symbols) extensions))
      (expect (every? #(contains? % :docs) extensions)))))

(defdescribe meta-extension-docs-test
  (it "single-arg form returns summaries for a registered extension"
    (let [docs ((private-fn "meta-extension-docs") {} 'meta)]
      (expect (vector? docs))
      (expect (= #{"README.md"} (set (map :name docs))))
      (expect (every? #(string? (:description %)) docs))
      (expect (every? #(vector? (:links %)) docs))
      (expect (every? #(vector? (:reflinks %)) docs))))

  (it "summaries do NOT include :content (catalog stays small)"
    (let [docs ((private-fn "meta-extension-docs") {} 'meta)]
      (expect (every? #(not (contains? % :content)) docs))))

  (it "reflinks reflect cross-extension authored links"
    ;; vis-common-meta links to vis (companion) and vis-common-editing
    ;; links back to meta -- so each gets one inbound reflink.
    (let [meta-readme (first ((private-fn "meta-extension-docs") {} 'meta))
          vis-readme  (first ((private-fn "meta-extension-docs") {} 'vis))]
      (expect (some #(= 'vis  (:from-id %)) (:reflinks meta-readme)))
      (expect (some #(= 'meta (:from-id %)) (:reflinks vis-readme)))))

  (it "no-arg form returns the full registry keyed by id symbol"
    (let [registry ((private-fn "meta-extension-docs") {})]
      (expect (map? registry))
      (expect (contains? registry 'meta))))

  (it "unknown reference returns nil"
    (expect (nil? ((private-fn "meta-extension-docs") {} 'no.such.extension)))))

(defdescribe meta-extension-doc-test
  (it "returns the full descriptor map for a declared doc"
    (let [doc ((private-fn "meta-extension-doc") {} 'meta "README.md")]
      (expect (map? doc))
      (expect (= "README.md" (:name doc)))
      (expect (string? (:description doc)))
      (expect (string? (:content doc)))
      (expect (clojure.string/includes? (:content doc) "# Meta extension"))
      (expect (vector? (:links doc)))
      (expect (pos? (count (:links doc))))
      (expect (vector? (:reflinks doc)))))

  (it "links carry author-declared targets and contexts"
    (let [doc   ((private-fn "meta-extension-doc") {} 'meta "README.md")
          links (:links doc)]
      ;; meta links to vis README (cross-ext doc), the RLM paper
      ;; (URL), and the loader source file (file).
      (expect (some #(= ['vis "README.md"] [(:to-id %) (:to-doc %)]) links))
      (expect (some #(some? (:url %)) links))
      (expect (some #(some? (:file %)) links))))

  (it "returns nil for an unknown doc name"
    (expect (nil? ((private-fn "meta-extension-doc") {} 'meta "NOPE.md"))))

  (it "returns nil for an unknown extension reference"
    (expect (nil? ((private-fn "meta-extension-doc") {} 'no.such.ext "README.md")))))

(defdescribe meta-extension-readme-test
  (it "resolves by id symbol"
    (let [text ((private-fn "meta-extension-readme") {} 'meta)]
      (expect (string? text))
      (expect (clojure.string/includes? text "meta/extensions"))))

  (it "resolves by id keyword"
    (let [text ((private-fn "meta-extension-readme") {} :meta)]
      (expect (string? text))
      (expect (clojure.string/includes? text "meta/extension-readme"))))

  (it "resolves by full extension namespace"
    (let [text ((private-fn "meta-extension-readme") {} 'com.blockether.vis.ext.common-meta.core)]
      (expect (string? text))
      (expect (clojure.string/includes? text "# Meta extension"))))

  (it "resolves by alias-ns symbol"
    (let [text ((private-fn "meta-extension-readme") {} 'vis.ext.meta)]
      (expect (string? text))))

  (it "returns nil for an unknown extension reference"
    (expect (nil? ((private-fn "meta-extension-readme") {} 'no.such.extension)))
    (expect (nil? ((private-fn "meta-extension-readme") {} :nope)))
    (expect (nil? ((private-fn "meta-extension-readme") {} nil)))))

;; -----------------------------------------------------------------------------
;; Failure modes — every fn must return nil/[], NEVER throw
;; -----------------------------------------------------------------------------

(defdescribe failure-mode-test
  (let [empty-result? #(or (nil? %) (and (coll? %) (empty? %)))]
    (it "returns nil-or-empty when DB is unreachable"
      (let [environment {:conversation-id "x"}]
        (expect (empty-result? ((private-fn "meta-turn") environment)))
        (expect (empty-result? ((private-fn "meta-conversation") environment)))
        (expect (empty-result? ((private-fn "meta-conversations") environment)))
        (expect (empty-result? ((private-fn "meta-var-history") environment 'foo)))
        (expect (empty-result? ((private-fn "meta-find-attempts") environment "grep")))
        (expect (empty-result? ((private-fn "meta-failures") environment)))
        (expect (= 0 (:failure-count ((private-fn "meta-diagnose") environment))))
        (expect (vector? ((private-fn "meta-extensions") environment)))
        (expect (map? ((private-fn "meta-extension-docs") environment)))
        (expect (nil? ((private-fn "meta-extension-docs") environment 'no.such.ext)))
        (expect (nil? ((private-fn "meta-extension-doc") environment 'no.such.ext "README.md")))
        (expect (nil? ((private-fn "meta-extension-readme") environment 'no.such.ext)))))

    (it "returns nil-or-empty when conversation-id is missing"
      (let [s (h/store)
            environment {:db-info s}]
        (expect (empty-result? ((private-fn "meta-turn") environment)))
        (expect (empty-result? ((private-fn "meta-conversation") environment)))))))
