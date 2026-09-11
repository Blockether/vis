(ns com.blockether.vis.internal.loop-test
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [com.blockether.vis-python-runtime :as python-runtime]
            [com.blockether.vis.test-python-context :as tpc]
            [clojure.java.io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.svar.internal.llm :as svar-llm]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as activity-event]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.channel.form :as form]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.python.worker :as python-worker]
            [com.blockether.vis.internal.sandbox.policy]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.context.engine :as eng]
            [com.blockether.vis.internal.session.titling :as titling]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.view.core :as hi]
            [com.blockether.vis.internal.channel.events :as ce]
            [com.blockether.vis.internal.provider.error :as perr]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [taoensso.telemere :as tel]
            [com.blockether.vis.internal.session.model :as session-model]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.internal.attachment.vision-describe :as vision-describe]
            [lazytest.core :refer [defdescribe describe it expect throws?]]))

(defdescribe
  python-providers-before-router-test
  (it "loads Python providers before resolving the first router, without reloading a warm router"
      (let [order
            (atom [])

            router
            {:providers [{:id :fixture}]}]

        (with-redefs-fn {#'lp/router-atom (atom nil)
                         #'python-extensions/ensure-python-extensions-loaded! (fn []
                                                                                (swap! order conj
                                                                                  :extensions))
                         #'config/load-config (fn [_]
                                                (swap! order conj :config)
                                                {:fixture true})
                         #'lp/build-router (fn [_]
                                             (swap! order conj :build)
                                             router)
                         #'lp/honor-config-roots! (fn [r _]
                                                    (swap! order conj :roots)
                                                    r)}
          (fn []
            (expect (= router (lp/get-router)))
            (expect (= router (lp/get-router)))
            (expect (= [:extensions :config :build :roots] @order)))))))

(defdescribe
  auto-bound-provider-precedence-test
  (it "does not promote preset transport fields to explicit config for auto-bound providers"
      (let [preset-row
            {:id :fixture
             :base-url "http://127.0.0.1:1/v1"
             :api-style :openai
             :models [{:name "fixture-model"}]}

            explicit-row
            (assoc preset-row :base-url "http://127.0.0.1:2/v1")

            seen
            (atom nil)]

        (with-redefs [providers/authenticated-preset-providers
                      (constantly [preset-row])

                      config/->svar-provider
                      (fn [p]
                        (reset! seen p)
                        p)]

          (#'lp/runtime-router-providers {:providers []})
          (expect (= (dissoc preset-row :base-url :api-style) @seen))
          (#'lp/runtime-router-providers {:providers [explicit-row]})
          (expect (= explicit-row @seen))))))

(defn- helper-router
  [provider-id network]
  (cond-> (svar/make-router [{:id provider-id
                              :api-key "test"
                              :base-url "http://127.0.0.1:1234/v1"
                              :models [{:name "model"}]}])
    network
    (update :providers
            #(mapv (fn [provider]
                     (assoc provider :network network))
                   %))))

(def ^:private helper-provider-network
  "Streaming policy the default helper router's provider carries.

   A local runtime prefills for minutes before its first token, so its own
   policy is deliberately wider than Vis' cloud-shaped runtime defaults — which
   is exactly the precedence these tests pin."
  {:timeout-ms 1800000
   :first-byte-timeout-ms 600000
   :idle-timeout-ms 600000
   :semantic-timeout-ms 600000})

(defn- captured-svar-ask-code-opts
  "Opts a global-router helper hands to `svar/ask-code!`, with no network call."
  ([invoke!]
   (captured-svar-ask-code-opts (helper-router :lmstudio helper-provider-network) invoke!))
  ([router invoke!]
   (let [seen (atom nil)]
     (with-redefs-fn {#'lp/get-router (fn []
                                        router)
                      #'svar/ask-code! (fn [router opts]
                                         (reset! seen {:router router :opts opts})
                                         {:blocks [] :raw ""})}
       invoke!)
     @seen)))

(defn- captured-ask-code-opts [opts] (captured-svar-ask-code-opts #(lp/ask-code! opts)))

(defn- captured-llm-text-opts [opts] (captured-svar-ask-code-opts #(lp/llm-text! opts)))

(def ^:private provider-error-explanation perr/provider-error-explanation)

(def ^:private turn-eval-evidence (deref #'lp/turn-eval-evidence))

(def ^:private ask-code-block-observation (deref #'lp/ask-code-block-observation))

(def ^:private log-stage-level (deref #'lp/log-stage-level))

(defn- context-token-observations
  "Capture request-count diagnostics at the real dispatch boundary, without network IO."
  [{:keys [input-tokens counter error served-model] :or {counter (constantly 1000)}}]
  (let [environment
        (lp/create-environment {:providers [{:id :lmstudio}]} {:db :memory})

        messages
        [{:role "user" :content "private-user-text"}
         {:role "assistant"
          :content [{:type "thinking"
                     :thinking "private-reasoning"
                     :thinking-signature "opaque-signed-reasoning"}]}]

        caught
        (atom nil)

        counted
        (atom [])

        result
        (atom nil)

        svar-log-data
        (atom nil)]

    (try
      (let [{:keys [signals]}
            (tel/with-signals
              (with-redefs [svar-router/count-messages (fn ^long [model request]
                                                         (swap! counted conj [model request])
                                                         (long (counter model request)))
                            svar-router/count-tokens (fn ^long [_ _]
                                                       20)
                            svar/ask-code!
                            (fn [_ _]
                              (reset! svar-log-data (#'svar-llm/log-data
                                                     {:request-id "upstream-request"}))
                              (if error
                                (throw error)
                                {:stop-reason :end
                                 :content "done"
                                 :routed/model (or served-model "gpt-4o")
                                 :routed/provider-id :lmstudio
                                 :api-usage (when (some? input-tokens)
                                              {:input-tokens input-tokens :output-tokens 1})}))]

                (try (reset! result (lp/run-iteration
                                      environment
                                      messages
                                      {:iteration 2
                                       :resolved-model {:provider :lmstudio :name "gpt-4o"}
                                       :request-context {:request-id "request-3"
                                                         :context-recovery-attempt 1
                                                         :prompt-base :resumed
                                                         :base-message-count 2
                                                         :trailer-iteration-count 0}}))
                     (catch Exception e (reset! caught e)))))]
        {:observations (filterv #(= ::lp/context-token-counts (:id %)) signals)
         :health (:request-health @result)
         :error @caught
         :counted @counted
         :svar-log-data @svar-log-data
         :messages messages
         :session-id (:session-id environment)})
      (finally (lp/dispose-environment! environment)))))

(defdescribe
  context-token-logging-test
  (it "compares the same request in both directions and correlates the served route"
      ;; #186: logs and persisted breakdown must compare the same logical scope, including tools.
      (doseq [[input delta ratio] [[400 620 2.55] [2500 -1480 0.408]]]
        (let [{:keys [observations error health counted messages session-id svar-log-data]}
              (context-token-observations {:input-tokens input :served-model "gpt-4.1"})
              {:keys [level data]} (first observations)]

          (expect (nil? error))
          (expect (= 1 (count observations)))
          (expect (= :info level))
          (expect (= :succeeded (:outcome data)))
          (expect (= session-id (:session-id data)))
          (expect (= "request-3" (:request-id data)))
          (expect (= (:request-id data) (:query-id svar-log-data)))
          (expect (= (:iteration data) (:iteration svar-log-data)))
          (expect (= "upstream-request" (:request-id svar-log-data)))
          (expect (= 3 (:iteration data)))
          (expect (= 1 (:context-recovery-attempt data)))
          (expect (= :resumed (:prompt-base data)))
          (expect (= 0 (:trailer-iteration-count data)))
          (expect (= :lmstudio (:provider data)))
          (expect (= "gpt-4.1" (:model data)))
          (expect (= "gpt-4.1" (:local-estimate-model data)))
          (expect (some #{["gpt-4.1" [(first messages)]]} counted))
          (expect (= :logical-request (:counted-projection data)))
          (expect (= :svar-estimate (:local-count-source data)))
          (expect (= 1020 (:local-input-tokens data)))
          (expect (= (:local-input-tokens data)
                     (:estimated-input-tokens health)
                     (reduce + (map :tokens (:breakdown health)))))
          (expect (= :provider-usage (:provider-count-source data)))
          (expect (= input (:provider-input-tokens data)))
          (expect (= delta (:local-minus-provider-tokens data)))
          (expect (= ratio (:local-to-provider-ratio data)))
          (expect (= 1 (:thinking-block-count data)))
          (expect (= (count "opaque-signed-reasoning") (:thinking-signature-chars data)))
          (expect (not-any? #(str/includes? (pr-str data) %)
                            ["private-user-text" "private-reasoning" "opaque-signed-reasoning"])))))
  (it "keeps missing and zero provider counts distinct, without manufacturing a ratio"
      (doseq [input [nil 0]]
        (let [{:keys [observations error]} (context-token-observations {:input-tokens input})
              data (:data (first observations))]

          (expect (nil? error))
          (expect (= 1 (count observations)))
          (expect (= input (:provider-input-tokens data)))
          (expect (= (if (some? input) :provider-usage :unavailable) (:provider-count-source data)))
          (expect (nil? (:local-to-provider-ratio data))))))
  (it "does not let a diagnostic tokenizer failure fail a successful request or expose its message"
      (let [{:keys [observations error]}
            (context-token-observations {:input-tokens 400
                                         :counter (fn [& _]
                                                    (throw (ex-info "private-tokenizer-data" {})))})

            data
            (:data (first observations))]

        (expect (nil? error))
        (expect (= 1 (count observations)))
        (expect (= 400 (:provider-input-tokens data)))
        (expect (nil? (:local-input-tokens data)))
        (expect (= :unavailable (:local-count-source data)))
        (expect (nil? (:local-to-provider-ratio data)))
        (expect (not (str/includes? (pr-str data) "private-tokenizer-data"))))))

(defdescribe
  context-overflow-logging-test
  (it "never relabels a preflight or unattributed rejection as provider usage"
      ;; Session 33cfa509: local preflight counts were logged as provider-tokens.
      (doseq [[type source expected] [[:svar.core/context-overflow nil :preflight]
                                      [:svar.tokens/context-overflow :preflight :preflight]
                                      [:svar.tokens/context-overflow :provider :provider]
                                      [:svar.tokens/context-overflow nil :unknown]]]
        (let [rejection (ex-info "private-provider-message"
                                 {:type type
                                  :source source
                                  :input-tokens 276317
                                  :max-input-tokens 272000
                                  :body "private-provider-body"})
              {:keys [observations error]} (context-token-observations {:error rejection})
              {:keys [level data]} (first observations)]

          (expect (identical? rejection error))
          (expect (= 1 (count observations)))
          (expect (= :warn level))
          (expect (= :context-overflow (:outcome data)))
          (expect (= expected (:rejection-source data)))
          (expect (= (if (= :provider expected) :provider-error :unspecified)
                     (:reported-count-source data)))
          (expect (= 276317 (:reported-input-tokens data)))
          (expect (= 272000 (:reported-input-limit data)))
          (expect (= 1020 (:local-input-tokens data)))
          (expect (= :unavailable (:provider-count-source data)))
          (expect (nil? (:provider-input-tokens data)))
          (expect (nil? (:local-to-provider-ratio data)))
          (expect (not-any? #(str/includes? (pr-str data) %)
                            ["private-provider-message" "private-provider-body" "private-user-text"
                             "private-reasoning" "opaque-signed-reasoning"])))))
  (it
    "correlates the terminal no-fold decision with the exact failed dispatch"
    (let [environment
          (lp/create-environment (helper-router :lmstudio nil) {:db :memory})

          tid
          (persistance/db-store-session-turn! (:db-info environment)
                                              {:parent-session-id (:session-id environment)
                                               :user-request "measure"})

          calls
          (atom 0)]

      (try (let [{:keys [signals]}
                 (tel/with-signals
                   (with-redefs [svar/ask-code! (fn [_ _]
                                                  (swap! calls inc)
                                                  (throw (ex-info "Context overflow"
                                                                  {:type :svar.core/context-overflow
                                                                   :input-tokens 276317
                                                                   :max-input-tokens 272000})))]
                     (lp/iteration-loop environment "measure" {:session-turn-id tid})))

                 failed
                 (:data (first (filter #(= ::lp/context-token-counts (:id %)) signals)))

                 terminal
                 (:data (first (filter #(= ::lp/context-overflow-terminal (:id %)) signals)))

                 correlation
                 [:request-id :session-id :session-turn-id :iteration :prompt-base
                  :base-message-count :trailer-iteration-count]]

             (expect (= 1 @calls))
             (expect (some? (:request-id failed)))
             (expect (= (select-keys failed correlation) (select-keys terminal correlation)))
             (expect (= 0 (:trailer-iteration-count terminal)))
             (expect (= :canonical (:prompt-base terminal)))
             (expect (= :preflight (:rejection-source terminal)))
             (expect (= 1 (:recovery-attempts terminal)))
             (expect (false? (:output-started? terminal))))
           (finally (lp/dispose-environment! environment))))))

(defdescribe
  request-health-persistence-test
  (it
    "persists the served request's input and ceiling, leaving unknown model limits absent"
    (let [router
          (svar/make-router [{:id :lmstudio
                              :base-url "http://127.0.0.1:1234/v1"
                              :api-key "test"
                              :models [{:name "pinned" :input-limit 1000000}
                                       {:name "small" :input-limit 128000}]}])

          environment
          (lp/create-environment router {:db :memory})

          db
          (:db-info environment)

          sid
          (:session-id environment)]

      (try (doseq [[model input expected-limit expected-budget]
                   [["small" 32000 128000 115200] ["not-in-catalog" 4000 nil 200000]]]
             (let [tid (persistance/db-store-session-turn! db
                                                           {:parent-session-id sid
                                                            :user-request "measure"})]
               (with-redefs [svar/ask-code! (fn [_ _]
                                              {:stop-reason :end
                                               :tool-calls []
                                               :content "done"
                                               :routed/provider-id :lmstudio
                                               :routed/model model
                                               :api-usage {:input-tokens input :output-tokens 1}
                                               :tokens {}})]
                 (lp/iteration-loop environment "measure" {:session-turn-id tid}))
               (let [usage (persistance/db-session-usage-stats db sid)
                     health (:health usage)]

                 (expect (= input (:last-request-tokens health)))
                 (expect (= expected-limit (:model-input-limit health)))
                 (expect (= expected-budget (:budget-tokens health)))
                 (expect (= (* 3/4 expected-budget) (:reminder-tokens health)))
                 (expect (seq (:breakdown health)))
                 ;; #186: retain the estimate's scope and total alongside its own measured input.
                 (expect (= :logical-request (:counted-projection health)))
                 (expect (= (:estimated-input-tokens health)
                            (reduce + (map :tokens (:breakdown health)))))
                 (expect (false? (:stale health))))))
           (expect (= 36000 (:input-tokens (persistance/db-session-usage-stats db sid))))
           (finally (lp/dispose-environment! environment))))))

(defn- fold-usage-scenario
  "Drive Python folding, persisted accounting and the next provider request together."
  ([fold-code] (fold-usage-scenario fold-code 0))
  ([fold-code prior-folds]
   (let [router
         (svar/make-router [{:id :lmstudio
                             :base-url "http://127.0.0.1:1234/v1"
                             :api-key "test"
                             :models [{:name "model" :input-limit 1000000}]}])

         environment
         (lp/create-environment router {:db :memory})

         db
         (:db-info environment)

         sid
         (:session-id environment)

         _
         (when (pos? prior-folds)
           (let [prior-tid (persistance/db-store-session-turn! db
                                                               {:parent-session-id sid
                                                                :user-request "prior work"})]
             (persistance/db-store-iteration! db
                                              {:session-turn-id prior-tid
                                               :idx 0
                                               :code "compact()"
                                               :status :done
                                               :forms [{:vis/tool-name "python_execution"
                                                        :src "compact()"
                                                        :stdout ""
                                                        :vis/fold-count prior-folds}]})))

         snapshots
         (atom [])

         tid
         (persistance/db-store-session-turn! db
                                             {:parent-session-id sid
                                              :user-request "fold settled work"})

         requests
         (atom [])

         steps
         [{:input 150000 :code "print('settled evidence')"} {:input 157843 :code fold-code}
          {:input 15614
           :code
           (str
             "print('sandbox-fold-count:', session['utilization'].get('fold_count'))\n"
             "print('sandbox-fold-measurement:', session['utilization'].get('fold_measurement'))")}
          {:input 20736}]]

     (try (with-redefs [svar/ask-code!
                        (fn [_ opts]
                          (let [idx (count @requests)
                                {:keys [input code]} (nth steps idx)]

                            (swap! requests conj (:messages opts))
                            (swap! snapshots conj (ctx-loop/session-snapshot environment))
                            (merge {:api-usage {:input-tokens input :output-tokens 1}
                                    :routed/provider-id :lmstudio
                                    :routed/model "model"
                                    :tokens {}}
                                   (if code
                                     {:stop-reason :tool-calls
                                      :tool-calls [{:id (str "call-" idx)
                                                    :name "python_execution"
                                                    :input {:code code}}]}
                                     {:stop-reason :end :tool-calls [] :content "done"}))))]
            (lp/iteration-loop environment "fold settled work" {:session-turn-id tid}))
          (expect (= 4 (count @requests)))
          (let [wire
                (str/join "\n" (filter string? (tree-seq coll? seq (last @requests))))

                readings
                (re-seq #"last_request_tokens[^\n]*?(\d+)" wire)

                folds
                (:fold-count (persistance/db-session-usage-stats db sid))]

            (expect (= [prior-folds prior-folds folds folds]
                       (mapv #(get-in % ["session_utilization" "fold_count"]) @snapshots)))
            ;; Blockether/vis#174: report the next request's provider-measured net
            ;; reduction, once for all folds issued between the same two requests.
            (if (> folds prior-folds)
              (do (expect (= "pending"
                             (get-in (nth @snapshots 2)
                                     ["session_utilization" "fold_measurement" "status"])))
                  (expect (= {"status" "measured"
                              "source" "provider_usage"
                              "before_input_tokens" 157843
                              "after_input_tokens" 15614
                              "net_reduction_tokens" 142229
                              "fold_count" (- folds prior-folds)}
                             (select-keys
                               (get-in (last @snapshots) ["session_utilization" "fold_measurement"])
                               ["status" "source" "before_input_tokens" "after_input_tokens"
                                "net_reduction_tokens" "fold_count"])))
                  (expect (= (get-in (last @snapshots) ["session_utilization" "fold_measurement"])
                             (get-in (persistance/db-session-usage-stats db sid)
                                     [:health :fold-measurement])))
                  (expect (re-find #"sandbox-fold-measurement: [^\n]*'net_reduction_tokens': 142229"
                                   wire)))
              (expect (nil? (get-in (last @snapshots) ["session_utilization" "fold_measurement"]))))
            (expect (str/includes? wire (str "sandbox-fold-count: " folds)))
            {:latest-input (some-> readings
                                   last
                                   second
                                   parse-long)
             :wire-folds (some-> (re-seq #"sandbox-fold-count: (\d+)" wire)
                                 last
                                 second
                                 parse-long)
             :folds folds})
          (finally (lp/dispose-environment! environment))))))

(defdescribe
  post-fold-utilization-and-accounting-test
  ;; Blockether/vis#174: the fold executes before run-iteration returns its usage.
  (it "prices the fold against the response that requested it, not the previous response"
      (let [baselines
            (atom [])

            original
            @#'lp/compaction-verbs]

        (with-redefs-fn {#'lp/compaction-verbs
                         (fn [& args]
                           (let [verbs
                                 (apply original args)

                                 fold
                                 (get verbs 'fold-session)

                                 ctx
                                 (first args)]

                             (assoc verbs
                               'fold-session
                               (fn [& params]
                                 (swap! baselines conj
                                   (get-in @ctx ["engine_utilization" "last_request_tokens"]))
                                 (apply fold params)))))}
          #(fold-usage-scenario "fold_session('-t1/i1', 'checkpoint')"))
        (expect (= [157843] @baselines))))
  ;; Regression: a successful fold looked ineffective because the next request
  ;; still carried pre-fold usage, and an unprinted receipt counted as zero folds.
  (it "publishes fresh provider usage and counts a fold without its printed receipt"
      (expect (= {:latest-input 15614 :folds 1 :wire-folds 1}
                 (fold-usage-scenario
                   "fold_session('-t1/i1', 'evidence retained')\nprint('folded')"))))
  (it "publishes the fold count when the Python block prints nothing"
      (expect (= {:latest-input 15614 :folds 1 :wire-folds 1}
                 (fold-usage-scenario "fold_session('-t1/i1', 'silent checkpoint')"))))
  (it "seeds resumed utilization from recorded operations before the first request"
      (expect (= {:latest-input 15614 :folds 4 :wire-folds 4}
                 (fold-usage-scenario "fold_session('-t2/i1', 'resumed checkpoint')" 3))))
  (it "counts folds invoked through a helper even when a later statement fails"
      (expect (= {:latest-input 15614 :folds 2 :wire-folds 2}
                 (fold-usage-scenario (str
                                        "def compact():\n    fold_session('-t1/i1', 'checkpoint')\n"
                                        "compact()\ncompact()\nraise ValueError('after folds')")))))
  (it "does not count fabricated receipts or a refused live-step fold"
      (expect (= {:latest-input 15614 :folds 0 :wire-folds 0}
                 (fold-usage-scenario
                   (str "example = \"fold_session('-t1/i1', 'unused')\"\n"
                        "print('folded through t1/i1')\nfold_session('t1/i2', 'live')"))))))

(defn- fold-measurement-fixture
  "A foldable context whose stale utilization deliberately disagrees with the response."
  [response]
  (let [ctx (atom (#'lp/record-provider-input
                   {"session_turn" 2
                    "engine_iter_universe" ["t1/i1" "t1/i2"]
                    "engine_iter_weights" {"t1/i1" 12000 "t1/i2" 3400}
                    "engine_utilization" {"last_request_tokens" 999999}}
                   response))]
    [ctx (get (#'lp/compaction-verbs ctx) 'fold-session)]))

(defdescribe
  provider-fold-measurement-test
  ;; Blockether/vis#174: provider totals, not tokenizer estimates, settle a fold batch.
  (it
    "keeps signed net changes and refuses missing or incomparable provider samples"
    (let [response
          {:llm-provider :lmstudio :llm-model "model" :api-usage {:input-tokens 100000}}

          input-path
          [:api-usage :input-tokens]]

      (doseq [[before after turn expected]
              [[response (assoc-in response input-path 20000) 2
                {"status" "measured" "net_reduction_tokens" 80000}]
               [response response 2 {"status" "measured" "net_reduction_tokens" 0}]
               [response (assoc-in response input-path 120000) 2
                {"status" "measured" "net_reduction_tokens" -20000}]
               [(dissoc response :api-usage) response 2
                {"status" "unavailable" "reason" "missing_before_usage"}]
               [(assoc-in response input-path 0) response 2
                {"status" "unavailable" "reason" "missing_before_usage"}]
               [(assoc-in response input-path -1) response 2
                {"status" "unavailable" "reason" "missing_before_usage"}]
               [response (dissoc response :api-usage) 2
                {"status" "unavailable" "reason" "missing_after_usage"}]
               [response (assoc-in response input-path 0) 2
                {"status" "unavailable" "reason" "missing_after_usage"}]
               [response (assoc response :llm-provider :other) 2
                {"status" "unavailable" "reason" "route_changed"}]
               [response (assoc response :llm-model "other") 2
                {"status" "unavailable" "reason" "route_changed"}]
               [(dissoc response :llm-provider) response 2
                {"status" "unavailable" "reason" "unknown_route"}]
               [response (assoc response :llm-model " ") 2
                {"status" "unavailable" "reason" "unknown_route"}]
               [response response 3 {"status" "unavailable" "reason" "turn_changed"}]]]
        (let [[ctx fold] (fold-measurement-fixture before)]
          (expect (str/includes? (fold "t1/i1" "retained") "provider net change pending"))
          (swap! ctx assoc "session_turn" turn)
          (swap! ctx #'lp/record-provider-input after)
          (let [measurement (get @ctx "engine_fold_measurement")]
            (expect (= expected
                       (select-keys measurement ["status" "reason" "net_reduction_tokens"])))
            (expect (= 1 (get measurement "fold_count")))
            (expect (= "provider_usage" (get measurement "source")))
            (expect (= measurement
                       (get-in (eng/session-view @ctx) ["session_utilization" "fold_measurement"])))
            ;; A later response must not settle the same fold again, even when the
            ;; first post-fold response had no usage or a different route.
            (swap! ctx #'lp/record-provider-input response)
            (expect (= measurement (get @ctx "engine_fold_measurement"))))))))
  (it "groups folds before one response and starts the next batch from its fresh input"
      (let [response
            {:llm-provider :lmstudio :llm-model "model" :api-usage {:input-tokens 100000}}

            [ctx fold]
            (fold-measurement-fixture response)]

        (fold "t1/i1" "first")
        (fold "t1/i1" "refined")
        (swap! ctx #'lp/record-provider-input (assoc-in response [:api-usage :input-tokens] 20000))
        (expect (= {"fold_count" 2 "net_reduction_tokens" 80000}
                   (select-keys (get @ctx "engine_fold_measurement")
                                ["fold_count" "net_reduction_tokens"])))
        (fold "t1/i2" "next batch")
        (expect (= {"status" "pending" "fold_count" 1 "before_input_tokens" 20000}
                   (select-keys (get @ctx "engine_fold_measurement")
                                ["status" "fold_count" "before_input_tokens"
                                 "net_reduction_tokens"])))
        (swap! ctx #'lp/record-provider-input (assoc-in response [:api-usage :input-tokens] 30000))
        (expect (= {"status" "measured" "fold_count" 1 "net_reduction_tokens" -10000}
                   (select-keys (get @ctx "engine_fold_measurement")
                                ["status" "fold_count" "net_reduction_tokens"]))))))

(defdescribe
  loop-stage-logging-test
  (it "keeps routine telemetry debug-only but logs failed turns and timeouts at error level"
      (expect (= :debug (log-stage-level :provider-call/stop {:duration-ms 12})))
      (expect (= :error (log-stage-level :error {:reason :provider-failed})))
      (expect (= :error (log-stage-level :code-result {:timeout? true})))
      (expect (= :error (log-stage-level :turn/complete {:status :error})))
      (expect (= :info (log-stage-level :error {:reason :cancelled})))
      (expect (= :info (log-stage-level :turn/complete {:status :cancelled})))))

(defn- reasoning-effort-router
  []
  (svar/make-router [{:id :test-zai
                      :api-key "test"
                      :base-url "http://example.invalid"
                      :api-style :anthropic
                      :models [{:name "glm-5.2"
                                :reasoning? true
                                :reasoning-style :zai-effort
                                :reasoning-options [{:type "effort" :values ["high" "max"]}]}]}]))

(defdescribe
  provider-native-reasoning-effort-preflight-test
  (it "rejects unsupported effort before the iteration/provider phase"
      (let [provider-calls
            (atom 0)

            env
            {:db-info ::db :environment-id ::environment :router (reasoning-effort-router)}

            thrown
            (try (with-redefs [lp/run-turn! (fn [& _]
                                              (swap! provider-calls inc)
                                              (throw (ex-info "should not run" {})))]
                   (lp/turn! env [{:role "user" :content "task"}] {:reasoning-effort "medium"}))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]

        (expect (= 0 @provider-calls))
        (expect (= :vis/unsupported-reasoning-effort (:type (ex-data thrown))))
        (expect (= ["high" "max"] (:supported (ex-data thrown))))))
  (it "returns content-free health for the request actually handed to the provider"
      (let [environment
            (lp/create-environment {:providers [{:id :lmstudio}]} {:db :memory})

            messages
            [{:role "system" :content "core"} {:role "user" :content "abcdefgh"}]

            seen
            (atom nil)]

        (try (with-redefs [svar/ask-code! (fn [_ opts]
                                            (reset! seen opts)
                                            {:stop-reason :end
                                             :tool-calls []
                                             :content "done"
                                             :api-usage {:input-tokens 300 :output-tokens 1}
                                             :tokens {}})]
               (let [result (lp/run-iteration environment
                                              messages
                                              {:iteration 0
                                               :resolved-model {:provider :lmstudio
                                                                :name "local-model"}})]
                 (expect (= (:request-health result)
                            (prompt/request-health environment
                                                   (:messages @seen)
                                                   (:tools @seen)
                                                   "local-model")))
                 (expect (seq (get-in result [:request-health :breakdown])))
                 (expect (not (str/includes? (pr-str (:request-health result)) "abcdefgh")))))
             (finally (lp/dispose-environment! environment)))))
  (it
    "does not inject a prompt for models without native reasoning"
    (let [environment
          (lp/create-environment {:providers [{:id :lmstudio
                                               :network {:timeout-ms 1800000
                                                         :first-byte-timeout-ms 600000
                                                         :idle-timeout-ms 600000
                                                         :semantic-timeout-ms 600000}}]}
                                 {:db :memory})

          seen
          (atom nil)

          messages
          [{:role "system" :content "core"} {:role "user" :content "task"}]

          message-text
          (fn [{:keys [content]}]
            (if (string? content) content (apply str (keep :text content))))]

      (try (with-redefs [svar/ask-code!
                         (fn [_router opts]
                           (reset! seen opts)
                           {:stop-reason :end :tool-calls [] :content "done" :tokens {}})]
             (lp/run-iteration environment
                               messages
                               {:iteration 0
                                :reasoning-level :deep
                                :resolved-model
                                {:provider :lmstudio :name "local-model" :reasoning? false}})
             (expect (= ["system" "user"] (mapv :role (:messages @seen))))
             (expect (= ["core" "task"] (mapv message-text (:messages @seen))))
             (expect (not (contains? @seen :reasoning))))
           (expect (= 1800000 (:timeout-ms @seen)))
           (expect (= 600000 (:first-byte-timeout-ms @seen)))
           (expect (= 600000 (:idle-timeout-ms @seen)))
           (expect (= 600000 (:semantic-timeout-ms @seen)))
           ;; Provider policy leaves Vis' existing pre-header TTFT behavior intact.
           (expect (= rt/ASK_CODE_TTFT_TIMEOUT_MS (:ttft-timeout-ms @seen)))
           (finally (lp/dispose-environment! environment)))))
  (it "builds valid evidence for same-model retries"
      (let [iteration
            {:iteration 1
             :provider "zai-coding-plan"
             :model "glm-5.2"
             :effective "high"
             :wire-style :zai-effort
             :wire-fragment {:thinking {:type "enabled"} :reasoning_effort "high"}
             :fallback? false
             :selected {:provider "zai-coding-plan" :model "glm-5.2"}
             :requested "high"}

            eval
            (turn-eval-evidence "high" [{:reasoning-effort iteration}])]

        (expect (true? (:valid? eval)))
        (expect (= [iteration] (get-in eval [:reasoning-effort :iterations])))))
  (it "explains invalid evidence when no iteration completed"
      (let [eval (turn-eval-evidence "high" [])]
        (expect (false? (:valid? eval)))
        (expect (= :missing-reasoning-effort-evidence (get-in eval [:invalid-reasons 0 :type])))))
  (it "a later provider/model fallback invalidates the whole eval"
      (let [base
            {:provider "zai-coding-plan"
             :model "glm-5.2"
             :effective "max"
             :wire-style :zai-effort
             :wire-fragment {:thinking {:type "enabled"} :reasoning_effort "max"}
             :selected {:provider "zai-coding-plan" :model "glm-5.2"}
             :requested "max"}

            eval
            (turn-eval-evidence "max"
                                [{:reasoning-effort (assoc base
                                                      :iteration 1
                                                      :fallback? false)}
                                 {:reasoning-effort (assoc base
                                                      :iteration 2
                                                      :provider "zai"
                                                      :fallback? true)}])]

        (expect (false? (:valid? eval)))
        (expect (= :provider-model-fallback (get-in eval [:invalid-reasons 0 :type])))
        (expect (= 2 (count (get-in eval [:reasoning-effort :iterations]))))))
  (it
    "threads raw max unchanged and bypasses abstract quick translation"
    (let [environment
          (lp/create-environment ::router {:db :memory})

          seen
          (atom nil)]

      (try (with-redefs [svar/ask-code! (fn [_router opts]
                                          (reset! seen opts)
                                          {:stop-reason :end
                                           :tool-calls []
                                           :content "done"
                                           :tokens {}
                                           :routed/reasoning-effort {:requested "max"
                                                                     :effective "max"
                                                                     :supported ["high" "max"]
                                                                     :wire-style :zai-effort
                                                                     :extra-body
                                                                     {:thinking {:type "enabled"}
                                                                      :reasoning_effort "max"}}})]
             (let [result (lp/run-iteration environment
                                            []
                                            {:iteration 0
                                             :reasoning-level :quick
                                             :reasoning-effort "max"
                                             :resolved-model {:provider :zai-coding-plan
                                                              :name "glm-5.2"
                                                              :reasoning? true
                                                              :reasoning-style :zai-effort}})]
               (expect (= "max" (:reasoning-effort @seen)))
               (expect (not (contains? @seen :reasoning)))
               (expect (= "max" (get-in result [:reasoning-effort-resolution :effective])))))
           (finally (lp/dispose-environment! environment))))))

(def ^:private prose-beyond-code (deref #'lp/prose-beyond-code))

(def ^:private test-prompt-cache-context {:id "pcctx-v1-test" :fixed-prefix-weight 0})

(defn- sample-prompt-cache!
  [history provider model messages input-tokens cache-read-tokens request-start-ms]
  (dissoc (#'lp/note-prompt-cache-request!
           history
           provider
           model
           test-prompt-cache-context
           messages
           input-tokens
           cache-read-tokens
           request-start-ms)
    :reuse-kind))

(defn- resume-prompt-cache
  [history provider model turn-position summaries stable-messages turn-messages]
  (#'lp/resumable-prompt-message-base
   history
   provider
   model
   test-prompt-cache-context
   turn-position
   summaries
   stable-messages
   turn-messages))

(defdescribe
  prompt-cache-reusable-prefix-test
  "The reuse denominator is what the previous same-route request could still have
   left in the provider cache. A fold, a rewrite or an expiry is CLASSIFIED and
   keeps its denominator: a metric that drops its own misses can only report
   survivors."
  (it
    "includes the fixed tool prefix, honors measured reads, and names estimated samples"
    (let [sample!
          @#'lp/note-prompt-cache-request!

          prior
          [{:role "system" :content "stable"}
           {:role "user" :content (apply str (repeat 100 "old"))}]

          rewrite
          [{:role "system" :content "stable"} {:role "user" :content "folded"}]

          context
          {:id "pcctx-v1-tools" :fixed-prefix-weight 1000}

          no-fixed
          {:id "pcctx-v1-no-tools" :fixed-prefix-weight 0}

          with-fixed
          (atom {})

          without-fixed
          (atom {})]

      (expect (= {:reusable-tokens 0 :continuity :initial :reuse-kind nil}
                 (sample! with-fixed :anthropic "opus" context prior 10000 0 0)))
      (sample! without-fixed :anthropic "opus" no-fixed prior 10000 0 0)
      (let [estimated
            (sample! with-fixed :anthropic "opus" context rewrite 8000 7900 1000)

            message-only
            (sample! without-fixed :anthropic "opus" no-fixed rewrite 8000 0 1000)]

        ;; The one Python tool and provider preamble precede messages on the cache
        ;; wire. Once the shared system breakpoint survives, that fixed prefix is
        ;; part of the estimate rather than disappearing from its denominator.
        (expect (= :estimated (:reuse-kind estimated)))
        (expect (= :rewrite (:continuity estimated)))
        (expect (= 7900 (:reusable-tokens estimated)))
        (expect (> (:reusable-tokens estimated) (:reusable-tokens message-only))))
      ;; The provider's cache-read count is hard evidence and floors an estimate,
      ;; but never above this request's total input.
      (expect (= 8000
                 (:reusable-tokens (sample! with-fixed
                                            :anthropic
                                            "opus"
                                            context
                                            [{:role "system" :content "elsewhere"}]
                                            8000
                                            9000
                                            2000))))))
  (it
    "never estimates shared bytes beyond the last surviving breakpoint"
    (let [sample!
          @#'lp/note-prompt-cache-request!

          context
          {:id "pcctx-v1-anchors" :fixed-prefix-weight 0}

          prior
          [{:role "system" :content "stable"} {:role "user" :content "u1"}
           {:role "assistant" :content "a1"} {:role "user" :content "u2"}
           {:role "assistant" :content "a2"} {:role "user" :content "u3"}
           {:role "assistant" :content "a3"}]

          shared-unmarked
          (conj (subvec (vec prior) 0 4) {:role "assistant" :content "changed"})

          system-only
          [(first prior) {:role "user" :content "changed"}]

          sample-after
          (fn [messages]
            (let [history (atom {})]
              (sample! history :anthropic "opus" context prior 10000 0 0)
              (sample! history :anthropic "opus" context messages 10000 0 1000)))

          through-unmarked
          (sample-after shared-unmarked)

          through-system
          (sample-after system-only)]

      ;; With seven prior messages, the anchors are the system block and indexes
      ;; 4-6. Shared messages 1-3 have no independent cache entry, so diverging
      ;; before index 4 can recover no more than the system-only rewrite.
      (expect (= :estimated (:reuse-kind through-unmarked)))
      (expect (pos? (:reusable-tokens through-unmarked)))
      (expect (= (:reusable-tokens through-system) (:reusable-tokens through-unmarked)))))
  (it "rotates the baseline when the provider fixed-prefix identity changes"
      (let [history
            (atom {})

            sample!
            @#'lp/note-prompt-cache-request!

            messages
            [{:role "system" :content "stable"} {:role "user" :content "one"}]

            first-context
            {:id "pcctx-v1-first" :fixed-prefix-weight 500}

            changed-context
            {:id "pcctx-v1-changed" :fixed-prefix-weight 500}]

        (sample! history :anthropic "opus" first-context messages 6000 0 0)
        (expect (= {:reusable-tokens 0 :continuity :cache-context-changed :reuse-kind nil}
                   (sample! history :anthropic "opus" changed-context messages 6000 0 1000)))
        (expect (= changed-context (get-in @history [[:anthropic "opus"] :prompt-cache-context])))))
  (it "counts the whole prior request while it is still an exact prefix"
      (let [history
            (atom {})

            sample!
            sample-prompt-cache!]

        (expect
          (= {:reusable-tokens 0 :continuity :initial}
             (sample! history :openai-codex "gpt-5.6" [{:role "user" :content "one"}] 9463 0 1000)))
        (expect (= {:reusable-tokens 9463 :continuity :append-only}
                   (sample! history
                            :openai-codex
                            "gpt-5.6"
                            [{:role "user" :content "one"} {:role "assistant" :content "two"}]
                            31867
                            9728
                            2000)))
        ;; Another model is another cache, so it starts its own baseline instead
        ;; of inheriting one it could never have read.
        (expect (= {:reusable-tokens 0 :continuity :initial}
                   (sample! history
                            :openai-codex
                            "gpt-5.6-sol"
                            [{:role "user" :content "one"}]
                            12000
                            0
                            3000)))))
  (it "keeps a fold in the denominator as the share of the prior request that survived"
      (let [history
            (atom {})

            sample!
            sample-prompt-cache!

            msg
            (fn [n]
              {:role "user" :content (str n)})

            four
            (mapv msg (range 4))]

        (sample! history :anthropic "opus" four 4000 0 0)
        ;; A fold replaces the tail: half of the prior request survives, so half
        ;; of its input stays measurable — the case a coverage number must not
        ;; be allowed to skip.
        (expect (= {:reusable-tokens 2000 :continuity :rewrite}
                   (sample! history
                            :anthropic
                            "opus"
                            [(msg 0) (msg 1) {:role "user" :content "folded"}]
                            2600
                            1900
                            1000)))
        (expect
          (=
            {:reusable-tokens 0 :continuity :rewrite}
            (sample! history :anthropic "opus" [{:role "user" :content "elsewhere"}] 900 0 2000)))))
  (it
    "calls an untouched prefix expired when the window passed and nothing came back"
    (let [history
          (atom {})

          sample!
          sample-prompt-cache!

          one
          [{:role "user" :content "one"}]

          two
          (conj one {:role "assistant" :content "two"})]

      (sample! history :anthropic "opus" one 1000 0 0)
      ;; :anthropic writes 1-hour breakpoints, so five minutes is not stale there.
      (expect (= {:reusable-tokens 1000 :continuity :append-only}
                 (sample! history :anthropic "opus" two 1200 0 300001)))
      (expect (= {:reusable-tokens 1200 :continuity :expired}
                 (sample! history
                          :anthropic
                          "opus"
                          (conj two {:role "user" :content "three"})
                          1400
                          0
                          3900002)))
      ;; The provider proved the prefix is still resident, so age alone must not
      ;; call it expired.
      (expect (= {:reusable-tokens 1400 :continuity :append-only}
                 (sample!
                   history
                   :anthropic
                   "opus"
                   (conj two {:role "user" :content "three"} {:role "assistant" :content "four"})
                   1600
                   1100
                   7200002)))
      ;; A route left on the five-minute tier keeps the tighter window.
      (sample! history :zai-coding-plan "glm" one 1000 0 0)
      (expect (= {:reusable-tokens 1000 :continuity :expired}
                 (sample! history :zai-coding-plan "glm" two 1200 0 300001))))))

;; Regression, user report: a completed turn was reconstructed into a different request on
;; the next turn, discarding a provider-resident prefix; folds could then rewrite it repeatedly.
(defdescribe
  cross-turn-prompt-prefix-test
  "A successful live turn remains the exact provider prefix of its immediate follow-up;
   route or semantic-history changes deliberately fall back to the canonical recap."
  (it
    "refuses exact replay when the opaque provider cache context changed"
    (let [history
          (atom {})

          sample!
          @#'lp/note-prompt-cache-request!

          complete!
          @#'lp/mark-prompt-cache-turn-complete!

          resume
          @#'lp/resumable-prompt-message-base

          context
          {:id "pcctx-v1-current" :fixed-prefix-weight 400}

          changed
          {:id "pcctx-v1-new-tool-schema" :fixed-prefix-weight 450}

          stable
          [{:role "system" :content "stable"}]

          request
          (conj stable {:role "user" :content "turn one"})]

      (sample! history :anthropic "opus" context request 5000 0 (util/now-ms))
      (complete! history :anthropic "opus" 1 [] 1 {:role "assistant" :content "done"})
      (expect (:resumed? (resume history
                                 :anthropic
                                 "opus"
                                 context
                                 2
                                 []
                                 stable
                                 [{:role "user" :content "turn two"}])))
      (expect (nil? (resume history
                            :anthropic
                            "opus"
                            changed
                            2
                            []
                            stable
                            [{:role "user" :content "turn two"}])))))
  (it
    "appends the accepted answer and next user message to the exact final request"
    (let [history
          (atom {})

          sample!
          sample-prompt-cache!

          complete!
          @#'lp/mark-prompt-cache-turn-complete!

          resume
          resume-prompt-cache

          stable
          [{:role "system" :content "stable"}]

          request
          (conj stable
                {:role "user" :content "turn one"}
                {:role "assistant" :content "tool call"}
                {:role "user" :content "tool result"})

          final-assistant
          {:role "assistant" :content "answer one"}

          next-user
          [{:role "user" :content "turn two"}]

          summaries
          [{"scopes" ["t1/i1"] "gist" "settled"}]]

      (sample! history :zai-coding-plan "glm-5.3-flash" request 12000 0 (util/now-ms))
      (complete! history
                 :zai-coding-plan
                 "glm-5.3-flash"
                 1
                 summaries
                 (count stable)
                 final-assistant)
      (let [base (resume history :zai-coding-plan "glm-5.3-flash" 2 summaries stable next-user)]
        (expect (:resumed? base))
        (expect (= request (subvec (:messages base) 0 (count request))))
        (expect (= (into (conj request final-assistant) next-user) (:messages base))))))
  (it
    "refuses a stale entry, different route, non-adjacent turn, changed ledger, or changed system"
    (let [history
          (atom {})

          sample!
          sample-prompt-cache!

          complete!
          @#'lp/mark-prompt-cache-turn-complete!

          resume
          resume-prompt-cache

          stable
          [{:role "system" :content "stable one"} {:role "system" :content "stable two"}]

          request
          (conj stable {:role "user" :content "turn one"})

          summaries
          [{"scopes" ["t1/i1"] "gist" "settled"}]

          args
          [:zai-coding-plan "glm-5.3-flash" 2 summaries stable [{:role "user" :content "turn two"}]]

          now
          (util/now-ms)]

      (sample! history :zai-coding-plan "glm-5.3-flash" request 8000 0 now)
      (complete! history
                 :zai-coding-plan
                 "glm-5.3-flash"
                 1
                 summaries
                 (count stable)
                 {:role "assistant" :content "answer one"})
      (expect (nil? (apply resume history (assoc args 1 "other-model"))))
      (expect (nil? (apply resume history (assoc args 2 3))))
      (expect (nil? (apply resume history (assoc args 3 []))))
      (expect (nil? (apply resume history (assoc args 4 [(first stable)]))))
      (expect (nil? (apply resume
                      history
                      (assoc args
                        4 [{:role "system" :content "changed one"}
                           {:role "system" :content "changed two"}]))))
      (swap! history assoc-in [[:zai-coding-plan "glm-5.3-flash"] :at-ms] (- now 300001))
      (expect (nil? (apply resume history args)))))
  (it
    "carries three turns, falls back once to the stable prefix after a fold, then grows again"
    (let [history
          (atom {})

          sample!
          sample-prompt-cache!

          complete!
          @#'lp/mark-prompt-cache-turn-complete!

          resume
          resume-prompt-cache

          base!
          @#'lp/prompt-message-base!

          common-prefix
          @#'lp/common-prefix-count

          provider
          :zai-coding-plan

          model
          "glm-5.3-flash"

          stable
          [{:role "system" :content "stable one"} {:role "system" :content "stable two"}]

          summaries
          []

          user-message
          (fn [position]
            {:role "user" :content (str "turn " position)})

          assistant-message
          (fn [position]
            {:role "assistant" :content (str "answer " position)})

          started-at
          (- (util/now-ms) 100)]

      (letfn
        [(finish-turn! [position request input-tokens]
           (sample! history provider model request input-tokens 0 (+ started-at position))
           (complete! history
                      provider
                      model
                      position
                      summaries
                      (count stable)
                      (assistant-message position))
           request)
         (next-base [position]
           (resume history provider model position summaries stable [(user-message position)]))]
        (let [request-1
              (finish-turn! 1 (conj stable (user-message 1)) 8000)

              base-2
              (next-base 2)

              request-2
              (finish-turn! 2 (:messages base-2) 8200)

              base-3
              (next-base 3)

              request-3
              (finish-turn! 3 (:messages base-3) 8400)

              base-4
              (next-base 4)

              request-before-fold
              (:messages base-4)

              _
              (sample! history provider model request-before-fold 8600 8400 (+ started-at 4))

              folded-summaries
              [{"through" "t3/i1" "gist" "folded"}]

              canonical-calls
              (atom 0)

              canonical-messages
              (into stable [{:role "user" :content "folded through turn three"} (user-message 4)])

              base-state
              (atom base-4)

              post-fold
              (base! base-state
                     folded-summaries
                     (fn []
                       (swap! canonical-calls inc)
                       canonical-messages))

              post-fold-again
              (base! base-state folded-summaries #(throw (ex-info "must stay stable" {})))

              rewrite
              (sample! history provider model (:messages post-fold) 8700 0 (+ started-at 5))

              request-after-tool
              (conj (:messages post-fold)
                    {:role "assistant" :content "tool call"}
                    {:role "user" :content "tool result"})

              recached
              (sample! history provider model request-after-tool 8900 8500 (+ started-at 6))]

          (expect (:resumed? base-2))
          (expect (:resumed? base-3))
          (expect (:resumed? base-4))
          (expect (= request-1 (subvec request-2 0 (count request-1))))
          (expect (= request-2 (subvec request-3 0 (count request-2))))
          (expect (= request-3 (subvec request-before-fold 0 (count request-3))))
          (expect (= 1 @canonical-calls))
          (expect (false? (:resumed? post-fold)))
          (expect (= post-fold post-fold-again))
          (expect (= (count stable) (common-prefix request-before-fold (:messages post-fold))))
          (expect (= :rewrite (:continuity rewrite)))
          (expect (pos? (:reusable-tokens rewrite)))
          (expect (< (:reusable-tokens rewrite) 8600))
          (expect (= :append-only (:continuity recached)))
          (expect (= 8700 (:reusable-tokens recached)))))))
  (it "does not duplicate prior-turn seeds while an exact carried prefix is active"
      (let [visible
            @#'lp/conversation-trailer-for-base

            seeded
            [1 {:preserved-thinking/replay? false :blocks [{:stdout "old"}]}]

            current
            [1 {:preserved-thinking/replay? true :blocks [{:stdout "new"}]}]

            trailer
            [seeded current]]

        (expect (= [current] (visible trailer true)))
        (expect (= trailer (visible trailer false))))))

(defdescribe
  prompt-cache-turn-completion-test
  (it
    "marks and checkpoints a persisted accepted answer, then removes the internal handoff"
    (let [history
          (atom {})

          stored
          (atom nil)

          sample!
          sample-prompt-cache!

          request
          [{:role "system" :content "stable"} {:role "user" :content "turn one"}]

          assistant
          {:role "assistant" :content "done"}

          standing
          {:block "<context>exact</context>" :baseline {"session_id" "s1"}}

          env
          {:db-info ::db
           :session-id "s1"
           :session/state-id "state-1"
           :router (helper-router :zai-coding-plan nil)
           :turn-state-atom (ctx-loop/make-turn-state-atom)
           :standing-ctx-atom (atom standing)
           :prompt-cache-history-atom history}]

      (sample! history :zai-coding-plan "model" request 5000 0 (util/now-ms))
      (with-redefs [persistance/db-store-session-turn!
                    (fn [& _]
                      "turn-1")

                    persistance/db-update-session-turn!
                    (fn [& _]
                      true)

                    persistance/db-set-session-prompt-cache-state!
                    (fn [_db state-id state]
                      (reset! stored [state-id state])
                      state)

                    lp/session-turn-position
                    (fn [& _]
                      1)

                    titling/maybe-auto-title!
                    (fn [& _]
                      nil)

                    lp/iteration-loop
                    (fn [& _]
                      {:answer "done"
                       :iteration-count 1
                       :duration-ms 1
                       :prompt-cache-completion {:provider :zai-coding-plan
                                                 :model "model"
                                                 :turn-position 1
                                                 :summaries []
                                                 :stable-message-count 1
                                                 :assistant-message assistant}})]

        (let [result (#'lp/run-normal-turn! env "finish" {})]
          (expect (nil? (:prompt-cache-completion result)))
          (expect
            (= {:turn-position 1 :summaries [] :stable-message-count 1 :assistant-message assistant}
               (get-in @history [[:zai-coding-plan "model"] :completed-turn])))
          (expect (= "state-1" (first @stored)))
          (expect (= [:zai-coding-plan "model"] (get-in @stored [1 :route])))
          (expect (= request (get-in @stored [1 :entry :messages])))
          (expect (= standing (get-in @stored [1 :standing-ctx])))))))
  (it
    "does not mark a terminal that another caller already claimed"
    (let [history
          (atom {})

          sample!
          sample-prompt-cache!

          env
          {:db-info ::db
           :session-id "s1"
           :router (helper-router :zai-coding-plan nil)
           :turn-state-atom (ctx-loop/make-turn-state-atom)
           :prompt-cache-history-atom history}]

      (sample! history :zai-coding-plan "model" [{:role "system" :content "stable"}] 5000 0 1000)
      (with-redefs [persistance/db-store-session-turn!
                    (fn [& _]
                      "turn-1")

                    persistance/db-update-session-turn!
                    (fn [& _]
                      true)

                    lp/session-turn-position
                    (fn [& _]
                      1)

                    titling/maybe-auto-title!
                    (fn [& _]
                      nil)

                    lp/iteration-loop
                    (fn [& _]
                      {:answer "done"
                       :iteration-count 1
                       :duration-ms 1
                       :prompt-cache-completion {:provider :zai-coding-plan
                                                 :model "model"
                                                 :turn-position 1
                                                 :summaries []
                                                 :stable-message-count 1
                                                 :assistant-message {:role "assistant"
                                                                     :content "done"}}})]

        (#'lp/run-normal-turn! env "finish" {:hooks {:claim-terminal! (constantly false)}})
        (expect (nil? (get-in @history [[:zai-coding-plan "model"] :completed-turn])))))))

(defdescribe
  copilot-action-service-headers-test
  (it "marks Copilot Enterprise requests with X-Initiator for the action service"
      (expect (= {"X-Initiator" "agent"}
                 (#'lp/copilot-llm-headers {:provider :github-copilot-enterprise} "agent"))))
  (it "does not add action-service headers for non-Copilot providers"
      (expect (nil? (#'lp/copilot-llm-headers {:provider :anthropic-coding-plan} "agent")))))

;; Provider-specific request identity enters through the extension lifecycle; the
;; engine only applies the generic provider/header contribution it receives.
(defdescribe
  session-provider-kickoff-headers-test
  (it "keeps session headers local and stable across conversation environments"
      (let [shared-router
            {:providers [{:id :opencode-go :llm-headers {"existing" "kept"}} {:id :anthropic}]}

            first-env
            (lp/create-environment shared-router {:db :memory})

            second-env
            (lp/create-environment shared-router {:db :memory})]

        (try (let [first-session
                   (str (:session-id first-env))

                   second-session
                   (str (:session-id second-env))

                   first-headers
                   (get-in first-env [:router :providers 0 :llm-headers])

                   second-headers
                   (get-in second-env [:router :providers 0 :llm-headers])]

               (expect (= {"existing" "kept" "x-opencode-session" first-session} first-headers))
               (expect (= second-session (get second-headers "x-opencode-session")))
               (expect (= first-session
                          (get-in first-env
                                  [:session-llm-headers :opencode-go "x-opencode-session"])))
               (expect (not= first-session second-session))
               (expect (= {:id :anthropic} (get-in first-env [:router :providers 1])))
               (expect (= {:providers [{:id :opencode-go :llm-headers {"existing" "kept"}}
                                       {:id :anthropic}]}
                          shared-router)))
             (finally (lp/dispose-environment! first-env) (lp/dispose-environment! second-env))))))

(defdescribe
  session-provider-kickoff-router-refresh-test
  (it "kicks off a provider added to a live session router"
      (let [initial-router
            {:providers [{:id :anthropic}]}

            refreshed-router
            {:providers [{:id :opencode-go :llm-headers {"existing" "kept"}} {:id :anthropic}]}

            environment
            (lp/create-environment initial-router {:db :memory})

            local-cache
            (atom {(:session-id environment) {:environment environment}})]

        (try (with-redefs-fn {#'lp/cache local-cache}
               (fn []
                 (lp/refresh-cached-routers! refreshed-router)))
             (let [refreshed-environment
                   (get-in @local-cache [(:session-id environment) :environment])

                   session-id
                   (str (:session-id environment))]

               (expect (= {"existing" "kept" "x-opencode-session" session-id}
                          (get-in refreshed-environment [:router :providers 0 :llm-headers])))
               (expect (= {"x-opencode-session" session-id}
                          (get-in refreshed-environment [:session-llm-headers :opencode-go])))
               (expect (= refreshed-router
                          {:providers [{:id :opencode-go :llm-headers {"existing" "kept"}}
                                       {:id :anthropic}]})))
             (finally (lp/dispose-environment! environment))))))

(defdescribe
  codex-stateful-session-test
  (it
    "sends only the new canonical suffix over one sticky Codex session"
    (let [session-atom
          (atom nil)

          history
          (atom [])

          opened
          (atom 0)

          turns
          (atom [])

          closed
          (atom [])

          environment
          {:router ::router :llm-session-atom session-atom}

          resolved
          {:provider :openai-codex :name "gpt-5.6"}

          system
          {:role "system" :content "stable"}

          user
          {:role "user" :content "act"}

          assistant
          {:role "assistant" :content [{:type "tool_use" :id "call-1"}]}

          result
          {:role "user" :content [{:type "tool_result" :tool_use_id "call-1"}]}]

      (with-redefs [svar/open-session
                    (fn [router opts]
                      (expect (= ::router router))
                      (expect (nil? (:messages opts)))
                      (swap! opened inc)
                      ::session)

                    svar/session-history
                    (fn [_]
                      @history)

                    svar/ask!
                    (fn [session opts]
                      (swap! turns conj [session (:messages opts)])
                      (reset! history (into @history (concat (:messages opts) [assistant])))
                      {:stop-reason :tool-calls :assistant-message assistant})

                    svar/close-session!
                    (fn [session]
                      (swap! closed conj session))]

        (#'lp/ask-code-with-session! environment resolved {:messages [system user]})
        (#'lp/ask-code-with-session!
         environment
         resolved
         {:messages [system user assistant result]})
        (expect (= 1 @opened))
        (expect (= [[::session [system user]] [::session [result]]] @turns))
        (expect (= [] @closed)))))
  ;; Regression, issue 9cc1d0a0-2836-4518-b504-bc9f70eae7c4: the real Codex
  ;; credential hydration adds the token, endpoint, and account header to its bare
  ;; provider preset on every attempt. Equal effective routers were distinct objects,
  ;; so the session identity check closed and re-opened the socket every iteration.
  (it
    "keeps one session across equivalent real Codex credential hydration"
    (let [session-atom
          (atom nil)

          history
          (atom [])

          opened
          (atom 0)

          closed
          (atom [])

          router
          {:providers [{:id :openai-codex}] :health ::live}

          credential
          {:token "k"
           :api-url "https://chatgpt.example.test/backend-api"
           :llm-headers {"chatgpt-account-id" "account-1"}}

          environment
          {:router router :llm-session-atom session-atom}

          resolved
          {:provider :openai-codex :name "gpt-5.6"}

          system
          {:role "system" :content "stable"}

          user
          {:role "user" :content "act"}

          assistant
          {:role "assistant" :content [{:type "tool_use" :id "call-1"}]}

          result
          {:role "user" :content [{:type "tool_result" :tool_use_id "call-1"}]}]

      (with-redefs [registry/provider-by-id
                    (fn [_]
                      {:provider/get-token-fn (constantly credential)})

                    config/command-token
                    (fn [_]
                      nil)

                    svar/open-session
                    (fn [_ _]
                      (swap! opened inc)
                      ::session)

                    svar/session-history
                    (fn [_]
                      @history)

                    svar/ask!
                    (fn [_ opts]
                      (reset! history (into @history (concat (:messages opts) [assistant])))
                      {:stop-reason :tool-calls :assistant-message assistant})

                    svar/close-session!
                    (fn [session]
                      (swap! closed conj session))]

        (let [first-attempt
              (#'lp/hydrate-environment-router environment)

              first-router
              (:router first-attempt)]

          (expect (not (identical? router first-router)))
          (expect (= "k" (get-in first-router [:providers 0 :api-key])))
          (expect (= (:api-url credential) (get-in first-router [:providers 0 :base-url])))
          (#'lp/ask-code-with-session! first-attempt resolved {:messages [system user]})
          (let [second-attempt
                (#'lp/hydrate-environment-router environment)

                second-router
                (:router second-attempt)]

            (expect (= first-router second-router))
            (expect (not (identical? first-router second-router)))
            (#'lp/ask-code-with-session!
             second-attempt
             resolved
             {:messages [system user assistant result]})
            (expect (= 1 @opened))
            (expect (= [] @closed)))))))
  (it
    "reuses one Codex socket across model changes and pins each turn's model"
    (let [session-atom
          (atom nil)

          router
          (Object.)

          history
          (atom [])

          opened
          (atom [])

          turns
          (atom [])

          closed
          (atom [])

          system
          {:role "system" :content "stable"}

          first-user
          {:role "user" :content "luna"}

          first-assistant
          {:role "assistant" :content "luna answer"}

          second-user
          {:role "user" :content "terra"}

          second-assistant
          {:role "assistant" :content "terra answer"}]

      (with-redefs [svar/open-session
                    (fn [actual-router opts]
                      (swap! opened conj [actual-router opts])
                      ::session)

                    svar/session-history
                    (fn [_]
                      @history)

                    svar/ask!
                    (fn [session opts]
                      (let [assistant (if (empty? @turns) first-assistant second-assistant)]
                        (swap! turns conj [session (:messages opts) (:routing opts)])
                        (reset! history (into @history (concat (:messages opts) [assistant])))
                        {:assistant-message assistant}))

                    svar/close-session!
                    (fn [session]
                      (swap! closed conj session))]

        (#'lp/ask-code-with-session!
         {:router router :llm-session-atom session-atom}
         {:provider :openai-codex :name "gpt-5.6-luna"}
         {:messages [system first-user]})
        (#'lp/ask-code-with-session!
         {:router router :llm-session-atom session-atom}
         {:provider :openai-codex :name "gpt-5.6-terra"}
         {:messages [system first-user first-assistant second-user]})
        (expect (= 1 (count @opened)))
        (expect (= [[::session [system first-user] {:provider :openai-codex :model "gpt-5.6-luna"}]
                    [::session [second-user] {:provider :openai-codex :model "gpt-5.6-terra"}]]
                   @turns))
        (expect (= [] @closed)))))
  (it
    "replaces a Codex session when reload supplies a new router snapshot"
    (let [old-router
          (with-meta {:id :router} {:generation :old})

          new-router
          (with-meta {:id :router} {:generation :new})

          session-atom
          (atom {:provider :openai-codex :router old-router :session ::old})

          opened
          (atom [])

          closed
          (atom [])

          asked
          (atom nil)]

      (with-redefs [svar/open-session
                    (fn [router opts]
                      (swap! opened conj [router opts])
                      ::fresh)

                    svar/session-history
                    (fn [_]
                      [])

                    svar/ask!
                    (fn [session opts]
                      (reset! asked [session opts])
                      {:stop-reason :end})

                    svar/close-session!
                    (fn [session]
                      (swap! closed conj session))]

        (#'lp/ask-code-with-session!
         {:router new-router :llm-session-atom session-atom}
         {:provider :openai-codex :name "gpt-5.6"}
         {:messages [{:role "user" :content "after reload"}]})
        (expect (= old-router new-router))
        (expect (not (identical? old-router new-router)))
        (expect (= [::old] @closed))
        (expect (= 1 (count @opened)))
        (expect (identical? new-router (ffirst @opened)))
        (expect (= ::fresh (first @asked))))))
  (it
    "replays divergent canonical history without replacing the Codex session"
    (let [session-atom
          (atom {:provider :openai-codex :router ::router :session ::sticky})

          opened
          (atom [])

          closed
          (atom [])

          sent
          (atom nil)

          messages
          [{:role "system" :content "rebased"} {:role "user" :content "continue"}]]

      (with-redefs [svar/session-history
                    (fn [_]
                      [{:role "system" :content "old"}])

                    svar/open-session
                    (fn [_ opts]
                      (swap! opened conj opts)
                      ::fresh)

                    svar/close-session!
                    (fn [session]
                      (swap! closed conj session))

                    svar/ask!
                    (fn [session opts]
                      (reset! sent [session (:history opts) (:messages opts)])
                      {:stop-reason :end})]

        (#'lp/ask-code-with-session!
         {:router ::router :llm-session-atom session-atom}
         {:provider :openai-codex :name "gpt-5.6"}
         {:messages messages})
        (expect (= [] @closed))
        (expect (= [] @opened))
        (expect (= [::sticky messages nil] @sent))
        (expect (= ::sticky (:session @session-atom))))))
  (it
    "closes the sticky Codex session for another provider and opens fresh on return"
    (let [session-atom
          (atom {:provider :openai-codex :router ::router :session ::session})

          closed
          (atom [])

          opened
          (atom [])

          asked
          (atom nil)]

      (with-redefs [svar/close-session!
                    (fn [session]
                      (swap! closed conj session))

                    svar/ask-code!
                    (fn [router opts]
                      [router opts])

                    svar/open-session
                    (fn [router opts]
                      (swap! opened conj [router opts])
                      ::fresh)

                    svar/session-history
                    (fn [_]
                      [])

                    svar/ask!
                    (fn [session opts]
                      (reset! asked [session opts])
                      {:stop-reason :end})]

        (expect (= [::router {:messages []}]
                   (#'lp/ask-code-with-session!
                    {:router ::router :llm-session-atom session-atom}
                    {:provider :anthropic :name "claude"}
                    {:messages []})))
        (expect (= [::session] @closed))
        (expect (nil? @session-atom))
        (#'lp/ask-code-with-session!
         {:router ::router :llm-session-atom session-atom}
         {:provider :openai-codex :name "gpt-5.6"}
         {:messages [{:role "user" :content "back"}]})
        (expect (= 1 (count @opened)))
        (expect (= ::fresh (first @asked)))
        (expect (= ::fresh (:session @session-atom)))))))

(defdescribe
  environment-lifecycle-test
  ;; Regression, user report: a cold disposed environment could start a new worker.
  (it "never builds Python for creation, teardown, or entry after cold disposal"
      (let [builds (atom 0)]
        (with-redefs [env/create-python-context (fn [& _]
                                                  (swap! builds inc)
                                                  (throw (ex-info "unexpected build" {})))]
          (let [environment (lp/create-environment ::router {:db :memory})]
            (expect (nil? (env/python-context-if-built environment)))
            (expect (zero? @builds))
            (lp/dispose-environment! environment)
            (expect (zero? @builds))
            (expect (try (env/python-context environment) false (catch Throwable _ true)))
            (expect (zero? @builds))))))
  (it "refuses the python session of an environment it disposed"
      (let [environment
            (lp/create-environment ::router {:db :memory})

            python-context
            (env/python-context environment)]

        (lp/dispose-environment! environment)
        (expect (try (env/run-python-block python-context "1") false (catch Throwable _ true)))))
  ;; Regression, user report: a rebuilt CLI session defaulted to TUI, removing its
  ;; autonomous system block and breaking the exact provider prefix between turns.
  (it
    "restores the CLI channel and exact provider prefix after process rebuild"
    (let [dir
          (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-prompt-prefix"
                     (make-array java.nio.file.attribute.FileAttribute 0)))

          db-path
          (.getPath (java.io.File. dir "vis.mdb"))

          route
          [:zai-coding-plan "glm-5.3-flash"]

          standing
          {:block "<context>exact</context>" :baseline {"session_id" "s1"}}

          stable-for
          (fn [environment]
            (prompt/assemble-stable-prompt-messages environment
                                                    {:active-extensions (prompt/active-extensions
                                                                          environment)
                                                     :session-context (:block standing)}))

          session-id
          (atom nil)

          persisted-entry
          (atom nil)

          first-stable
          (atom nil)]

      (try (let [first-env (lp/create-environment ::router {:db db-path :channel :cli})]
             (try (let [stable (stable-for first-env)
                        request (conj stable {:role "user" :content "turn one"})
                        entry {:messages request
                               :weights (vec (repeat (count request) 1))
                               :input-tokens 12000
                               :at-ms (util/now-ms)
                               :prompt-cache-context test-prompt-cache-context
                               :completed-turn {:turn-position 1
                                                :summaries []
                                                :stable-message-count (count stable)
                                                :assistant-message {:role "assistant"
                                                                    :content "done"}}}]

                    (reset! session-id (:session-id first-env))
                    (reset! persisted-entry entry)
                    (reset! first-stable stable)
                    (expect (some #(str/includes? (:content %) "NON-INTERACTIVE ONE-SHOT RUN")
                                  stable))
                    (persistance/db-set-session-prompt-cache-state!
                      (:db-info first-env)
                      (:session/state-id first-env)
                      {:route route :entry entry :standing-ctx standing}))
                  (finally (lp/dispose-environment! first-env))))
           (let [resumed (lp/create-environment ::router {:db db-path :session @session-id})]
             (try (let [stable (stable-for resumed)
                        next-user [{:role "user" :content "turn two"}]
                        base (#'lp/resumable-prompt-message-base
                              (:prompt-cache-history-atom resumed)
                              (first route)
                              (second route)
                              test-prompt-cache-context
                              2
                              []
                              stable
                              next-user)]

                    (expect (= :cli (:channel resumed)))
                    (expect (= @first-stable stable))
                    (expect (= {route @persisted-entry} @(:prompt-cache-history-atom resumed)))
                    (expect (= standing @(:standing-ctx-atom resumed)))
                    (expect (:resumed? base))
                    (expect (= (:messages @persisted-entry)
                               (subvec (:messages base) 0 (count (:messages @persisted-entry))))))
                  (finally (lp/dispose-environment! resumed))))
           (finally (doseq [file (reverse (file-seq dir))]
                      (.delete ^java.io.File file)))))))

(defdescribe
  workspace-policy-isolation-test
  ;; Regression: two projects on one gateway shared its startup policy and health roots.
  (it
    "uses the pinned workspace for new/resumed environments, prompts, and health roots"
    (let [dir
          (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-project-policy"
                     (make-array java.nio.file.attribute.FileAttribute 0)))

          db
          (persistance/db-create-connection! :memory)

          store
          (clojure.java.io/file dir "global")

          startup
          (clojure.java.io/file dir "startup")

          other
          (clojure.java.io/file dir "other")

          bare
          (clojure.java.io/file dir "bare")

          old-dir
          (System/getProperty "user.dir")]

      (try (doseq [project [store startup other bare]]
             (.mkdirs project))
           (doseq [[project name] [[startup "startup"] [other "other"]]]
             (.mkdirs (clojure.java.io/file project "linked"))
             (spit (clojure.java.io/file project "linked/AGENTS.md") (str name " guidance"))
             (spit (clojure.java.io/file project "AGENTS.md") (str name "-instructions-marker"))
             (spit (clojure.java.io/file project "vis.yml")
                   (str "system_prompt: "
                        name
                        "-project-marker\n"
                        "workspace:\n  filesystem:\n    - id: linked\n      path: "
                        (.getCanonicalPath (clojure.java.io/file project "linked"))
                        "\n" "jail:\n  filesystem:\n    allow: [linked]\n")))
           (System/setProperty "user.dir" (.getCanonicalPath startup))
           (with-redefs [config/config-dir
                         (constantly (.getPath store))

                         lp/last-good-security-snapshot
                         (atom {})]

             (doseq [[project name] [[startup "startup"] [other "other"] [bare nil]]]
               (let [ws (workspace/create-trunk-at! db (.getCanonicalPath project))
                     environment (lp/create-environment ::router {:db db :workspace-id (:id ws)})
                     sid (:session-id environment)
                     check!
                     (fn [environment]
                       (let [policy (:security-policy environment)
                             linked (.getCanonicalPath (clojure.java.io/file project "linked"))
                             health (prompt/request-health environment [] [])
                             project-roots (filterv #(str/starts-with? (:path %)
                                                                       (.getCanonicalPath dir))
                                             (:roots health))]

                         (expect (= (if name {"linked_path" linked} {}) (:project-paths policy)))
                         (expect (= (if name [linked] []) (mapv :path project-roots)))
                         (when name
                           (expect (= "available" (get-in project-roots [0 :guidance :status]))))
                         (doseq [text [(:system-prompt (persistance/db-get-session db sid))
                                       (prompt/stable-prompt-text
                                         (prompt/assemble-stable-prompt-messages environment
                                                                                 {:active-extensions
                                                                                  []}))]]
                           (expect (= (boolean (= name "startup"))
                                      (str/includes? text "startup-project-marker")))
                           (expect (= (boolean (= name "other"))
                                      (str/includes? text "other-project-marker"))))
                         (let [text (prompt/stable-prompt-text
                                      (prompt/assemble-stable-prompt-messages environment
                                                                              {:active-extensions
                                                                               []}))]
                           (expect (= (= name "startup")
                                      (str/includes? text "startup-instructions-marker")))
                           (expect (= (= name "other")
                                      (str/includes? text "other-instructions-marker"))))))]

                 (try (check! environment) (finally (lp/dispose-environment! environment)))
                 ;; Resume carries no explicit workspace id and runs outside any workspace binding.
                 (let [resumed (lp/create-environment ::router {:db db :session sid})]
                   (try (check! resumed) (finally (lp/dispose-environment! resumed)))))))
           (finally (System/setProperty "user.dir" old-dir)
                    (config/invalidate-config-cache!)
                    (persistance/db-dispose-connection! db)
                    (doseq [file (reverse (file-seq dir))]
                      (.delete ^java.io.File file))))))
  (it "never borrows a last-good policy from another workspace on invalid config"
      (with-redefs [lp/last-good-security-snapshot (atom {})]
        (let [snapshot #(binding [workspace/*workspace-root* %1] (with-redefs
                                                                   [config/load-config-raw
                                                                    (constantly %2)]
                                                                   (#'lp/security-config-snapshot)))
              valid {"workspace" {"filesystem" [{"id" "linked" "path" "/project-a/linked"}]}
                     "jail" {"filesystem" {"allow" ["linked"]}}}
              first-policy (snapshot "/project-a" valid)
              invalid {"unknown_config_key" true}
              other-policy (snapshot "/project-b" invalid)]

          (expect (= {"linked_path" "/project-a/linked"} (:project-paths first-policy)))
          (expect (some? (:config-error other-policy)))
          (expect (empty? (:project-paths other-policy)))
          (expect (= (:project-paths first-policy)
                     (:project-paths (snapshot "/project-a" invalid))))))))

(defdescribe
  permission-config-snapshot-test
  (it "keeps every process-jail and network grant immutable until environment rebuild"
      (require 'com.blockether.vis.internal.config.validation :reload)
      (require 'com.blockether.vis.internal.loop :reload)
      (let [cfg
            (atom
              {"workspace" {"filesystem" [{"id" "full" "path" "/approved/full"}
                                          {"id" "read" "path" "/approved/read" "access" "read-only"}
                                          {"id" "cache" "path" "/approved/cache" "search" false}]}
               "jail" {"enabled" true
                       "filesystem" {"allow" ["full" "read" "cache"]}
                       "network" {"allowed_domains" ["approved.example"] "inbound_ports" [5273]}}})

            vis-home-root
            ;; Engine-level implicit grant: Vis's own session folder.
            (.getCanonicalPath (java.io.File. (System/getProperty "user.home") ".vis"))

            snapshot
            ;; The synthetic snapshot must not become the fallback for later
            ;; environments when their ambient configuration is unavailable.
            (with-redefs [lp/last-good-security-snapshot
                          (atom nil)

                          config/load-config-raw
                          #(deref cfg)

                          com.blockether.vis.internal.sandbox.policy/java-read-roots
                          (constantly [])]

              ((ns-resolve 'com.blockether.vis.internal.loop 'security-config-snapshot)))]

        ;; This models a tool editing writable vis.yml after environment creation.
        (reset! cfg {"workspace" {"filesystem"
                                  [{"id" "full" "path" "/escaped/full"}
                                   {"id" "read" "path" "/escaped/read" "access" "read-only"}
                                   {"id" "cache" "path" "/escaped/cache" "search" false}]}
                     "jail" {"enabled" false
                             "filesystem" {"allow" ["full" "read" "cache"]}
                             "network" {"allowed_domains" ["escaped.example"]
                                        "inbound_ports" [9999]}}})
        (expect (= true (:jail-enabled snapshot)))
        ;; Vis's own session folder is granted implicitly by the engine.
        (expect (= ["/approved/full" "/approved/cache" vis-home-root]
                   (get-in snapshot [:process-jail :allow-read-write])))
        (expect (= ["/approved/read"] (get-in snapshot [:process-jail :allow-read])))
        (expect (= ["/approved/cache" vis-home-root] (get-in snapshot [:process-jail :no-search])))
        (expect (= [5273] (get-in snapshot [:process-jail :inbound-ports])))
        (expect (= ["approved.example"] (get-in snapshot [:network :allowed-domains])))
        (expect (not= @cfg snapshot))))
  (it "fails closed when no router is supplied"
      (expect (throws? clojure.lang.ExceptionInfo #(lp/create-environment nil {})))))

(defdescribe
  prose-beyond-code-test
  ;; The model often restates its python_execution code in its message prose; that
  ;; prose must be SUPPRESSED so it doesn't render as a dim duplicate of the
  ;; real code block. Only genuine commentary survives.
  ;; The door (`normalize-tool-calls`) already stringified every argument key,
  ;; so this reads `"code"` — there is no keyword variant to fall back to.
  (let [tc [{:input {"code" "await patch(x)"}}]]
    (it "suppresses prose that is only the code in a fence"
        (expect (nil? (prose-beyond-code "```python\nawait patch(x)\n```" tc))))
    (it "suppresses prose that is the code verbatim (no fence)"
        (expect (nil? (prose-beyond-code "await patch(x)" tc))))
    (it "keeps prose that adds commentary beyond the code"
        (expect (= "I'll bump the **timeout**.\n```python\nawait patch(x)\n```"
                   (prose-beyond-code "I'll bump the **timeout**.\n```python\nawait patch(x)\n```"
                                      tc))))
    (it "keeps pure commentary with no code at all"
        (expect (= "Done — re-running tests." (prose-beyond-code "Done — re-running tests." tc))))
    (it "is nil for blank / nil prose"
        (expect (nil? (prose-beyond-code nil tc)))
        (expect (nil? (prose-beyond-code "   " tc))))))

(def ^:private eval-timeout-ms-for-code (deref #'rt/eval-timeout-ms-for-code))

(def ^:private preserved-thinking-replay-messages (deref #'lp/preserved-thinking-replay-messages))

(def ^:private compatible-preserved-thinking-trailer-iters
  (deref #'lp/compatible-preserved-thinking-trailer-iters))

(def ^:private conversation-suffix (deref #'lp/conversation-suffix))

(def ^:private target-supports-vision? (deref #'lp/target-supports-vision?))

(def ^:private max-tokens-exceeded-error? (deref #'lp/max-tokens-exceeded-error?))

(def ^:private next-retry-counters (deref #'lp/next-retry-counters))

(def ^:private emergency-fold-projection (deref #'lp/emergency-fold-projection))

(def ^:private context-overflow-recovery! (deref #'lp/context-overflow-recovery!))

(def ^:private estimator-undercount (deref #'lp/estimator-undercount))

(def ^:private overflow-fold-budget (deref #'lp/overflow-fold-budget))

(def ^:private provider-output-chunk? (deref #'lp/provider-output-chunk?))

(def ^:private bumped-max-tokens-extra-body (deref #'lp/bumped-max-tokens-extra-body))

(def ^:private llm-provider-error-context (deref #'lp/llm-provider-error-context))

(def ^:private iteration-error-feedback (deref #'lp/iteration-error-feedback))

(def ^:private previous-turn-context (deref #'lp/previous-turn-context))

(def ^:private previous-request-usage (deref #'lp/previous-request-usage))

(def ^:private run-normal-turn! (deref #'lp/run-normal-turn!))

(def ^:private maybe-auto-title! (deref #'titling/maybe-auto-title!))

;; Regression, issue #105: Vis retried a provider stream failure after Svar had already
;; finished its own policy, issuing the same user request more than once.
(defdescribe provider-stream-failure-is-terminal-test
             (it "does not retry a provider failure that escapes Svar"
                 (let [env
                       (lp/create-environment ::router {:db :memory})

                       calls
                       (atom 0)]

                   (try (with-redefs [svar/ask-code! (fn [_router _opts]
                                                       (swap! calls inc)
                                                       (throw (ex-info
                                                                "Stream connection error: closed"
                                                                {:type :svar.core/http-error
                                                                 :stream? true})))]
                          (expect (throws? clojure.lang.ExceptionInfo
                                           #(lp/run-iteration env
                                                              []
                                                              {:iteration 0
                                                               :resolved-model {:provider :openai
                                                                                :name "gpt-x"}
                                                               :on-chunk (fn [_])})))
                          (expect (= 1 @calls)))
                        (finally (lp/dispose-environment! env))))))

;; Regression, issue #116: svar's empty-reply resend ladder was only collected in
;; an atom and prepended to the routing trace AFTER `ask-code!` returned, so a turn
;; healing empty replies painted NOTHING for minutes and a user Esc threw the whole
;; recap away with the cancelled turn.
(defdescribe
  empty-reply-resend-live-chunk-test
  (it
    "streams every empty-reply re-send to the channel WHILE the call is in flight"
    (let [env
          (lp/create-environment ::router {:db :memory})

          chunks
          (atom [])

          during
          (atom nil)]

      (try (with-redefs [svar/ask-code!
                         (fn [_router opts]
                           (when-let [on-resend (:on-empty-reply-resend opts)]
                             (on-resend {:model "claude-x"
                                         :provider-id :anthropic
                                         :attempt 1
                                         :max-resends 3
                                         :delay-ms 2000}))
                           ;; Everything the UI had been told while the ladder was still running.
                           (reset! during (vec @chunks))
                           {:stop-reason :end :tool-calls [] :content "ok" :tokens {}})]
             (lp/run-iteration env
                               []
                               {:iteration 0
                                :resolved-model {:provider :anthropic :name "claude-x"}
                                :on-chunk #(swap! chunks conj %)})
             (let [live (filterv #(= :provider-retry-reset (:phase %)) @during)
                   ev (:event (first live))]

               (expect (= 1 (count live)))
               (expect (= {:iteration 1 :attempt 1 :max-retries 3 :delay-ms 2000}
                          (select-keys (first live) [:iteration :attempt :max-retries :delay-ms])))
               (expect (= :svar.llm/empty-content (get-in (first live) [:error :type])))
               (expect (= :empty-content (:reason ev)))
               (expect (= "anthropic" (:from-provider ev)))
               (expect (= "claude-x" (:from-model ev)))))
           (finally (lp/dispose-environment! env))))))

;; Regression: svar speaks its own session notices on the SAME streaming callback as
;; routing events, and every chunk carrying `:event/type` was labelled
;; `:provider-fallback`. One session's journal recorded 54 provider swaps that never
;; happened - all of them Codex rate-limit snapshots - and a stream restart, which
;; replays text already painted, arrived as a fallback instead of a rewind.
(defdescribe
  session-notice-phase-test
  (let [chunks-of
        (fn [emit!]
          (let [env (lp/create-environment ::router {:db :memory})
                chunks (atom [])]

            (try (with-redefs [svar/ask-code!
                               (fn [_router opts]
                                 (emit! (:on-chunk opts))
                                 {:stop-reason :end :tool-calls [] :content "ok" :tokens {}})]
                   (lp/run-iteration env
                                     []
                                     {:iteration 0
                                      :resolved-model {:provider :anthropic :name "claude-x"}
                                      :on-chunk #(swap! chunks conj %)})
                   @chunks)
                 (finally (lp/dispose-environment! env)))))]
    (it "never reports a session telemetry snapshot as a provider fallback"
        (let [chunks (chunks-of (fn [on-chunk]
                                  (on-chunk {:event/type :llm.session/rate-limits
                                             :rate-limits {:limit-id "codex" :plan-type "pro"}
                                             :content ""
                                             :done? false})))]
          (expect (empty? (filterv #(= :provider-fallback (:phase %)) chunks)))
          (expect (nil? (some #(get-in % [:event :rate-limits]) chunks)))))
    (it "rewinds the live attempt when the session stream restarts"
        (let [chunks (chunks-of (fn [on-chunk]
                                  (on-chunk {:reasoning "thinking hard" :done? false})
                                  (on-chunk {:event/type :llm.session/stream-restarted
                                             :restarted? true
                                             :reason :reconnect
                                             :attempt 2
                                             :max-retries 5
                                             :content ""
                                             :done? false})
                                  (on-chunk {:reasoning "thinking hard" :done? false})))
              restarts (filterv #(= :provider-retry-reset (:phase %)) chunks)
              deltas (filterv seq (mapv :delta (filterv #(= :reasoning (:phase %)) chunks)))]

          (expect (= 1 (count restarts)))
          (expect (= {:attempt 2 :max-retries 5}
                     (select-keys (first restarts) [:attempt :max-retries])))
          (expect (= :llm.session/stream-restarted (get-in (first restarts) [:error :type])))
          ;; The replayed attempt streams its text from zero, so an append-only
          ;; consumer must be told to drop what it drew - and see the whole tail again.
          (expect (= ["thinking hard" "thinking hard"] deltas))))
    (it "rewinds reasoning when the router retries a semantic stall"
        (let [chunks (chunks-of (fn [on-chunk]
                                  (on-chunk {:reasoning "partial thought" :done? false})
                                  (on-chunk {:event/type :llm.routing/provider-retry
                                             :reason :stream-timeout
                                             :attempt 1
                                             :content ""
                                             :done? false})
                                  (on-chunk {:reasoning "replacement thought" :done? false})))
              resets (filterv #(= :provider-retry-reset (:phase %)) chunks)
              deltas (filterv seq (mapv :delta (filterv #(= :reasoning (:phase %)) chunks)))]

          (expect (= 1 (count resets)))
          (expect (= :llm.routing/provider-retry (get-in (first resets) [:event :event/type])))
          (expect (= ["partial thought" "replacement thought"] deltas))))
    (it "reports no increment when the provider rewrites the reasoning cumulative"
        ;; Regression: the increment was sliced at the previous LENGTH, so when
        ;; Codex re-joined its reasoning summary parts with a blank line the
        ;; cumulative grew WITHOUT extending - and every append-only consumer was
        ;; handed a mid-word tail (`on**`) instead of the text the model wrote.
        (let [first-part "**Designing the role**"
              glued (str first-part "**Implementing the log**")
              rewritten (str first-part "\n\n**Implementing the log**")
              chunks (chunks-of (fn [on-chunk]
                                  (on-chunk {:reasoning first-part :done? false})
                                  (on-chunk {:reasoning glued :done? false})
                                  (on-chunk {:reasoning rewritten :done? false})))
              reasoning-chunks (filterv #(= :reasoning (:phase %)) chunks)
              deltas (mapv :delta reasoning-chunks)]

          (expect (= [first-part "**Implementing the log**"] (filterv seq deltas)))
          (expect (some #{rewritten} (mapv :thinking reasoning-chunks)))))
    (it "still reports a routing event as a provider fallback"
        (let [chunks (chunks-of (fn [on-chunk]
                                  (on-chunk {:event/type :llm.routing/provider-fallback
                                             :reason :provider-error
                                             :content ""
                                             :done? false})))
              fallbacks (filterv #(= :provider-fallback (:phase %)) chunks)]

          (expect (= 1 (count fallbacks)))
          (expect (= :llm.routing/provider-fallback
                     (get-in (first fallbacks) [:event :event/type])))))))

;; Regression, issue #120: every provider request looked identical in the TUI, so a
;; long tool-result loop was indistinguishable from the client re-sending on its
;; own — the spinner said "Vis is calling the provider (iter 12)" and never why.
(defdescribe
  provider-call-continuation-reason-test
  (let [reason-of
        (fn [iteration]
          (let [env (lp/create-environment ::router {:db :memory})
                chunks (atom [])]

            (try (with-redefs [svar/ask-code!
                               (fn [_router _opts]
                                 {:stop-reason :end :tool-calls [] :content "ok" :tokens {}})]
                   (lp/run-iteration env
                                     []
                                     {:iteration iteration
                                      :resolved-model {:provider :anthropic :name "claude-x"}
                                      :on-chunk #(swap! chunks conj %)}))
                 (finally (lp/dispose-environment! env)))
            (:reason (first (filter #(= :provider-call (:phase %)) @chunks)))))]
    (it "names the FIRST provider call of a turn as the human's own submit"
        (expect (= :user-submit (reason-of 0))))
    (it "names every later call a tool-result continuation"
        (expect (= :tool-result (reason-of 1)))
        (expect (= :tool-result (reason-of 7))))))

(defdescribe persisted-form-keeps-canonical-facts-test
             (it "stores printed output once as stdout"
                 (let [[envelope] (eng/blocks->forms
                                    [{:id 0 :code "print('hello')" :stdout "hello\n"}]
                                    {:turn 1 :iter 1})]
                   (expect (= "hello\n" (:stdout envelope)))
                   (expect (str/includes? (get-in (form/result-card envelope) [:body]) "hello"))))
             (it "carries the timeout flag and derives no card from it"
                 (let [[envelope] (eng/blocks->forms [{:id 0
                                                       :code "time.sleep(99)"
                                                       :timeout? true
                                                       :error {:message "Timeout (30s)"
                                                               :data {:timeout-ms 30000}}}]
                                                     {:turn 1 :iter 1})]
                   (expect (true? (:timeout? envelope)))
                   (expect (nil? (form/stdout-display envelope))))))

(defdescribe
  guest-interrupt-on-eval-timeout-test
  ;; REGRESSION: an eval timeout (and Esc cancel) only did `Future.cancel(true)`.
  ;; The interpreter does NOT observe `Thread.interrupt` inside guest code, so a
  ;; model block that spins (`while True: ...`) kept burning a whole core FOREVER
  ;; — measured at 1.01 busy cores with BOTH worker futures already cancelled —
  ;; and pinned its virtual thread's carrier. Only the interpreter's own
  ;; interrupt unwinds the guest frame, and it must leave the session REUSABLE.
  (it
    "unwinds a runaway guest loop and keeps the context usable"
    (tpc/with-own
      [pc {}]
      (let [cpu-ns
            (fn []
              (.getProcessCpuTime
                ^com.sun.management.OperatingSystemMXBean
                (java.lang.management.ManagementFactory/getOperatingSystemMXBean)))

            ;; `getProcessCpuTime` is JVM-WIDE, so it also counts the rest of the
            ;; suite running in parallel — on a loaded CI runner the absolute
            ;; number reached 2.14 cores with no guest alive at all (CI run
            ;; 30586924255). Only the DELTA against a baseline sampled under the
            ;; same load says anything about the guest, and a live spinning guest
            ;; is worth a whole extra core.
            busy-cores
            (fn [ms]
              (let [before (cpu-ns)]
                (Thread/sleep (long ms))
                (/ (double (- (cpu-ns) before)) (* 1.0e6 (double ms)))))

            ;; 250ms is plenty to see a whole core: a live spinner adds
            ;; ~1.0 to the delta, and the threshold below is 0.75.
            baseline
            (busy-cores 250)

            retired
            (atom false)

            environment
            {:python-context-retired-atom retired}]

        (try (let [result
                   (binding [rt/*eval-timeout-ms* 400]
                     ((deref #'lp/run-python-code) pc "while True:\n    pass" :env environment))]
               (expect (true? (:timeout? result)))
               ;; The guest is GONE: no EXTRA core is spinning after the timeout.
               ;; Take the quieter of two samples so one unlucky GC/JIT burst
               ;; cannot decide the verdict.
               (expect (< (- (min (busy-cores 250) (busy-cores 250)) baseline) 0.75))
               ;; ...and the interrupt did not retire or poison the context.
               (expect (false? @retired))
               (expect (= "42"
                          (clojure.string/trim (str (:stdout ((deref #'lp/run-python-code)
                                                               pc
                                                               "print(40 + 2)"
                                                               :env
                                                               environment)))))))
             (finally (try (env/dispose-python-context! pc) (catch Throwable _ nil))))))))

(defdescribe
  retired-python-follow-up-test
  ;; Issue #180: the first post-timeout block contained council.publish, but the
  ;; retired-environment guard failed before it could execute any Python.
  (it
    "records rejected print and Council blocks and treats retirement as terminal"
    (let [environment
          (lp/create-environment (helper-router :lmstudio nil) {:db :memory})

          pc
          (env/python-context environment)

          retired
          (:python-context-retired-atom environment)

          entered
          (atom [])

          run-block
          env/run-python-block]

      (try
        (#'lp/run-python-code pc "print('ready')" :env environment)
        (let [timeout (binding [rt/*eval-timeout-ms* 3000]
                        (#'lp/run-python-code pc "import time\ntime.sleep(10)" :env environment))]
          (expect (true? (:timeout? timeout))))
        (expect (loop [remaining 100]
                  (cond @retired true
                        (zero? remaining) false
                        :else (do (Thread/sleep 50) (recur (dec remaining))))))
        (expect (false? (env/context-enterable? environment)))
        (with-redefs [env/run-python-block (fn [context code & args]
                                             (swap! entered conj code)
                                             (apply run-block context code args))]
          (doseq
            [code
             ["print(await council.publish('after timeout', kind='coordination', title='Timeout regression'))"
              "print(42)"]]
            (with-redefs [svar/ask-code! (fn [_ _]
                                           {:stop-reason :tool-calls
                                            :tool-calls [{:id "follow_up"
                                                          :name "python_execution"
                                                          :input {:code code}}]})]
              (let [result (try (lp/run-iteration environment
                                                  []
                                                  {:iteration 1
                                                   :resolved-model {:provider :lmstudio
                                                                    :name "model"}})
                                (catch Exception e {:thrown (ex-message e)}))
                    block (first (:blocks result))]

                (expect (nil? (:thrown result)))
                (expect (= code (:code block)))
                (expect (= ::env/context-retired (get-in block [:error :type])))))))
        (expect (empty? @entered))
        (let [error (try (env/python-context environment) nil (catch Exception e e))]
          (expect (= ::env/context-retired (:type (ex-data error))))
          (doseq [cause [error (ex-info "iteration wrapper" {} error)]]
            (let [result (lp/handle-iteration-exception! cause {:iteration 2 :messages []})
                  card (first (#'lp/python-error-content (::lp/iteration-error result)))]

              (expect (true? (::lp/fatal-iteration-error result)))
              (expect (= "python_environment_retired" (get card "code")))
              (expect (false? (get card "retryable"))))))
        (finally (lp/dispose-environment! environment))))))

(defdescribe
  retired-python-stops-turn-test
  ;; Issue #180: no automatic model retry or side-effect replay on a dead worker.
  (doseq [same-response? [false true]]
    (it
      (str "stops after the native timeout; Council shares response=" same-response?)
      (let
        [environment (lp/create-environment (helper-router :lmstudio nil) {:db :memory})
         db (:db-info environment)
         tid (persistance/db-store-session-turn! db
                                                 {:parent-session-id (:session-id environment)
                                                  :user-request "timeout regression"})
         timeout-code "import time\nprint('before native wait')\ntime.sleep(10)"
         publish-code
         "print(await council.publish('after timeout', kind='coordination', title='Timeout regression'))"
         calls (atom 0)
         chunks (atom [])
         response (fn [codes]
                    {:stop-reason :tool-calls
                     :tool-calls
                     (mapv (fn [idx code]
                             {:id (str "call_" idx) :name "python_execution" :input {:code code}})
                           (range)
                           codes)})]

        (try (expect (nil? (:error (#'lp/execute-code environment "print('ready')"))))
             (let [result (binding [rt/*eval-timeout-ms* 3000]
                            (with-redefs [svar/ask-code! (fn [_ _]
                                                           (case (swap! calls inc)
                                                             1
                                                             (response (cond-> [timeout-code]
                                                                         same-response?
                                                                         (conj publish-code)))

                                                             2
                                                             (response [publish-code])

                                                             {:stop-reason :end
                                                              :content "unexpected retry"}))]
                              (lp/iteration-loop environment
                                                 "timeout regression"
                                                 {:session-turn-id tid
                                                  :hooks {:on-chunk #(swap! chunks conj %)}})))
                   iterations (persistance/db-list-session-turn-iterations db tid)
                   forms (:forms (first iterations))
                   terminal (first (:answer result))]

               (expect (= 1 @calls))
               (expect (= :error (:status result)))
               (expect (= "python_environment_retired" (get terminal "code")))
               (expect (false? (get terminal "retryable")))
               (expect (= 1 (count iterations)))
               (expect (= (if same-response? [timeout-code publish-code] [timeout-code])
                          (mapv :src forms)))
               (expect (str/includes? (str (:stdout (first forms))) "before native wait"))
               (expect (every? :error forms))
               (let [ids (mapv #(get-in % [:error :complain_entry_id]) forms)
                     complaints (mapv #(persistance/db-council-get db %) ids)]

                 (expect (every? pos-int? ids))
                 (expect (every? #(= "autocomplain" (:source %)) complaints))
                 (expect (every? #(= [1 1] ((juxt :turn :iter) (get-in % [:source_ref :scope])))
                                 complaints))
                 (expect (every? #(get-in % [:source_ref :session_turn_iteration_id]) complaints)))
               (expect (= (if same-response? 2 1)
                          (count (filter #(= :form-result (:phase %)) @chunks)))))
             (finally (lp/dispose-environment! environment)))))))

(defdescribe
  retired-python-cancelled-turn-test
  (it "keeps an explicit user cancellation distinct from runtime failure"
      (let [environment
            (lp/create-environment (helper-router :lmstudio nil) {:db :memory})

            tid
            (persistance/db-store-session-turn! (:db-info environment)
                                                {:parent-session-id (:session-id environment)
                                                 :user-request "cancel regression"})

            cancelled
            (atom false)

            calls
            (atom 0)]

        (try (let [result (with-redefs [svar/ask-code!
                                        (fn [_ _]
                                          (swap! calls inc)
                                          (reset! (:python-context-retired-atom environment) true)
                                          (reset! cancelled true)
                                          {:stop-reason :tool-calls
                                           :tool-calls [{:id "cancelled_call"
                                                         :name "python_execution"
                                                         :input {:code "print(42)"}}]})]
                            (lp/iteration-loop environment
                                               "cancel regression"
                                               {:session-turn-id tid :cancel-atom cancelled}))]
               (expect (= :cancelled (:status result)))
               (expect (= 1 @calls)))
             (finally (lp/dispose-environment! environment))))))

(defdescribe eval-timeout-keeps-partial-stdout-test
             ;; The wall-clock backstop used to return only a timeout error. The
             ;; guest never reaches its final stdout outcome, so every line printed
             ;; before the timeout disappeared and the model reran the block blind.
             (it "surfaces what the block printed before the wall fired"
                 (tpc/with-own
                   [pc {}]
                   (try (let [result (binding [rt/*eval-timeout-ms* 500]
                                       ((deref #'lp/run-python-code)
                                         pc
                                         "print('fetched 1')\nwhile True:\n    pass"))]
                          (expect (true? (:timeout? result)))
                          (expect (some? (re-find #"fetched 1" (str (:stdout result))))))
                        (finally (try (env/dispose-python-context! pc) (catch Throwable _ nil)))))))

(defdescribe python-block-runs-in-the-session-context-test
             ;; REGRESSION: the eval worker thread bound only the per-block sinks, so the
             ;; block itself ran with NO session context. A sandbox SHIM bridge reads the
             ;; AMBIENT context (an extension SYMBOL installs its own around every call),
             ;; so `ls` saw an EMPTY `workspace/*filesystem-roots*` and refused every bound
             ;; extra filesystem root — "escapes the allowed workspace roots" — while
             ;; `cat`/`grep` on the very same path answered normally.
             (it
               "gives a shim bridge the filesystem roots the session actually bound"
               (tpc/with-own
                 [pc {}]
                 (let [;; Outside the primary cwd and outside every always-on root
                       ;; (temp dirs, `~/.vis`) - reachable ONLY as a bound root. An empty
                       ;; directory of its own, never the whole home: the guest `ls` COUNTS
                       ;; what it lists, so a home-sized walk outlives the block's eval wall,
                       ;; and the abandoned guest thread is still inside that host call when
                       ;; the teardown below closes the context - a close that then waits for
                       ;; it forever.
                       outside
                       (let [dir (java.io.File. (System/getProperty "user.home")
                                                (str "vis-loop-outside-" (System/nanoTime)))]
                         (.mkdirs dir)
                         (spit (java.io.File. dir "marker.txt") "marker\n")
                         (.getAbsolutePath dir))

                       code
                       (str "try:\n"
                            "    rows = ls(" (pr-str outside)
                            ")\n" "    print('listed', isinstance(rows, str) and len(rows) > 0)\n"
                            "except Exception as e:\n" "    print('refused', e)\n")

                       env-with
                       (fn [roots]
                         {:workspace/root (System/getProperty "user.dir")
                          :workspace {:repo-root (System/getProperty "user.dir")
                                      :root (System/getProperty "user.dir")}
                          :security-policy {:jail-enabled true}
                          :security/filesystem-roots roots
                          :security/no-search-roots []})

                       listing
                       (fn [roots]
                         (str (:stdout
                                ((deref #'lp/run-python-code) pc code :env (env-with roots)))))]

                   (try
                     ;; The path is genuinely outside what confinement grants by itself...
                     (expect (str/starts-with? (listing []) "refused"))
                     ;; ...and the moment the session binds it, the block's own `ls` reaches it.
                     (expect (str/starts-with? (listing [outside]) "listed True"))
                     (finally (try (doseq [^java.io.File f (reverse (file-seq (java.io.File.
                                                                                ^String outside)))]
                                     (.delete f))
                                   (catch Throwable _ nil))
                              (try (env/dispose-python-context! pc) (catch Throwable _ nil))))))))

(defdescribe
  python-tool-activity-lifecycle-test
  ;; Regression, issue td-3614f0: opening Activity lifted the evaluation wall,
  ;; so a tool that hung after its start event never timed out or settled.
  ;; Regression, issue 81dbadcc-620e-43b8-a3e7-661804a2718f: the presentation-only
  ;; Activity receipt was appended to Python stdout and therefore shown twice and
  ;; sent back to the model as though the block had printed it.
  ;; Protocol 7 retired td-1ec00e with the shape that caused it: a settled receipt
  ;; is a value ON the form, so there is no live view to close and no attachment to
  ;; strand when a close fails. The redef below stays only to prove the retry path
  ;; is never entered.
  (it
    "pairs a real guest tool call and settles its Activity receipt on timeout"
    (let [env {:session-id "session-activity"
               :iteration-id "iteration-activity"
               :workspace/root (System/getProperty "user.dir")
               :workspace {:root (System/getProperty "user.dir")
                           :repo-root (System/getProperty "user.dir")}}]
      (tpc/with-own
        [pc (extension/builtin-sandbox-bindings (constantly env))]
        (let [events (atom [])
              close-attempts (atom 0)
              close-live! hi/close-live!]

          (with-redefs-fn {(requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir)
                           (constantly (java.io.File. (System/getProperty "java.io.tmpdir")
                                                      (str "vis-activity-" (random-uuid))))
                           #'hi/close-live! (fn [& args]
                                              (if (= 1 (swap! close-attempts inc))
                                                (throw (ex-info "simulated first close failure" {}))
                                                (apply close-live! args)))}
            #(let
               [result
                (binding [rt/*eval-timeout-ms* 400]
                  ((deref #'lp/run-python-code)
                    pc
                    "print('python-only')\ngrep({'query': 'org.clojure', 'paths': ['deps.edn']})\nwhile True:\n    pass"
                    :tool-event-fn
                    (fn [event]
                      (swap! events conj event))
                    :env
                    env))] (expect (true? (:timeout? result))) (expect (= [:start :terminal]
                                                                          (mapv :phase @events)))
               (expect (= 1 (count (set (map :invocation-id @events)))))
               ;; Ownerless: a lifecycle event names its own invocation and carries
               ;; nothing about the evaluation, iteration or form that ran it — the
               ;; form the block becomes is the snapshot's only identity.
               (expect (not-any? (fn [event]
                                   (some (partial contains? event)
                                         [:schema-version :evaluation-id :form-index]))
                                 @events))
               ;; No close, and no receipt filed: Activity rides the form now.
               (expect (= [0 []] [@close-attempts (vec (:attachments result))])) (expect
                                                                                   (=
                                                                                     "python-only\n"
                                                                                     (:stdout
                                                                                       result)))
               ;; The host derives presentation from the printed fact after execution.
               (expect (str/includes? (:body (form/stdout-display result)) "python-only"))
               (expect (not (str/includes? (:stdout result) "# Activity"))) (expect (not
                                                                                      (str/includes?
                                                                                        (:stdout
                                                                                          result)
                                                                                        "grep")))
               (expect (not (str/includes? (:stdout result) "[running]")))
               (expect (empty? (hi/open-live-ids))))))))))

(defdescribe
  end-only-activity-publication-test
  (it
    "keeps starts and intermediate content internal until the operation settles"
    (doseq [show-start [false true]]
      (let [snapshots (atom [])
            events (atom [])
            processed (promise)
            details {:operation :quick_read
                     :presenter :generic
                     :activity {:headline "Read record" :show-start show-start}}]

        (tpc/with-own
          [pc {}]
          (with-redefs [env/run-python-block
                        (fn [_ _ _]
                          (let [ctx (activity-event/context)
                                invocation (activity-event/invocation ctx nil)
                                started (util/now-ms)]

                            (extension/*tool-event-sink*
                              (activity-event/start-event ctx invocation details))
                            (extension/*tool-event-sink*
                              (activity-event/content-event
                                ctx
                                invocation
                                details
                                {"headline" "Read record" "summary" "One record" "content" []}))
                            (expect (deref processed 2000 false))
                            (expect (= show-start (boolean (seq @snapshots))))
                            (extension/*tool-event-sink* (activity-event/terminal-event
                                                           ctx
                                                           invocation
                                                           (assoc details
                                                             :started-at-ms started
                                                             :outcome :succeeded
                                                             :result 1)))
                            {:stdout "done"}))]
            (let [result (#'lp/run-python-code
                          pc
                          "pass"
                          :env
                          {:activity/on-snapshot #(swap! snapshots conj %)}
                          :tool-event-fn
                          (fn [event]
                            (swap! events conj event)
                            (when (= :content (:phase event)) (deliver processed true))))]
              (expect (nil? (:error result)))
              (expect (= [:start :content :terminal] (mapv :phase @events)))
              (expect (= "succeeded" (get-in result [:activity :rows 0 :state])))
              (expect (= "Read record"
                         (get-in result [:activity :rows 0 :presentation "headline"]))))))))))

(defdescribe
  activity-coalesced-content-test
  (it "flushes the final throttled presentation while the tool is still waiting"
      ;; The installed SDK's HTTP/stdio flow exposed a lost trailing update: a
      ;; start frame consumed the window and no timer published the pending content.
      (let [visible
            (promise)

            details
            {:operation "sdk_wait" :presenter "generic" :classification :observation}

            snapshot!
            (fn [snapshot]
              (when (= "Waiting" (get-in snapshot [:rows 0 :presentation "summary"]))
                (deliver visible true)))]

        (tpc/with-own
          [pc {}]
          (with-redefs [env/run-python-block
                        (fn [_ _ _]
                          (let [ctx (activity-event/context)
                                invocation (activity-event/invocation ctx nil)
                                started (util/now-ms)]

                            (extension/*tool-event-sink*
                              (activity-event/start-event ctx invocation details))
                            (extension/*tool-event-sink*
                              (activity-event/content-event
                                ctx
                                invocation
                                details
                                {"headline" "SDK" "summary" "Waiting" "content" []}))
                            (let [observed (deref visible 2000 false)]
                              (extension/*tool-event-sink* (activity-event/terminal-event
                                                             ctx
                                                             invocation
                                                             (assoc details
                                                               :started-at-ms started
                                                               :outcome :succeeded
                                                               :result {})))
                              {:stdout (str observed)})))]
            (let [result (#'lp/run-python-code pc "pass" :env {:activity/on-snapshot snapshot!})]
              (expect (= "true" (:stdout result)))
              (expect (nil? (:error result)))))))))

(defdescribe activity-dispatch-order-test
             ;; Regression, issue td-74427c: concurrent callbacks could reduce S2 before
             ;; publishing stale S1, while a slow listener blocked the tool callback itself.
             (it
               "publishes snapshots FIFO without blocking their callback threads"
               (let [[dispatch! shutdown!]
                     (#'lp/serial-activity-dispatcher)

                     release-first
                     (promise)

                     entered-first
                     (promise)

                     snapshots
                     (atom [])

                     ;; `dispatch!` answers the Future the settler waits on, so the
                     ;; drain is deref-ing what was submitted rather than a third
                     ;; function the dispatcher no longer hands out.
                     submitted
                     (atom [])

                     submit
                     (fn [n]
                       (swap! submitted conj
                         (dispatch! (fn []
                                      (when (= 1 n) (deliver entered-first true) @release-first)
                                      (swap! snapshots conj n)))))]

                 (try (submit 1)
                      @entered-first
                      (let [started (System/nanoTime)]
                        (submit 2)
                        (submit 3)
                        (expect (< (/ (- (System/nanoTime) started) 1e6) 100.0)))
                      (deliver release-first true)
                      (run! deref @submitted)
                      (expect (= [1 2 3] @snapshots))
                      (finally (shutdown!))))))

(defdescribe tool-call-execution-test
             ;; REGRESSION: tool calling once shipped 100% broken — `run-iteration`
             ;; synthesized `env* (assoc environment)` (a 1-arg assoc) before execute-code, so
             ;; EVERY tool-call iteration threw ArityException ("Provider unavailable / Wrong
             ;; number of args (1) passed to clojure.core/assoc"). 120+ loop tests stayed green
             ;; because none drove a real tool-call response through `run-iteration`. This does.
             (it
               "executes a python_execution tool call through run-iteration without throwing"
               (let [env
                     (lp/create-environment ::router {:db :memory})

                     chunks
                     (atom [])]

                 (try (with-redefs [svar/ask-code! (fn [_router _opts]
                                                     {:stop-reason :tool-calls
                                                      :tool-calls [{:id "call_1"
                                                                    :name "python_execution"
                                                                    :input {:code "print(6*7)"}}]
                                                      :content nil
                                                      :reasoning "computing"
                                                      :tokens {}})]
                        ;; The bug threw HERE — a tool-call iteration reaching the execute path.
                        (let [result (lp/run-iteration env
                                                       []
                                                       {:iteration 0
                                                        :resolved-model {:provider :zai-coding-plan
                                                                         :name "glm-5.1"}
                                                        :on-chunk #(swap! chunks conj %)})
                              tool-calls (:tool-calls result)
                              form-res (first (filter #(= :form-result (:phase %)) @chunks))]

                          ;; tool-call iteration (not a final answer)
                          (expect (nil? (:final-result result)))
                          (expect (= 1 (count tool-calls)))
                          (expect (= "python_execution" (:name (first tool-calls))))
                          ;; the call ACTUALLY executed in the sandbox (env was passed correctly):
                          ;; python_execution returns what it print()s.
                          (expect (some? form-res))
                          (expect (nil? (:error form-res)))
                          (expect (str/includes? (str (:stdout form-res)) "42"))))
                      (finally (lp/dispose-environment! env))))))

(defdescribe
  responses-execution-boundary-test
  ;; Issue #173: cross the SSE parser, Vis normalization, actual Python execution
  ;; and Responses replay. Streamed items are the only items; a terminal snapshot
  ;; that repeats them under new ids adds no calls, results or replay entries.
  (doseq [echo? [true false]]
    (it
      (str "executes and replays each streamed call once; snapshot echo=" echo?)
      (let [environment (lp/create-environment ::router {:db :memory})
            code "execution_count = globals().get('execution_count', 0) + 1\nprint(execution_count)"
            call (fn [n]
                   {"type" "function_call"
                    "id" (str "fc_" n)
                    "call_id" (str "call_" n)
                    "name" "python_execution"
                    "arguments" (json/write-json-str {"code" code})})
            calls [(call "one") (call "two")]
            events (conj (vec (map-indexed (fn [idx item]
                                             {"type" "response.output_item.done"
                                              "output_index" idx
                                              "item" item})
                                           calls))
                         {"type" "response.completed"
                          "response"
                          {"output"
                           (if echo? (mapv #(assoc % "id" (str (get % "id") "_final")) calls) [])}})
            stream (apply str (map #(str "data: " (json/write-json-str %) "\n\n") events))]

        (try (with-redefs [http/post (fn [_ _]
                                       {:status 200
                                        :body (java.io.ByteArrayInputStream. (.getBytes stream
                                                                                        "UTF-8"))})
                           svar/ask-code! (fn [_ _]
                                            (assoc (svar-llm/openai-responses-completion
                                                     {:model "test-model" :input []}
                                                     {:api-key "test"
                                                      :base-url "https://gateway.example.com/v1"
                                                      :on-chunk (constantly nil)})
                                              :stop-reason :tool-calls
                                              :tokens {}))]

               (let [result (lp/run-iteration environment
                                              []
                                              {:iteration 0
                                               :resolved-model {:provider :openai :name "gpt-4o"}})
                     forms (eng/blocks->forms (:blocks result) {:turn 1 :iter 1})
                     replay (#'lp/conversation-suffix
                             [[1 (assoc result :forms-vec forms)]]
                             {:provider :openai :model "gpt-4o"})
                     wire (mapcat #'svar-llm/responses-message-input-entries replay)
                     wire-calls (filter #(= "function_call" (:type %)) wire)
                     results (filter #(= "function_call_output" (:type %)) wire)]

                 (expect (= 2 (count forms)))
                 (expect (not-any? :error forms))
                 (expect
                   (= ["call_one" "call_two"] (mapv :call_id wire-calls) (mapv :call_id results)))
                 (expect (= ["fc_one" "fc_two"] (mapv :id wire-calls)))
                 (expect (= ["1" "2"] (mapv #(str/trim (str (:stdout %))) forms)))
                 (expect (not-any? #(str/includes? (:output %) "printed nothing") results))))
             (finally (lp/dispose-environment! environment)))))))

(defdescribe
  activity-ownership-boundary-test
  (it
    "emits the persisted final Activity as its own settled chunk before the result"
    (let [env
          (lp/create-environment ::router {:db :memory})

          chunks
          (atom [])]

      (try
        (with-redefs [svar/ask-code!
                      (fn [_router _opts]
                        {:stop-reason :tool-calls
                         :tool-calls
                         [{:id "call_activity"
                           :name "python_execution"
                           :input {:code "grep({'query': 'defproject', 'paths': ['deps.edn']})"}}]
                         :content nil
                         :reasoning "checking"
                         :tokens {}})]
          (let [result (lp/run-iteration env
                                         []
                                         {:iteration 0
                                          :resolved-model {:provider :zai-coding-plan
                                                           :name "glm-5.1"}
                                          :on-chunk #(swap! chunks conj %)})
                block (first (:blocks result))
                terminal (first (filter #(= :form-result (:phase %)) @chunks))
                activity-chunks (filterv #(= :form-activity (:phase %)) @chunks)
                settled (last activity-chunks)
                forms (eng/blocks->forms (:blocks result) {:turn 1 :iter 1})
                activity (:activity block)]

            (expect (seq activity-chunks))
            (expect (true? (:settled? settled)))
            (expect (= activity (:activity settled) (:activity (first forms))))
            (expect (not (contains? terminal :activity)))
            (expect (< (.indexOf @chunks settled) (.indexOf @chunks terminal)))
            (expect (= "succeeded" (:state activity)))
            (expect (seq (:rows activity)))
            (expect (empty? (:attachments block)))))
        (finally (lp/dispose-environment! env))))))

(defdescribe
  previous-turn-context-test
  ;; Blockether/vis#174: an interrupted user message may be dense code, not prose.
  (it "keeps prior user input when diagnostic token counting is unavailable"
      (with-redefs [persistance/db-list-session-turns
                    (constantly
                      [{:id "t1" :position 1 :status :interrupted :user-request "keep this input"}])

                    persistance/db-list-session-turn-iterations
                    (constantly [])

                    svar-router/count-messages
                    (fn [_ _]
                      (throw (ex-info "tokenizer unavailable" {})))]

        (let [ca
              (atom {})

              prior
              (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom ca} "t2")]

          (expect (= "keep this input" (:user-request (first prior))))
          (expect (nil? (get @ca "engine_turn_weights"))))))
  (it "tokenizes the rendered recap of an interrupted turn without iterations"
      (let [payload
            (apply str (repeat 1000 "ą中42={x:17};\n"))

            ca
            (atom {})

            model
            "gpt-4"]

        (with-redefs [persistance/db-list-session-turns
                      (constantly
                        [{:id "t1" :position 1 :status :interrupted :user-request payload}])

                      persistance/db-list-session-turn-iterations
                      (constantly [])]

          (let [prior
                (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom ca} "t2" model)

                rendered
                (prompt/previous-turn-context-block prior)

                expected
                (- (svar-router/count-messages model [{:role "user" :content rendered}])
                   (svar-router/count-messages model []))]

            (expect (= expected (get-in @ca ["engine_turn_weights" 1])))
            (expect (> (get-in @ca ["engine_turn_weights" 1]) (quot (count payload) 4)))))))
  ;; Cross-process RESUME carry must be a pure function of the DB so the wire is
  ;; identical regardless of process (see DERIVED_WIRE.md). These pin: ALL prior
  ;; answered turns carried (not just the latest), each with its r[] scope index;
  ;; determinism; and summary-awareness (drop/summarize reshape uniformly).
  (it
    "carries ALL prior answered turns with their r[] scope index"
    (with-redefs [persistance/db-list-session-turns
                  (fn [_db session-id]
                    (expect (= "s1" session-id))
                    [{:id "t1"
                      :status :done
                      :position 1
                      :user-request "Read a"
                      :content [(content/prose "Read it")]}
                     {:id "t2"
                      :status :done
                      :position 2
                      :user-request "Read b"
                      :content [(content/prose "Read b too")]}
                     {:id "t3" :status :running :user-request "yes"}])

                  persistance/db-list-session-turn-iterations
                  (fn [_db id]
                    (case id
                      "t1"
                      [{:status :done
                        :position 1
                        :forms [{:scope "t1/i1/f1" :src "cat(\"a\")" :stdout "a"}
                                {:scope "t1/i1/f2" :src "set_session_title(...)" :silent? true}]}]

                      "t2"
                      [{:status :done
                        :position 1
                        :forms [{:scope "t2/i1/f1" :src "rg({...})" :stdout ""}]}]

                      []))]

      (let [out (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})} "t3")]
        (expect (= 2 (count out))) ; both answered turns, not just latest
        (expect (= "Read a" (:user-request (first out))))
        (expect (= [{:scope "t1/i1/f1" :src "cat(\"a\")"}] (:results (first out)))) ; silent f2 excluded
        (expect (= [{:scope "t2/i1/f1" :src "rg({...})"}] (:results (second out)))))))
  ;; Regression, reported from the app: interrupt filed the record after the
  ;; iteration had settled, but the next request's resumed context omitted it.
  (it
    "carries a late live-view record into the next model request"
    (with-redefs [persistance/db-list-session-turns
                  (constantly [{:id "t1"
                                :status :done
                                :position 1
                                :user-request "watch it"
                                :content [(content/prose "watching")]}])

                  persistance/db-list-session-turn-iterations
                  (constantly
                    [{:id "i1"
                      :status :done
                      :forms [{:scope "t1/i1/f1" :svar/tool-call-id "call-1" :stdout "started"}]}])

                  persistance/db-list-iterations-attachments-meta
                  (fn [_db ids]
                    (expect (= ["i1"] (mapv str ids)))
                    ;; Protocol 7 files no Activity attachment, so there is no
                    ;; classified sibling here for the replay path to exclude.
                    {"i1" [{:id "record-1"
                            :tool-call-id "call-1"
                            :filename "record.live.ndjson"
                            :media-type "application/vnd.vis.live+ndjson"}]})]

      (let [results
            (:results (first (previous-turn-context
                               {:session-id "s1" :db-info ::db :ctx-atom (atom {})}
                               "t2")))

            records
            (keep :live-record results)]

        (expect (= 1 (count records)))
        (expect (str/includes? (first records) "record.live.ndjson"))
        (expect (str/includes? (first records) "record-1"))
        (expect (str/includes? (first records) "read_attachment"))
        (expect (not (str/includes? (str/join "\n" records) "activity.live.ndjson"))))))
  (it "keeps synthetic slash commands out of later provider context"
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :done
                                  :user-request "/cd /repo"
                                  :content [(content/prose "Changed workspace")]}])

                    persistance/db-list-session-turn-iterations
                    (constantly
                      [{:status :done
                        :forms
                        [{:scope "t1/i1/f1" :tag :user-slash :src "/cd /repo" :silent? true}]}])]

        (expect (nil? (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})}
                                             "t2")))))
  (it "is deterministic — same DB ⇒ identical output (process-invariant)"
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :done
                                  :position 1
                                  :user-request "q"
                                  :content [(content/prose "a")]}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:status :done
                                  :position 1
                                  :forms [{:scope "t1/i1/f1" :src "cat(x)" :stdout "x"}]}])]

        (let [env {:session-id "s1" :db-info ::db :ctx-atom (atom {})}]
          (expect (= (previous-turn-context env "t9") (previous-turn-context env "t9"))))))
  (it "is summary-aware at ITERATION granularity: gist-less folds drop, gists summarize"
      ;; Folds are recorded at iteration scope (tN/iN) — what the prompt instructs
      ;; and what the live wire (apply-summaries) matches. Each form carries a FORM
      ;; scope (tN/iN/fN); prior-turn-scope-index normalizes form→iteration before
      ;; matching. A gist-less fold collapses to ONE `dropped` audit line; a fold
      ;; with a gist collapses multiple forms to ONE gist line.
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :done
                                  :position 1
                                  :user-request "q"
                                  :content [(content/prose "a")]}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:status :done
                                  :position 1
                                  :forms [{:scope "t1/i1/f1" :src "cat(a)" :stdout "a"}
                                          {:scope "t1/i2/f1" :src "cat(b)" :stdout "b"}
                                          {:scope "t1/i2/f2" :src "cat(c)" :stdout "c"}]}])]

        (let [env
              {:session-id "s1"
               :db-info ::db
               :ctx-atom (atom {"session_summaries" [{"scopes" #{"t1/i1"} "note" "wrong file"}
                                                     {"scopes" #{"t1/i2"} "gist" "b pinned"}]})}

              ; fold i2
              results
              (:results (first (previous-turn-context env "t9")))]

          (expect (= 2 (count results))) ; i1 dropped-line + i2 gist (each deduped)
          (let [by-scope (into {} (map (juxt :scope identity)) results)]
            (expect (= {:scope "t1/i1" :dropped? true :note "wrong file"} (get by-scope "t1/i1")))
            (expect (= {:scope "t1/i2" :gist "b pinned"} (get by-scope "t1/i2")))))))
  (it "returns nil when every prior turn is current/running/blank-answer"
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :done
                                  :position 1
                                  :user-request "old"
                                  :content [(content/prose "")]}
                                 {:id "t2"
                                  :status :running
                                  :user-request "now"
                                  :content [(content/prose "partial")]}])

                    persistance/db-list-session-turn-iterations
                    (constantly [])]

        (expect (nil? (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})}
                                             "t2")))))
  (it "carries prior provider-error turns as unfinished cross-turn context"
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :error
                                  :position 1
                                  :user-request "fix web"
                                  :content [(content/error "provider_error" "failed" true)]}
                                 {:id "t2" :status :running :user-request "continue"}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:status :done
                                  :position 1
                                  :forms [{:scope "t1/i1/f1" :src "cat(ui)" :stdout "read ui"}]}])]

        (let [out (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})} "t2")]
          (expect (= [{:turn 1
                       :user-request "fix web"
                       :answer nil
                       :interrupted? true
                       :results [{:scope "t1/i1/f1" :src "cat(ui)"}]}]
                     out)))))
  (it "fold-of-fold removes every covered turn recap; trailer owns one checkpoint"
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :done
                                  :position 1
                                  :user-request "old q1"
                                  :content [(content/prose "old a1")]}
                                 {:id "t2"
                                  :status :done
                                  :position 2
                                  :user-request "old q2"
                                  :content [(content/prose "old a2")]}
                                 {:id "t3" :status :running :position 3 :user-request "now"}])

                    persistance/db-list-session-turn-iterations
                    (fn [_ id]
                      [{:status :done
                        :position 1
                        :forms
                        [{:scope (str id "/i1/f1") :src (str "cat(" id ")") :stdout "ok"}]}])]

        (let [env {:session-id "s1"
                   :db-info ::db
                   :ctx-atom (atom {"session_summaries" [{"scopes" #{"t1/i1"} "gist" "fine detail"}
                                                         {"through" "t2/i1"
                                                          "issued_turn" 3
                                                          "gist" "one durable checkpoint"}]})}]
          (expect (nil? (previous-turn-context env "t3"))))))
  (it "a gist-less whole-turn fold of a no-iteration turn leaves a visible tombstone checkpoint"
      ;; No done iterations → no trailer anchor exists anywhere, so previous-
      ;; turn-context must materialize the checkpoint itself instead of letting
      ;; the turn vanish without a trace.
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :done
                                  :position 1
                                  :user-request "spent request"
                                  :content [(content/prose "spent answer")]}
                                 {:id "t2" :status :running :position 2}])

                    persistance/db-list-session-turn-iterations
                    (constantly [])]

        (let [out (previous-turn-context {:session-id "s1"
                                          :db-info ::db
                                          :ctx-atom (atom {"session_summaries"
                                                           [{"scopes" #{"t1"} "issued_turn" 2}]})}
                                         "t2")]
          (expect (= 1 (count out)))
          (expect (:checkpoint? (first out)))
          (expect (= [1] (:turns (first out))))
          (expect (clojure.string/includes? (str (:gist (first out))) "dropped"))
          (expect (nil? (:user-request (first out)))))))
  (it "an enumerated iteration fold covering EVERY iteration still keeps the turn's Q/A recap"
      ;; Regression: 'all iterations folded' must NOT be inferred as whole-turn
      ;; intent — only a bare tN or a spanning range selector removes Q/A.
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :done
                                  :position 1
                                  :user-request "keep my question"
                                  :content [(content/prose "keep my answer")]}
                                 {:id "t2" :status :running :position 2}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:status :done
                                  :position 1
                                  :forms [{:scope "t1/i1/f1" :src "cat(a)" :stdout "1"}]}])]

        (let [out (previous-turn-context
                    {:session-id "s1"
                     :db-info ::db
                     :ctx-atom (atom {"session_summaries" [{"scopes" #{"t1/i1"} "gist" "read a"}]})}
                    "t2")]
          (expect (= 1 (count out)))
          (expect (= "keep my question" (:user-request (first out))))
          (expect (= "keep my answer" (:answer (first out))))
          (expect (= [{:scope "t1/i1" :gist "read a"}] (:results (first out)))))))
  (it
    "a whole-turn fold ISSUED DURING that turn keeps its Q/A recap next request (answer produced after the fold)"
    ;; Loophole fix (issued_turn invariant): a bare `tN` / spanning-range fold
    ;; recorded mid-turn N stamps `issued_turn` = N. It resolves to whole-turn
    ;; coverage of N against next request's complete universe, but must NOT erase
    ;; N's own answer — which was produced AFTER the fold, so no gist summarizes
    ;; it. Degrades to the enumerated path: Q/A recap kept, result lines folded.
    (with-redefs [persistance/db-list-session-turns
                  (constantly [{:id "t1"
                                :status :done
                                :position 1
                                :user-request "keep my question"
                                :content [(content/prose "keep my answer")]}
                               {:id "t2" :status :running :position 2}])

                  persistance/db-list-session-turn-iterations
                  (constantly [{:status :done
                                :position 1
                                :forms [{:scope "t1/i1/f1" :src "cat(a)" :stdout "1"}]}])]

      (let [env-base
            {:session-id "s1" :db-info ::db}

            out
            (previous-turn-context
              (assoc env-base
                :ctx-atom (atom {"session_summaries"
                                 [{"scopes" #{"t1"} "issued_turn" 1 "gist" "folded so far"}]}))
              "t2")

            unstamped-out
            (previous-turn-context (assoc env-base
                                     :ctx-atom (atom {"session_summaries"
                                                      [{"scopes" #{"t1"}
                                                        "gist" "invalid unstamped fold"}]}))
                                   "t2")]

        (expect (= 1 (count out)))
        (expect (= "keep my question" (:user-request (first out))))
        (expect (= "keep my answer" (:answer (first out))))
        ;; Canonical fold intents always carry issued_turn. Missing ownership
        ;; cannot erase a complete prior Q/A recap.
        (expect (= "keep my question" (:user-request (first unstamped-out))))
        (expect (= "keep my answer" (:answer (first unstamped-out)))))))
  (it "a whole-turn fold ISSUED IN A LATER turn still removes the target turn's Q/A recap"
      ;; The normal prior-turn case: turn 2 folds turn 1 (issued_turn 2 > 1) — it
      ;; actually saw turn 1's answer, so removal is safe and the trailer owns the
      ;; one checkpoint. With only turn 1 present, the whole context collapses.
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :done
                                  :position 1
                                  :user-request "old q"
                                  :content [(content/prose "old a")]}
                                 {:id "t2" :status :running :position 2}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:status :done
                                  :position 1
                                  :forms [{:scope "t1/i1/f1" :src "cat(a)" :stdout "1"}]}])]

        (expect (nil? (previous-turn-context {:session-id "s1"
                                              :db-info ::db
                                              :ctx-atom (atom {"session_summaries"
                                                               [{"scopes" #{"t1"}
                                                                 "issued_turn" 2
                                                                 "gist" "folded prior turn"}]})}
                                             "t2")))))
  (it "carries cancelled turns with settled work and an explicit cancellation boundary"
      (with-redefs [persistance/db-list-session-turns
                    (constantly
                      [{:id "t1" :status :cancelled :position 1 :user-request "inspect and fix"}
                       {:id "t2" :status :running :position 2 :user-request "continue"}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:status :done
                                  :position 1
                                  :forms [{:scope "t1/i1/f1" :src "cat(src)" :stdout "src"}]}
                                 {:status :running
                                  :position 2
                                  :forms [{:scope "t1/i2/f1" :src "patch(src)"}]}])]

        (expect (= [{:turn 1
                     :user-request "inspect and fix"
                     :answer nil
                     :interrupted? false
                     :cancelled? true
                     :results [{:scope "t1/i1/f1" :src "cat(src)"}]}]
                   (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})}
                                          "t2"))))))

(defdescribe previous-request-usage-test
             (it "loads latest persisted request before current turn for iter-1 utilization"
                 (with-redefs [persistance/db-list-session-turns
                               (fn [_db-info session-id]
                                 (expect (= "s1" session-id))
                                 [{:id "t1" :position 1} {:id "t2" :position 2}
                                  {:id "t3" :position 3 :status :running}])

                               persistance/db-list-session-turn-iterations
                               (fn [_db-info turn-id]
                                 (case turn-id
                                   "t2"
                                   [{:position 1 :input-tokens 42000}
                                    {:position 2 :input-tokens 51000}]

                                   "t1"
                                   [{:position 1 :input-tokens 10000}]

                                   []))]

                   (expect (= {:last-request-tokens 51000
                               :last-request-turn-id "t2"
                               :last-request-turn-position 2
                               :last-request-iteration 2}
                              (previous-request-usage {:session-id "s1" :db-info ::db} "t3")))))
             (it "returns nil when no prior iteration has input tokens"
                 (with-redefs [persistance/db-list-session-turns
                               (constantly [{:id "t1" :position 1} {:id "t2" :position 2}])

                               persistance/db-list-session-turn-iterations
                               (constantly [{:position 1 :input-tokens 0}])]

                   (expect (nil? (previous-request-usage {:session-id "s1" :db-info ::db} "t2"))))))

(defdescribe stamp-utilization-monotonic-test
             ;; Regression: the stamp used to (dissoc "engine_utilization") on a nil
             ;; measurement, so a transient req=0 (iter-1 seed miss / errored iter)
             ;; BLANKED an already-shown "session_utilization" — the "sometimes works,
             ;; sometimes doesn't" flicker. The stamp must be monotonic.
             (let [stamp
                   (var-get #'lp/stamp-utilization!)

                   util1
                   {"last_request_tokens" 5000 "saturation" 3}

                   util2
                   {"last_request_tokens" 9000 "saturation" 5}]

               (it "stamps a real measurement onto the ctx-atom"
                   (let [ca (atom {})]
                     (stamp ca util1)
                     (expect (= util1 (get @ca "engine_utilization")))))
               (it "NEVER blanks an existing value on a transient nil measurement"
                   (let [ca (atom {"engine_utilization" util1})]
                     (stamp ca nil)
                     (expect (= util1 (get @ca "engine_utilization")))))
               (it "upgrades to a fresh measurement when one arrives"
                   (let [ca (atom {"engine_utilization" util1})]
                     (stamp ca util2)
                     (expect (= util2 (get @ca "engine_utilization")))))
               (it "is a no-op on a nil ctx-atom" (expect (nil? (stamp nil util1))))))

(defdescribe
  fold-session-scope-test
  (let [scope-key
        (var-get #'eng/scope-key)

        expand-through
        (var-get #'eng/expand-through)

        apply-summaries
        (var-get #'lp/apply-summaries)

        stamp-iter-universe!
        (var-get #'lp/stamp-iter-universe!)

        prior-scope-index
        (var-get #'lp/prior-turn-scope-index)]

    (it "scope-key parses iter + form scopes, dropping the form index"
        (expect (= [1 2] (scope-key "t1/i2")))
        (expect (= [1 2] (scope-key "t1/i2/f3")))
        (expect (= [10 20] (scope-key "t10/i20")))
        (expect (nil? (scope-key "garbage"))))
    (it "expand-through resolves a range cursor against the universe (inclusive)"
        (let [out (expand-through [{"through" "t1/i3" "gist" "g"}]
                                  ["t1/i1" "t1/i2" "t1/i3" "t1/i4"])]
          (expect (= #{"t1/i1" "t1/i2" "t1/i3"} (get (first out) "scopes")))
          (expect (nil? (get (first out) "through")))
          (expect (= "g" (get (first out) "gist")))))
    (it "expand-through leaves explicit-scope summaries untouched"
        (let [s [{"scopes" #{"t1/i2"} "gist" "g"}]]
          (expect (= s (expand-through s ["t1/i1" "t1/i2"])))))
    (it "apply-summaries collapses a through-range over the trailer, sparing later steps"
        (let [trailer
              [[0 {:forms-vec [{:scope "t1/i1/f1" :stdout "a"}]}]
               [1 {:forms-vec [{:scope "t1/i2/f1" :stdout "b"}]}]
               [2 {:forms-vec [{:scope "t1/i3/f1" :stdout "c"}]}]]

              out
              (apply-summaries trailer [{"through" "t1/i2" "gist" "early" "at_turn" 1}])]

          (expect (true? (:collapsed? (second (nth out 0)))))
          (expect (true? (:collapsed? (second (nth out 1)))))
          (expect (nil? (:collapsed? (second (nth out 2)))))))
    (it "prices only the projected wire while retaining the raw universe"
        (let [trailer
              [[0
                {:forms-vec [{:scope "t1/i1/f1"
                              :svar/tool-call-id "call-big"
                              :stdout (apply str (repeat 4000 "x"))}]}]
               [1 {:forms-vec [{:scope "t1/i2/f1" :stdout (apply str (repeat 400 "y"))}]}]]

              ca
              (atom {"session_summaries"
                     [{"scopes" #{"t1/i1"} "gist" "already folded" "at_turn" 1}]})

              wire
              (apply-summaries trailer (get @ca "session_summaries"))]

          (stamp-iter-universe! ca trailer wire)
          ;; A collapsed iteration keeps its identity in the universe but no longer
          ;; contributes its historical raw weight to a later broad fold.
          (expect (= ["t1/i1" "t1/i2"] (get @ca "engine_iter_universe")))
          (expect (= {"t1/i1" 0 "t1/i2" 373} (get @ca "engine_iter_weights")))
          (expect (nil? (get @ca "engine_iter_ntr")))))
    ;; Phantom-reclaim regression (session 881eb071…): the FIRST `{"through" …}`
    ;; fold of a new turn sweeps in every prior-turn seed iteration that was never
    ;; explicitly folded. Those seeds emit NOTHING on the wire when their turn
    ;; completed normally (`conversation-suffix`'s `:preserved-thinking/replay?
    ;; false` branch — the outcome rides in the prior-turn recap), yet they were
    ;; priced at full historical payload: cards claimed to reclaim more than the
    ;; whole request they folded, and the phantom tokens fed the session-rebase
    ;; counter. A seed from a terminal INCOMPLETE turn does replay its settled
    ;; results as plain text, so it keeps its weight.
    (it "prices completed-turn cross-turn seeds at zero, incomplete-turn seeds in full"
        (let [payload
              (apply str (repeat 4000 "x"))

              seed
              (fn [scope status]
                {:forms-vec [{:scope scope :stdout payload}]
                 :cross-turn/turn-status status
                 :preserved-thinking/replay? false})

              trailer
              [[0 (seed "t1/i1/f1" :done)] [1 (seed "t2/i1/f1" :cancelled)]
               [2 {:forms-vec [{:scope "t3/i1/f1" :stdout payload}]}]]

              ca
              (atom {})]

          (stamp-iter-universe! ca trailer)
          (expect (= {"t1/i1" 0 "t2/i1" 1480 "t3/i1" 1480} (get @ca "engine_iter_weights")))))
    ;; Frozen-prompt regression (session 0cfd25a7…): a fold recorded under an
    ;; EARLIER/foreign turn numbering kept re-resolving its range cursor against
    ;; every later live turn, collapsing the whole trailer. The model then never
    ;; saw its own tool results and re-issued the same call for 60+ iterations.
    (it "a fold whose cursor outlives the live turn numbering never collapses the live turn"
        (let [trailer
              [[0 {:forms-vec [{:scope "t95/i1/f1" :stdout "a"}]}]
               [1 {:forms-vec [{:scope "t95/i2/f1" :stdout "b"}]}]
               [2 {:forms-vec [{:scope "t95/i3/f1" :stdout "c"}]}]]

              out
              (apply-summaries trailer [{"through" "t113/i9" "gist" "stale numbering"}])]

          (expect (every? (fn [[_ rec]]
                            (nil? (:collapsed? rec)))
                          out))
          (expect (= trailer out))))
    (it
      "a fold recorded in an EARLIER turn never collapses the live turn"
      (let [trailer
            [[0 {:forms-vec [{:scope "t96/i1/f1" :stdout "a"}]}]
             [1 {:forms-vec [{:scope "t96/i2/f1" :stdout "b"}]}]]

            out
            (apply-summaries trailer [{"scopes" #{"t96/i1" "t96/i2"} "gist" "old" "at_turn" 95}])]

        (expect (every? (fn [[_ rec]]
                          (nil? (:collapsed? rec)))
                        out))))
    (it "an unstamped fold cannot collapse live-turn iterations"
        (let [trailer
              [[0 {:forms-vec [{:scope "t96/i1/f1" :stdout "a"}]}]
               [1 {:forms-vec [{:scope "t96/i2/f1" :stdout "b"}]}]]

              out
              (apply-summaries trailer [{"scopes" #{"t96/i1" "t96/i2"} "gist" "unstamped"}])]

          (expect (= trailer out))))
    (it "a fold recorded in THIS turn still collapses its own live iterations"
        (let [trailer
              [[0 {:forms-vec [{:scope "t96/i1/f1" :stdout "a"}]}]
               [1 {:forms-vec [{:scope "t96/i2/f1" :stdout "b"}]}]]

              out
              (apply-summaries trailer [{"through" "t96/i1" "gist" "in-turn" "at_turn" 96}])]

          (expect (true? (:collapsed? (second (nth out 0)))))
          (expect (nil? (:collapsed? (second (nth out 1)))))))
    (it "a stale-numbered fold still collapses PRIOR-turn scopes on the trailer"
        (let [trailer
              [[0
                {:preserved-thinking/replay? false :forms-vec [{:scope "t94/i1/f1" :stdout "old"}]}]
               [1 {:forms-vec [{:scope "t95/i1/f1" :stdout "live"}]}]]

              out
              (apply-summaries trailer [{"scopes" #{"t94/i1" "t95/i1"} "gist" "g" "at_turn" 94}])]

          (expect (true? (:collapsed? (second (nth out 0)))))
          (expect (nil? (:collapsed? (second (nth out 1)))))))
    (it "prior-turn-scope-index: gist applies via form->iter normalization, ONE deduped entry"
        ;; The path-A regression: a fold recorded at iteration scope (t1/i1) must
        ;; apply to forms carrying FORM scopes (t1/i1/f1, t1/i1/f2) and collapse to
        ;; a SINGLE gist line, not repeat per form.
        (let [forms
              [{:scope "t1/i1/f1" :stdout "a" :src "(cat \"x\")"}
               {:scope "t1/i1/f2" :stdout "b" :src "(rg \"y\")"}
               {:scope "t1/i2/f1" :stdout "c" :src "(ls)"}]

              out
              (prior-scope-index forms [{"scopes" #{"t1/i1"} "gist" "explored"}])]

          (expect (= 1 (count (filter :gist out))))
          (expect (= {:scope "t1/i1" :gist "explored"} (first (filter :gist out))))
          (expect (some #(= "t1/i2/f1" (:scope %)) out))))
    (it
      "prior-turn-scope-index: ONE fold over many iterations emits ONE gist line, not one per iteration"
      ;; The resume-bloat regression: dedup used to key on the ITERATION scope, so a
      ;; single fold_session covering 40 iterations replayed its identical gist 40
      ;; times in every later request (and in every message queued behind a running
      ;; turn). Dedup keys on the breadcrumb TEXT, so one fold costs one line.
      (let [forms
            (vec (for [i (range 1 21)]
                   {:scope (str "t1/i" i "/f1") :stdout "r" :src "(cat)"}))

            out
            (prior-scope-index forms
                               [{"scopes" (into #{} (map #(str "t1/i" %)) (range 1 21))
                                 "gist" "one big gist"}])]

        (expect (= [{:scope "t1/i1" :gist "one big gist"}] out))))
    (it "prior-turn-scope-index: distinct gists stay distinct while each collapses to one line"
        (let [forms
              [{:scope "t1/i1/f1" :stdout "a" :src "(cat)"}
               {:scope "t1/i2/f1" :stdout "b" :src "(rg)"}
               {:scope "t1/i3/f1" :stdout "c" :src "(ls)"}
               {:scope "t1/i4/f1" :stdout "d" :src "(ls)"}]

              out
              (prior-scope-index forms
                                 [{"scopes" #{"t1/i1" "t1/i2"} "gist" "A"}
                                  {"scopes" #{"t1/i3" "t1/i4"} "gist" "B"}])]

          (expect (= [{:scope "t1/i1" :gist "A"} {:scope "t1/i3" :gist "B"}] out))))
    (it "prior-turn-scope-index: a gist-less fold emits ONE dropped breadcrumb"
        (let [forms
              [{:scope "t1/i1/f1" :stdout "a" :src "(cat)"}
               {:scope "t1/i1/f2" :stdout "b" :src "(rg)"}
               {:scope "t1/i2/f1" :stdout "c" :src "(ls)"}]

              out
              (prior-scope-index forms [{"scopes" #{"t1/i1"} "note" " · saved ~1 token"}])]

          (expect (= {:scope "t1/i1" :dropped? true :note " · saved ~1 token"}
                     (first (filter :dropped? out))))
          (expect (= 1 (count (filter :dropped? out))))
          (expect (not-any? #(re-find #"^t1/i1/" (str (:scope %))) out))
          (expect (some #(= "t1/i2/f1" (:scope %)) out))))
    (it "supersede-summaries collapses summary-of-summary (subset dropped, superset/newer wins)"
        (let [supersede (var-get #'eng/supersede-summaries)]
          ;; proper subset is covered by the broader fold → only the superset survives
          (expect (= [{"scopes" #{"t1/i2" "t1/i3" "t1/i4"} "gist" "B"}]
                     (supersede [{"scopes" #{"t1/i2" "t1/i3"} "gist" "A"}
                                 {"scopes" #{"t1/i2" "t1/i3" "t1/i4"} "gist" "B"}])))
          ;; equal sets → the later (newer) gist wins
          (expect (= [{"scopes" #{"t1/i1"} "gist" "new"}]
                     (supersede [{"scopes" #{"t1/i1"} "gist" "old"}
                                 {"scopes" #{"t1/i1"} "gist" "new"}])))
          ;; disjoint and partial-overlap → both kept (coverage differs)
          (expect (= 2
                     (count (supersede [{"scopes" #{"t1/i1"} "gist" "A"}
                                        {"scopes" #{"t1/i2"} "gist" "B"}]))))
          (expect (= 2
                     (count (supersede [{"scopes" #{"t1/i1" "t1/i2"} "gist" "A"}
                                        {"scopes" #{"t1/i2" "t1/i3"} "gist" "B"}]))))))))

(defdescribe
  turn-position-state-test
  (it
    "seeds turn-state with persisted turn position before iteration render"
    (let [seen
          (atom nil)

          env
          {:db-info ::db :session-id "s1" :turn-state-atom (ctx-loop/make-turn-state-atom)}]

      (with-redefs [persistance/db-store-session-turn!
                    (fn [_db opts]
                      (expect (=
                                {:parent-session-id "s1" :user-request "follow up" :status :running}
                                opts))
                      "turn-3")

                    persistance/db-update-session-turn!
                    (fn [_db turn-id opts]
                      (reset! seen {:turn-id turn-id :opts opts}))

                    lp/session-turn-position
                    (fn [_env turn-id]
                      (expect (= "turn-3" turn-id))
                      3)

                    lp/iteration-loop
                    (fn [env* user-request opts]
                      (expect (= "follow up" user-request))
                      (expect (= "turn-3" (:session-turn-id opts)))
                      (expect (= 3 (:turn-position (ctx-loop/read-turn-state env*))))
                      (expect (nil? (:iteration (ctx-loop/read-turn-state env*))))
                      {:iteration-count 1 :duration-ms 0})]

        (let [result (run-normal-turn! env "follow up" {})]
          (expect (= "turn-3" (:session-turn-id result)))
          (expect (= "turn-3" (:turn-id @seen))))))))

;; Regression, issue #71: cancelling a foreground turn launched the deferred LLM title request.
(defdescribe
  cancelled-turn-titling-test
  (it
    "does not launch an LLM title request after cancellation"
    (let [title-requests
          (atom 0)

          env
          {:db-info ::db :session-id "s1" :turn-state-atom (ctx-loop/make-turn-state-atom)}]

      (with-redefs [persistance/db-store-session-turn!
                    (fn [_db _opts]
                      "turn-1")

                    persistance/db-update-session-turn!
                    (fn [& _]
                      nil)

                    lp/session-turn-position
                    (fn [& _]
                      1)

                    lp/iteration-loop
                    (fn [& _]
                      {:status :cancelled :iteration-count 0 :duration-ms 0})

                    titling/maybe-auto-title!
                    (fn [& _]
                      nil)

                    titling/after-turn-auto-title!
                    (fn [& _]
                      (swap! title-requests inc))]

        (let [result (run-normal-turn! env "cancel this" {})]
          (expect (= :cancelled (:status result)))
          (expect (zero? @title-requests)))))))

(defdescribe max-tokens-exceeded-retry-test
             (it "recognises :svar.llm/max-tokens-exceeded as retry-able"
                 (let [e (ex-info "max_tokens hit"
                                  {:type :svar.llm/max-tokens-exceeded
                                   :output-tokens 2048
                                   :reasoning-length 1900})]
                   (expect (true? (max-tokens-exceeded-error? e)))))
             (it "does not confuse other svar errors with the max-tokens variant"
                 ;; `:svar.llm/empty-content` is the genuine \"model returned nothing useful\"
                 ;; failure mode. It must NOT trigger the max-tokens-bump retry path — that
                 ;; would burn provider tokens without any chance of fixing the underlying
                 ;; problem (the model is confused, more budget will not help).
                 (let [e (ex-info "blank" {:type :svar.llm/empty-content})]
                   (expect (false? (max-tokens-exceeded-error? e))))
                 (let [e (ex-info "http" {:type :svar.core/http-error :status 500})]
                   (expect (false? (max-tokens-exceeded-error? e)))))
             (it "doubles max_tokens from the reported `:output-tokens`"
                 ;; Provider reports the exact number it cut off at — doubling that gives
                 ;; the next attempt enough headroom in the common case (reasoning ate
                 ;; roughly all of the budget).
                 (expect (= {:max_tokens 4096} (bumped-max-tokens-extra-body nil 2048)))
                 (expect (= {:max_tokens 16000} (bumped-max-tokens-extra-body nil 8000)))
                 ;; Preserves caller-supplied extra-body keys so the bump does not drop
                 ;; their overrides (e.g. `:store false` for Codex).
                 (expect (= {:store false :max_tokens 4096}
                            (bumped-max-tokens-extra-body {:store false} 2048))))
             (it "falls back to 8192 when no previous max is known"
                 ;; Defensive: the error carries no `:output-tokens` (older svar version,
                 ;; or non-streaming path). Use a moderate-sized cap as fallback so we
                 ;; don't accidentally explode the request body.
                 (expect (= {:max_tokens 16384} (bumped-max-tokens-extra-body nil nil)))))

(defdescribe llm-provider-error-context-test
             ;; Iteration-error-data shape (built by `format-exception`):
             ;;   {:class "..."      — exception class name
             ;;    :message "..."    — ex-message
             ;;    :data {...}       — raw `(ex-data t)` from svar, untouched
             ;;    :context {...}}   — vis loop ctx snapshot
             ;; So predicate / context helpers consume `(:data iter-err)` for any
             ;; svar-side ex-info keys, NOT top-level. Tests reflect that.
             (it "surfaces dedicated copy + hint for :svar.llm/max-tokens-exceeded"
                 (let [iter-err
                       {:type :svar.llm/max-tokens-exceeded
                        :data {:reasoning-length 1900 :output-tokens 2048}}

                       ctx
                       (llm-provider-error-context 3 iter-err)]

                   (expect (= :llm-provider/max-tokens-exhausted (:type ctx)))
                   (expect (= 1900 (:reasoning-length ctx)))
                   (expect (= 2048 (:output-tokens ctx)))
                   (expect (str/includes? (:message ctx) "max_tokens"))
                   (expect (str/includes? (:message ctx) "hidden reasoning"))
                   (expect (str/includes? (:hint ctx) "canonical"))
                   (expect (not (str/includes? (:hint ctx) "v/strategy")))
                   (expect (not (str/includes? (:hint ctx) ":start/:max-lines")))))
             (it "keeps the legacy `:llm-provider/output-budget-exhausted` mapping"
                 ;; Anthropic native `:svar.core/stream-incomplete + :reason
                 ;; max_output_tokens` is detected through `:data` (nested), not
                 ;; top-level — `format-exception` puts raw `ex-data` under `:data`.
                 (let [iter-err
                       {:data {:type :svar.core/stream-incomplete :reason "max_output_tokens"}}

                       ctx
                       (llm-provider-error-context 2 iter-err)]

                   (expect (= :llm-provider/output-budget-exhausted (:type ctx))))))

(defn- stub-iter
  "Build a synthetic trailer-iters entry for preserved-thinking tests.
   `id` is any unique label for the position; `provider`/`model` control
   how `compatible-preserved-thinking-trailer-iters` filters; the rest
   default to a same-model, replay-eligible canonical thinking block."
  [{:keys [id provider model thinking signature replay?]
    :or {provider :zai-coding-plan model "glm-5.1" replay? true}}]
  [id
   {:assistant-message {:role "assistant"
                        :content [{:type "thinking"
                                   :thinking (or thinking (str "think-" id))
                                   :thinking-signature (or signature (str "sig-" id))}]}
    :llm-provider provider
    :llm-model model
    :preserved-thinking/replay? replay?}])

(defdescribe
  preserved-thinking-replay-test
  (it "returns every compatible assistant message in arrival order"
      ;; Why every message, not just the last: GLM clear_thinking,
      ;; Anthropic HMAC chains, and OpenAI Responses encrypted reasoning
      ;; all require the full assistant chain since the last user turn.
      ;; Returning only the latest step (the pre-fix behaviour) made GLM
      ;; re-derive scratch state each iteration, pinning `cached_tokens`
      ;; across many iterations before the fix.
      (let [target
            {:provider :zai-coding-plan :model "glm-5.1"}

            trailer
            (mapv #(stub-iter {:id %}) [1 2 3])

            compat
            (compatible-preserved-thinking-trailer-iters trailer target)

            replays
            (preserved-thinking-replay-messages compat)]

        (expect (= 3 (count compat)))
        (expect (= 3 (count replays)))
        (expect (= ["think-1" "think-2" "think-3"]
                   (mapv (fn [m]
                           (-> m
                               :content
                               first
                               :thinking))
                         replays)))))
  (it "drops iterations from a different provider/model"
      ;; Cross-provider replay is forbidden: provider-native thinking
      ;; signatures are not portable (z.ai = raw text, Anthropic = HMAC,
      ;; OpenAI Responses = JSON reasoning item). The compatible filter
      ;; must reject mismatches before this fn sees them.
      (let [target
            {:provider :zai-coding-plan :model "glm-5.1"}

            trailer
            [(stub-iter {:id 1}) (stub-iter {:id 2 :provider :anthropic :model "claude-sonnet-4.6"})
             (stub-iter {:id 3})]

            compat
            (compatible-preserved-thinking-trailer-iters trailer target)

            replays
            (preserved-thinking-replay-messages compat)]

        (expect (= 2 (count replays)))
        (expect (= ["think-1" "think-3"]
                   (mapv (fn [m]
                           (-> m
                               :content
                               first
                               :thinking))
                         replays)))))
  (it "replays only iterations explicitly opted in"
      ;; Cross-turn seeds opt out and live iterations opt in. Missing ownership is
      ;; not interpreted as consent to replay provider-native state.
      (let [target
            {:provider :zai-coding-plan :model "glm-5.1"}

            missing-flag
            (-> (stub-iter {:id 0})
                second
                (dissoc :preserved-thinking/replay?))

            trailer
            [[0 missing-flag] (stub-iter {:id 1 :replay? false}) (stub-iter {:id 2 :replay? true})]

            compat
            (compatible-preserved-thinking-trailer-iters trailer target)

            replays
            (preserved-thinking-replay-messages compat)]

        (expect (= 1 (count replays)))
        (expect (= ["think-2"]
                   (mapv (fn [m]
                           (-> m
                               :content
                               first
                               :thinking))
                         replays)))))
  (it
    "returns empty when no iteration has an :assistant-message"
    ;; Iterations that errored before the model produced a usable
    ;; assistant turn (e.g. provider HTTP 4xx mid-stream) lack
    ;; `:assistant-message`; the compatible filter drops them so the
    ;; replay never tries to send an empty/partial block.
    (let [target
          {:provider :zai-coding-plan :model "glm-5.1"}

          trailer
          [[1
            {:llm-provider :zai-coding-plan :llm-model "glm-5.1" :preserved-thinking/replay? true}]]

          compat
          (compatible-preserved-thinking-trailer-iters trailer target)

          replays
          (preserved-thinking-replay-messages compat)]

      (expect (zero? (count compat)))
      (expect (zero? (count replays))))))

(defn- stub-tool-iter
  "Trailer entry for conversation-suffix tests: one tool call with its
   result, an assistant message carrying thinking + the tool_use."
  [{:keys [id provider model replay? content attachments]
    :or {provider :lmstudio model "google/gemma-4-12b-qat" replay? true}}]
  [id
   {:assistant-message {:role "assistant"
                        :content (or content
                                     [{:type "thinking"
                                       :thinking (str "think-" id)
                                       :thinking-signature (str "sig-" id)}
                                      {:type "tool_use"
                                       :id (str "tc-" id)
                                       :name "python_execution"
                                       :input {"query" "lmstudio"}}])}
    :llm-provider provider
    :llm-model model
    :preserved-thinking/replay? replay?
    :attachments attachments
    :tool-calls [{:id (str "tc-" id) :name "python_execution" :input {"query" "lmstudio"}}]
    :forms-vec [{:scope (str "t1/i" id)
                 :svar/tool-call-id (str "tc-" id)
                 :stdout "{\"item_count\":2,\"paths\":[\"a.clj\",\"b.clj\"]}"}]}])

(defdescribe
  conversation-suffix-mismatch-test
  ;; The session-c4b630c7 regression: the health gate demoted lmstudio so the
  ;; SELECTED model (target) was anthropic/opus while the ACTUAL server was
  ;; lmstudio/gemma. The old suffix dropped the whole [assistant, tool_result]
  ;; pair on that mismatch — the model never saw its own grep result and
  ;; re-issued the identical call every iteration.
  (it "replays [assistant sans thinking, tool_result] on provider/model mismatch"
      (let [target
            {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

            suffix
            (conversation-suffix [(stub-tool-iter {:id 1})] target)]

        (expect (= 2 (count suffix)))
        (let [[assistant results]
              suffix

              types
              (mapv :type (:content assistant))]

          (expect (= "assistant" (:role assistant)))
          ;; thinking stripped, tool_use kept — the tool_result stays answerable
          (expect (= ["tool_use"] types))
          (expect (= "user" (:role results)))
          (expect (= "tc-1"
                     (-> results
                         :content
                         first
                         :tool_use_id)))
          (expect (string? (-> results
                               :content
                               first
                               :content)))
          (expect (str/includes? (-> results
                                     :content
                                     first
                                     :content)
                                 "item_count")))))
  (it "replays thinking verbatim when provider+model match the target"
      (let [target
            {:provider :lmstudio :model "google/gemma-4-12b-qat"}

            suffix
            (conversation-suffix [(stub-tool-iter {:id 1})] target)]

        (expect (= 2 (count suffix)))
        (expect (= ["thinking" "tool_use"] (mapv :type (:content (first suffix)))))))
  (it "degrades to a plain-text results message when only thinking remains"
      ;; No tool_use survives the strip → a tool_result would be orphaned
      ;; (wire error on Anthropic), so the outputs ride as plain text.
      (let [target
            {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

            entry
            (stub-tool-iter
              {:id 1
               :content [{:type "thinking" :thinking "only-thinking" :thinking-signature "sig"}]})

            suffix
            (conversation-suffix [entry] target)]

        (expect (= 1 (count suffix)))
        (let [[results] suffix]
          (expect (= "user" (:role results)))
          (expect (string? (:content results)))
          (expect (str/includes? (:content results) "item_count")))))
  (it "still excludes successful cross-turn seeds entirely"
      (let [target
            {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

            suffix
            (conversation-suffix [(stub-tool-iter {:id 1 :replay? false})] target)]

        (expect (empty? suffix))))
  (it
    "replays terminal-incomplete cross-turn results as plain text without orphaned tool_result blocks"
    (let [target
          {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

          [pos rec]
          (stub-tool-iter {:id 1 :replay? false})

          suffix
          (conversation-suffix [[pos (assoc rec :cross-turn/turn-status :cancelled)]] target)]

      (expect (= 1 (count suffix)))
      (let [[results] suffix]
        (expect (= "user" (:role results)))
        (expect (string? (:content results)))
        (expect (str/includes? (:content results) "item_count"))
        (expect (not (vector? (:content results))))))))

(defdescribe
  cancellation-continuity-provider-messages-test
  (it
    "assembles the cancelled request, abort boundary, settled call, and settled output without a tool protocol orphan"
    (let [target
          {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

          initial
          (prompt/assemble-initial-messages {:previous-turn-context
                                             [{:turn 1
                                               :user-request "inspect and fix"
                                               :cancelled? true
                                               :results [{:scope "t1/i1/f1" :src "cat(src)"}]}]
                                             :turn-context "session[\"turn\"] = 2"
                                             :initial-user-content "continue"})

          [pos rec]
          (stub-tool-iter {:id 1 :replay? false})

          messages
          (into initial
                (conversation-suffix [[pos (assoc rec :cross-turn/turn-status :cancelled)]]
                                     target))]

      (expect (= 3 (count messages)))
      (expect (str/includes? (:content (first messages)) "inspect and fix"))
      (expect (str/includes? (:content (first messages)) "cat(src)"))
      (expect (str/includes? (:content (first messages)) "<turn_cancelled>"))
      (expect (str/includes? (:content (second messages)) "continue"))
      (expect (string? (:content (last messages))))
      (expect (str/includes? (:content (last messages)) "item_count")))))

;; 1x1 red PNG — REAL pixels. Every image block the loop emits is decoded at
;; SEND time, so a placeholder payload is (correctly) refused and never reaches
;; a provider.
(def ^:private replay-png-b64
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")

(defdescribe
  conversation-suffix-image-replay-test
  ;; Generated figures (matplotlib plt.show()) an iteration's tool call
  ;; produced are persisted as :attachments and replayed to the model as
  ;; their OWN vision user message AFTER the <results> — but ONLY when the
  ;; target model advertises :vision.
  (let [att {:tool-call-id "tc-1"
             :media-type "image/png"
             :base64 replay-png-b64
             :filename "plot.png"
             :size 67}]
    (it "appends a vision user message with the image AFTER results"
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})] target)]

          ;; [assistant-replay, <results>, image-user] — image is LAST so it
          ;; never sits between a tool_use and its tool_result.
          (expect (= 3 (count suffix)))
          (let [img (last suffix)]
            (expect (= "user" (:role img)))
            (expect (= ["image_url"] (mapv :type (:content img))))
            (expect (= (str "data:image/png;base64," replay-png-b64)
                       (-> img
                           :content
                           first
                           :image_url
                           :url))))))
    (it "omits the image entirely for a text-only (non-vision) target"
        (let [target {:provider :zai-coding-plan :model "glm-5-turbo"}
              suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})] target)]

          ;; back to the plain [assistant, results] pair, no image block
          (expect (= 2 (count suffix)))
          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            suffix))))
    (it "skips a non-image attach artifact — a csv never rides as an image block"
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              csv {:tool-call-id "tc-2"
                   :media-type "text/csv"
                   :base64 "YSxi"
                   :filename "data.csv"
                   :size 3
                   :kind "file"}
              suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att csv]})] target)
              img (last suffix)]

          ;; the image still replays, but ONLY it — the csv artifact is
          ;; DB/display-only, never a broken data:text/csv image block.
          (expect (= 3 (count suffix)))
          (expect (= "user" (:role img)))
          (expect (= ["image_url"] (mapv :type (:content img))))
          (expect (= (str "data:image/png;base64," replay-png-b64)
                     (-> img
                         :content
                         first
                         :image_url
                         :url)))))
    (it
      "rasterizes an SVG figure to PNG on the way out"
      ;; No wire reads markup, so the vector is converted at SEND time — the
      ;; stored attachment stays SVG and is re-judged on every later turn.
      (let
        [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
         svg
         {:tool-call-id "tc-3"
          :media-type "image/svg+xml"
          :base64
          (.encodeToString
            (java.util.Base64/getEncoder)
            (.getBytes
              "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect width=\"16\" height=\"16\" fill=\"#333\"/></svg>"
              "UTF-8"))
          :filename "fig.svg"
          :kind "image"}
         suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [svg]})] target)]

        (expect (= 3 (count suffix)))
        (expect (str/starts-with? (get-in (last suffix) [:content 0 :image_url :url])
                                  "data:image/png;base64,"))))
    (it "skips what cannot become a picture at all"
        ;; The session killer: an attachment that merely SAYS `image/…` used to
        ;; satisfy a coarse media-type test, so unrenderable markup, a corrupt
        ;; raster or an unverifiable blank type rode as an image block and the
        ;; provider answered 400 on EVERY later turn, because attachments replay
        ;; — the session never recovered. Now the bytes must decode here.
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              broken-svg {:tool-call-id "tc-3"
                          :media-type "image/svg+xml"
                          :base64 (.encodeToString (java.util.Base64/getEncoder)
                                                   (.getBytes "not an image at all" "UTF-8"))
                          :filename "fig.svg"
                          :kind "image"}
              ;; a perfect PNG signature + IHDR over an unreadable stream
              corrupt (assoc broken-svg
                        :media-type "image/png"
                        :filename "dot.png"
                        :base64 (.encodeToString
                                  (java.util.Base64/getEncoder)
                                  (byte-array (concat (take 33
                                                            (.decode (java.util.Base64/getDecoder)
                                                                     ^String replay-png-b64))
                                                      (repeat 24 0)))))
              blank (assoc broken-svg
                      :media-type ""
                      :filename "fig")
              only-bad (conversation-suffix [(stub-tool-iter
                                               {:id 1 :attachments [broken-svg corrupt blank]})]
                                            target)
              mixed (conversation-suffix [(stub-tool-iter {:id 1 :attachments [corrupt att]})]
                                         target)]

          ;; nothing replayable left → no vision message at all
          (expect (= 2 (count only-bad)))
          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            only-bad))
          ;; the good PNG beside it still rides, alone
          (expect (= 3 (count mixed)))
          (expect (= [(str "data:image/png;base64," replay-png-b64)]
                     (mapv #(get-in % [:image_url :url]) (:content (last mixed)))))))
    (it "drops the image when fold_session collapsed the iteration"
        ;; The invariant: a figure's vision visibility TRACKS its iteration's
        ;; textual visibility. Once fold_session collapses the step, its image
        ;; bytes leave the wire with it and are never re-billed.
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              [pos rec] (stub-tool-iter {:id 1 :attachments [att]})
              suffix (conversation-suffix [[pos (assoc rec :collapsed? true)]] target)]

          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            suffix))))
    (it "drops a folded cross-turn seed's image — collapse wins over the seed branch"
        ;; The leak this guards: a prior-turn figure carried as a seed
        ;; (:preserved-thinking/replay? false) used to be byte-immune to
        ;; compaction because the seed branch ran BEFORE the collapse check.
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              [pos rec] (stub-tool-iter {:id 1 :replay? false :attachments [att]})
              suffix (conversation-suffix [[pos (assoc rec :collapsed? true)]] target)]

          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            suffix))))
    (it "still rides a NON-folded cross-turn seed's image to a vision target"
        ;; The reorder must not break the one path that legitimately emits a
        ;; seed's image: its bytes were never wired to any prior turn.
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              suffix (conversation-suffix [(stub-tool-iter
                                             {:id 1 :replay? false :attachments [att]})]
                                          target)]

          (expect (= 1 (count suffix)))
          (expect (= ["image_url"] (mapv :type (:content (first suffix)))))))
    (it "emits no image message when an iteration produced no attachments"
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              suffix (conversation-suffix [(stub-tool-iter {:id 1})] target)]

          (expect (= 2 (count suffix)))))))

(defdescribe
  target-vision-is-provider-scoped-test
  "Whether the target can SEE is a question about this provider's serving of the
   model, not about the model's name."
  ;; Regression: the gate asked svar for name-only metadata, so it read a curated
  ;; table plus a regex over the name. On a real fleet that mislabels one model in
  ;; four: every namespaced OpenRouter slug, Copilot's proxied upstreams, mistral's
  ;; whole line and `o3` were called blind and lost their pixels, while
  ;; `gpt-4o-search-preview` — text-only upstream — was handed image blocks and 400'd
  ;; on every replay of that attachment.
  (let [att {:tool-call-id "tc-1"
             :media-type "image/png"
             :base64 replay-png-b64
             :filename "plot.png"
             :size 67}]
    (it "believes the catalog over the model name, in both directions"
        ;; Sighted upstream, name says nothing.
        (expect (target-supports-vision? {:provider :openrouter
                                          :model "qwen/qwen2.5-vl-72b-instruct"}))
        (expect (target-supports-vision? {:provider :mistral :model "mistral-medium-latest"}))
        ;; Text-only upstream behind a name that matches the gpt-4o vision pattern.
        (expect (not (target-supports-vision? {:provider :openrouter
                                               :model "openai/gpt-4o-search-preview"})))
        ;; Unchanged: a coding-plan text model stays blind.
        (expect (not (target-supports-vision? {:provider :zai-coding-plan :model "glm-5-turbo"}))))
    (it "reads a provider the curated table never mentions"
        ;; models.dev carries `opencode-go`; svar's own KNOWN_PROVIDERS does not, and
        ;; the name heuristic has no idea what `mimo-v2.5` is.
        (expect (target-supports-vision? {:provider :opencode-go :model "mimo-v2.5"}))
        (expect (not (target-supports-vision? {:provider :opencode-go
                                               :model "deepseek-v4-flash"}))))
    (it "lets config speak for a model no catalog carries"
        ;; The escape hatch for local runtimes: nothing else can know a local
        ;; llava reads images.
        (expect (target-supports-vision?
                  {:provider :ollama :model "llava:13b" :capabilities #{:chat :vision}}))
        (expect (not (target-supports-vision? {:provider :ollama :model "llava:13b"}))))
    (it "answers nothing for a target with no provider but keeps the name's word"
        (expect (target-supports-vision? {:model "claude-opus-4-8"}))
        (expect (not (target-supports-vision? {:model "glm-5-turbo"}))))
    (it "replays PIXELS to a catalog-sighted target instead of paying for a description"
        (let [asked (atom 0)
              describer (fn [images]
                          (swap! asked inc)
                          (mapv (fn [_]
                                  {:text "a plot" :model "seer-1"})
                                images))
              suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})]
                                          {:provider :opencode-go :model "mimo-v2.5"}
                                          {:describe-images describer})]

          (expect (= 3 (count suffix)))
          (expect (= ["image_url"] (mapv :type (:content (last suffix)))))
          (expect (zero? @asked))))))

(defdescribe target-vision-follows-the-live-router-test
             "The gate's verdict rides the LIVE router, so a provider-level veto reaches it."
             ;; Regression: a real gateway proxies models models.dev lists as multimodal, but
             ;; its own request schema has no image content part — six of the fleet's models
             ;; answered an attached screenshot with HTTP 400 `unknown variant image_url,
             ;; expected text`. Trusting the catalog alone would have sent pixels into that
             ;; 400 on every turn that carried an image.
             (let [router-for
                   (fn [veto?]
                     (svar/make-router [(cond-> {:id :openrouter
                                                 :api-key "k"
                                                 :models [{:name "qwen/qwen2.5-vl-72b-instruct"}]}
                                          veto?
                                          (assoc :image-input? false))]))

                   target-for
                   (fn [veto?]
                     ((deref #'lp/replay-context)
                       (svar-router/resolve-effective-model (router-for veto?) {})))]

               (it "carries the router's capabilities into the replay target"
                   (expect (contains? (:capabilities (target-for false)) :vision))
                   (expect (not (contains? (:capabilities (target-for true)) :vision))))
               (it "sends pixels only while the provider's wire accepts them"
                   (expect (target-supports-vision? (target-for false)))
                   (expect (not (target-supports-vision? (target-for true)))))))

(defn- with-throwaway-vision-store
  "Run `f` against a `~/.vis` of this test's own. What the gate learns about image support
   is PERSISTED, and the machine running the suite has a real memory to keep out of."
  [f]
  (let [dir (java.io.File. (System/getProperty "java.io.tmpdir")
                           (str "vis-vision-gate-" (System/nanoTime)))]
    (.mkdirs dir)
    (try (with-redefs [config/config-dir (constantly (.getPath dir))
                       config/state-path (constantly (str (.getPath dir) "/state.yml"))]

           (f))
         (finally (vision-describe/clear-image-blind!)))))

;; Regression: the wire's own answer died with the request. A gateway that refuses
;; image content parts (HTTP 400 `unknown variant image_url, expected text`) was asked
;; again on the very next turn, because every later decision re-read the catalog —
;; which describes the MODEL and knows nothing about the endpoint in front of it.
(defdescribe
  image-blind-learning-reaches-the-gate-test
  "What a failed request taught outranks every capability table, for this process and
   for the ones after it."
  (it "stops sending pixels to a provider whose wire refused them"
      (with-throwaway-vision-store
        (fn []
          (expect (target-supports-vision? {:provider :opencode-go :model "mimo-v2.5"}))
          (vision-describe/remember-image-blind! :opencode-go)
          (expect (not (target-supports-vision? {:provider :opencode-go :model "mimo-v2.5"})))
          ;; Only that provider: the rest of the fleet keeps its eyes.
          (expect (target-supports-vision? {:provider :openrouter
                                            :model "qwen/qwen2.5-vl-72b-instruct"})))))
  (it "learns from the turn's own failure, not only from the describer's"
      (with-throwaway-vision-store
        (fn []
          (lp/handle-iteration-exception!
            (ex-info "All providers exhausted"
                     {:type :svar.llm/all-providers-exhausted
                      :attempts [{:provider "opencode-go"
                                  :model "mimo-v2.5"
                                  :status 400
                                  :error (str "Error from provider (Console Go): unknown variant "
                                              "`image_url`, expected `text`")}
                                 {:provider "anthropic"
                                  :model "claude-opus-4-8"
                                  :status 429
                                  :error "rate limited"}]})
            {:iteration 1 :messages [] :routing {} :reasoning-level nil})
          (expect (vision-describe/image-blind-provider? :opencode-go))
          ;; A provider that merely ran out of quota keeps its eyes.
          (expect (not (vision-describe/image-blind-provider? :anthropic))))))
  (it "teaches nothing when the turn failed for an ordinary reason"
      (with-throwaway-vision-store
        (fn []
          (lp/handle-iteration-exception!
            (ex-info
              "Exceptional status code: 400"
              {:status 400 :provider-id :opencode-go :body "messages: at least one required"})
            {:iteration 1 :messages [] :routing {} :reasoning-level nil})
          (expect (not (vision-describe/image-blind-provider? :opencode-go))))))
  ;; The costly over-generalization in the other direction: one small model that
  ;; cannot read pixels used to cost its whole provider every pair of eyes it had,
  ;; because the refusal was charged to the endpoint that carried it.
  (it "keeps the provider's OTHER models when only one refused pixels"
      (with-throwaway-vision-store
        (fn []
          (expect (target-supports-vision? {:provider :opencode-go :model "mimo-v2.5"}))
          (expect (target-supports-vision? {:provider :opencode-go :model "qwen3.7-plus"}))
          (lp/handle-iteration-exception!
            (ex-info "Exceptional status code: 400"
                     {:status 400
                      :provider-id :opencode-go
                      :model "mimo-v2.5"
                      :body "The model mimo-v2.5 does not support image input"})
            {:iteration 1 :messages [] :routing {} :reasoning-level nil})
          (expect (not (target-supports-vision? {:provider :opencode-go :model "mimo-v2.5"})))
          (expect (target-supports-vision? {:provider :opencode-go :model "qwen3.7-plus"}))
          (expect (not (vision-describe/image-blind-provider? :opencode-go)))
          (expect (vision-describe/image-blind-model? "mimo-v2.5")))))
  ;; A restart used to be amnesia: the gate re-read the catalog, sent pixels into the
  ;; same 400, and the user paid the discovery again on the first image of the session.
  (it "still knows, in the next process, what this one learned"
      (with-throwaway-vision-store
        (fn []
          (vision-describe/remember-image-blind! :opencode-go)
          (vision-describe/clear-image-blind!)
          (expect (not (target-supports-vision? {:provider :opencode-go :model "mimo-v2.5"})))))))

(defdescribe
  conversation-suffix-blind-description-test
  "A target with NO vision used to lose every generated figure outright: the image
   was skipped and the model was told to open the file with PIL, which answers pixel
   size and never meaning. With a sighted model anywhere in the fleet, the same
   newest-first plan now replays each figure as that model's REPORT — text the blind
   model can actually read — while the pixels themselves stay off its wire."
  (let [att
        {:tool-call-id "tc-1"
         :media-type "image/png"
         :base64 replay-png-b64
         :filename "plot.png"
         :size 67}

        blind-target
        {:provider :zai-coding-plan :model "glm-5-turbo"}

        seeing-target
        {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

        describer
        (fn [images]
          (mapv (fn [img]
                  {:text (str "a plot of " (:filename img)) :model "seer-1"})
                images))

        suffix-for
        (fn [target opts]
          (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})] target opts))]

    (it "replays the figure as text for a blind target"
        (let [suffix
              (suffix-for blind-target {:describe-images describer})

              note
              (last suffix)]

          (expect (= 3 (count suffix)))
          (expect (= "user" (:role note)))
          ;; TEXT, not pixels: a blind wire never sees an image block.
          (expect (string? (:content note)))
          (expect (str/includes? (:content note) "a plot of plot.png"))
          (expect (str/includes? (:content note) "plot.png"))
          (expect (str/includes? (:content note) "seer-1"))
          (expect (str/includes? (:content note) "second-hand"))
          (expect (not (str/includes? (pr-str suffix) "image_url")))))
    (it "keeps today's silent behaviour when no describer is injected"
        ;; The 2-arity is the estimator's and the no-vision-model path: back to the
        ;; plain [assistant, results] pair.
        (let [suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})]
                                          blind-target)]
          (expect (= 2 (count suffix)))))
    (it "never describes for a target that can SEE"
        (let [called
              (atom 0)

              suffix
              (suffix-for seeing-target
                          {:describe-images (fn [images]
                                              (swap! called inc)
                                              (describer images))})]

          (expect (zero? @called))
          (expect (= 3 (count suffix)))
          (expect (= ["image_url"] (mapv :type (:content (last suffix)))))))
    (it "degrades to today's behaviour when the describer answers nothing"
        ;; Toggle off, blind fleet, refused ask, deadline — all arrive here as nil.
        (let [suffix (suffix-for blind-target
                                 {:describe-images (fn [_]
                                                     nil)})]
          (expect (= 2 (count suffix)))))
    ;; Regression: the describer ran per ITERATION, so a turn that produced a figure
    ;; in each of three steps paid three SERIAL round trips — and three separate
    ;; hard deadlines — inside request assembly, with the user waiting on all of it.
    (it "describes the WHOLE trailer in one pass"
        (let [passes
              (atom [])

              trailer
              (mapv (fn [i]
                      (stub-tool-iter {:id (inc i)
                                       :attachments [(assoc att
                                                       :filename (str "plot-" i ".png")
                                                       :tool-call-id (str "tc-" (inc i)))]}))
                    (range 3))

              suffix
              (conversation-suffix trailer
                                   blind-target
                                   {:describe-images (fn [images]
                                                       (swap! passes conj (mapv :filename images))
                                                       (describer images))})

              reports
              (filter #(str/includes? (str (:content %)) "a plot of") suffix)]

          (expect (= 1 (count @passes)))
          (expect (= #{"plot-0.png" "plot-1.png" "plot-2.png"} (set (first @passes))))
          ;; Each iteration still gets ITS figure's report, in its own place.
          (expect (= 3 (count reports)))))
    (it "describes only the images the replay budget kept"
        (let [seen
              (atom [])

              suffix
              (suffix-for blind-target
                          {:describe-images (fn [images]
                                              (swap! seen into (map :filename images))
                                              (describer images))})]

          (expect (= ["plot.png"] @seen))
          (expect (= 3 (count suffix)))))))

(defdescribe
  conversation-suffix-real-describer-test
  "The replay seam wired to the REAL side-channel (only the provider call stubbed),
   so the loop's own plumbing — toggle, capability routing, cache, message shape —
   is exercised, not a hand-rolled stand-in."
  (let [att
        {:tool-call-id "tc-1"
         :media-type "image/png"
         :base64 replay-png-b64
         :filename "plot.png"
         :size 67}

        seeing-router
        (svar/make-router [{:id :seeing
                            :api-key "k"
                            :base-url "http://seeing.invalid"
                            :api-style :openai
                            :models [{:name "seer" :capabilities #{:chat :vision}}]}])]

    (it "replays a figure a blind model cannot see as that fleet's report"
        (vision-describe/clear-cache!)
        (let [suffix
              (with-redefs-fn {#'svar/ask! (fn [_ _]
                                             {:result {:description "a red pixel on white"}})}
                #(conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})]
                                      {:provider :zai-coding-plan :model "glm-5-turbo"}
                                      {:describe-images ((deref #'lp/replay-image-describer)
                                                          {:router seeing-router}
                                                          "why is the plot empty?"
                                                          :seeing)}))

              note
              (last suffix)]

          (expect (= 3 (count suffix)))
          (expect (string? (:content note)))
          (expect (str/includes? (:content note) "a red pixel on white"))
          (expect (str/includes? (:content note) "seer"))
          (expect (not (str/includes? (pr-str suffix) "image_url")))))))

(defdescribe
  replay-image-describer-test
  "The describer is resolved from the SESSION's own fleet, so a session with nothing
   that can see never pays for the attempt."
  (let [describer #(deref #'lp/replay-image-describer)]
    (it "is nil when no configured model has vision"
        (let [router (svar/make-router [{:id :blind
                                         :api-key "k"
                                         :base-url "http://blind.invalid"
                                         :api-style :openai
                                         :models [{:name "cheap-blind" :capabilities #{:chat}}]}])]
          (expect (nil? ((describer) {:router router} "ctx" :blind)))))
    (it "is a fn when the fleet has a seeing model"
        (let [router (svar/make-router [{:id :seeing
                                         :api-key "k"
                                         :base-url "http://seeing.invalid"
                                         :api-style :openai
                                         :models [{:name "seer" :capabilities #{:chat :vision}}]}])]
          (expect (fn? ((describer) {:router router} "ctx" :seeing)))))
    ;; Regression: a router-SHAPED config map (no live provider state) made the sight
    ;; probe throw an NPE out of svar's resolver, so a plain text turn — no images
    ;; anywhere near it — died on the way into the request.
    (it "is nil for a router-shaped config map, and never throws"
        (expect (nil? ((describer)
                        {:router {:providers [{:id :zai-coding-plan
                                               :models [{:name "glm-5-turbo"}]}]}}
                        "ctx"
                        :zai-coding-plan))))
    (it "is nil while the vision_fallback_describe toggle is off"
        (let [router (svar/make-router [{:id :seeing
                                         :api-key "k"
                                         :base-url "http://seeing.invalid"
                                         :api-style :openai
                                         :models [{:name "seer" :capabilities #{:chat :vision}}]}])]
          (toggles/set-value! "vision_fallback_describe" false)
          (try (expect (nil? ((describer) {:router router} "ctx" :seeing)))
               (finally (toggles/reset-to-default! "vision_fallback_describe")))))))

(defdescribe
  conversation-suffix-image-budget-test
  "An image is never REFERENCED by a later request, it is re-uploaded in full on
   every one of them — so an unbudgeted trailer eventually exceeds the provider's
   request limit and then EVERY turn fails, text-only ones included. Newest
   images ride; the rest are NAMED, with the id that brings them back."
  (let [target
        {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

        att
        (fn [id]
          {:id (str "att-" id)
           :tool-call-id (str "tc-" id)
           :media-type "image/png"
           :base64 replay-png-b64
           :filename (str "plot-" id ".png")
           :size 67})

        trailer
        (fn []
          (mapv (fn [id]
                  (stub-tool-iter {:id id :attachments [(att id)]}))
                [1 2 3]))

        image-msgs
        (fn [suffix]
          (filterv #(and (vector? (:content %))
                         (some (fn [b]
                                 (= "image_url" (:type b)))
                               (:content %)))
            suffix))

        notes
        (fn [suffix]
          (filterv #(re-find #"show_attachment" (str (:content %))) suffix))]

    (it "replays every image when the budget is not under pressure"
        (let [suffix (conversation-suffix (trailer) target)]
          (expect (= 3 (count (image-msgs suffix))))
          (expect (empty? (notes suffix)))))
    (it "keeps the NEWEST image and names the older ones once the COUNT budget is spent"
        (with-redefs-fn {#'lp/max-replay-images 1}
          (fn []
            (let [suffix
                  (conversation-suffix (trailer) target)

                  dropped
                  (str/join " " (map (comp str :content) (notes suffix)))]

              (expect (= 1 (count (image-msgs suffix))))
              (expect (= 2 (count (notes suffix))))
              ;; the freshest figure is the one the model is reasoning about
              (expect (= ["image_url"] (mapv :type (:content (last suffix)))))
              (expect (str/includes? dropped "att-1"))
              (expect (str/includes? dropped "att-2"))
              (expect (not (str/includes? dropped "att-3")))))))
    (it "never starves the model: the newest image rides even alone over the BYTE budget"
        (with-redefs-fn {#'lp/max-replay-image-bytes 1}
          (fn []
            (let [suffix (conversation-suffix (trailer) target)]
              (expect (= 1 (count (image-msgs suffix))))
              (expect (= 2 (count (notes suffix))))))))
    (it "spends nothing on a text-only target — no images, and no notes about them"
        (with-redefs-fn {#'lp/max-replay-images 1}
          (fn []
            (let [suffix (conversation-suffix (trailer)
                                              {:provider :zai-coding-plan :model "glm-5-turbo"})]
              (expect (empty? (image-msgs suffix)))
              (expect (empty? (notes suffix)))))))
    (it "keeps an audience \"user\" artifact out of the budget and off the wire entirely"
        (let [suffix (conversation-suffix
                       [(stub-tool-iter {:id 1 :attachments [(assoc (att 1) :audience "user")]})]
                       target)]
          ;; stored + displayed, but never an image block and never a budget note:
          ;; the caller already decided the model does not need these pixels.
          (expect (= 2 (count suffix)))
          (expect (empty? (image-msgs suffix)))
          (expect (empty? (notes suffix)))))))

(defdescribe
  finalize-answer-test
  "The ANSWER is the model's own prose and NOTHING else: `finalize-answer!` stores
   it verbatim as the turn's best answer — no artifact is ever spliced under it."
  (it "leaves the answer exactly as the model wrote it"
      (let [env
            {:turn-state-atom (atom {}) :ctx-atom (atom {})}

            value
            (with-redefs [ctx-loop/finalize-turn! (fn [_ _]
                                                    nil)]
              (#'lp/finalize-answer! env "Just prose."))]

        (expect (= "Just prose." (if (map? value) (:answer value) value)))
        (expect (= "Just prose." (:answer-markdown (:best-answer @(:turn-state-atom env))))))))

;; multi-fence-hint / attach-multi-fence-hint / empty-code-error-with-observation
;; tests removed: those fns were deleted with the fenced-era machinery (lenient
;; mode yields <=1 block, so multi-fence merge + fence-dropped diagnostics are
;; unreachable). See refactor "remove dead fenced-era code-block machinery".

(defdescribe
  token-cost-test
  (it "applies an explicit service-tier cost multiplier to every billed token class"
      (let [estimate
            (deref #'lp/estimate-token-cost)

            usage
            {:input-tokens 8298 :output-tokens 6}

            standard
            (estimate "gpt-5.6-sol" 8298 6 {:api-usage usage})

            priority
            (estimate "gpt-5.6-sol" 8298 6 {:api-usage usage :cost-multiplier 2.0})]

        (doseq [k ["input_cost" "output_cost" "total_cost"]]
          (expect (< (Math/abs (- (* 2.0 (double (get standard k))) (double (get priority k))))
                     1.0E-12)))))
  (it "prices Fast intent only when Codex serves the request"
      (let [multiplier
            (deref (ns-resolve 'com.blockether.vis.internal.loop 'codex-fast-cost-multiplier))

            fast
            {"codex_fast_mode" true}]

        (expect (= 2.0 (multiplier {} fast :openai-codex)))
        (expect (= 2.0 (multiplier {"service_tier" "PRIORITY"} {} "openai-codex")))
        (expect (= 1.0 (multiplier {:service_tier "priority"} fast :openai)))
        (expect (= 1.0 (multiplier {} {} :openai-codex))))))

;; Regression, reported session b30f87ac-f20e-4d7f-9fd2-416788d10527:
;; a channel chose Codex Priority before the final provider route was known.
(defdescribe
  codex-fast-request-projection-test
  (let [project-router
        (deref #'lp/codex-fast-router)

        sanitize
        (deref #'lp/provider-extra-body)

        fast
        {"codex_fast_mode" true}

        verbosity
        {:text {:verbosity "high"}}

        router
        {:providers [{:id :openai-codex :extra-body {:service_tier "auto" :provider-option true}}
                     {:id :anthropic-coding-plan
                      :extra-body {:service_tier "standard_only" :max_tokens 1024}}]}

        provider-body
        (fn [r provider-id]
          (:extra-body (some #(when (= provider-id (:id %)) %) (:providers r))))]

    (it "scopes Fast to the Codex router entry before Svar can fall back"
        (let [projected
              (project-router router verbosity fast)

              caller-body
              (sanitize verbosity)]

          (expect (= {:service_tier "priority" :provider-option true :text {:verbosity "high"}}
                     (merge (provider-body projected :openai-codex) caller-body)))
          (expect (= {:service_tier "standard_only" :max_tokens 1024 :text {:verbosity "high"}}
                     (merge (provider-body projected :anthropic-coding-plan) caller-body)))))
    (it "moves legacy caller Priority to Codex without deleting valid tiers"
        (let [legacy
              {"service_tier" "PRIORITY" :max_tokens 512}

              projected
              (project-router router legacy {})

              caller-body
              (sanitize legacy)]

          (expect (= {:service_tier "priority" :provider-option true :max_tokens 512}
                     (merge (provider-body projected :openai-codex) caller-body)))
          (expect (= {:service_tier "standard_only" :max_tokens 512}
                     (merge (provider-body projected :anthropic-coding-plan) caller-body)))
          (expect (= {:service_tier "auto" :max_tokens 1024}
                     (sanitize {:service_tier "auto" :max_tokens 1024})))))))

(defdescribe ask-code-block-observation-test
             (it "reports the block count (lenient mode: only the count is meaningful)"
                 (expect (= {:form-count 1}
                            (ask-code-block-observation {:blocks [{:source "(def x 1)"
                                                                   :lang "clojure"}]})))
                 (expect (= {:form-count 0} (ask-code-block-observation {:blocks []})))
                 (expect (= {:form-count 0} (ask-code-block-observation {})))))

(defdescribe
  auto-title-test
  (it "does NOT re-title when a real title already exists (generate once, never re-title)"
      (let [env (lp/create-environment {:providers []} {:db :memory :title "Old focus"})]
        (try
          ;; svar/ask! must NEVER fire — guard it so a regression to re-titling
          ;; throws instead of silently passing.
          (with-redefs [svar/ask! (fn [& _]
                                    (throw (ex-info "must not re-title" {})))]
            (expect (nil? (maybe-auto-title! env "some unrelated new request")))
            (expect (= "Old focus" @(:session-title-atom env))))
          (finally (lp/dispose-environment! env)))))
  (it "auto-title treats Untitled placeholders as missing previous titles"
      (let [seen
            (atom nil)

            router-stub
            {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}

            env
            (lp/create-environment router-stub {:db :memory :title "Untitled"})]

        (try (with-redefs [svar/ask! (fn [_router opts]
                                       (reset! seen opts)
                                       {:result {:title "Current Bug Triage"}})]
               ;; The LLM upgrade is DEFERRED past the foreground turn by
               ;; default (Blockether/vis#71), so it is this entry point that
               ;; carries it, not `maybe-auto-title!`.
               (let [f (titling/after-turn-auto-title! env "Wez to sprawdz")]
                 @f
                 (expect (= "Current Bug Triage" @(:session-title-atom env)))
                 (expect (str/includes? (-> @seen
                                            :messages
                                            second
                                            :content)
                                        "Previous title: <none>"))))
             (finally (lp/dispose-environment! env)))))
  (it
    "auto-title declares the preferred plan order, then deterministic fallback when the chain fails"
    (let [router-stub
          {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}
                       {:id :openai-codex :models [{:name "gpt-5.3-codex"}]}]}

          seen
          (atom nil)

          env
          (lp/create-environment router-stub {:db :memory :title "Untitled"})]

      (try
        ;; svar owns the per-provider walk now; the host makes ONE call that
        ;; declares `:prefer-providers`. A thrown call → deterministic fallback.
        (with-redefs [svar/ask! (fn [_router opts]
                                  (reset! seen opts)
                                  (throw (ex-info "Exceptional status code: 400" {})))]
          (let [f (titling/after-turn-auto-title!
                    env
                    "1dff1f5a-76dc-431e-ad2b-97af14c731f1 can you check why TUI title is missing?")]
            @f
            (expect (= [:zai-coding-plan :alibaba-coding-plan]
                       (take 2 (get-in @seen [:routing :prefer-providers]))))
            (expect (= "can you check why TUI title is missing?" @(:session-title-atom env)))))
        (finally (lp/dispose-environment! env)))))
  (it "set_session_title is NOT a tool — the title is host-generated"
      (let [env (lp/create-environment ::router {:db :memory})]
        (try
          ;; The model has no `set_session_title` binding; calling it raises
          ;; (NameError) and surfaces as a structured eval error.
          (let [bad (env/run-python-block (env/python-context env)
                                          "set_session_title(\"Liveness check\")")]
            (expect (some? (:error bad))))
          (finally (lp/dispose-environment! env))))))

(defdescribe
  provider-error-explanation-test
  (it
    "diagnoses auth failures; the re-authenticate step is a SEPARATE next-step block"
    (let
      [err
       {:message
        "API authentication failed. Check your API key. (Original: Exceptional status code: 401)"
        :data
        {:status 401
         :body
         "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"Invalid authentication credentials\"}}"}}

       text
       (provider-error-explanation err)

       step
       (perr/provider-error-next-step err)]

      ;; explanation = diagnosis only
      (expect (str/includes? text "rejected your credentials"))
      (expect (str/includes? text "Invalid authentication credentials"))
      ;; the actionable step lives in provider-error-next-step now, and stays
      ;; surface-agnostic — no CLI command, no TUI keybinding (Vis also ships native)
      (expect (str/includes? step "NEXT STEP: re-authenticate"))
      (expect (nil? (re-find #"(?i)ctrl\\+k|`vis " step))))))

(defdescribe
  ask-code-idle-timeout-test
  (it "leaves a provider with no policy on Vis' own 200s/300s/240s defaults"
      ;; 200s, not svar's two minutes: under Vis' pinned provider+model route
      ;; svar's router has no second candidate to cross to, and the first header
      ;; is the ONE wait Vis can retry for free — `pre-output-stream-retryable?`
      ;; does, so a slow queue gets three visible tries instead of one verdict.
      (expect (= 200000 rt/ASK_CODE_TTFT_TIMEOUT_MS))
      (expect (= 300000 rt/ASK_CODE_IDLE_TIMEOUT_MS))
      ;; A live transport without model progress is bounded independently.
      (expect (= 240000 rt/ASK_CODE_SEMANTIC_TIMEOUT_MS))
      (let [opts (:opts (captured-svar-ask-code-opts (helper-router :cloud nil)
                                                     #(lp/ask-code! {:lang "clojure"
                                                                     :messages []})))]
        (expect (= rt/ASK_CODE_TTFT_TIMEOUT_MS (:ttft-timeout-ms opts)))
        (expect (= rt/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts)))
        (expect (= rt/ASK_CODE_SEMANTIC_TIMEOUT_MS (:semantic-timeout-ms opts)))))
  (it "hands the routed provider's own policy the stream bounds instead"
      ;; Provider policy sits BETWEEN the call's explicit opts and those
      ;; defaults, so a local runtime's wide prefill window is what bounds this
      ;; stream — Vis' cloud-shaped numbers would call it dead mid-prefill.
      (let [opts (:opts (captured-ask-code-opts {:lang "clojure" :messages []}))]
        (expect (= (:first-byte-timeout-ms helper-provider-network) (:first-byte-timeout-ms opts)))
        (expect (= (:idle-timeout-ms helper-provider-network) (:idle-timeout-ms opts)))
        (expect (= (:semantic-timeout-ms helper-provider-network) (:semantic-timeout-ms opts)))
        ;; the policy names no TTFT, so that one still comes from Vis
        (expect (= rt/ASK_CODE_TTFT_TIMEOUT_MS (:ttft-timeout-ms opts)))))
  (it "preserves explicit ask-code TTFT and idle timeout overrides"
      (expect (= 77 (:ttft-timeout-ms (:opts (captured-ask-code-opts {:ttft-timeout-ms 77})))))
      (expect (contains? (:opts (captured-ask-code-opts {:ttft-timeout-ms nil})) :ttft-timeout-ms))
      (expect (nil? (:ttft-timeout-ms (:opts (captured-ask-code-opts {:ttft-timeout-ms nil})))))
      (expect (= 42 (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms 42})))))
      (expect (contains? (:opts (captured-ask-code-opts {:idle-timeout-ms nil})) :idle-timeout-ms))
      (expect (nil? (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms nil}))))))
  (it "accepts explicit semantic watchdog opt-in and opt-out"
      (let [opts (:opts (captured-ask-code-opts {:semantic-timeout-ms 180000}))]
        (expect (= 180000 (:semantic-timeout-ms opts)))
        ;; the override names ONE watchdog; the provider's idle bound stays put
        (expect (= (:idle-timeout-ms helper-provider-network) (:idle-timeout-ms opts))))
      (let [opts (:opts (captured-ask-code-opts {:semantic-timeout-ms nil}))]
        (expect (contains? opts :semantic-timeout-ms))
        (expect (nil? (:semantic-timeout-ms opts))))))

(defdescribe
  python-eval-test
  (it "gives a plain Python block five minutes before the backstop fires"
      ;; The watchdog is a BACKSTOP for a block that will never finish on its
      ;; own, never a co-deadline for work in progress. At two minutes ordinary
      ;; in-sandbox compute — a large parse, an image pass, an analytic loop —
      ;; was killed exactly where it got expensive, and it names no bounded call
      ;; for the widener below to see.
      (expect (= (* 5 60 1000) rt/DEFAULT_EVAL_TIMEOUT_MS))
      (expect (= rt/DEFAULT_EVAL_TIMEOUT_MS
                 (eval-timeout-ms-for-code rt/DEFAULT_EVAL_TIMEOUT_MS "print(1)")))
      ;; Every bounded-call floor stays ABOVE the plain default, so a block that
      ;; makes one still gets that call's own budget plus the widener's grace.
      (expect (< rt/DEFAULT_EVAL_TIMEOUT_MS (+ (* 1000 rt/RUN_TESTS_FLOOR_SECS) 10000)))
      (expect (< rt/DEFAULT_EVAL_TIMEOUT_MS (+ (* 1000 rt/HTTP_CALL_FLOOR_SECS) 10000)))
      (expect (< rt/DEFAULT_EVAL_TIMEOUT_MS rt/MAX_EVAL_TIMEOUT_MS)))
  (it "extends the outer eval timeout when shell code asks for a longer timeout"
      (expect (= 120000 (eval-timeout-ms-for-code 120000 "print(1)")))
      ;; Any `shell` call floors at the CAP, literal budget or not: the literal
      ;; bounds ONE call, and a second unannotated call in the same block owns
      ;; shell's default — which IS the cap.
      (expect (= (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000)
                 (eval-timeout-ms-for-code
                   120000
                   "await shell({\"command\": \"clojure -M:test\", \"timeout_secs\": 180})")))
      (expect (= 310000
                 (eval-timeout-ms-for-code 120000
                                           "subprocess.run([\"sleep\", \"1\"], timeout=300)"))))
  (it "keeps the eval ceiling above the longest shell budget plus its grace"
      ;; The widener floors the watchdog at the shell cap + grace; a ceiling at or
      ;; below that would clamp the watchdog back UNDER the shell envelope and kill
      ;; a legal wait with a bare `Timeout` and no output.
      (expect (< (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000) rt/MAX_EVAL_TIMEOUT_MS)))
  (it "reads a millisecond budget too, so repl_eval's own timeout is not preempted"
      ;; REGRESSION: the scan only understood seconds, so an explicitly long
      ;; `timeout_ms` (repl_eval, MCP) died at the 120s watchdog instead.
      (expect (= 310000
                 (eval-timeout-ms-for-code
                   120000
                   "await repl_eval(\"clojure\", code=\"(x)\", timeout_ms=300000)")))
      ;; Sub-second budgets round UP, never to a zero-second widening.
      (expect (= 120000
                 (eval-timeout-ms-for-code 120000 "await repl_eval(\"clojure\", timeout_ms=500)"))))
  (it "floors the watchdog above a bounded call whose timeout is NOT a literal"
      ;; REGRESSION: the watchdog EQUALLED shell's own 120s default, and a
      ;; timeout that is a variable / expression / plain default is invisible
      ;; to a text scan. The watchdog started first, so it always won and the
      ;; turn got a bare `Timeout (120s)` with no stdout instead of shell's
      ;; structured envelope.
      ;;
      ;; The floor is shell's CAP (`MAX_SHELL_TIMEOUT_SECS`, thirty minutes), not
      ;; a shorter budget: a `wait` whose budget the scan cannot read may legally
      ;; own the full cap, and the watchdog is a BACKSTOP, never a co-deadline.
      (expect (= (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000)
                 (eval-timeout-ms-for-code 120000 "r = await shell(command=\"sleep 300\")")))
      ;; Regression: a HANDLE wait (`sh.wait(600)`) parks the guest in a host call
      ;; for up to the same cap, but the scan only knew the verb — the block died
      ;; at the base watchdog, the interrupt could not reach a thread parked in
      ;; the host, and the worker was retired mid-turn.
      (expect (= (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000)
                 (eval-timeout-ms-for-code 120000 "sh = await shell(\"make\")\nr = sh.wait(600)")))
      (expect (= (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000)
                 (eval-timeout-ms-for-code 120000 "r = sh.wait(secs=600)")))
      (expect (= 610000 (eval-timeout-ms-for-code 120000 "r = await block(secs=600)")))
      (expect (= (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000)
                 (eval-timeout-ms-for-code
                   120000
                   "secs = 600\nr = await shell(op=\"wait\", id=\"j\", timeout_secs=secs)")))
      ;; A literal budget does not lower the floor either: it bounds ONE call, and
      ;; the next call in the block can still own shell's default.
      (expect (= (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000)
                 (eval-timeout-ms-for-code 120000
                                           "r = await shell(command=\"x\", timeout_secs=180)")))
      ;; A test run owns a multi-minute budget and answers timeouts itself.
      (expect (= (+ (* 1000 rt/RUN_TESTS_FLOOR_SECS) 10000)
                 (eval-timeout-ms-for-code
                   120000
                   "r = await run_tests({\"paths\": [\"test/a_test.clj\"]})")))
      ;; Prose that merely mentions the word must not widen anything.
      (expect (= 120000 (eval-timeout-ms-for-code 120000 "print('shell is bounded')"))))
  (it "keeps the eval wall above a test run's own ten-minute budget"
      ;; One run may legitimately take ten minutes — a cold full suite pays
      ;; JVM start, namespace loading and compilation before the first
      ;; assertion. The run answers its own timeout with a STRUCTURED result,
      ;; so the watchdog above it has to fire later than the run's budget or
      ;; that result is lost and the block dies on a bare `Timeout`.
      (expect (= (* 10 60 1000) rt/RUN_TESTS_TIMEOUT_MS))
      (expect (= rt/RUN_TESTS_FLOOR_SECS (quot rt/RUN_TESTS_TIMEOUT_MS 1000)))
      (expect (< rt/RUN_TESTS_TIMEOUT_MS
                 (eval-timeout-ms-for-code rt/DEFAULT_EVAL_TIMEOUT_MS
                                           "r = await run_tests({\"paths\": [\"test\"]})")))
      (expect (< (+ (* 1000 rt/RUN_TESTS_FLOOR_SECS) 10000) rt/MAX_EVAL_TIMEOUT_MS)))
  (it "floors the watchdog above a block that reaches the network"
      ;; REGRESSION: HTTP was not a bounded-call FAMILY at all. The shims'
      ;; own per-request default is 30s, a sweep loops over N hosts, and the
      ;; fetch helper usually lives in an EARLIER block — so an ordinary
      ;; crawl raced the 120s watchdog and lost, with no output to show for
      ;; it.
      (expect (= (+ (* 1000 rt/HTTP_CALL_FLOOR_SECS) 10000)
                 (eval-timeout-ms-for-code 120000 "r = httpx.post(url, json=payload)")))
      (expect (= 310000 (eval-timeout-ms-for-code 120000 "import requests\nfetch_all(hosts)")))
      (expect (= 310000 (eval-timeout-ms-for-code 120000 "urlopen(u).read()")))
      ;; A literal `timeout=` bounds ONE request, never the loop around it,
      ;; so it must NOT shrink the floor the way a shell budget does.
      (expect (= 310000
                 (eval-timeout-ms-for-code 120000
                                           "for u in urls:\n    requests.get(u, timeout=5)")))
      ;; …but a longer explicitly requested budget still wins outright.
      (expect (= 610000 (eval-timeout-ms-for-code 120000 "requests.get(u, timeout=600)")))
      ;; Prose that merely mentions a client must not widen anything.
      (expect (= 120000 (eval-timeout-ms-for-code 120000 "print('requests are bounded')")))))

(defdescribe final-answer-gate-test
             ;; `final-answer-gate-error` itself carries ONLY extension
             ;; :turn.answer/validate vetoes. The structural "done() shared its fence with
             ;; a MUTATION/FAILED op" structural gate is GONE with the fence reader
             ;; (a reply with a ```python fence = code, else the prose is the answer);
             ;; this fn now carries ONLY extension
             ;; :turn.answer/validate vetoes.
             (it "does not reject a done() that ran alongside a pure read (cat)"
                 (expect (nil? (lp/final-answer-gate-error
                                 {}
                                 1
                                 [{:id 0
                                   :code "cat(\"deps.edn\")"
                                   :channel [{:success? true :tag :observation :result [:ast {}]}]
                                   :error nil}]
                                 {:answer "done"}
                                 nil))))
             (it "allows answer-only iterations when no extension tool ran"
                 (expect (nil? (lp/final-answer-gate-error {}
                                                           1
                                                           [{:id 0 :code "1 + 2" :error nil}]
                                                           {:answer "done"}
                                                           nil)))))

;; def-sink -> vars-snapshot (per-var precise source extraction)

(defdescribe
  iteration-summarize-test
  "summarize/drop operate at ITERATION (tN/iN) granularity: a summarized step
   collapses entirely (its assistant+tool_result pair leaves the wire) to one
   gist line; a non-collapsed step renders as a tool_result tagged `# tN/iN`."
  (let [apply-summaries
        (var-get #'lp/apply-summaries)

        irm
        (var-get #'lp/iteration-results-message)]

    (it "summarize([tN/iN]) tags the iteration :collapsed? and swaps it for the gist"
        (let [tis
              [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big output"}]}]
               [2 {:forms-vec [{:scope "t1/i2/f1" :stdout "keep me"}]}]]

              out
              (apply-summaries tis [{"scopes" #{"t1/i1"} "gist" "did the thing" "at_turn" 1}])

              r1
              (second (first out))

              r2
              (second (second out))]

          (expect (true? (:collapsed? r1)))
          (expect (nil? (:collapsed? r2)))
          ;; collapsed → plain-text gist line (NOT a tool_result)
          (expect (= "# ⋯ folded t1/i1 · did the thing" (:content (irm r1))))))
    (it "a gist-less fold collapses to a `⋯ dropped <scopes> · <note>` line"
        (let [out (apply-summaries [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big"}]}]]
                                   [{"scopes" #{"t1/i1"} "note" " · saved ~1 token" "at_turn" 1}])]
          (expect (= "# ⋯ dropped t1/i1 · saved ~1 token" (:content (irm (second (first out))))))))
    (it "a live step renders as a tool_result tagged with its # tN/iN handle"
        (let [m (irm {:forms-vec [{:scope "t1/i1/f1" :stdout "hello"}] :tool-calls [{:id "c1"}]})]
          (expect (= "c1" (get-in m [:content 0 :tool_use_id])))
          (expect (str/includes? (get-in m [:content 0 :content]) "# t1/i1"))
          (expect (str/includes? (get-in m [:content 0 :content]) "hello"))))
    ;; Regression, reported from the app: a live view interrupted after its tool
    ;; block returned filed a record, but the next model request was never told.
    (it "puts semantic live-view records back into the model's tool result"
        (let [m
              (irm {:forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "c1" :stdout "watching"}]
                    :tool-calls [{:id "c1"}]
                    ;; Protocol 7 files no Activity attachment, so the only record
                    ;; this rail can meet is a semantic one.
                    :attachments [{:id "record-1"
                                   :tool-call-id "c1"
                                   :filename "native.live.ndjson"
                                   :media-type "application/vnd.vis.live+ndjson"}]})

              body
              (get-in m [:content 0 :content])]

          (expect (str/includes? body "native.live.ndjson"))
          (expect (str/includes? body "record-1"))
          (expect (str/includes? body "read_attachment"))
          (expect (str/includes? body "watching"))))
    (it "no summaries ⇒ trailer-iters unchanged"
        (let [tis [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "x"}]}]]]
          (expect (= tis (apply-summaries tis [])))))))

(defdescribe
  failed-block-keeps-its-output-test
  "A raise ends the block, not its output: whatever the program printed before
   the failure answers the call ALONGSIDE the error, so a block that batched
   several calls keeps the ones that already finished."
  ;; Regression: `form-output` short-circuited on `:error`, so a block whose LAST
  ;; call raised came back to the model as the traceback alone — every call that
  ;; had already printed was dropped on the wire (the store and the human card
  ;; kept it), and the whole round had to be run again.
  (let [irm
        (var-get #'lp/iteration-results-message)

        body
        (fn [form]
          (get-in (irm {:tool-calls [{:id "A" :name "python_execution"}]
                        :forms-vec [(merge {:scope "t1/i1/f1" :svar/tool-call-id "A"} form)]})
                  [:content 0 :content]))]

    (it "partial stdout rides along with the error — printed output first"
        (let [b (body {:stdout "LISTED_THE_TREE"
                       :error {:message "NameError: name 'no_such_call' is not defined"}})]
          (expect (str/includes? b "LISTED_THE_TREE"))
          (expect (str/includes? b "no_such_call"))
          (expect (< (str/index-of b "LISTED_THE_TREE") (str/index-of b "no_such_call")))))
    (it "an awaited host verb that failed keeps the output too — phase is irrelevant"
        (let [b (body {:stdout "GREPPED_47_HITS"
                       :error {:message "No lint-fn handler for language nonexistent-lang"
                               :data {:phase :python/host}}})]
          (expect (str/includes? b "GREPPED_47_HITS"))
          (expect (str/includes? b "No lint-fn handler"))
          (expect (< (str/index-of b "GREPPED_47_HITS") (str/index-of b "No lint-fn handler")))))
    (it "a failure that printed nothing answers with the error alone"
        (expect (str/includes? (body {:error {:message "ValueError: boom"}})
                               "✗ error: ValueError: boom")))))

(defdescribe
  tool-result-pairing-test
  "An iteration is a list of `python_execution` calls. Each tool_use gets its own
   tool_result containing exactly what that call printed, grouped by
   `:svar/tool-call-id`."
  (let [irm
        (var-get #'lp/iteration-results-message)

        pre
        (var-get #'lp/code-entries-preflight)]

    (it "answers each parallel tool_use with its own stdout"
        (let [m
              (irm {:tool-calls [{:id "A" :name "python_execution"}
                                 {:id "B" :name "python_execution"}
                                 {:id "P" :name "python_execution"}]
                    :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "A" :stdout "AAA"}
                                {:scope "t1/i1/f2" :svar/tool-call-id "B" :stdout "BBB"}
                                {:scope "t1/i1/f3" :svar/tool-call-id "P" :stdout "PPP"}]})

              by-id
              (into {} (map (juxt :tool_use_id :content)) (:content m))]

          (expect (= 3 (count (:content m))))
          (expect (str/includes? (by-id "A") "AAA"))
          (expect (not (str/includes? (by-id "A") "BBB")))
          (expect (str/includes? (by-id "B") "BBB"))
          (expect (str/includes? (by-id "P") "PPP"))))
    (it "advertises no recovery handle for printed output"
        (let [m
              (irm {:tool-calls [{:id "toolu_A" :name "python_execution"}
                                 {:id "P" :name "python_execution"}]
                    :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "toolu_A" :stdout "AAA"}
                                {:scope "t1/i1/f2" :svar/tool-call-id "P" :stdout "PPP"}]})

              all
              (str/join "\n" (map :content (:content m)))]

          (expect (str/includes? all "AAA"))
          (expect (str/includes? all "PPP"))
          (expect (not (str/includes? all "ntr")))
          (expect (not (str/includes? all "# saved:")))))
    (it "flags a failed call's tool_result and leaves successful output unflagged"
        (let [m
              (irm {:tool-calls [{:id "ok" :name "python_execution"}
                                 {:id "bad" :name "python_execution"}]
                    :forms-vec
                    [{:scope "t1/i1/f1" :svar/tool-call-id "ok" :stdout "FILE"}
                     {:scope "t1/i1/f2" :svar/tool-call-id "bad" :error "No such file"}]})

              by-id
              (into {} (map (juxt :tool_use_id identity)) (:content m))]

          ;; svar passes :is_error to Anthropic as `is_error: true`; on OpenAI/Gemini
          ;; the error text carries the signal.
          (expect (nil? (:is_error (by-id "ok"))))
          (expect (true? (:is_error (by-id "bad"))))
          (expect (str/includes? (:content (by-id "bad")) "No such file"))))
    (it "returns the no-output hint when Python printed nothing"
        (let [m (irm {:tool-calls [{:id "P" :name "python_execution"}]
                      :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "P"}]})]
          (expect (str/includes? (get-in m [:content 0 :content]) "no return"))))
    (it "folds an unpaired summary onto the first call"
        (let [m
              (irm {:tool-calls [{:id "A" :name "python_execution"}]
                    :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "A" :stdout "body"}
                                {:summary? true :summary-iters ["t1/i0"] :summary-gist "ctx"}]})

              c
              (get-in m [:content 0 :content])]

          (expect (str/includes? c "body"))
          (expect (str/includes? c "folded t1/i0 · ctx"))))
    (it "code-entries-preflight keeps distinct tool-calls SEPARATE (no merge)"
        (let [entries (:code-entries (pre 1
                                          [{:lang "python"
                                            :source "cat(\"a\")"
                                            :svar/tool-call-id "A"
                                            :vis/tool-name "cat"}
                                           {:lang "python"
                                            :source "rg({\"any\":[\"x\"]})"
                                            :svar/tool-call-id "B"
                                            :vis/tool-name "rg"}]))]
          (expect (= 2 (count entries)))
          (expect (= ["A" "B"] (mapv :svar/tool-call-id entries)))))
    (it "code-entries-preflight STILL merges legacy id-less blocks (provider stutter)"
        (let [entries (:code-entries (pre 1
                                          [{:lang "python" :source "x = 1"}
                                           {:lang "python" :source "y = 2"}]))]
          (expect (= 1 (count entries)))))))

(defdescribe
  tool-call-identity-regression-test
  ;; Issue #173: source equality is not call identity, a repeated call id is
  ;; reported rather than merged, and a missing execution is not evidence that
  ;; Python printed nothing.
  (let [call
        (fn [id]
          {:id id :name "python_execution" :input {"code" "print(123)"}})

        blocks
        (fn [calls]
          (mapv (fn [tc]
                  {:lang "python" :source (get-in tc [:input "code"]) :svar/tool-call-id (:id tc)})
                calls))]

    (it "executes distinct logical calls even when their programs are identical"
        (let [calls
              (mapv call ["call_one|fc_one" "call_two|fc_two"])

              entries
              (:code-entries (#'lp/code-entries-preflight 1 (blocks calls)))

              forms
              (mapv #(assoc %1 :stdout %2) entries ["FIRST" "SECOND"])

              result
              (#'lp/iteration-results-message {:tool-calls calls :forms-vec forms})]

          (expect (= 2 (count entries)))
          (expect (= ["FIRST" "SECOND"] (mapv :content (:content result))))))
    (it "passes a repeated call id through and reports it"
        (let [calls
              (mapv call ["call_one|fc_old" "call_one|fc_old"])

              {:keys [signals]}
              (tel/with-signals (#'lp/normalize-tool-calls calls))

              warning
              (first (filter #(= ::lp/duplicate-tool-call-ids (:id %)) signals))]

          (expect (= ["call_one|fc_old" "call_one|fc_old"]
                     (mapv :id (#'lp/normalize-tool-calls calls))))
          (expect (= :warn (:level warning)))
          (expect (= ["call_one|fc_old"] (get-in warning [:data :ids])))))
    (it "reports missing execution as an error rather than a successful empty print"
        (let [result
              (#'lp/iteration-results-message {:tool-calls [(call "missing")] :forms-vec []})

              block
              (first (:content result))]

          (expect (true? (:is_error block)))
          (expect (not (str/includes? (:content block) "printed nothing")))))))

(defdescribe
  csv-attachment-wire-test
  "A `attach`ed CSV is DATA for the HUMAN. The ````vis-table` fence reaches
   the TRANSCRIPT whole — the channel paints it as a live grid — while the model
   wire keeps only the `[Table: …]` headline and a pointer back to the stored
   attachment. Replayed rows are the most expensive thing a session can carry:
   they are re-uploaded on every later request for the rest of the session."
  (let [irm
        (var-get #'lp/iteration-results-message)

        display
        form/stdout-display

        fence
        (str "````vis-table\n" "[Table: fleet.csv 2 rows × 2 cols, 12 B] fleet counts\n"
             "fleet.csv\n" "text/csv\n"
             "2x2\n" "12 B\n"
             "machine,sessions\n" "studio,12\n"
             "rack-01,120\n" "````")

        stdout
        (str "before\n" fence "\nafter")

        wire
        (fn [out]
          (str (:content (first (:content (irm {:tool-calls [{:id "P" :name "python_execution"}]
                                                :forms-vec [{:scope "t1/i1/f1"
                                                             :svar/tool-call-id "P"
                                                             :stdout out}]}))))))]

    (it "keeps the headline on the wire but not one data row"
        (let [text (wire stdout)]
          (expect (str/includes? text "[Table: fleet.csv 2 rows × 2 cols, 12 B] fleet counts"))
          (expect (str/includes? text "read_attachment"))
          (expect (not (str/includes? text "rack-01")))
          (expect (not (str/includes? text "machine,sessions")))
          (expect (not (str/includes? text "````vis-table")))
          ;; Text around the fence is untouched — only the payload goes.
          (expect (str/includes? text "before"))
          (expect (str/includes? text "after"))))
    (it "leaves a vis-image fence alone: it carries a host path, not a payload"
        (let [text (wire (str "````vis-image\n[Image: shot.png 2×2, 9 B]\n"
                              "/tmp/shot.png\nimage/png\n2x2\n9 B\n````"))]
          (expect (str/includes? text "````vis-image"))
          (expect (str/includes? text "/tmp/shot.png"))))
    (it "still hands the WHOLE grid to the transcript"
        (let [body (str (:body (display {:stdout stdout})))]
          (expect (str/includes? body "````vis-table"))
          (expect (str/includes? body "rack-01,120"))))))

(defdescribe
  repeated-actions-continue-test
  "Repeated actions are valid work. The loop continues until the model returns an answer."
  (it "does not checkpoint or force-finalize identical actions"
      (let [router-stub
            (svar/make-router [{:id :zai-coding-plan
                                :api-key "test"
                                :base-url "https://example.invalid"
                                :models [{:name "glm-5-turbo"}]}])

            env
            (lp/create-environment router-stub {:db :memory})

            calls
            (atom 0)]

        (try (with-redefs [svar/ask-code!
                           (fn [_ _]
                             (if (<= (swap! calls inc) 4)
                               {:stop-reason :tool-calls
                                :tool-calls [{:id (str "repeat-" @calls)
                                              :name "python_execution"
                                              :input {:code "print(1)"}}]
                                :tokens {}}
                               {:stop-reason :end :tool-calls [] :content "finished" :tokens {}}))]
               (let [result (lp/turn! env [(svar/user "repeat if needed")] {})]
                 (expect (= 5 @calls))
                 (expect (= "finished" (lp/answer-markdown (:answer result))))))
             (finally (lp/dispose-environment! env))))))

(defdescribe
  honor-config-roots-test
  (describe
    "honor-config-roots! — explicit primary/fallback pairs are the router's roots"
    (let [f (var-get #'lp/honor-config-roots!)]
      (it "honors string-valued defaults without treating provider/model order as configuration"
          (let [router {:providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}
                                    {:id :anthropic-coding-plan
                                     :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]}
                config {:default-provider "anthropic-coding-plan"
                        :default-model "claude-fable-5"
                        :providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}
                                    {:id :anthropic-coding-plan
                                     :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]}
                routed (f router config)
                p (first (:providers routed))]

            (expect (= [:anthropic-coding-plan :zai-coding-plan] (mapv :id (:providers routed))))
            (expect (= ["claude-fable-5" "claude-opus-4-8"] (mapv :name (:models p))))
            (expect (= "claude-fable-5" (:root p)))
            (expect (= "claude-fable-5" (:name (lp/resolve-effective-model routed))))))
      (it "keeps legacy first-provider/first-model behavior when explicit defaults are absent"
          (let [router {:providers [{:id :anthropic-coding-plan
                                     :models [{:name "claude-opus-4-8"}
                                              {:name "claude-sonnet-4-6"}]}]}
                config {:providers [{:id :anthropic-coding-plan :models ["claude-sonnet-4-6"]}]}
                routed (f router config)]

            (expect (= "claude-sonnet-4-6" (:name (lp/resolve-effective-model routed))))))
      (it "leaves the router intact when the explicit pair is not in its catalog"
          (let [router {:providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}]}
                config {:default-provider "anthropic-coding-plan"
                        :default-model "claude-fable-5"
                        :providers [{:id :anthropic-coding-plan
                                     :models [{:name "claude-fable-5"}]}]}]

            (expect (= router (f router config)))))
      (it "a resolvable provider still wins when the model name does not match its catalog"
          (let [router {:providers
                        [{:id :zai-coding-plan :models [{:name "glm-5.2"} {:name "glm-4.7"}]}
                         {:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]}
                config {:default-provider "anthropic-coding-plan"
                        :default-model "typo-model"
                        :providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}
                                    {:id :anthropic-coding-plan
                                     :models [{:name "claude-fable-5"}]}]}
                routed (f router config)]

            (expect (= :anthropic-coding-plan (:id (first (:providers routed)))))
            (expect (= "claude-fable-5" (:root (first (:providers routed)))))))
      (it "a slash INSIDE a model id is not a provider tag"
          ;; openrouter serves ids like `z-ai/glm-4.6v`. Splitting them on the
          ;; slash asked for provider `:z-ai`, which no fleet has, so the
          ;; default the user picked never became the router's root.
          (let [fleet [{:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}
                       {:id :openrouter :models [{:name "glm-5.2"} {:name "z-ai/glm-4.6v"}]}]
                router {:providers fleet}
                config
                {:default-provider "openrouter" :default-model "z-ai/glm-4.6v" :providers fleet}
                routed (f router config)
                p (first (:providers routed))]

            (expect (= :openrouter (:id p)))
            (expect (= "z-ai/glm-4.6v" (:root p)))
            (expect (= "z-ai/glm-4.6v" (:name (lp/resolve-effective-model routed))))))
      (it "default_model accepts the provider/model form and its provider wins"
          (let [router {:providers
                        [{:id :zai-coding-plan :models [{:name "glm-5.2"} {:name "glm-4.7"}]}
                         {:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]}
                config {:default-provider "anthropic-coding-plan"
                        :default-model "zai-coding-plan/glm-4.7"
                        :providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}
                                    {:id :anthropic-coding-plan
                                     :models [{:name "claude-fable-5"}]}]}
                routed (f router config)
                p (first (:providers routed))]

            (expect (= :zai-coding-plan (:id p)))
            (expect (= "glm-4.7" (:root p)))
            (expect (= ["glm-4.7" "glm-5.2"] (mapv :name (:models p))))
            (expect (= "glm-4.7" (:name (lp/resolve-effective-model routed)))))))))

(defdescribe
  router-for-model-test
  (describe
    "router-for-model — a coordinator PROPOSES a child model"
    (let [router {:providers [{:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"}]}
                              {:id :anthropic
                               :models [{:name "claude-haiku-4-5"} {:name "claude-sonnet-4-6"}]}]}]
      (it "the proposed model becomes the child's EFFECTIVE model"
          (expect (= "claude-haiku-4-5"
                     (:name (lp/resolve-effective-model (lp/router-for-model router
                                                                             "claude-haiku-4-5")))))
          (expect (= "claude-sonnet-4-6"
                     (:name (lp/resolve-effective-model
                              (lp/router-for-model router "claude-sonnet-4-6"))))))
      (it "an ORDERED preference list reorders provider/model order (svar falls back)"
          (let [r (lp/router-for-model router ["claude-sonnet-4-6" "claude-haiku-4-5"])]
            ;; most-preferred is effective; the full order reflects the preference
            ;; then the rest as fallback — svar routes this order, no svar change.
            (expect (= "claude-sonnet-4-6" (:name (lp/resolve-effective-model r))))
            (expect (= ["claude-sonnet-4-6" "claude-haiku-4-5" "claude-opus-4-8"]
                       (vec (for [p (:providers r)
                                  m (:models p)]

                              (:name m)))))))
      (it "omitted (nil/blank) → child inherits the parent's default model"
          (expect (= "claude-opus-4-8"
                     (:name (lp/resolve-effective-model (lp/router-for-model router nil)))))
          (expect (= "claude-opus-4-8"
                     (:name (lp/resolve-effective-model (lp/router-for-model router "  "))))))
      (it "unknown model → falls back to the parent's default (no crash)"
          (expect (= "claude-opus-4-8"
                     (:name (lp/resolve-effective-model (lp/router-for-model router "gpt-9"))))))
      (it "preserves the full provider set (just reordered) so keys/opts survive"
          (expect (= #{:anthropic-coding-plan :anthropic}
                     (set (map :id
                               (:providers (lp/router-for-model router "claude-haiku-4-5"))))))))))

(defdescribe
  router-order-binds-svar-selection-test
  "Reordering `:providers` is DECORATION unless `:priority` and `:root` move with
   it: svar sorts candidates by `:priority`, and `:strategy :root` reads the
   provider's `:root` NAME rather than the `:models` head. A `models` preference
   carries no forced `:routing`, so the turn
   kept running the default provider's root model while the turn card and the
   cost row named the cheap model that never ran."
  (let [router
        (svar/make-router [{:id :prov-a
                            :api-key "k"
                            :base-url "https://a.example.com"
                            :models [{:name "a-big"} {:name "a-small"}]}
                           {:id :prov-b
                            :api-key "k"
                            :base-url "https://b.example.com"
                            :models [{:name "b-cheap"}]}])

        ;; What svar ACTUALLY calls, not what Vis displays.
        selected
        (fn [r prefs]
          (let [[p m] (svar-router/select-provider r prefs)]
            [(:id p) (:name m)]))

        seats
        (fn [r]
          (mapv (juxt :id :priority :root) (:providers r)))]

    (it "svar itself picks the coordinator's model across providers"
        (expect (= [:prov-a "a-big"] (selected router {:strategy :root})))
        (expect (= [:prov-b "b-cheap"]
                   (selected (lp/router-for-model router "b-cheap") {:strategy :root}))))
    (it "…and WITHIN one provider, where only `:root` decides"
        (expect (= [:prov-a "a-small"]
                   (selected (lp/router-for-model router "a-small") {:strategy :root}))))
    (it "the preferred provider is renumbered to priority 0 and roots the pick"
        (expect (= [[:prov-b 0 "b-cheap"] [:prov-a 1 "a-big"]]
                   (seats (lp/router-for-model router "b-cheap")))))
    (it "every provider named in the list gets its own preferred root"
        (expect (= [[:prov-b 0 "b-cheap"] [:prov-a 1 "a-small"]]
                   (seats (lp/router-for-model router ["b-cheap" "a-small"])))))
    (it "an unknown model changes nothing — no accidental renumbering or reroot"
        (expect (= router (lp/router-for-model router "gpt-nope")))
        (expect (= [:prov-a "a-big"]
                   (selected (lp/router-for-model router "gpt-nope") {:strategy :root}))))
    (it "a pinned PROVIDER leads svar's priority sort too"
        ;; svar drops `:force-provider` on an auth fallback and re-sorts by
        ;; priority alone, so the pin must own the number, not just the slot.
        (let [pinned (@#'lp/router-for-pinned-provider router :prov-b)]
          (expect (= [[:prov-b 0 "b-cheap"] [:prov-a 1 "a-big"]] (seats pinned)))
          (expect (= [:prov-b "b-cheap"] (selected pinned {:strategy :root})))))))

(defdescribe
  router-for-pinned-provider-test
  (describe
    "a session pin must ATTRIBUTE to the provider it calls (duplicate model names)"
    (let [;; both providers expose "gpt-5.4" — the tie `router-for-model` cannot break
          router
          {:providers [{:id :openai-codex :models [{:name "gpt-5.4"} {:name "gpt-5.5"}]}
                       {:id :github-copilot-individual
                        :models [{:name "gpt-5.4"} {:name "claude-opus-5"}]}]}

          hoist
          #'lp/router-for-pinned-provider

          forced
          #'lp/forced-routing-for-pref]

      (it "model-only hoisting picks the CONFIG-order provider — the old, wrong attribution"
          (expect (= :openai-codex
                     (:provider (lp/resolve-effective-model (lp/router-for-model router
                                                                                 "gpt-5.4"))))))
      (it "hoisting the PINNED provider makes root provider+model match the forced routing"
          (let [pinned
                (hoist (lp/router-for-model router "gpt-5.4") :github-copilot-individual)

                root
                (lp/resolve-effective-model pinned)]

            (expect (= :github-copilot-individual (:provider root)))
            (expect (= "gpt-5.4" (:name root)))
            (expect (= {:provider :github-copilot-individual :model "gpt-5.4"}
                       (forced router "github-copilot-individual" "gpt-5.4")))))
      (it "the pinned provider leads the FALLBACK order and no provider is dropped"
          (let [pinned (hoist router :github-copilot-individual)]
            (expect (= [:github-copilot-individual :openai-codex] (mapv :id (:providers pinned))))))
      (it "a string id works (that is how the DB pref stores it)"
          (expect (= :github-copilot-individual
                     (:id (first (:providers (hoist router "github-copilot-individual")))))))
      (it "an unknown / nil provider leaves the router untouched"
          (expect (= router (hoist router :not-configured)))
          (expect (= router (hoist router nil)))))))

;; Regression: a session pinned to openai-codex/gpt-5.6-sol RAN on
;; github-copilot-individual. The pinned provider was missing from the router (its
;; credential failed to build), so the pin degraded to model-only routing and the
;; other vendor advertising the same model NAME took the conversation — then 400d.
(defdescribe
  pinned-provider-missing-from-router-test
  (describe
    "a pin whose PROVIDER the router cannot serve is never re-homed onto another vendor"
    (let [;; openai-codex dropped at router build (expired credential); Copilot
          ;; advertises the very same `gpt-5.6-*` names.
          router
          {:providers [{:id :anthropic-coding-plan :models [{:name "claude-opus-5"}]}
                       {:id :github-copilot-individual
                        :models [{:name "gpt-5.6-sol"} {:name "claude-opus-5"}]}]}

          forced
          #'lp/forced-routing-for-pref

          prepare
          #'lp/prepare-turn-context

          env
          {:db-info ::db :session-id "session-1" :router router}

          messages
          [{:role "user" :content "hello"}]]

      (it "forces NOTHING for the absent provider — Copilot never inherits the pick"
          (expect (= {} (forced router "openai-codex" "gpt-5.6-sol"))))
      (it "a pin that names NO provider still routes by model alone"
          (expect (= {:model "gpt-5.6-sol"} (forced router nil "gpt-5.6-sol"))))
      (it "the turn fails naming the pinned provider instead of calling another one"
          (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                      {:provider "openai-codex"
                                                       :model "gpt-5.6-sol"})}
            #(let [thrown (try (prepare env messages {}) nil (catch Exception e e)) data
                   (ex-data thrown)] (expect (some? thrown)) (expect
                                                               (= :vis/pinned-provider-unavailable
                                                                  (:type data)))
               (expect (true? (:vis/user-error data))) (expect (str/includes? (ex-message thrown)
                                                                              "openai-codex"))
               (expect (str/includes? (ex-message thrown) "gpt-5.6-sol"))))))))

(defdescribe
  prepare-turn-model-preference-test
  (let [prepare
        #'lp/prepare-turn-context

        router
        {:providers [{:id :openai-codex :models [{:name "shared"} {:name "gpt-explicit"}]}
                     {:id :lmstudio :models [{:name "shared"} {:name "ornith"}]}]}

        env
        {:db-info ::db :session-id "session-1" :router router}

        messages
        [{:role "user" :content "hello"}]]

    (it "uses a persisted provider and model as one indivisible pin"
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    {:provider "lmstudio" :model "ornith"})}
          #(let [ctx (prepare env messages {})] (expect (= :lmstudio (:root-provider ctx)))
             (expect (= "ornith" (:root-model ctx))) (expect (= {:provider :lmstudio
                                                                 :model "ornith"
                                                                 :on-transient-error :hybrid}
                                                                (:routing ctx))))))
    (it "trims the persisted pin so the display root names the model that RAN"
        ;; The routing helpers trim; the display/cost root did not. A pref with
        ;; stray whitespace (a hand-edited DB row, a client that pads the field)
        ;; therefore BOUND "ornith" while the turn card named LM Studio's FIRST
        ;; model — a model that turn never ran.
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    {:provider "  lmstudio  " :model "  ornith  "})}
          #(let [ctx (prepare env messages {})] (expect (= :lmstudio (:root-provider ctx)))
             (expect (= "ornith" (:root-model ctx))) (expect (= {:provider :lmstudio
                                                                 :model "ornith"
                                                                 :on-transient-error :hybrid}
                                                                (:routing ctx))))))
    (it "honors a live-catalog model the pinned provider does not list statically"
        ;; What the pickers actually offer: `/v1/providers/:id/models` (the TUI's
        ;; "show all models", the companion's router dialog) lists the provider's
        ;; LIVE catalog, which is wider than vis.yml. Such a pick used to fall
        ;; through to the default model because the pinned provider's static
        ;; `:models` did not contain it.
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    {:provider "lmstudio" :model "qwen3-next-80b"})}
          #(let [ctx (prepare env messages {})] (expect (= :lmstudio (:root-provider ctx)))
             (expect (= "qwen3-next-80b" (:root-model ctx))) (expect (= {:provider :lmstudio
                                                                         :model "qwen3-next-80b"
                                                                         :on-transient-error
                                                                         :hybrid}
                                                                        (:routing ctx))))))
    (it "does not combine a caller model with the persisted provider"
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    {:provider "lmstudio" :model "shared"})}
          #(let [ctx (prepare env messages {:model "shared"})]
             ;; Both providers offer this model. An explicit model retains the
             ;; router's normal provider choice instead of borrowing LM Studio
             ;; from an unrelated persisted pair.
             (expect (= :openai-codex (:root-provider ctx))) (expect (= "shared" (:root-model ctx)))
             (expect (= {:model "shared" :on-transient-error :hybrid} (:routing ctx))))))
    ;; Regression, issue #154: a queued route snapshot lost its provider before engine dispatch.
    (it "uses an explicit provider + model snapshot without consulting the current session pin"
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    (throw (ex-info "must not read changed pin"
                                                                    {})))}
          #(let [ctx (prepare env messages {:provider "lmstudio" :model "shared"})]
             (expect (= :lmstudio (:root-provider ctx))) (expect (= "shared" (:root-model ctx)))
             (expect (= {:provider :lmstudio :model "shared" :on-transient-error :hybrid}
                        (:routing ctx))))))))

;; Regression, issue #154 follow-up, session ff83edc9-247d-4f04-9fab-3dd7072d5a34:
;; a manually selected fallback exhausted its quota and its hard pin kept the configured primary out.
(defdescribe
  selected-provider-fallback-order-test
  (it
    "tries the session pick first, then wraps to the configured primary"
    (let [router
          (svar/make-router [{:id :openai-codex
                              :api-key "test"
                              :base-url "https://openai.example.com"
                              :models [{:name "gpt-5.6-sol"}]}
                             {:id :anthropic-coding-plan
                              :api-key "test"
                              :base-url "https://anthropic.example.com"
                              :models [{:name "claude-fable-5-1"}]}]
                            {:rate-limit {:same-provider-delays-ms []
                                          :fallback-after-ms 0
                                          :fallback-provider? true}})

          env
          {:db-info ::db :session-id "session-1" :router router}

          messages
          [{:role "user" :content "continue"}]

          calls
          (atom [])]

      (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                  {:provider "anthropic-coding-plan"
                                                   :model "claude-fable-5-1"})}
        #(let [ctx (#'lp/prepare-turn-context env messages {}) _
               (expect (= :hybrid (get-in ctx [:routing :on-transient-error]))) outcome
               (try (svar-router/with-provider-fallback
                      (:router ctx)
                      (:routing ctx)
                      (fn [provider model]
                        (swap! calls conj [(:id provider) (:name model)])
                        (if (= :anthropic-coding-plan (:id provider))
                          (throw (ex-info
                                   "Exceptional status code: 400"
                                   {:type :svar.core/http-error
                                    :status 400
                                    :body
                                    (str
                                      "{\"type\":\"error\",\"error\":{\"type\":"
                                      "\"invalid_request_error\",\"message\":"
                                      "\"Third-party apps now draw from your extra usage.\"}}")}))
                          {:api-usage {:total-tokens 1}})))
                    (catch Exception e e))] (expect (= [[:anthropic-coding-plan "claude-fable-5-1"]
                                                        [:openai-codex "gpt-5.6-sol"]]
                                                       @calls)) (expect (map? outcome)))))))

;; Regression, reported session b34c2a0f-3eff-4ab6-8b9c-bb0c8c7fbda5: the gateway freezes a turn
;; id in `submit-turn!` and names it in every `turn.*` event, cancel and forced terminal, and it
;; hands that id to the engine as `:session-turn-id`. The engine's own turn context dropped the
;; key, so the durable row was minted under a SECOND id: one submitted turn existed TWICE (the
;; gateway record the channels hold plus its persisted twin), and the trace behind the id the
;; channels hold answered nothing.
(defdescribe
  caller-frozen-turn-id-test
  (describe
    "a turn id the caller froze survives every engine hop down to the durable write"
    (let [prepare
          #'lp/prepare-turn-context

          run-phase
          #'lp/run-iteration-phase

          router
          {:providers [{:id :openai-codex :models [{:name "shared"}]}]}

          env
          {:db-info ::db :session-id "session-1" :router router}

          messages
          [{:role "user" :content "hello"}]

          captured-opts
          (fn [ctx]
            (let [seen (atom ::never-called)]
              (with-redefs-fn {#'lp/run-turn! (fn [_env _request opts]
                                                (reset! seen opts)
                                                {})}
                #(run-phase ctx))
              @seen))]

      (it "the turn context keeps the id the caller passed in opts"
          (let [ctx (prepare env messages {:model "shared" :session-turn-id "frozen-turn-id"})]
            (expect (= "frozen-turn-id" (:session-turn-id ctx)))))
      (it "the iteration phase hands that id to run-turn!, which stores the durable row"
          (expect (= "frozen-turn-id"
                     (:session-turn-id (captured-opts {:environment {}
                                                       :user-request "hello"
                                                       :session-turn-id "frozen-turn-id"})))))
      (it "a direct engine caller that froze no id leaves the key absent, so one is minted"
          (let [ctx (prepare env messages {:model "shared"})]
            (expect (nil? (:session-turn-id ctx)))
            (expect (not (contains? (captured-opts (assoc ctx :environment {}))
                                    :session-turn-id))))))))

(defdescribe
  context-overflow-terminal-breaker-test
  "Typed context overflow must never be fed back into an unreachable next model call."
  (let [overflow-ex
        (fn overflow-ex ([input max-input source] (overflow-ex :svar.tokens/context-overflow input
                                                    max-input source)) ([type input max-input
                                                                         source]
                                                                        (ex-info "Context overflow"
                                                                          {:type type
                                                                           :source source
                                                                           :model "claude-fable-5"
                                                                           :input-tokens input
                                                                           :max-input-tokens
                                                                           max-input
                                                                           :overflow
                                                                           (when (and input
                                                                                   max-input)
                                                                             (- input
                                                                               max-input))})))

        ctx
        {:iteration 1 :messages [] :routing {} :reasoning-level nil}]

    (doseq [[label type input max-input source]
            [["extreme preflight" :svar.tokens/context-overflow 81325 8192 :preflight]
             ["exact limit edge" :svar.tokens/context-overflow 8193 8192 :preflight]
             ["marginal preflight" :svar.tokens/context-overflow 9000 8192 :preflight]
             ["provider-confirmed" :svar.tokens/context-overflow 200001 200000 :provider]
             ["unmeasured provider overflow" :svar.tokens/context-overflow nil nil :provider]
             ;; The type svar's `ask-code!` PREFLIGHT guard actually throws. VIS matched
             ;; only the `tokens` variant, so this one skipped every overflow handler:
             ;; a 1.44M-of-1M-token session fed the overflow back for three iterations
             ;; (each retry ~180 tokens BIGGER) and died with a bogus provider card.
             ["ask-code! preflight guard" :svar.core/context-overflow 1437952 1000000 :preflight]
             ["ask-code! guard, marginal" :svar.core/context-overflow 9000 8192 :preflight]]]
      (it (str label " is terminal")
          (let [result (lp/handle-iteration-exception! (overflow-ex type input max-input source)
                                                       ctx)]
            (expect (contains? result :com.blockether.vis.internal.loop/iteration-error))
            (expect (true? (:com.blockether.vis.internal.loop/fatal-iteration-error result))))))
    (it "preserves typed details for the error card and diagnostics"
        (let [result
              (lp/handle-iteration-exception! (overflow-ex 210000 200000 :provider) ctx)

              data
              (get-in result [:com.blockether.vis.internal.loop/iteration-error :data])]

          (expect (= :svar.tokens/context-overflow (:type data)))
          (expect (= 210000 (:input-tokens data)))
          (expect (= 200000 (:max-input-tokens data)))
          (expect (= :provider (:source data)))))
    (it "does not make unrelated model errors terminal"
        (let [result (lp/handle-iteration-exception! (ex-info "NameError: nope"
                                                              {:type :vis/eval-error})
                                                     ctx)]
          (expect (not (:com.blockether.vis.internal.loop/fatal-iteration-error result)))))))

;; Regression: a pinned provider accepted the POST and sent no response header for
;; the whole TTFT budget. svar declined the retry (:no-retry-path), its router had
;; no second candidate under Vis' sticky provider+model pin, and the turn died with
;; ten iterations of finished work — the human had to type "Continue".
(defdescribe
  pre-output-stream-retry-test
  "A stream watchdog that fires before ANY output is the one provider failure Vis
   re-issues itself: no header, no byte, no token, nothing billed, nothing painted."
  (let [retryable?
        @#'lp/pre-output-stream-retryable?

        backoff
        @#'lp/pre-output-stream-backoff-ms

        next-counters
        @#'lp/next-retry-counters

        ttft
        (ex-info "Stream TTFT timeout (60000 ms)" {:type :svar.core/stream-ttft-timeout})]

    (it "re-issues every typed watchdog abort while no output has streamed"
        (doseq [error-type [:svar.core/stream-ttft-timeout :svar.core/stream-idle-timeout
                            :svar.core/stream-semantic-timeout]]
          (expect (true? (retryable? (ex-info "watchdog" {:type error-type})
                                     {:attempt 0 :output-started? false})))))
    (it "sees the typed abort through the HTTP client's wrapper exception"
        (expect (true? (retryable? (ex-info "HTTP client request failed" {} ttft)
                                   {:attempt 0 :output-started? false}))))
    (it "never resends once output has been painted"
        (expect (false? (retryable? ttft {:attempt 0 :output-started? true}))))
    (it "stops at the attempt budget instead of hiding a wedged endpoint"
        (expect (true? (retryable? ttft {:attempt 1 :output-started? false})))
        (expect (false? (retryable? ttft {:attempt 2 :output-started? false}))))
    (it "leaves a cancellation and every unrelated failure terminal"
        (expect (false? (retryable? (ex-info "cancelled" {:type :svar.core/stream-cancelled})
                                    {:attempt 0 :output-started? false})))
        (expect (false? (retryable? (ex-info "unauthorized" {:status 401})
                                    {:attempt 0 :output-started? false}))))
    (it "backs off briefly and spends exactly one attempt per re-issue"
        (expect (= 1000 (backoff 0)))
        (expect (= 3000 (backoff 1)))
        (expect (= 3000 (backoff 7)))
        (expect (= [1 1]
                   (next-counters :com.blockether.vis.internal.loop/retry-pre-output-stream
                                  {:attempt 0 :max-tokens-attempt 1}))))
    (it "still fails the turn once the pre-output budget is spent"
        (expect (true? (:com.blockether.vis.internal.loop/fatal-iteration-error
                         (lp/handle-iteration-exception! ttft
                                                         {:iteration 3
                                                          :messages [{:role "user"
                                                                      :content "hi"}]})))))))

(defdescribe
  stream-watchdog-terminal-error-test
  "Stream watchdog failures that reach here have spent BOTH svar's bounded
   retry/fallback policy and Vis' own pre-output re-issue budget
   (`pre-output-stream-retry-test`). They must end the turn instead of becoming
   visible model-feedback iterations."
  (let [ctx {:iteration 5 :messages [] :routing {} :reasoning-level nil}]
    (doseq [error-type [:svar.core/stream-cancelled :svar.core/stream-idle-timeout
                        :svar.core/stream-semantic-timeout]]
      (it (str error-type " is fatal and cannot create a duplicate next iteration")
          (let [result (lp/handle-iteration-exception! (ex-info "Terminal stream watchdog failure"
                                                                {:type error-type})
                                                       ctx)]
            (expect (contains? result :com.blockether.vis.internal.loop/iteration-error))
            (expect (true? (:com.blockether.vis.internal.loop/fatal-iteration-error result))))))))

(defdescribe
  provider-first-output-deadline-test
  (doseq [chunk [{:content "answer"} {:reasoning "thinking"} {:tool-input "partial"}
                 {:tool-call-preview {:name "python_execution"}}]]
    (it (str "does not interrupt an attempt that has produced " (keys chunk))
        (with-redefs-fn {#'lp/ask-code-with-session! (fn [_ _ opts]
                                                       ((:on-chunk opts) chunk)
                                                       (Thread/sleep 5)
                                                       (expect (false? ((:cancel-fn opts))))
                                                       :complete)}
          #(expect (= :complete (#'lp/ask-code-with-first-output-timeout! {} {} {} 1))))))
  (doseq [stop? [false true]]
    (it (str "drops late output and preserves an overriding Stop=" stop?)
        (let [cancelled (atom false)
              chunks (atom [])
              error (with-redefs-fn {#'lp/ask-code-with-session!
                                     (fn [_ _ opts]
                                       (Thread/sleep 5)
                                       (expect (true? ((:cancel-fn opts))))
                                       ((:on-chunk opts) {:content "late output"})
                                       (reset! cancelled stop?)
                                       (throw (ex-info "cancelled"
                                                       {:type :svar.core/stream-cancelled})))}
                      #(try (#'lp/ask-code-with-first-output-timeout!
                             {}
                             {}
                             {:on-chunk (fn [chunk]
                                          (swap! chunks conj chunk))
                              :cancel-fn (fn []
                                           @cancelled)}
                             1)
                            (catch Exception e e)))]

          (expect (= [] @chunks))
          (expect (= (if stop? :svar.core/stream-cancelled :svar.core/stream-semantic-timeout)
                     (:type (ex-data error))))))))

(defdescribe provider-first-output-svar-boundary-test
             (it "uses Svar's real pre-header cancellation without cancelling the turn"
                 (let [environment
                       (lp/create-environment (helper-router :lmstudio nil) {:db :memory})

                       requests
                       (atom 0)]

                   (try (with-redefs [rt/ASK_CODE_FIRST_OUTPUT_TIMEOUT_MS
                                      25

                                      http/post
                                      (fn [& _]
                                        (swap! requests inc)
                                        (Thread/sleep 2000)
                                        (throw (ex-info "Request was not interrupted" {})))]

                          (let [error (try (lp/run-iteration
                                             environment
                                             [{:role "user" :content "timeout"}]
                                             {:iteration 0
                                              :resolved-model {:provider :lmstudio :name "model"}
                                              :routing {:provider :lmstudio :model "model"}})
                                           (catch Exception e e))]
                            (expect (= :svar.core/stream-semantic-timeout (:type (ex-data error))))
                            (expect (= :vis-first-output-watchdog (:source (ex-data error))))
                            (expect (= 1 @requests))
                            (expect (not (.isInterrupted (Thread/currentThread))))))
                        (finally (lp/dispose-environment! environment))))))

(defdescribe
  provider-first-output-recovery-test
  ;; Regression: session 86f0b252-41c2-4727-be85-733f139e2462 reached the
  ;; gateway's first-output watchdog as stream-cancelled, bypassing bounded retry.
  (doseq [[label mode expected-status expected-calls]
          [["recovers" :recover :success 3] ["exhausts its budget" :exhaust :error 4]
           ["honors Stop" :stop :cancelled 2]
           ["honors Stop during retry backoff" :stop-retry :cancelled 2]]]
    (it
      label
      (let
        [cancelled (atom false)
         environment (assoc (lp/create-environment (helper-router :lmstudio nil) {:db :memory})
                       :cancel-atom cancelled)
         db (:db-info environment)
         tid (persistance/db-store-session-turn! db
                                                 {:parent-session-id (:session-id environment)
                                                  :user-request "timeout recovery"})
         calls (atom 0)
         chunks (atom [])
         code
         "provider_retry_runs = globals().get('provider_retry_runs', 0) + 1\nprint(provider_retry_runs)"]

        (try
          (let [result
                (with-redefs-fn
                  {#'lp/provider-network-policy
                   (fn [_ _]
                     {:ttft-timeout-ms 30 :idle-timeout-ms 30 :semantic-timeout-ms 30})
                   #'lp/PRE_OUTPUT_STREAM_RETRY_DELAYS_MS [0 0]
                   #'svar/ask-code!
                   (fn [_ opts]
                     (let [call (swap! calls inc)]
                       (cond
                         (= 1 call) {:stop-reason :tool-calls
                                     :tool-calls
                                     [{:id "once" :name "python_execution" :input {:code code}}]}
                         (and (= :recover mode) (= 3 call))
                         {:stop-reason :end :content "Recovered without repeating the tool."}
                         :else
                         (do
                           ;; Empty callbacks are keepalives, not model output.
                           ((:on-chunk opts) {:content "" :reasoning "" :done? false})
                           (when (= :stop mode) (reset! cancelled true))
                           (let [deadline (+ (System/currentTimeMillis) 500)]
                             (loop []

                               (cond ((:cancel-fn opts))
                                     (throw (ex-info
                                              "Responses WebSocket operation cancelled by caller."
                                              {:type :svar.core/stream-cancelled
                                               :stream? true
                                               :transport :websocket}))
                                     (>= (System/currentTimeMillis) deadline)
                                     (throw (ex-info "First-output deadline was not enforced"
                                                     {:type :svar.core/http-error :stream? true}))
                                     :else (do (Thread/sleep 2) (recur)))))))))}
                  #(lp/iteration-loop environment
                                      "timeout recovery"
                                      {:session-turn-id tid
                                       :cancel-atom cancelled
                                       :hooks {:on-chunk (fn [chunk]
                                                           (swap! chunks conj chunk)
                                                           (when (and (= :stop-retry mode)
                                                                      (= :stream-watchdog-pre-output
                                                                         (get-in chunk
                                                                                 [:event :reason])))
                                                             (reset! cancelled true)))}}))
                forms (mapcat :forms (persistance/db-list-session-turn-iterations db tid))
                retries (filter #(= :stream-watchdog-pre-output (get-in % [:event :reason]))
                                @chunks)]

            (expect (= expected-status (or (:status result) :success)))
            (when (= :recover mode)
              (expect (str/includes? (str (:answer result))
                                     "Recovered without repeating the tool.")))
            (expect (= expected-calls @calls))
            (expect (= (repeat expected-calls 10060)
                       (map :first-output-timeout-ms
                            (filter #(= :provider-call (:phase %)) @chunks))))
            (expect (= [code] (mapv :src forms)))
            (expect (= "1" (str/trim (:stdout (first forms)))))
            (expect (= (contains? #{:stop :stop-retry} mode) @cancelled))
            (expect (= (case mode
                         :recover
                         1

                         :exhaust
                         2

                         :stop
                         0

                         :stop-retry
                         1)
                       (count retries)))
            (when (= :exhaust mode) (expect (= "error" (get (first (:answer result)) "type")))))
          (finally (lp/dispose-environment! environment)))))))

(defdescribe
  provider-unavailable-is-terminal-test
  ;; Regression, issue #105: Vis used to retry svar's terminal provider-unavailable
  ;; result three more times, stacking a second retry ladder above svar.
  (it "surfaces svar's provider-unavailable result without an outer Vis retry"
      (let [ctx
            {:iteration 1 :messages [] :routing {} :reasoning-level nil}

            result
            (lp/handle-iteration-exception!
              (ex-info "Provider unavailable" {:type :svar.llm/provider-unavailable :status 503})
              ctx)]

        (expect (contains? result :com.blockether.vis.internal.loop/iteration-error))
        (expect (true? (:com.blockether.vis.internal.loop/fatal-iteration-error result))))))

;; The reply is ONE program: multiple fences a provider splits out collapse to a
;; single code-entry so the form cap spans the whole reply and r["…/fF"] numbers
;; continuously (no per-fence f1 collision).
(defdescribe code-entries-preflight-merge-test
             (it "collapses multiple fenced blocks into ONE code-entry = the normalized concat"
                 (let [pre
                       (@#'lp/code-entries-preflight
                        2
                        [{:source "rg(1)" :lang "python"} {:source "cat(2)" :lang "python"}])

                       entries
                       (:code-entries pre)]

                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)\n\ncat(2)" (:expr (first entries))))))
             (it "leaves a single fenced block untouched"
                 (let [entries (:code-entries (@#'lp/code-entries-preflight
                                               1
                                               [{:source "rg(1)" :lang "python"}]))]
                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)" (:expr (first entries))))))
             (it "joins identical id-less blocks as written instead of guessing a stutter"
                 (let [entries (:code-entries (@#'lp/code-entries-preflight
                                               2
                                               [{:source "rg(1)" :lang "python"}
                                                {:source "rg(1)" :lang "python"}]))]
                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)\n\nrg(1)" (:expr (first entries)))))))

;; The model-facing disclosure: a trimmed iteration tells the model what dropped.
(defdescribe
  literal-code-block-error-test
  ;; The guard's comment-only branch parses inside the SESSION's own CPython
  ;; context (post one-context consolidation), so the test threads a real
  ;; context through — built lazily ONCE for the block.
  (let [ctx
        (delay (:python-context (tpc/new-context {})))

        err
        (fn [expr]
          (#'lp/literal-code-block-error @ctx expr))]

    (it "valid Python code passes the guard (nil)" (expect (nil? (err "x = 1"))))
    (it "a bare string program is rejected and points at native answering, not :answer/:code"
        (let [m (err "\"just prose\"")]
          (expect (some? m))
          (expect (str/includes? m "python_execution"))
          (expect (not (str/includes? m ":answer")))))
    (it "a leaked Markdown fence says PYTHON, never Clojure"
        (let [m (err "```python")]
          (expect (some? m))
          (expect (str/includes? m "Python"))
          (expect (not (str/includes? m "Clojure")))))
    (it "a comment-only block references `#` (Python), not `;;`/`#_`"
        (let [m (err "# only a comment")]
          (expect (some? m))
          (expect (str/includes? m "#"))
          (expect (not (str/includes? m ";;")))))))

(defdescribe only-python-execution-is-advertised-test
             ;; ONE tool reaches the provider. Every other capability is already a bare Python
             ;; name inside that sandbox — found with `apropos(pattern)`, read with `doc(name)`,
             ;; called from inside a block — so a second JSON schema advertises a door the
             ;; model can open anyway and charges for it on every single request.
             (it
               "advertises exactly one tool, and it is python_execution"
               (let [tools
                     (@#'lp/model-facing-tools nil)

                     tool
                     (first tools)]

                 (expect (= ["python_execution"] (mapv :name tools)))
                 ;; No extension can add a tool: `model-facing-tools` does not take extensions
                 ;; at all any more, which is the proof rather than an assertion about them.
                 (expect (= 1 (count tools)))
                 ;; Regression: `github-copilot`/`gpt-5.6-terra` 400ed the WHOLE request over a
                 ;; `:strict true` flag Vis derived from Anthropic's own grammar subset, so every
                 ;; turn failed before a token. The one tool is advertised unconstrained.
                 (expect (not-any? :strict tools))
                 ;; The raw-result contract is FOLDED into the description exactly once, and the
                 ;; separate `:result` key never reaches a provider.
                 (expect (= 1 (count (re-seq #"Raw result:" (:description tool)))))
                 (expect (not (contains? tool :result)))
                 ;; One tool, one argument: the whole model-facing schema surface.
                 (expect (= {:type "object"
                             :properties {"code" {:type "string" :description "Python source."}}
                             :required ["code"]
                             :additionalProperties false}
                            (:schema tool)))
                 (doseq [fact ["project packages need a project REPL" "plain Python"
                               "errors surface"
                               ;; The sandbox has ONE success channel. The runtime used to hand a
                               ;; bare trailing expression's value back as a second result, so the
                               ;; one tool the model is given has to say that print is all there is.
                               "`print(...)` is the ONLY channel back"
                               "a bare trailing expression is never echoed"
                               ;; With no result store left, the description states the one rule that
                               ;; replaces it: what you did not print is gone when the block ends.
                               "gone from the transcript once the block ends"
                               ;; The sleep/poll prohibition lives HERE and nowhere else: the core
                               ;; prompt deliberately dropped its duplicate copy.
                               "`sh.logs()`" "no tool waits for you"
                               ;; Dropping a handle closes it — that is the interpreter's job, not
                               ;; ours. What still bites is what the block HOLDS: the descriptor
                               ;; table is the whole process's, and filling it stops `shell` from
                               ;; spawning at all, so the ceiling and its escape hatch are named.
                               "close what you KEEP" "VIS_PY_MAX_OPEN_FILES"]]
                   (expect (str/includes? (:description tool) fact))))))

;; ── post-refresh propagation backoff (gateway-wide OAuth-401 storm guard) ──
(def ^:private auth-last-refreshed (deref #'lp/auth-last-refreshed))

(def ^:private refresh-just-failed? (deref #'lp/refresh-just-failed?))

(def ^:private note-provider-request-ok! (deref #'lp/note-provider-request-ok!))

(def ^:private auth-refreshable-error? (deref #'lp/auth-refreshable-error?))

(def ^:private auth-propagation-backoff-ms (deref #'lp/auth-propagation-backoff-ms))

(def ^:private AUTH_PROPAGATION_WINDOW_MS (deref #'lp/AUTH_PROPAGATION_WINDOW_MS))

(defn- auth-401
  []
  (ex-info "boom"
           {:status 401 :body "{\"error\":{\"message\":\"Invalid authentication credentials\"}}"}))

(defdescribe
  auth-provider-fallback-routing-test
  "Terminal auth recovery releases one dead provider only after refresh handling ends."
  (it "unpinns the failed provider and enables observable fleet auth fallback"
      (let [fallback
            @#'lp/auth-fallback-routing

            error
            (ex-info "OAuth access token has been revoked"
                     {:type :svar.core/http-error :status 401})

            routing
            {:provider :openai-codex
             :model "gpt-5.6-sol"
             :on-transient-error :fallback-model-in-the-same-provider
             :reasoning :deep}

            result
            (fallback error routing {:provider :openai-codex})]

        (expect (= {:on-transient-error :hybrid
                    :on-auth-error :fallback-provider
                    :exclude-providers #{:openai-codex}
                    :reasoning :deep}
                   result))))
  (it
    "preserves existing exclusions and refuses replay after visible output"
    (let [fallback
          @#'lp/auth-fallback-routing

          base
          {:provider :openai-codex :model "gpt-5.6-sol" :exclude-providers #{:broken}}

          model
          {:provider :openai-codex}]

      (expect (= #{:broken :openai-codex}
                 (:exclude-providers (fallback (ex-info "Unauthorized" {:status 401}) base model))))
      (expect (nil?
                (fallback (ex-info "Unauthorized" {:status 401 :content-acc-len 1}) base model)))
      (expect
        (nil? (fallback (ex-info "Unauthorized" {:status 401 :reasoning-acc-len 1}) base model)))))
  (it "runs at most once and only for an identified failing provider"
      (let [fallback
            @#'lp/auth-fallback-routing

            error
            (ex-info "Unauthorized" {:status 401})]

        (expect (nil?
                  (fallback error {:on-auth-error :fallback-provider} {:provider :openai-codex})))
        (expect (= nil (fallback error {} {})))))
  (it "threads the auth-fallback retry without consuming another retry budget"
      (let [next-counters
            @#'lp/next-retry-counters

            base
            {:attempt 2 :max-tokens-attempt 1}]

        (expect (= [2 1] (next-counters {::lp/retry-auth-fallback {}} base))))))

(defdescribe
  auth-cooldown-routing-test
  "A 401 fallback must OUTLIVE its iteration. The rescue route lives in a
   per-iteration atom, so before the cooldown every next iteration re-sent to the
   dead provider: 20 401s and 19 fallbacks in a quarter of an hour (issue #82)."
  (it "keeps a released provider excluded on the NEXT iteration and re-admits it on success"
      (let [cooldown
            @#'lp/provider-auth-cooldown

            note!
            @#'lp/note-provider-auth-cooldown!

            request-ok!
            @#'lp/note-provider-request-ok!

            apply-cooldown
            @#'lp/apply-auth-cooldown-routing

            base
            {:on-transient-error :fallback-model-in-the-same-provider}]

        (try (reset! cooldown {})
             ;; Iteration 1 exhausts auth recovery and falls back: FIRST trip, warns.
             (expect (= true (note! :rbi-genai)))
             ;; A repeat inside the window is not a first trip (logged at :debug).
             (expect (= false (note! :rbi-genai)))
             ;; Iteration 2 therefore STARTS with the dead provider released.
             (expect (= {:on-transient-error :hybrid
                         :on-auth-error :fallback-provider
                         :exclude-providers #{:rbi-genai}}
                        (apply-cooldown base)))
             ;; An accepted request (re-login, rotated key) re-admits it immediately.
             (request-ok! {:provider :rbi-genai} {:llm-provider :rbi-genai})
             (expect (= base (apply-cooldown base)))
             (finally (reset! cooldown {})))))
  (it
    "expires with the window and releases even an explicitly pinned provider"
    (let [cooldown
          @#'lp/provider-auth-cooldown

          apply-cooldown
          @#'lp/apply-auth-cooldown-routing

          now
          (System/currentTimeMillis)]

      (try
        ;; A lapsed window releases routing, but the STRIKE record survives it: the
        ;; next rejection has to escalate rather than restart at the base window
        ;; (issue #154). Only a record lapsed for longer than the ceiling is pruned.
        (reset! cooldown {:rbi-genai {:until (- now 1) :since (- now 60000) :hits 3 :strikes 3}})
        (expect (= {} (apply-cooldown {})))
        (expect (= 3 (:strikes (get @cooldown :rbi-genai))))
        (reset! cooldown {:rbi-genai
                          {:until (- now 7200000) :since (- now 7260000) :hits 3 :strikes 3}})
        (expect (= {} (apply-cooldown {})))
        (expect (= {} @cooldown))
        ;; A pin does NOT outrank the cooldown: every main turn is pinned, so the
        ;; old exemption exempted every turn (see `auth-cooldown-storm-test`).
        (reset! cooldown {:rbi-genai {:until (+ now 60000) :since now :hits 1}})
        (expect (= {:on-auth-error :fallback-provider
                    :on-transient-error :hybrid
                    :exclude-providers #{:rbi-genai}}
                   (apply-cooldown {:provider :rbi-genai :model "m"})))
        ;; A pin on a HEALTHY provider survives, with the cooled peer excluded around it.
        (expect (= {:provider :openai
                    :on-auth-error :fallback-provider
                    :on-transient-error :hybrid
                    :exclude-providers #{:rbi-genai}}
                   (apply-cooldown {:provider :openai})))
        (finally (reset! cooldown {})))))
  (it "reports the cooldown for observability"
      (let [cooldown @#'lp/provider-auth-cooldown]
        (try (reset! cooldown {})
             (@#'lp/note-provider-auth-cooldown! :rbi-genai)
             (let [metrics (lp/auth-cooldown-metrics)]
               (expect (= #{:rbi-genai} (:cooled-providers metrics)))
               (expect (= 300000 (:cooldown-ms metrics)))
               (expect (= 1 (:hits (get (:cooldowns metrics) :rbi-genai))))
               ;; The ceiling is part of the snapshot: an escalating window is only
               ;; readable if the reader knows where it stops.
               (expect (= 3600000 (:cooldown-max-ms metrics)))
               (expect (= 1 (:strikes (get (:cooldowns metrics) :rbi-genai)))))
             (finally (reset! cooldown {}))))))

;; Regression, issue #114: vis logged `Provider auth recovery exhausted; falling back
;; {:cooldown-ms 300000}` and then re-probed the very same dead credential ~12-16s
;; later, minting a fresh OAuth token on every single iteration. Two holes fed the
;; storm: every MAIN turn is pinned to the active provider (`prepare-turn-context`
;; forces provider+model into `:routing`) and a pinned provider was EXEMPT from the
;; cooldown, so the exemption covered every real turn; and a turn RESCUED on a peer
;; cleared the DEAD provider's cooldown, because the accepted request was noted
;; against Vis' pre-call guess (the router HEAD = the pinned provider) instead of
;; the provider that actually answered.
(defdescribe
  auth-cooldown-storm-test
  "The logged cooldown must be ENFORCED: a dead credential is neither re-probed nor
   re-minted until the window elapses or the provider itself accepts a request."
  (let [cooldown
        @#'lp/provider-auth-cooldown

        note!
        @#'lp/note-provider-auth-cooldown!

        request-ok!
        @#'lp/note-provider-request-ok!

        apply-cooldown
        @#'lp/apply-auth-cooldown-routing

        released
        {:on-auth-error :fallback-provider
         :on-transient-error :hybrid
         :exclude-providers #{:rbi-genai}}]

    (it "releases a PINNED dead provider instead of re-probing it every iteration"
        (try
          (reset! cooldown {})
          (note! :rbi-genai)
          ;; Exactly what a depth-0 turn asks for: the active provider+model, pinned.
          (expect (= released
                     (apply-cooldown {:provider :rbi-genai
                                      :model "gpt-5"
                                      :on-transient-error :fallback-model-in-the-same-provider})))
          ;; `:force-provider` is the same pin under another name.
          (expect (= released (apply-cooldown {:force-provider :rbi-genai :force-model "gpt-5"})))
          (finally (reset! cooldown {}))))
    (it "keeps the cooldown armed when a PEER served the turn"
        (try (reset! cooldown {})
             (note! :rbi-genai)
             ;; `resolved-model` is the pre-call guess (router head, i.e. the pin); only
             ;; the iteration result knows which provider actually answered.
             (request-ok! {:provider :rbi-genai :name "gpt-5"} {:llm-provider :openai})
             (expect (= #{:rbi-genai} (:cooled-providers (lp/auth-cooldown-metrics))))
             (expect (= released (apply-cooldown {:provider :rbi-genai :model "gpt-5"})))
             ;; The provider that DID answer is re-admitted at once.
             (request-ok! {:provider :rbi-genai :name "gpt-5"} {:llm-provider :rbi-genai})
             (expect (= #{} (:cooled-providers (lp/auth-cooldown-metrics))))
             (finally (reset! cooldown {}))))))

(defdescribe
  wrapped-auth-exhaustion-cooldown-test
  "Once svar's router has walked the fleet it throws `Provider unavailable` with no
   status and no auth prose: the 401s survive only on `:attempts`. The auth ladder
   read the WRAPPER only, so a wrapped credential failure took neither the rescue
   route nor the cooldown and the dead provider was re-probed every iteration
   (issue #82)."
  (let [wrapper
        (ex-info "Provider unavailable"
                 {:type :svar.llm/provider-unavailable
                  :attempts [{:provider :rbi-genai
                              :model "gpt-5"
                              :status 401
                              :reason :authentication
                              :error "API authentication failed. Check your API key."}
                             {:provider :openai
                              :model "gpt-5"
                              :status 401
                              :reason :authentication
                              :error "Incorrect API key provided"}]})

        mixed
        (ex-info
          "Provider unavailable"
          {:type :svar.llm/provider-unavailable
           :attempts
           [{:provider :rbi-genai :status 401 :reason :authentication :error "bad key"}
            {:provider :openai :status 503 :reason :transient-error :error "upstream down"}]})

        shaped?
        @#'lp/auth-error-shaped?

        fallback-routing
        @#'lp/auth-fallback-routing

        resolved
        {:provider :rbi-genai :name "gpt-5"}]

    (it "reads the credential verdict off the attempts when the wrapper hides it"
        (expect (= true (shaped? wrapper)))
        ;; One transient attempt means the fleet did NOT die on credentials: that
        ;; is an outage, and cooling the provider down would be wrong.
        (expect (= false (shaped? mixed))))
    (it "gives the wrapped failure the same rescue route a bare 401 gets"
        (expect (= {:on-auth-error :fallback-provider
                    :exclude-providers #{:rbi-genai}
                    :on-transient-error :hybrid}
                   (fallback-routing wrapper {} resolved)))
        (expect (nil? (fallback-routing mixed {} resolved)))
        ;; Visible output already streamed: replaying would duplicate it.
        (expect (nil? (fallback-routing (ex-info "Provider unavailable"
                                                 (assoc (ex-data wrapper) :content-acc-len 12))
                                        {}
                                        resolved))))
    (it "arms the cooldown so the NEXT iteration skips the dead credential"
        (let [cooldown @#'lp/provider-auth-cooldown]
          (try (reset! cooldown {})
               (expect (some? (fallback-routing wrapper {} resolved)))
               (expect (= true (@#'lp/note-provider-auth-cooldown! (:provider resolved))))
               (expect (= {:on-auth-error :fallback-provider
                           :exclude-providers #{:rbi-genai}
                           :on-transient-error :hybrid}
                          (@#'lp/apply-auth-cooldown-routing {})))
               (@#'lp/note-provider-request-ok! resolved {:llm-provider :rbi-genai})
               (expect (= {} (@#'lp/apply-auth-cooldown-routing {})))
               (finally (reset! cooldown {})))))))

;; Regression, issue #154: a provider whose credentials had been dead for hours was
;; re-probed every five minutes forever, and the session's PICK never moved off it — so
;; the picker chip named a provider the session could not reach, every later turn re-pinned
;; it, and each turn paid a 401, a forced refresh and a fallback before rescuing itself.
(defdescribe
  auth-rescue-pick-move-test
  "A rescued turn must MOVE the session, not just survive: the pick follows the provider
   that actually answered, the surfaces are told why, and the dead credential is probed
   ever less often instead of on a fixed five-minute loop."
  (let [cooldown
        @#'lp/provider-auth-cooldown

        note!
        @#'lp/note-provider-auth-cooldown!

        request-ok!
        @#'lp/note-provider-request-ok!

        window
        @#'lp/auth-cooldown-window-ms

        cooled
        @#'lp/auth-cooled-providers

        move
        @#'lp/auth-rescue-pick-move

        reseat!
        @#'lp/reseat-pick-after-auth-rescue!

        ;; Exactly the shape the DB hands back: provider and model as strings.
        pinned
        {:provider "rbi-genai" :model "gpt-5"}

        served
        {:llm-provider :openai :llm-model "gpt-5.4"}]

    (it "moves the pick onto the provider that answered once the pinned one is cooled"
        (expect (= {:from {:provider "rbi-genai" :model "gpt-5"}
                    :to {:provider "openai" :model "gpt-5.4"}}
                   (move pinned served #{:rbi-genai}))))
    (it "leaves a pick alone when its provider is not the one that died"
        ;; A model-level or transient fallback is not a verdict on the credential; the
        ;; session keeps the pick the human made.
        (expect (nil? (move pinned served #{})))
        (expect (nil? (move pinned served #{:openai}))))
    (it "moves nothing when the session never pinned a provider"
        ;; Nothing on screen is wrong yet, and adopting a rescue as a pin would silently
        ;; narrow a session that had the whole fleet.
        (expect (nil? (move nil served #{:rbi-genai})))
        (expect (nil? (move {:provider "rbi-genai"} served #{:rbi-genai}))))
    (it "refuses to land on a peer that is itself serving a cooldown"
        (expect (nil? (move pinned served #{:rbi-genai :openai}))))
    (it "refuses a rescue route that names no model, which would CLEAR the pick"
        (expect (nil? (move pinned {:llm-provider :openai} #{:rbi-genai})))
        (expect (nil? (move pinned {:llm-provider :openai :llm-model "  "} #{:rbi-genai}))))
    (it "writes the new pick with its reason and returns the chunk that says why"
        (let [set-args
              (atom nil)

              switch-args
              (atom nil)]

          (try (reset! cooldown {})
               (note! :rbi-genai)
               (with-redefs [session-model/model-of
                             (fn [& _]
                               pinned)

                             session-model/set-model!
                             (fn [& args]
                               (reset! set-args (vec args)))

                             session-model/record-switch!
                             (fn [& args]
                               (reset! switch-args (vec args)))]

                 (expect (= {:from {:provider "rbi-genai" :model "gpt-5"}
                             :to {:provider "openai" :model "gpt-5.4"}}
                            (reseat! {:db-info :db :session-id "sess-1"} served)))
                 ;; The reason rides the write, so the broadcast can tell the surfaces why
                 ;; the chip changed under the user's hands.
                 (expect (= [:db "sess-1" "openai" "gpt-5.4" :authentication-fallback] @set-args))
                 (expect (= [:db "sess-1" {:provider "rbi-genai" :model "gpt-5"}
                             {:provider "openai" :model "gpt-5.4"} :authentication-fallback]
                            @switch-args)))
               (finally (reset! cooldown {})))))
    (it "touches nothing when the pick was never on the dead provider"
        (let [set-args (atom nil)]
          (try (reset! cooldown {})
               (note! :some-other-provider)
               (with-redefs [session-model/model-of (fn [& _]
                                                      pinned)
                             session-model/set-model! (fn [& args]
                                                        (reset! set-args (vec args)))
                             session-model/record-switch! (fn [& _])]

                 (expect (nil? (reseat! {:db-info :db :session-id "sess-1"} served)))
                 (expect (nil? @set-args)))
               (finally (reset! cooldown {})))))
    (it "announces the move on the live stream and in the turn's own routing trace"
        ;; Two different readers: the progress chunk is what a channel draws while the
        ;; turn runs, the trace event is what the finished turn's note explains from.
        (let [move {:from {:provider "rbi-genai" :model "gpt-5"}
                    :to {:provider "openai" :model "gpt-5.4"}}]
          (expect (= {:phase :provider-fallback
                      :iteration 3
                      :reason :authentication-fallback
                      :failed-provider "rbi-genai/gpt-5"
                      :new-provider "openai/gpt-5.4"
                      :event {:event/type :llm.routing/provider-fallback
                              :reason :authentication-fallback
                              :scope :session-pick
                              :from-provider "rbi-genai"
                              :from-model "gpt-5"
                              :to-provider "openai"
                              :to-model "gpt-5.4"}}
                     (@#'lp/pick-moved-chunk 3 move)))
          ;; `:scope :session-pick` keeps it out of the turn's own route summary: the
          ;; turn is still reported as having run where it ran.
          (expect (= {:selected {:provider "rbi-genai" :model "gpt-5"}
                      :actual {:provider "openai" :model "gpt-5.4"}
                      :fallback? true
                      :trace [(@#'lp/pick-move-event move)]}
                     (@#'lp/llm-routing-summary
                      {:provider :rbi-genai :name "gpt-5"}
                      {:llm-provider :openai
                       :llm-model "gpt-5.4"
                       :llm-routing-trace [(@#'lp/pick-move-event move)]})))))
    (it "probes a credential that keeps failing ever less often, up to an hour"
        ;; The point of the escalation: a key nobody has fixed stops costing a 401 +
        ;; refresh + fallback every five minutes for the rest of the day.
        (expect (= [300000 600000 1200000 2400000] (mapv window [1 2 3 4])))
        (expect (= 3600000 (window 9)))
        (expect (= 3600000 (window 99)))
        ;; A first rejection, and anything malformed, still gets the base window.
        (expect (= 300000 (window nil))))
    (it "escalates across UNBROKEN strikes and forgets the streak once a request lands"
        (try (reset! cooldown {})
             (note! :rbi-genai)
             (expect (= 1 (:strikes (get @cooldown :rbi-genai))))
             ;; A window that lapsed without any success is still a strike: re-probing
             ;; found the credential just as dead.
             (swap! cooldown assoc-in [:rbi-genai :until] (- (System/currentTimeMillis) 1000))
             (expect (= #{} (cooled)))
             (note! :rbi-genai)
             (let [entry (get @cooldown :rbi-genai)]
               (expect (= 2 (:strikes entry)))
               (expect (= #{:rbi-genai} (cooled)))
               ;; Doubled, not restarted at the base window.
               (expect (< (+ (System/currentTimeMillis) 300000) (long (:until entry)))))
             ;; One accepted request is proof the credential works again: back to base.
             (request-ok! {:provider :rbi-genai :name "gpt-5"} {:llm-provider :rbi-genai})
             (note! :rbi-genai)
             (let [entry (get @cooldown :rbi-genai)]
               (expect (= 1 (:strikes entry)))
               (expect (>= (+ (System/currentTimeMillis) 300000) (long (:until entry)))))
             (finally (reset! cooldown {}))))
    (it "forgets a record whose window lapsed long ago instead of hoarding it"
        (try (reset! cooldown {})
             (note! :rbi-genai)
             (swap! cooldown assoc-in [:rbi-genai :until] (- (System/currentTimeMillis) 7200000))
             (expect (= #{} (cooled)))
             (expect (nil? (get @cooldown :rbi-genai)))
             (finally (reset! cooldown {}))))))

;; Regression, issue #154: nothing let a human REFUSE the rescue. Every automatic route
;; off the picked model — the cooldown exclusion, the post-refresh rescue, the refusal
;; switch — happened unasked, answered from a model they had not chosen, and started that
;; provider's prompt cache from cold for the rest of the session.
(defdescribe
  provider-fallback-toggle-test
  "`provider_fallback` is the human's call. Every automatic route off the session's pick
   asks it first, and an untouched install still rescues exactly as before."
  (let [cooldown
        @#'lp/provider-auth-cooldown

        note!
        @#'lp/note-provider-auth-cooldown!

        cooldown-routing
        @#'lp/apply-auth-cooldown-routing

        fallback-routing
        @#'lp/auth-fallback-routing

        refusals
        @#'lp/refusal-fallbacks-for

        pin
        @#'lp/pin-routing-to-model

        auth-error
        (ex-info "Unauthorized" {:status 401})

        pinned
        {:provider :rbi-genai :model "gpt-5"}

        claude-fleet
        {:providers [{:id :anthropic-coding-plan
                      :models [{:name "claude-opus-5"} {:name "claude-opus-4-8"}]}]}

        refused-model
        {:provider :anthropic-coding-plan :name "claude-opus-5"}]

    (it "ships ON, so an untouched install rescues turns exactly as before"
        (let [spec (toggles/toggle-spec "provider_fallback")]
          (expect (= :boolean (:type spec)))
          (expect (true? (:default spec)))
          (expect (true? (toggles/enabled? "provider_fallback")))))
    (it "drops a cooled pin and excludes the dead credential while fallback is allowed"
        (try (reset! cooldown {})
             (note! :rbi-genai)
             (let [routed (cooldown-routing pinned)]
               (expect (= #{:rbi-genai} (:exclude-providers routed)))
               (expect (= :fallback-provider (:on-auth-error routed)))
               (expect (nil? (:provider routed))))
             (finally (reset! cooldown {}))))
    (it "leaves the pin exactly as it found it once fallback is off"
        ;; With nowhere to land, excluding the cooled provider would only trade its real
        ;; 401 for a routing failure that names no credential at all.
        (try (reset! cooldown {})
             (note! :rbi-genai)
             (toggles/set-value! "provider_fallback" false)
             (expect (= pinned (cooldown-routing pinned)))
             (finally (toggles/reset-to-default! "provider_fallback") (reset! cooldown {}))))
    (it "builds no cross-provider rescue route once fallback is off"
        (expect (some? (fallback-routing auth-error pinned {:provider :rbi-genai})))
        (try (toggles/set-value! "provider_fallback" false)
             (expect (nil? (fallback-routing auth-error pinned {:provider :rbi-genai})))
             (finally (toggles/reset-to-default! "provider_fallback"))))
    (it "still offers the in-provider refusal switch once provider fallback is off"
        ;; A refusal is not a provider failure: the credential, the provider and the wire
        ;; all worked. Refusing PEER credentials must not also refuse the sibling model
        ;; Anthropic documents as the recovery — `refusal_fallback` owns that call.
        (expect (= ["claude-opus-4-8"] (refusals claude-fleet refused-model)))
        (try (toggles/set-value! "provider_fallback" false)
             (expect (= ["claude-opus-4-8"] (refusals claude-fleet refused-model)))
             (finally (toggles/reset-to-default! "provider_fallback"))))
    (it "pins the call to the model it resolved once fallback is off"
        ;; Without the pin, svar's own provider walk answers from a peer no matter what
        ;; Vis excluded upstream — the exclusion list is advice, the pin is a contract.
        (expect (= {} (pin nil {:provider :rbi-genai :name "gpt-5"})))
        (try (toggles/set-value! "provider_fallback" false)
             (expect (= {:provider :rbi-genai :model "gpt-5"}
                        (pin nil {:provider :rbi-genai :name "gpt-5"})))
             (expect (= {:capabilities #{:vision} :provider :rbi-genai :model "gpt-5"}
                        (pin {:capabilities #{:vision}} {:provider :rbi-genai :name "gpt-5"})))
             (finally (toggles/reset-to-default! "provider_fallback"))))
    (it "never pins half a route"
        ;; A provider with no model name — or a name with no provider — would pin a
        ;; DIFFERENT model than the one this very call resolved.
        (try (toggles/set-value! "provider_fallback" false)
             (expect (= {} (pin nil {:provider :rbi-genai})))
             (expect (= {} (pin nil {:name "gpt-5"})))
             (finally (toggles/reset-to-default! "provider_fallback"))))))

;; Regression, issue #154 follow-up: a safety refusal was handled like a provider failure.
;; Turning `provider_fallback` off silenced the recovery Anthropic documents, the fallback
;; name was sent without checking that provider serves it, and nothing pinned the provider —
;; so an HTTP-200 content decline could be answered by a peer credential.
(defdescribe
  refusal-fallback-scope-test
  "An Anthropic refusal is HTTP 200 from a HEALTHY provider: the recovery is a sibling
   model of that same provider, gated by its own switch, and never a route off it."
  (let [refusals
        @#'lp/refusal-fallbacks-for

        pin-provider
        @#'lp/pin-routing-to-provider

        chunk
        @#'lp/refusal-fallback-chunk

        fleet
        {:providers [{:id :anthropic-coding-plan
                      :models [{:name "claude-opus-5"} {:name "claude-opus-4-8"}]}
                     {:id :peer :models [{:name "claude-opus-4-8"}]}]}

        refused
        {:provider :anthropic-coding-plan :name "claude-opus-5"}]

    (it "offers the sibling the refusing provider actually serves"
        (expect (= ["claude-opus-4-8"] (refusals fleet refused))))
    (it "offers nothing when the refusing provider serves no sibling"
        ;; svar hands the name back as `:force-model`. A name this provider does not serve
        ;; either dies as a routing failure naming no credential, or resolves on the peer
        ;; that does serve it — moving billing because of a content decision.
        (expect (nil? (refusals {:providers [{:id :anthropic-coding-plan
                                              :models [{:name "claude-opus-5"}]}]}
                                refused))))
    (it "offers nothing when no router can confirm what the provider serves"
        (expect (nil? (refusals nil refused))))
    (it "never re-asks the very model that declined"
        ;; The chain is data. Were the refusing model listed in it, re-sending it would
        ;; earn the identical decline — a refusal is deterministic.
        (with-redefs-fn {#'lp/refusal-fallback-models ["claude-opus-5" "claude-opus-4-8"]}
          (fn []
            (expect (= ["claude-opus-4-8"] (refusals fleet refused))))))
    (it "leaves models that never emit a refusal alone"
        ;; Only the Claude 5 family carries the safety classifier; a chain elsewhere pays
        ;; a model switch for an error it will never see.
        (expect (nil? (refusals fleet {:provider :peer :name "claude-opus-4-8"}))))
    (it "is its own switch: off surfaces the decline, on keeps the recovery"
        (try (toggles/set-value! "refusal_fallback" false)
             (expect (nil? (refusals fleet refused)))
             (finally (toggles/reset-to-default! "refusal_fallback")))
        (expect (= ["claude-opus-4-8"] (refusals fleet refused))))
    (it "pins the provider that refused, so the switch cannot buy a peer credential"
        (expect (= {:provider :anthropic-coding-plan} (pin-provider nil refused)))
        (expect (= {:capabilities #{:vision} :provider :anthropic-coding-plan}
                   (pin-provider {:capabilities #{:vision}} refused))))
    (it "leaves an explicit provider and a half-named resolution untouched"
        (expect (= {:provider :peer} (pin-provider {:provider :peer} refused)))
        (expect (= {} (pin-provider nil {:name "claude-opus-5"}))))
    (it "traces the switch as a MODEL fallback, never as a provider retry"
        ;; A retry event says the provider misbehaved; this one answered first time and
        ;; declined on purpose. The model-scoped type is also what tells the turn note the
        ;; cache is cold, because an Anthropic prompt cache belongs to ONE model.
        (let [ev (:event (chunk 2
                                {:from-model "claude-opus-5"
                                 :to-model "claude-opus-4-8"
                                 :category "policy"
                                 :attempt 1}))]
          (expect (= :llm.routing/model-fallback (:event/type ev)))
          (expect (= :refusal (:reason ev)))
          (expect (= "claude-opus-5" (:from-model ev)))
          (expect (= "claude-opus-4-8" (:to-model ev)))))))

;; Regression, issue #154: a turn rescued onto a peer kept being MEASURED against the model
;; it had left. `session["routing"]` named the provider that failed and `session_utilization`
;; priced the pin's context window, so a 1M-window pin rescued onto a 128K peer read ~90%
;; free right up to the provider's rejection.
(defdescribe
  served-route-follows-the-answer-test
  "What answered the last request is what the next one is measured against: the served
   provider/model reaches both the session dict's routing and the context ceiling, and
   never outlives the turn that observed it."
  (let [limit
        @#'lp/iteration-context-limit

        fold-budget
        @#'lp/context-fold-budget

        stamp!
        @#'lp/stamp-served-route!

        served-model
        @#'lp/turn-served-model

        router
        {:providers [{:id :pinned :models [{:name "big" :input-limit 1000000}]}
                     {:id :peer :models [{:name "small" :input-limit 128000}]}]}]

    (it "measures the window of the model that ANSWERED, not the one that was pinned"
        (expect (= 128000 (limit nil {:input-limit 128000} {:input-limit 1000000}))))
    (it "keeps the pinned window until something has actually served"
        (expect (= 1000000 (limit nil nil {:input-limit 1000000}))))
    (it "lets a caller-supplied ceiling outrank every resolved model"
        (expect (= 42 (limit 42 {:input-limit 128000} {:input-limit 1000000}))))
    (it "reads :context when models.dev exposes no separate input cap"
        (expect (= 300000 (limit nil {:context 300000} {:input-limit 1000000}))))
    (it "keeps the historical 200K advisory ceiling for a model nothing is known about"
        (expect (= 200000 (limit nil nil nil)))
        (expect (= 200000 (fold-budget (limit nil nil nil)))))
    (it "lowers the folding budget only when the effective window is below 200K"
        (expect (= 7372 (fold-budget 8192)))
        (expect (= 115200 (fold-budget 128000)))
        (expect (= 129600 (fold-budget 144000)))
        (expect (= 200000 (fold-budget 200000)))
        (expect (= 200000 (fold-budget 1000000))))
    ;; A window is config a human edits and a catalog a provider ships; neither is a promise,
    ;; and this number divides every saturation the session prints.
    (it "accepts a window quoted as a string, exactly as YAML hands one over"
        (expect (= 128000 (limit "128000" nil nil)))
        (expect (= 96000 (limit nil {:context " 96000 "} nil))))
    (it "skips a source that cannot name a positive window instead of publishing it"
        (expect (= 64000 (limit 0 {:input-limit -1} {:context 64000})))
        (expect (= 64000 (limit "" {:input-limit "n/a"} {:context 64000})))
        (expect (= 200000 (limit ##NaN {:input-limit ##Inf} {:context {}}))))
    (it "always answers a positive whole number of tokens"
        (expect (every? #(and (integer? %) (pos? %))
                        (for [a
                              [nil 0 -1 "x" ##NaN 1e14 :kw]

                              b
                              [nil {} {:context "128000"} {:input-limit 0}]]

                          (limit a b nil)))))
    (it "stamps the provider that answered and resolves it back to that model"
        (let [ctx-atom (atom {"session_turn" 3})]
          (stamp! {:ctx-atom ctx-atom :router router} {:llm-provider :peer :llm-model "small"})
          (expect (= {"provider" "peer" "model" "small"} (eng/served-route @ctx-atom)))
          (expect (= 128000 (:input-limit (served-model {:ctx-atom ctx-atom :router router}))))))
    (it "ignores a stamp left by an earlier turn"
        ;; The human may have picked another model in between, and a turn's first request
        ;; is sent before anything has served it.
        (let [ctx-atom (atom {"session_turn" 3})]
          (stamp! {:ctx-atom ctx-atom} {:llm-provider :peer :llm-model "small"})
          (swap! ctx-atom assoc "session_turn" 4)
          (expect (nil? (eng/served-route @ctx-atom)))
          (expect (nil? (served-model {:ctx-atom ctx-atom :router router})))))
    (it "refuses a half-named route rather than replacing the pin with it"
        (let [ctx-atom (atom {"session_turn" 1})]
          (stamp! {:ctx-atom ctx-atom} {:llm-provider :peer})
          (expect (nil? (eng/served-route @ctx-atom)))))
    (it "names the model that answered in the session dict the agent reads"
        (let [ctx
              (eng/stamp-served-route {"session_turn" 2 "session_id" "s1"} :peer "small")

              enriched
              (ctx-loop/enrich-ctx {:routing {"provider" "pinned" "model" "big"}} ctx)]

          (expect (= {"provider" "peer" "model" "small"} (get enriched "session_routing")))))
    (it "still shows the pinned route while the turn has been served by nobody"
        (let [enriched (ctx-loop/enrich-ctx {:routing {"provider" "pinned" "model" "big"}}
                                            {"session_turn" 2})]
          (expect (= {"provider" "pinned" "model" "big"} (get enriched "session_routing")))))))

;; Regression, issue #154: Vis recomputed and restored a stale prompt-cache ratio instead
;; of rendering the current, route-scoped status already measured by Svar.
(defdescribe
  prompt-cache-status-render-test
  "Vis treats Svar's prompt-cache status as opaque current-process telemetry."
  (let [stamp! @#'lp/stamp-prompt-cache-status!]
    (it "passes Svar's status through without inventing another cache metric"
        (let [status {:kind :provider-prompt-cache
                      :provider-id :openai-codex
                      :model "gpt-5.6-sol"
                      :fresh? true
                      :sample-count 2
                      :token-read-percent 82
                      :request-hit-percent 50}
              ctx-atom (atom {"session_id" "s"
                              "session_turn" 2
                              "engine_utilization" {"last_request_tokens" 1000}})]

          (stamp! ctx-atom status)
          (let [util (get (eng/session-view @ctx-atom) "session_utilization")]
            (expect (= {"kind" "provider-prompt-cache"
                        "provider_id" "openai-codex"
                        "model" "gpt-5.6-sol"
                        "is_fresh" true
                        "sample_count" 2
                        "token_read_percent" 82
                        "request_hit_percent" 50}
                       (get util "prompt_cache")))
            (expect (not (contains? util "cache_hit_rate")))
            (expect (not (contains? util "cache_hit_window")))
            (expect (not (contains? (eng/session-view @ctx-atom) eng/prompt-cache-status-key))))))
    (it "omits prompt-cache telemetry before Svar measures this process and turn"
        (let [util (eng/utilization 1000 200000 1000 100000)]
          (expect (= util (eng/with-prompt-cache-status util nil)))
          (expect (nil? (eng/with-prompt-cache-status nil {:fresh? true})))
          (expect (not (contains? (get (eng/session-view {"session_id" "s"
                                                          "session_turn" 1
                                                          "engine_utilization" util})
                                       "session_utilization")
                                  "prompt_cache")))))
    (it "clears the prior turn's snapshot while preserving an idempotent same-turn sync"
        (let [status {"kind" "provider-prompt-cache" "is_fresh" true}
              ctx {"session_turn" 2 eng/prompt-cache-status-key status}]

          (expect (= status (get (eng/enter-turn ctx 2) eng/prompt-cache-status-key)))
          (expect (not (contains? (eng/enter-turn ctx 3) eng/prompt-cache-status-key)))))
    (it "restores only the latest request size, never provider-cache telemetry from disk"
        (let [asked (atom [])]
          (with-redefs [persistance/db-list-session-turns (constantly [{:id "t1" :position 1}
                                                                       {:id "t2" :position 2}
                                                                       {:id "t3" :position 3}])
                        persistance/db-list-session-turn-iterations
                        (fn [_db-info turn-id]
                          (swap! asked conj turn-id)
                          (case turn-id
                            "t2"
                            [{:position 4 :input-tokens 2000 :input-cache-read-tokens 1800}]

                            "t1"
                            [{:position 1 :input-tokens 1000 :input-cache-read-tokens 900}]

                            []))]

            (let [restored (previous-request-usage {:session-id "s1" :db-info ::db} "t3")]
              (expect (= {:last-request-tokens 2000
                          :last-request-turn-id "t2"
                          :last-request-turn-position 2
                          :last-request-iteration 4}
                         restored))
              (expect (= ["t2"] @asked))
              (expect (not (contains? restored :cache-samples)))))))
    (it "carries Svar's prompt-cache status out of a one-shot iteration unchanged"
        (let [environment (lp/create-environment ::router {:db :memory})
              status {:kind :provider-prompt-cache
                      :provider-id :lmstudio
                      :model "local-model"
                      :fresh? true
                      :token-read-percent 75
                      :request-hit-percent 100}]

          (try (with-redefs [svar/ask-code! (fn [_router _opts]
                                              {:stop-reason :end
                                               :tool-calls []
                                               :content "done"
                                               :tokens {}
                                               :prompt-cache status})]
                 (let [result (lp/run-iteration environment
                                                []
                                                {:iteration 0
                                                 :resolved-model {:provider :lmstudio
                                                                  :name "local-model"
                                                                  :reasoning? false}})]
                   (expect (= status (:prompt-cache result)))))
               (finally (lp/dispose-environment! environment)))))))

(defdescribe
  router-with-pinned-model-test
  "A session pick naming a model only the provider's LIVE catalog lists must still
   BIND. It used to validate away to `{}` and the turn silently ran the default
   model while the picker showed the pick as applied (issue #81)."
  (let [router
        {:providers [{:id :openai-codex :models [{:name "gpt-5.4"}]}
                     {:id :zai-coding-plan :models [{:name "glm-4.7"}]}]}

        materialise
        @#'lp/router-with-pinned-model

        forced
        @#'lp/forced-routing-for-pref]

    (it "a config-unknown model alone forces NOTHING — the regression"
        (expect (= {} (forced router :zai-coding-plan "glm-4.8"))))
    (it "materialising the pin makes the pick force routing and own the display root"
        (let [pinned (materialise router :zai-coding-plan "glm-4.8")]
          (expect (= [{:name "glm-4.7"} {:name "glm-4.8"}] (:models (second (:providers pinned)))))
          (expect (= {:provider :zai-coding-plan :model "glm-4.8"}
                     (forced pinned :zai-coding-plan "glm-4.8")))
          ;; …and display/cost attribution follows the same router.
          (expect (= :zai-coding-plan
                     (:provider (lp/resolve-effective-model (lp/router-for-model pinned
                                                                                 "glm-4.8")))))))
    (it "accepts the id as a string, exactly as the session pref stores it"
        (expect (= {:provider :zai-coding-plan :model "glm-4.8"}
                   (forced (materialise router "zai-coding-plan" "glm-4.8")
                           "zai-coding-plan"
                           "glm-4.8"))))
    (it "leaves the router untouched without a real pin"
        ;; Already listed, unknown provider, no provider, blank model.
        (expect (= router (materialise router :zai-coding-plan "glm-4.7")))
        (expect (= router (materialise router :nope "glm-4.8")))
        (expect (= router (materialise router nil "glm-4.8")))
        (expect (= router (materialise router :zai-coding-plan "   ")))
        (expect (= router (materialise router :zai-coding-plan nil))))))

(defdescribe
  post-refresh-propagation-backoff-test
  (describe
    "a token we JUST refreshed that 401s again is treated as propagation lag, not dead"
    (it "refresh-just-failed? fires when we force-refreshed within the propagation window"
        (reset! auth-last-refreshed {:ap {:at (System/currentTimeMillis)}})
        (expect (true? (refresh-just-failed? (auth-401) {:provider :ap}))))
    (it "does NOT fire once the last refresh is older than the window (real rotation → refresh)"
        (reset! auth-last-refreshed {:ap {:at (- (System/currentTimeMillis)
                                                 (long AUTH_PROPAGATION_WINDOW_MS)
                                                 1)}})
        (expect (not (refresh-just-failed? (auth-401) {:provider :ap}))))
    (it "does NOT fire when the provider was never refreshed"
        (reset! auth-last-refreshed {})
        (expect (not (refresh-just-failed? (auth-401) {:provider :ap}))))
    (it "fires regardless of token VALUE — covers providers that mint a fresh token each exchange"
        ;; Regression for the Copilot 401 storm: the old value-equality check
        ;; (minted == baked-token) never matched a rotating-token provider and
        ;; fell open into an endless re-mint. Recency matches every provider.
        (reset! auth-last-refreshed {:ap {:at (System/currentTimeMillis)}})
        (with-redefs [config/baked-token (fn [_]
                                           "a-totally-different-token")]
          (expect (true? (refresh-just-failed? (auth-401) {:provider :ap})))))
    (it "note-provider-request-ok! clears the marker so a later 401 re-mints, not backs off"
        (reset! auth-last-refreshed {:ap {:at (System/currentTimeMillis)}})
        (note-provider-request-ok! {:provider :ap} {:llm-provider :ap})
        (expect (nil? (get @auth-last-refreshed :ap)))
        (expect (not (refresh-just-failed? (auth-401) {:provider :ap}))))
    (it "a post-refresh 401 stays REFRESHABLE-shaped but routes to backoff, never a dead latch"
        ;; No dead-credential latch exists any more: the provider is
        ;; always eligible to recover; the classifier just prefers the
        ;; SAME-token backoff over another re-mint while lag settles.
        (with-redefs [config/baked-token
                      (fn [_]
                        "T-fresh")

                      registry/provider-by-id
                      (fn [_]
                        {:provider/get-token-fn (fn []
                                                  {:token "T-fresh"})
                         :provider/refresh-token-fn (fn [& _]
                                                      :ok)})]

          (reset! auth-last-refreshed {:ap {:at (System/currentTimeMillis)}})
          (expect (true? (auth-refreshable-error? (auth-401) {:provider :ap})))))
    (it "backoff widens with the attempt count and is capped at 5s"
        (expect (= 1200 (auth-propagation-backoff-ms 0)))
        (expect (= 3600 (auth-propagation-backoff-ms 2)))
        (expect (= 5000 (auth-propagation-backoff-ms 10))))))

;; ── request-bound OAuth credentials + forced-refresh circuit breaker ──────
(def ^:private auth-refresh-events (deref #'lp/auth-refresh-events))

(def ^:private auth-refresh-allowed? (deref #'lp/auth-refresh-allowed?))

(def ^:private hydrate-router-credentials (deref #'lp/hydrate-router-credentials))

(def ^:private try-refresh-provider-token! (deref #'lp/try-refresh-provider-token!))

(def ^:private managed-auth-flights (deref #'lp/managed-auth-flights))

(def ^:private ensure-managed-provider-auth! (deref #'lp/ensure-managed-provider-auth!))

(def ^:private hydrate-environment-router (deref #'lp/hydrate-environment-router))

(def ^:private AUTH_REFRESH_WINDOW_MS (deref #'lp/AUTH_REFRESH_WINDOW_MS))

(def ^:private AUTH_REFRESH_WINDOW_MAX (deref #'lp/AUTH_REFRESH_WINDOW_MAX))

;; Regression, issue #165: concurrent first requests to an automatically bound
;; managed provider could not invoke its declared interactive authentication at all.
(defdescribe
  managed-provider-first-use-auth-test
  (it "runs one authentication flight and lets every concurrent request reuse its credential"
      (reset! managed-auth-flights {})
      (let [token
            (atom nil)

            auth-calls
            (atom 0)

            entered
            (promise)

            release
            (promise)

            descriptor
            {:provider/id :corp
             :provider/is-managed true
             :provider/get-token-fn (fn []
                                      {:token @token})
             :provider/auth-fn (fn [_]
                                 (swap! auth-calls inc)
                                 (deliver entered true)
                                 @release
                                 (reset! token "signed-in"))}]

        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (let [requests (vec (repeatedly 8 #(future (ensure-managed-provider-auth! :corp))))]
            (expect (= true (deref entered 2000 ::timed-out)))
            (deliver release true)
            (expect (every? #(= {:token "signed-in"} (deref % 2000 ::timed-out)) requests))
            (expect (= 1 @auth-calls)))))
      (reset! managed-auth-flights {}))
  (it "starts authentication only at the actual provider-request boundary"
      (reset! managed-auth-flights {})
      (let [token
            (atom nil)

            auth-calls
            (atom 0)

            descriptor
            {:provider/id :corp
             :provider/is-managed true
             :provider/get-token-fn (fn []
                                      {:token @token})
             :provider/auth-fn (fn [_]
                                 (swap! auth-calls inc)
                                 (reset! token "signed-in"))}

            environment
            {:router {:providers [{:id :corp}]}}]

        (with-redefs [registry/provider-by-id (constantly descriptor)]
          (expect (= environment (hydrate-environment-router environment)))
          (expect (zero? @auth-calls))
          (expect (= "signed-in"
                     (get-in (hydrate-environment-router environment :corp)
                             [:router :providers 0 :api-key])))
          (expect (= 1 @auth-calls))))
      (reset! managed-auth-flights {}))
  (it
    "returns one clear failure when authentication produces no credential"
    (reset! managed-auth-flights {})
    (let [auth-calls
          (atom 0)

          descriptor
          {:provider/id :corp
           :provider/is-managed true
           :provider/get-token-fn (constantly {:token nil})
           :provider/auth-fn (fn [_]
                               (swap! auth-calls inc))}

          failure
          (with-redefs [registry/provider-by-id (constantly descriptor)]
            (try (ensure-managed-provider-auth! :corp) nil (catch clojure.lang.ExceptionInfo e e)))]

      (expect (= 1 @auth-calls))
      (expect (= :provider/authentication-failed (:type (ex-data failure))))
      (expect (str/includes? (ex-message failure) "did not produce a usable credential")))
    (reset! managed-auth-flights {})))

(defdescribe
  auth-refresh-circuit-breaker-test
  "The breaker budgets forced refreshes per window and drains once they stop."
  (it "grants exactly the per-window budget, reports open, then denies without recording"
      (reset! auth-refresh-events {})
      (expect (every? true? (repeatedly AUTH_REFRESH_WINDOW_MAX #(auth-refresh-allowed? :ap))))
      (expect (= #{:ap} (:breaker-open (lp/auth-refresh-metrics))))
      (dotimes [_ 25]
        (expect (false? (auth-refresh-allowed? :ap))))
      (expect (= (long AUTH_REFRESH_WINDOW_MAX) (long (count (get @auth-refresh-events :ap))))))
  (it "closes again once the recorded refreshes age out of the window"
      (let [stale (- (System/currentTimeMillis) (long AUTH_REFRESH_WINDOW_MS) 1)]
        (reset! auth-refresh-events {:ap (vec (repeat AUTH_REFRESH_WINDOW_MAX stale))})
        (expect (true? (auth-refresh-allowed? :ap)))
        (expect (= 1 (count (get @auth-refresh-events :ap))))))
  (it "budgets each provider independently"
      (reset! auth-refresh-events {})
      (dotimes [_ AUTH_REFRESH_WINDOW_MAX]
        (auth-refresh-allowed? :ap))
      (expect (false? (auth-refresh-allowed? :ap)))
      (expect (true? (auth-refresh-allowed? :other)))
      (reset! auth-refresh-events {})))

(defdescribe
  request-bound-credential-hydration-test
  "Every provider attempt reads dynamic auth fields without rebuilding router state."
  (it "replaces all dynamic credential fields while preserving router state"
      (let [state
            (atom {:health :warm})

            router
            {:providers [{:id :ap
                          :api-key "old"
                          :base-url "https://old.example"
                          :llm-headers {"old" "header"}
                          :responses-path "/old"}]
             :state state
             :budget {:spent 42}}]

        (with-redefs [registry/provider-by-id (fn [_]
                                                {:provider/get-token-fn
                                                 (fn []
                                                   {:token "fresh"
                                                    :api-url "https://fresh.example"
                                                    :llm-headers {"fresh" "header"}
                                                    :responses-path "/responses"})})]
          (let [hydrated (hydrate-router-credentials router)]
            (expect (identical? state (:state hydrated)))
            (expect (= {:spent 42} (:budget hydrated)))
            (expect (= {:id :ap
                        :api-key "fresh"
                        :base-url "https://fresh.example"
                        :llm-headers {"fresh" "header"}
                        :responses-path "/responses"}
                       (first (:providers hydrated))))))))
  (it "reapplies session headers after dynamic credential hydration"
      (let [environment {:router {:providers [{:id :ap :llm-headers {"old" "header"}}]}
                         :session-llm-headers {:ap {"session" "stable" "shared" "session"}}}]
        (with-redefs [registry/provider-by-id (fn [_]
                                                {:provider/get-token-fn
                                                 (fn []
                                                   {:token "fresh"
                                                    :llm-headers {"fresh" "header"
                                                                  "shared" "credential"}})})]
          (expect (= {"fresh" "header" "session" "stable" "shared" "session"}
                     (get-in (hydrate-environment-router environment)
                             [:router :providers 0 :llm-headers]))))))
  (it "retains the exact old provider snapshot when token lookup fails"
      (let [provider
            {:id :ap :api-key "still-usable" :base-url "https://old.example"}

            router
            {:providers [provider]}]

        (with-redefs [registry/provider-by-id (fn [_]
                                                {:provider/get-token-fn
                                                 (fn []
                                                   (throw (ex-info "disk race" {})))})]
          (expect (= provider (first (:providers (hydrate-router-credentials router))))))))
  ;; Regression, issue #152: the credential could answer a responses path
  ;; but never the wire it speaks, so a runtime-issued Responses endpoint
  ;; kept the chat dialect the router had defaulted to.
  (it "fills a missing dialect from the credential, in any accepted spelling"
      (with-redefs [registry/provider-by-id (fn [_]
                                              {:provider/get-token-fn
                                               (fn []
                                                 {:token "t" :api-style "openai_responses"})})]
        (expect (= :openai-compatible-responses
                   (:api-style (first (:providers (hydrate-router-credentials {:providers
                                                                               [{:id :ap}]}))))))))
  (it "leaves a dialect the config already resolved alone"
      (with-redefs [registry/provider-by-id (fn [_]
                                              {:provider/get-token-fn
                                               (fn []
                                                 {:token "t" :api-style "openai-responses"})})]
        (expect (= :openai-compatible-chat
                   (:api-style (first (:providers (hydrate-router-credentials
                                                    {:providers [{:id :ap
                                                                  :api-style
                                                                  :openai-compatible-chat}]}))))))))
  (it "leaves static providers untouched"
      (let [router {:providers [{:id :static :api-key "configured"}]}]
        (with-redefs [registry/provider-by-id (constantly nil)]
          (expect (= router (hydrate-router-credentials router)))))))

(defdescribe request-bound-auth-refresh-test
             "401 recovery adopts peer rotations first and refreshes only the exact rejected token."
             (it "adopts a peer token without spending breaker budget or calling refresh"
                 (let [refreshes
                       (atom [])

                       attempt-router
                       {:providers [{:id :ap :api-key "rejected"}]}]

                   (reset! auth-refresh-events {})
                   (with-redefs [registry/provider-by-id
                                 (fn [_]
                                   {:provider/get-token-fn (fn []
                                                             {:token "peer-fresh"})
                                    :provider/refresh-token-fn (fn [rejected]
                                                                 (swap! refreshes conj rejected))})]
                     (expect (true? (try-refresh-provider-token! attempt-router {:provider :ap})))
                     (expect (= [] @refreshes))
                     (expect (= {} @auth-refresh-events)))))
             (it
               "passes the exact attempt token to refresh, not a process-global baked token"
               (let [refreshes
                     (atom [])

                     attempt-router
                     {:providers [{:id :ap :api-key "attempt-rejected"}]}]

                 (reset! auth-refresh-events {})
                 (with-redefs [config/baked-token
                               (constantly "wrong-global-token")

                               registry/provider-by-id
                               (fn [_]
                                 {:provider/get-token-fn (fn []
                                                           {:token "attempt-rejected"})
                                  :provider/refresh-token-fn (fn [rejected]
                                                               (swap! refreshes conj rejected))})]

                   (expect (true? (try-refresh-provider-token! attempt-router {:provider :ap})))
                   (expect (= ["attempt-rejected"] @refreshes))
                   (expect (= 1 (count (get @auth-refresh-events :ap)))))))
             (it "an open breaker neither refreshes nor mistakes the rejected token for a peer"
                 (let [refreshes
                       (atom 0)

                       attempt-router
                       {:providers [{:id :ap :api-key "same"}]}]

                   (reset! auth-refresh-events {:ap (vec (repeat AUTH_REFRESH_WINDOW_MAX
                                                                 (System/currentTimeMillis)))})
                   (with-redefs [registry/provider-by-id
                                 (fn [_]
                                   {:provider/get-token-fn (fn []
                                                             {:token "same"})
                                    :provider/refresh-token-fn (fn [& _]
                                                                 (swap! refreshes inc))})]
                     (expect (false? (try-refresh-provider-token! attempt-router {:provider :ap})))
                     (expect (= 0 @refreshes))
                     (expect (= AUTH_REFRESH_WINDOW_MAX (count (get @auth-refresh-events :ap))))))
                 (reset! auth-refresh-events {})))

(def ^:private env-cache (deref #'lp/cache))

(def ^:private new-cache-entry (deref #'lp/new-cache-entry))

(def ^:private touch-entry! (deref #'lp/touch-entry!))

(def ^:private evict-if-idle! (deref #'lp/evict-if-idle!))

(defn- backdate-entry!
  "Push `entry`'s :last-active `ms` into the past so it reads as idle."
  [entry ms]
  (let [^java.util.concurrent.atomic.AtomicLong la (:last-active entry)]
    (.set la (- (System/currentTimeMillis) (long ms)))))

(defdescribe env-reaper-test
             ;; The idle-env reaper is the authoritative backstop against unbounded
             ;; Python session growth. An empty {} env is safe to dispose:
             ;; dispose-environment! no-ops with no :python-context / :db-info.
             (describe "evict-if-idle!"
                       (it "disposes + evicts an idle, unlocked entry"
                           (let [k
                                 "reaper-test/idle"

                                 entry
                                 (new-cache-entry {})]

                             (swap! env-cache assoc k entry)
                             (try (backdate-entry! entry 10000)
                                  (expect (true? (evict-if-idle! k 5000)))
                                  (expect (not (contains? @env-cache k)))
                                  (finally (swap! env-cache dissoc k)))))
                       (it
                         "skips an entry whose lock is held (a running turn)"
                         (let [k
                               "reaper-test/busy"

                               entry
                               (new-cache-entry {})

                               ^java.util.concurrent.locks.ReentrantLock lock
                               (:lock entry)]

                           (swap! env-cache assoc k entry)
                           ;; A running turn holds the lock on ANOTHER thread;
                           ;; ReentrantLock is reentrant, so the lock MUST be
                           ;; held off-thread for tryLock to genuinely fail.
                           (let [held
                                 (promise)

                                 release
                                 (promise)

                                 holder
                                 (Thread. ^Runnable
                                          (fn []
                                            (.lock lock)
                                            (deliver held true)
                                            @release
                                            (.unlock lock)))]

                             (try (backdate-entry! entry 10000)
                                  (.start holder)
                                  @held
                                  (expect (false? (evict-if-idle! k 5000)))
                                  (expect (contains? @env-cache k))
                                  (finally (deliver release true)
                                           (.join holder 1000)
                                           (swap! env-cache dissoc k))))))
                       (it "keeps a freshly-touched (not-yet-idle) entry"
                           (let [k
                                 "reaper-test/warm"

                                 entry
                                 (new-cache-entry {})]

                             (swap! env-cache assoc k entry)
                             (try (touch-entry! entry)
                                  (expect (false? (evict-if-idle! k 60000)))
                                  (expect (contains? @env-cache k))
                                  (finally (swap! env-cache dissoc k)))))))

(def ^:private reap-idle-envs! (deref #'lp/reap-idle-envs!))

(def ^:private memory-pressure? (deref #'lp/memory-pressure?))

(defdescribe env-memory-pressure-test
             ;; Layer 3: under process memory pressure the reaper force-evicts EVERY
             ;; idle (unlocked) env this sweep, ignoring the idle TTL, to shed
             ;; Python sessions fast. A running turn (lock held off-thread) is
             ;; still skipped; the transcript reloads from the DB.
             (describe "reap-idle-envs! under memory pressure"
                       (it "force-evicts fresh, unlocked entries when pressured"
                           (let [k "watermark-test/fresh"]
                             (swap! env-cache assoc k (new-cache-entry {}))
                             (try
                               ;; not idle (just touched) + default 15m TTL: a
                               ;; normal sweep keeps it ...
                               (with-redefs [lp/memory-pressure? (constantly false)]
                                 (reap-idle-envs!)
                                 (expect (contains? @env-cache k)))
                               ;; ... but under pressure it is evicted now.
                               (with-redefs [lp/memory-pressure? (constantly true)]
                                 (expect (pos? (reap-idle-envs!)))
                                 (expect (not (contains? @env-cache k))))
                               (finally (swap! env-cache dissoc k)))))
                       (it "still skips a locked entry (a running turn) under pressure"
                           (let [k
                                 "watermark-test/busy"

                                 entry
                                 (new-cache-entry {})

                                 ^java.util.concurrent.locks.ReentrantLock lock
                                 (:lock entry)]

                             (swap! env-cache assoc k entry)
                             (let [held
                                   (promise)

                                   release
                                   (promise)

                                   holder
                                   (Thread. ^Runnable
                                            (fn []
                                              (.lock lock)
                                              (deliver held true)
                                              @release
                                              (.unlock lock)))]

                               (try (.start holder)
                                    @held
                                    (with-redefs [lp/memory-pressure? (constantly true)]
                                      (reap-idle-envs!)
                                      (expect (contains? @env-cache k)))
                                    (finally (deliver release true)
                                             (.join holder 1000)
                                             (swap! env-cache dissoc k))))))
                       (it "memory-pressure? is disabled when the RSS gate is off"
                           (expect (false? (with-redefs [lp/env-rss-budget-mb (delay 0)]
                                             (memory-pressure?)))))
                       (it "memory-pressure? fires on the RSS budget"
                           (expect (true? (with-redefs [lp/env-rss-budget-mb (delay 1)]
                                            (memory-pressure?)))))))

(def ^:private bump-turns! (deref #'lp/bump-turns!))

(def ^:private recycle-env! (deref #'lp/recycle-env!))

(defdescribe env-recycle-test
             ;; Layer 2: a single long-lived (never-idle) session's Context is
             ;; recycled between turns after `env-max-turns-per-ctx` turns — dispose
             ;; + rebuild IN PLACE, reusing the same lock so a queued caller stays
             ;; correct.
             (describe "bump-turns!"
                       (it "increments the per-context counter and returns the count"
                           (let [entry (new-cache-entry {})]
                             (expect (= 1 (bump-turns! entry)))
                             (expect (= 2 (bump-turns! entry)))
                             (expect (= 3 (bump-turns! entry)))))
                       (it "returns 0 for an entry with no counter"
                           (expect (= 0 (bump-turns! {})))))
             (describe
               "recycle-env!"
               (it
                 "swaps a fresh env in place, reuses the lock, disposes the old"
                 (let [k
                       "recycle-test/turn-cap"

                       old-env
                       {:marker :old}

                       fresh-env
                       {:marker :fresh}

                       entry
                       (new-cache-entry old-env)

                       disposed
                       (atom [])]

                   (swap! env-cache assoc k entry)
                   (try (with-redefs [lp/open-env!
                                      (fn [_ _]
                                        fresh-env)

                                      lp/dispose-environment!
                                      (fn [e]
                                        (swap! disposed conj e))]

                          (recycle-env! k))
                        (let [e2 (get @env-cache k)]
                          ;; fresh env installed under the same key
                          (expect (= fresh-env (:environment e2)))
                          ;; SAME lock preserved so a queued caller stays correct
                          (expect (identical? (:lock entry) (:lock e2)))
                          ;; turn counter reset for the fresh context
                          (expect (= 0 (.get ^java.util.concurrent.atomic.AtomicLong (:turns e2))))
                          ;; the OLD env disposed exactly once
                          (expect (= [old-env] @disposed)))
                        (finally (swap! env-cache dissoc k)))))))

(defn- with-reload-cache
  "Isolate reload lifecycle tests from the global cache and record sandbox disposal."
  [f]
  (with-redefs [lp/cache
                (atom {})

                lp/policy-reload-epoch
                (atom 0)

                lp/env-max-turns-per-ctx
                (delay 0)]

    (let [id
          (java.util.UUID/randomUUID)

          environment
          {:marker :old}

          entry
          (new-cache-entry environment)

          disposed
          (atom [])]

      (swap! lp/cache assoc id entry)
      (with-redefs [env/context-enterable?
                    (constantly true)

                    env/dispose-sandbox!
                    #(swap! disposed conj %)

                    lp/open-env!
                    (fn [_ _]
                      (throw (ex-info "unexpected eager rebuild" {})))]

        (f id entry disposed)))))

(defdescribe
  reload-sandbox-lifecycle-test
  (it "defers a reload on the turn's own thread until the turn returns"
      (with-reload-cache (fn [id entry disposed]
                           (with-redefs [lp/turn! (fn [environment _ _]
                                                    (lp/mark-policy-reload!)
                                                    (expect (empty? @disposed))
                                                    (expect (= (:environment entry) environment))
                                                    :finished)]
                             (expect (= :finished (lp/send! id "reload")))
                             (expect (= [(:environment entry)] @disposed))
                             (expect (identical? entry (get @lp/cache id)))))))
  (it "closes a busy sandbox after the turn even when turn or bookkeeping fails"
      (doseq [failure [nil :turn :bookkeeping]]
        (with-reload-cache
          (fn [id entry disposed]
            (let [started (promise)
                  release (promise)
                  touch @#'lp/touch-entry!]

              (with-redefs [lp/turn! (fn [_ _ _]
                                       (deliver started true)
                                       (when (= ::timeout (deref release 5000 ::timeout))
                                         (throw (ex-info "test turn was not released" {})))
                                       (when (= failure :turn) (throw (ex-info "turn failed" {})))
                                       :finished)
                            lp/touch-entry! (fn [cur]
                                              (when (= failure :bookkeeping)
                                                (throw (ex-info "bookkeeping failed" {})))
                                              (touch cur))]

                (let [turn (future (try (lp/send! id "busy") (catch Exception _ :failed)))]
                  (try (expect (= true (deref started 5000 ::timeout)))
                       (dotimes [_ 2]
                         (lp/mark-policy-reload!))
                       (expect (empty? @disposed))
                       (deliver release true)
                       (expect (= (if (= failure :turn) :failed :finished)
                                  (deref turn 5000 ::timeout)))
                       (expect (= [(:environment entry)] @disposed))
                       (expect (not (.isLocked ^java.util.concurrent.locks.ReentrantLock
                                               (:lock entry))))
                       (finally (deliver release true) (future-cancel turn))))))))))
  (it "does not close a replacement entry while holding its predecessor's lock"
      (with-reload-cache
        (fn [id _ disposed]
          (let [fresh
                (new-cache-entry {:marker :replacement})

                lock
                (proxy [java.util.concurrent.locks.ReentrantLock] []
                  (tryLock [] (swap! lp/cache assoc id fresh) (proxy-super tryLock)))]

            (swap! lp/cache assoc id (assoc (new-cache-entry {:marker :displaced}) :lock lock))
            (lp/mark-policy-reload!)
            (expect (empty? @disposed))
            (expect (identical? fresh (get @lp/cache id)))))))
  (it "keeps a failed rebuild stale and reports its error instead of entering a closed sandbox"
      (with-reload-cache (fn [id entry _]
                           (lp/mark-policy-reload!)
                           (with-redefs [env/context-enterable?
                                         (constantly false)

                                         lp/turn!
                                         (fn [& _]
                                           (throw (ex-info "must not run a turn" {})))]

                             (expect (= "unexpected eager rebuild"
                                        (try (lp/send! id "retry rebuild")
                                             nil
                                             (catch clojure.lang.ExceptionInfo e (ex-message e)))))
                             (expect (identical? entry (get @lp/cache id)))
                             (expect (not (.isLocked ^java.util.concurrent.locks.ReentrantLock
                                                     (:lock entry)))))))))

(defdescribe reload-unbuilt-sandbox-test
             (it "does not start a worker just to close it"
                 (with-redefs [lp/cache
                               (atom {})

                               lp/policy-reload-epoch
                               (atom 0)]

                   (let [environment
                         {:python-sandbox (delay (throw (ex-info "must stay lazy" {})))
                          :python-context-retired-atom (atom false)}

                         entry
                         (new-cache-entry environment)]

                     (swap! lp/cache assoc (java.util.UUID/randomUUID) entry)
                     (lp/mark-policy-reload!)
                     (expect (not (realized? (:python-sandbox environment))))
                     (expect @(:python-context-retired-atom environment))))))

;; Regression, issue #106: a Settings flip only reached live tool bindings when it
;; arrived through the gateway HTTP handler, which called
;; `sync-cached-extension-symbols!` inline. A flip made anywhere else — the TUI
;; settings dialog calls `toggles/set-enabled!` directly, as does any extension —
;; persisted to state.yml and refreshed nothing: every other cached session kept
;; its stale tool surface (no `shell`, no `subprocess`) until a restart.
(defdescribe
  toggle-change-refreshes-cached-envs-test
  (describe
    "a toggle change from ANY channel"
    (it "refreshes extension bindings in every idle cached env"
        (toggles/register-toggle! {:id "loop_test_fanout" :label "Fan-out" :default false})
        (let [k
              "toggle-fanout-test/idle"

              entry
              (new-cache-entry {:marker :idle})

              synced
              (atom [])]

          (swap! env-cache assoc k entry)
          (try (with-redefs [lp/sync-active-extension-symbols! (fn [e]
                                                                 (swap! synced conj e))]
                 ;; NOT the HTTP handler: the bare toggles API the TUI
                 ;; dialog and every extension flip goes through.
                 (toggles/set-enabled! "loop_test_fanout" true))
               (expect (some #(= {:marker :idle} %) @synced))
               (finally (swap! env-cache dissoc k)
                        (toggles/set-enabled! "loop_test_fanout" false)))))
    (it "leaves a session that is mid-turn to its own next-turn sync"
        (toggles/register-toggle!
          {:id "loop_test_fanout_busy" :label "Fan-out busy" :default false})
        (let [k
              "toggle-fanout-test/busy"

              entry
              (new-cache-entry {:marker :busy})

              ^java.util.concurrent.locks.ReentrantLock lock
              (:lock entry)

              release
              (promise)

              started
              (promise)

              holder
              (doto (Thread. ^Runnable
                             (fn []
                               (.lock lock)
                               (deliver started true)
                               (try @release (finally (.unlock lock)))))
                (.setDaemon true)
                (.start))

              synced
              (atom [])]

          (swap! env-cache assoc k entry)
          (try @started
               (with-redefs [lp/sync-active-extension-symbols! (fn [e]
                                                                 (swap! synced conj e))]
                 (toggles/set-enabled! "loop_test_fanout_busy" true))
               (expect (not-any? #(= {:marker :busy} %) @synced))
               (finally (deliver release true)
                        (.join holder 1000)
                        (swap! env-cache dissoc k)
                        (toggles/set-enabled! "loop_test_fanout_busy" false)))))))

(defdescribe env-reaper-enablement-test
             (it "starts for the RSS budget even when every older policy is off"
                 (let [enabled? (deref #'lp/env-reaper-enabled?)]
                   (expect (true? (with-redefs [lp/env-reaper-interval-ms (delay 1000)
                                                lp/env-idle-ttl-ms (delay 0)
                                                lp/env-cache-max (delay 0)
                                                lp/env-rss-budget-mb (delay 1)]

                                    (enabled?))))
                   (expect (false? (with-redefs [lp/env-reaper-interval-ms (delay 1000)
                                                 lp/env-idle-ttl-ms (delay 0)
                                                 lp/env-cache-max (delay 0)
                                                 lp/env-rss-budget-mb (delay 0)]

                                     (enabled?))))))
             (it "samples bounded runtime metrics without mutating the cache"
                 (let [before
                       (count @env-cache)

                       snapshot
                       (lp/gateway-runtime-metrics)]

                   (expect (= before (:env-cache-size snapshot)))
                   (expect (pos? (:jvm-heap-max-bytes snapshot)))
                   (expect (pos? (:process-rss-bytes snapshot)))
                   (expect (not (neg? (:jvm-gc-count-total snapshot))))
                   (expect (pos? (:jvm-thread-count snapshot))))))

(defdescribe env-rss-pressure-test
             (it "detects the native memory an embedded interpreter holds outside the JVM heap"
                 (let [pressure? (deref #'lp/memory-pressure?)]
                   (with-redefs-fn {#'lp/env-rss-budget-mb (delay 1)
                                    #'lp/process-rss-bytes (constantly (* 2 1024 1024))}
                     (fn []
                       (expect (true? (pressure?))))))))

(defdescribe
  emergency-context-fold-projection-test
  (describe
    "one-shot overflow rescue"
    (it
      "folds the OLDEST settled work only, shrinks wire input, and leaves canonical history unchanged"
      (let [large
            (apply str (repeat 20000 "x"))

            content
            [{:type "text" :text large}
             {:type "tool_use" :id "tc" :name "grep" :input {"query" "x"}}]

            trailer
            [(stub-tool-iter {:id 1 :content content}) (stub-tool-iter {:id 2 :content content})]

            original
            trailer

            recovery
            (emergency-fold-projection [{:role "system" :content "stable"}]
                                       trailer
                                       []
                                       {:provider :openai :model "gpt"}
                                       "gpt-4o"
                                       (constantly 1000000))]

        (expect (some? recovery))
        (expect (< (:after-tokens recovery) (:before-tokens recovery)))
        ;; Graduated: the newest iteration survives verbatim because folding the
        ;; oldest one already fits the budget.
        (expect (= #{"t1/i1"} (:scopes recovery)))
        (expect (= original trailer))
        (expect (= "stable"
                   (-> recovery
                       :messages
                       first
                       :content)))
        (expect (some #(re-find #"Emergency transport fold" (str (:content %)))
                      (:messages recovery)))
        (expect (some #(re-find #"1 tool call" (str (:content %))) (:messages recovery)))))
    (it "treats tool-only payloads as foldable context"
        ;; Boundary contract: Svar owns canonical structured-message token counting;
        ;; overflow rescue must inherit that count instead of treating tool payloads as free.
        (let [large
              (apply str (repeat 8000 "x"))

              content
              [{:type "tool_use" :id "tc" :name "python_execution" :input {"code" large}}]

              trailer
              [(stub-tool-iter {:id 1 :content content}) (stub-tool-iter {:id 2 :content content})]

              recovery
              (emergency-fold-projection []
                                         trailer
                                         []
                                         {:provider :openai :model "gpt-4o"}
                                         "gpt-4o"
                                         (fn [n]
                                           (- (long n) 100)))]

          (expect (some? recovery))
          (expect (< 1000 (long (:before-tokens recovery))))
          (expect (pos? (long (:saved-tokens recovery))))))
    (it "measures the estimator's undercount from the refused request, never a constant"
        (let [;; Session cd24926e: the provider priced the very same 132-iteration seed at
              ;; 1,437,952 where the local estimator read 963,503.
              factor
              (estimator-undercount 1437952 963503)

              budget
              (overflow-fold-budget {:reported-tokens 1437952 :reported-limit 1000000 :margin 0.9}
                                    963503)]

          (expect (< 1.49 (double factor) 1.5))
          ;; A DIFFERENT mix measures differently — that is the point of measuring.
          (expect (< 1.09 (double (estimator-undercount 1100 1000)) 1.11))
          ;; A generous estimator buys no extra room, and an unmeasurable side stays nil.
          (expect (= 1.0 (estimator-undercount 500 1000)))
          (expect (nil? (estimator-undercount nil 963503)))
          (expect (nil? (estimator-undercount 1437952 0)))
          ;; The budget is spent in LOCAL currency, and priced back through the measured
          ;; factor it lands under the provider's limit instead of hoping to.
          (expect (< 600000 (long budget) 606000))
          (expect (< (* (double budget) (double factor)) 1000000.0))
          ;; Blind path: no provider numbers to measure, so bisect our own estimate only.
          (expect (= 5000 (overflow-fold-budget {:cut 0.5} 10000)))
          (expect (= 5000 (overflow-fold-budget {:reported-limit 999 :cut 0.5} 10000)))
          (expect (nil? (overflow-fold-budget {} 10000)))))
    (it
      "escalates: each rescue folds strictly more, then goes terminal"
      (let [content
            [{:type "text" :text (apply str (repeat 5000 "x"))}
             {:type "tool_use" :id "tc" :name "grep" :input {"query" "x"}}]

            trailer
            (mapv (fn [i]
                    (stub-tool-iter {:id i :content content}))
                  (range 1 13))

            state
            (atom {:attempts 0})

            overflow
            (ex-info "Context overflow"
                     {:type :svar.core/context-overflow
                      :source :preflight
                      :input-tokens 20000
                      :max-input-tokens 10000})

            rescue
            (fn []
              (context-overflow-recovery! {:error overflow
                                           :output-started? (atom false)
                                           :recovery-state state
                                           :ctx-atom (atom {})
                                           :turn-input-tokens 0
                                           :base-messages []
                                           :trailer-iters trailer
                                           :summaries []
                                           :replay-target {:provider :openai :model "gpt"}
                                           :replay-policies {}
                                           :model "gpt-4o"}))

            rescues
            (mapv (fn [_]
                    (rescue))
                  (range 4))]

        (expect (= [1 2 3 nil] (mapv :attempt rescues)))
        (expect (nil? (last rescues)))
        ;; Every rescue keeps recent work; only the oldest prefix collapses.
        (expect (every? #(< (count (:scopes %)) (count trailer)) (butlast rescues)))
        (expect (apply < (mapv #(count (:scopes %)) (butlast rescues))))
        (expect (apply > (mapv :after-tokens (butlast rescues))))
        (expect (every? #(<= (long (:after-tokens %)) (long (:budget-tokens %))) (butlast rescues)))
        ;; Every rescue reports the undercount it measured, and its projection priced
        ;; through that measurement stays under the limit the provider refused.
        (expect (every? #(some? (:estimator-undercount %)) (butlast rescues)))
        (expect (every? #(= :preflight (:rejection-source %)) (butlast rescues)))
        (expect (every? #(= :svar-message-estimate (:projection-count-source %)) (butlast rescues)))
        (expect (not-any? #(contains? % :provider-tokens) (butlast rescues)))
        (expect (every? #(< (* (double (:after-tokens %)) (double (:estimator-undercount %)))
                            (double (:reported-input-limit %)))
                        (butlast rescues)))
        (expect (not-any? #(contains? (:scopes %) "t1/i12") (butlast rescues)))))
    (it "preserves existing semantic fold gists"
        (let [large
              (apply str (repeat 5000 "x"))

              content
              [{:type "text" :text large}
               {:type "tool_use" :id "tc" :name "grep" :input {"query" "x"}}]

              trailer
              [(stub-tool-iter {:id 1 :content content}) (stub-tool-iter {:id 2 :content content})]

              recovery
              (emergency-fold-projection
                []
                trailer
                [{"scopes" #{"t1/i1"} "gist" "IMPORTANT ROOT CAUSE" "at_turn" 1}]
                {:provider :openai :model "gpt"}
                "gpt-4o"
                (constantly 1000000))

              contents
              (mapv (comp str :content) (:messages recovery))]

          (expect (= #{"t1/i2"} (:scopes recovery)))
          (expect (some #(str/includes? % "IMPORTANT ROOT CAUSE") contents))
          (expect (some #(str/includes? % "Emergency transport fold") contents))))
    (it "refuses a retry whose folded estimate still exceeds the provider budget"
        (let [content
              [{:type "text" :text (apply str (repeat 5000 "x"))}
               {:type "tool_use" :id "tc" :name "grep" :input {"query" "x"}}]

              trailer
              [(stub-tool-iter {:id 1 :content content})]]

          (expect (nil? (emergency-fold-projection []
                                                   trailer
                                                   []
                                                   {:provider :openai :model "gpt"}
                                                   "gpt-4o"
                                                   (constantly 1))))))
    (it "distinguishes replay-safe lifecycle chunks from output and side effects"
        (expect (false? (provider-output-chunk? {:phase :provider-call})))
        (expect (false? (provider-output-chunk? {:phase :response-parse})))
        (doseq [phase [:reasoning :content :assistant-prose]]
          (expect (false? (provider-output-chunk? {:phase phase :delta ""})))
          (expect (true? (provider-output-chunk? {:phase phase :delta "output"}))))
        (doseq [phase [:form-start :tool-start :form-result]]
          (expect (true? (provider-output-chunk? {:phase phase})))))
    (it
      "performs exactly one smaller retry, preserves live and canonical input, then terminates"
      (let [large
            (apply str (repeat 30000 "x"))

            content
            [{:type "text" :text large}
             {:type "tool_use" :id "tc" :name "grep" :input {"query" "x"}}]

            canonical
            [(stub-tool-iter {:id 1 :content content})]

            original
            canonical

            base
            [{:role "system" :content "stable"} {:role "user" :content "CURRENT USER REQUEST"}]

            calls
            (atom [])

            rescue-state
            (atom {:attempts 0})

            output?
            (atom false)

            ctx-atom
            (atom {"session_turn" 1})

            overflow
            (ex-info "Context overflow"
                     {:type :svar.tokens/context-overflow
                      :source :preflight
                      :input-tokens 20000
                      :max-input-tokens 10000})

            terminal
            (loop [messages (into base
                                  (conversation-suffix canonical {:provider :openai :model "gpt"}))]
              (swap! calls conj messages)
              (let [result (try (throw overflow) (catch Exception e e))]
                (if-let [recovery (context-overflow-recovery! {:error result
                                                               :output-started? output?
                                                               :recovery-state rescue-state
                                                               :ctx-atom ctx-atom
                                                               :turn-input-tokens 0
                                                               :base-messages base
                                                               :trailer-iters canonical
                                                               :summaries []
                                                               :replay-target {:provider :openai
                                                                               :model "gpt"}
                                                               :replay-policies {}
                                                               :model "gpt-4o"})]
                  (recur (:messages recovery))
                  result)))]

        (expect (= 2 (count @calls)))
        (expect (< (count (pr-str (second @calls))) (count (pr-str (first @calls)))))
        (expect (every? #(some (fn [m]
                                 (= "CURRENT USER REQUEST" (:content m)))
                               %)
                        @calls))
        (expect (= original canonical))
        (expect (= :svar.tokens/context-overflow (:type (ex-data terminal))))
        (expect (= 20000 (get-in @ctx-atom ["engine_utilization" "last_request_tokens"])))
        (expect (= 10000 (get-in @ctx-atom ["engine_utilization" "model_input_limit"])))))
    (it "has an independent retry budget"
        (expect (= [2 1]
                   (next-retry-counters ::lp/retry-context-overflow
                                        {:attempt 2 :max-tokens-attempt 1}))))))

(defn- overflow-loop-scenario
  "Exercise overflow handling, Python execution and the following provider request."
  [{:keys [carried responses request-estimate]}]
  (let [router
        (svar/make-router [{:id :lmstudio
                            :base-url "http://127.0.0.1:1234/v1"
                            :api-key "test"
                            :models [{:name "model" :input-limit 50000}]}])

        environment
        (lp/create-environment router {:db :memory})

        tid
        (persistance/db-store-session-turn! (:db-info environment)
                                            {:parent-session-id (:session-id environment)
                                             :user-request "CURRENT REQUEST"})

        requests
        (atom [])

        signals
        (atom [])

        successes
        (atom 0)

        summaries
        (#'lp/current-session-summaries environment)

        replacements
        (cond-> {#'lp/previous-turn-context
                 (fn [& _]
                   [{:turn 1 :user-request "PRIOR REQUEST" :answer "PRIOR OUTCOME"}])
                 #'svar/ask-code!
                 (fn [_ opts]
                   (let [messages
                         (:messages opts)

                         n
                         (svar-router/count-messages "model" messages)]

                     (swap! requests conj messages)
                     (if (> n 50000)
                       (throw (ex-info "Context overflow"
                                       {:type :svar.core/context-overflow
                                        :source :preflight
                                        :input-tokens n
                                        :max-input-tokens 50000}))
                       (let [{:keys [code text]}
                             (nth responses @successes {})

                             id
                             (str "call-" (swap! successes inc))

                             call
                             {:id id :name "python_execution" :input {:code code}}]

                         (merge {:api-usage {:input-tokens n :output-tokens 1} :tokens {}}
                                (if code
                                  (cond-> {:stop-reason :tool-calls :tool-calls [call]}
                                    text
                                    (assoc :assistant-message
                                      {:role "assistant"
                                       :content [{:type "text" :text text}
                                                 (assoc call :type "tool_use")]}))
                                  {:stop-reason :end :tool-calls [] :content "done"}))))))}
          carried
          (assoc #'lp/resumable-prompt-message-base
            (fn [_history _provider _model _context _turn ledger _stable _current]
              {:messages carried :summaries ledger :resumed? true}))

          request-estimate
          (assoc #'lp/request-context-estimator
            (fn [& _]
              request-estimate)))]

    (try (with-redefs-fn replacements
           (fn []
             (tel/with-handler
               ::overflow-test
               (fn [signal]
                 (when (contains? #{::lp/context-token-counts ::lp/context-overflow-emergency-fold
                                    ::lp/context-overflow-terminal ::lp/context-proactive-fold}
                                  (:id signal))
                   (swap! signals conj (select-keys signal [:id :data]))))
               {:async? false}
               (lp/iteration-loop environment "CURRENT REQUEST" {:session-turn-id tid}))))
         {:requests @requests
          :counts (mapv :data (filter #(= ::lp/context-token-counts (:id %)) @signals))
          :rescues (mapv :data (filter #(= ::lp/context-overflow-emergency-fold (:id %)) @signals))
          :proactive (mapv :data (filter #(= ::lp/context-proactive-fold (:id %)) @signals))
          :summaries-unchanged? (= summaries (#'lp/current-session-summaries environment))}
         (finally (lp/dispose-environment! environment)))))

(defdescribe
  resumed-context-overflow-test
  ;; Regression: a refused carried prefix hid all prior settled work from the
  ;; first iteration's emergency fold. Force a missed estimate so these cases
  ;; keep exercising real preflight rejection independently of proactive folding.
  (it "rebuilds a refused carried prefix and keeps the next iteration canonical"
      (let [carried
            [{:role "system" :content "stable"}
             {:role "assistant"
              :content (str "CARRIED PAYLOAD " (apply str (repeat 80000 "old work ")))}
             {:role "user" :content "CURRENT REQUEST"}]

            {:keys [requests counts rescues summaries-unchanged?]}
            (overflow-loop-scenario {:carried carried
                                     :request-estimate (constantly 0)
                                     :responses [{:code "print('LIVE RESULT')"} {}]})]

        (expect (= 3 (count requests)))
        (expect (> (svar-router/count-messages "model" (first requests)) 50000))
        (expect (= [:resumed :canonical :canonical] (mapv :prompt-base counts)))
        (expect (= [0 1 0] (mapv :context-recovery-attempt counts)))
        (expect (= [:canonical-rebuild] (mapv :projection-kind rescues)))
        (expect summaries-unchanged?)
        (doseq [messages (rest requests)]
          (let [text (str messages)]
            (expect (not (str/includes? text "CARRIED PAYLOAD")))
            (expect (str/includes? text "CURRENT REQUEST"))
            (expect (str/includes? text "PRIOR OUTCOME"))
            (expect (< (svar-router/count-messages "model" messages) 50000))))
        (expect (str/includes? (str (last requests)) "LIVE RESULT"))))
  (it "retains a transport fold across the next successful iteration without changing the ledger"
      (let [{:keys [requests counts rescues summaries-unchanged?]}
            (overflow-loop-scenario
              {:request-estimate (constantly 0)
               :responses [{:code "print('old result')"
                            :text (str "SETTLED PAYLOAD " (apply str (repeat 80000 "old work ")))}
                           {:code "print('LIVE RESULT')"} {}]})]
        (expect (= 4 (count requests)))
        (expect (= [:succeeded :context-overflow :succeeded :succeeded] (mapv :outcome counts)))
        (expect (= [:emergency-fold] (mapv :projection-kind rescues)))
        (expect summaries-unchanged?)
        (doseq [messages (drop 2 requests)]
          (expect (not (str/includes? (str messages) "SETTLED PAYLOAD")))
          (expect (str/includes? (str messages) "Emergency transport fold")))
        (expect (str/includes? (str (last requests)) "LIVE RESULT")))))

(defdescribe proactive-context-fold-test
             ;; A low previous usage/hint must not let an oversized pending request reach
             ;; preflight. Both inherited history and newly completed tool work are foldable.
             (it "rebuilds an oversized resumed base before the first provider request"
                 (let [carried
                       [{:role "system" :content "stable"}
                        {:role "assistant"
                         :content (str "CARRIED PAYLOAD " (apply str (repeat 80000 "old work ")))}
                        {:role "user" :content "CURRENT REQUEST"}]

                       {:keys [requests counts rescues summaries-unchanged?]}
                       (overflow-loop-scenario {:carried carried
                                                :responses [{:code "print('LIVE RESULT')"} {}]})]

                   (expect (= 2 (count requests)))
                   (expect (every? #(= :succeeded (:outcome %)) counts))
                   (expect (every? #(= :canonical (:prompt-base %)) counts))
                   (expect (every? #(zero? (:context-recovery-attempt %)) counts))
                   (expect (empty? rescues))
                   (expect summaries-unchanged?)
                   (doseq [messages requests]
                     (expect (<= (svar-router/count-messages "model" messages) 45000))
                     (expect (not (str/includes? (str messages) "CARRIED PAYLOAD")))
                     (expect (str/includes? (str messages) "CURRENT REQUEST"))
                     (expect (str/includes? (str messages) "PRIOR OUTCOME")))
                   (expect (str/includes? (str (last requests)) "LIVE RESULT"))))
             (it "folds growth before a follow-up request and does not resurrect it afterwards"
                 (let [{:keys [requests counts rescues proactive summaries-unchanged?]}
                       (overflow-loop-scenario
                         {:responses [{:code "print('old result')"
                                       :text (str "SETTLED PAYLOAD "
                                                  (apply str (repeat 80000 "old work ")))}
                                      {:code "print('LIVE RESULT')"} {}]})]
                   (expect (= 3 (count requests)))
                   (expect (every? #(= :succeeded (:outcome %)) counts))
                   (expect (empty? rescues))
                   (expect (= [:proactive-fold] (mapv :projection-kind proactive)))
                   (expect (every? #(not (contains? % :attempt)) proactive))
                   (expect summaries-unchanged?)
                   (doseq [messages (rest requests)]
                     (expect (<= (svar-router/count-messages "model" messages) 45000))
                     (expect (not (str/includes? (str messages) "SETTLED PAYLOAD")))
                     (expect (str/includes? (str messages) "Proactive transport fold")))
                   (expect (str/includes? (str (last requests)) "LIVE RESULT")))))

(defdescribe
  request-context-estimator-test
  (it "adds pending assistant/tool/user growth once to exact provider input, including cached input"
      (let [prior
            [{:role "system" :content "stable"} {:role "user" :content "old request"}]

            context
            {:id "same-tools-and-account" :fixed-prefix-weight 20}

            entry
            {:messages prior
             :input-tokens 10000
             :cache-read-tokens 10000
             :at-ms 0
             :prompt-cache-context context}

            estimate
            (#'lp/request-context-estimator {[:openai "gpt-4o"] entry} :openai "gpt-4o" context)

            tail
            [{:role "assistant" :content "new reasoning and answer"}
             {:role "user"
              :content [{:type "tool_result" :tool_use_id "call-1" :content "new result"}]}
             {:role "user" :content "CURRENT REQUEST"}]]

        (expect (= 10000 (estimate prior)))
        (expect (= (+ 10000
                      (- (svar-router/count-messages "gpt-4o" tail)
                         (svar-router/count-messages "gpt-4o" [])))
                   (estimate (into prior tail))))))
  (it "invalidates the usage anchor after a rewrite, route change, or fixed-prefix change"
      (let [prior
            [{:role "user" :content "old request"}]

            context
            {:id "original" :fixed-prefix-weight 20}

            entry
            {:messages prior :input-tokens 10000 :prompt-cache-context context}

            history
            {[:openai "gpt-4o"] entry}]

        (doseq [[provider model ctx messages]
                [[:other "gpt-4o" context prior] [:openai "other-model" context prior]
                 [:openai "gpt-4o" (assoc context :id "different-account") prior]
                 [:openai "gpt-4o" (assoc context :fixed-prefix-weight 21) prior]
                 [:openai "gpt-4o" nil prior]
                 [:openai "gpt-4o" context [{:role "user" :content "folded recap"}]]]]
          (expect (= (svar-router/count-messages model messages)
                     ((#'lp/request-context-estimator history provider model ctx) messages))))))
  (it "ignores missing or invalid usage"
      (let [messages
            [{:role "user" :content (apply str (repeat 1000 "reasoning "))}]

            context
            {:id "original" :fixed-prefix-weight 20}

            local
            (svar-router/count-messages "gpt-4o" messages)]

        (doseq [input [nil 0 -1]]
          (expect (= local
                     ((#'lp/request-context-estimator
                       {[:openai "gpt-4o"]
                        {:messages messages :input-tokens input :prompt-cache-context context}}
                       :openai
                       "gpt-4o"
                       context)
                       messages)))))))

(defdescribe
  provider-usage-calibration-regression-test
  ;; Issue #173: an accepted prefix must not be estimated a second time at 2.2x.
  (it "uses measured input for the exact prefix and counts only the new tail locally"
      (let [prior
            [{:role "user" :content (apply str (repeat 10000 "evidence "))}]

            tail
            [{:role "assistant" :content "new evidence"}]

            context
            {:id "same-route" :fixed-prefix-weight 20}

            estimate
            (#'lp/request-context-estimator
             {[:openai "gpt-4o"] {:messages prior :input-tokens 4500 :prompt-cache-context context}}
             :openai
             "gpt-4o"
             context)]

        (expect (> (svar-router/count-messages "gpt-4o" prior) 9000))
        (expect (= 4500 (estimate prior)))
        (expect (= (+ 4500
                      (- (svar-router/count-messages "gpt-4o" tail)
                         (svar-router/count-messages "gpt-4o" [])))
                   (estimate (into prior tail))))
        (expect (nil? (#'lp/pre-request-context-projection
                       {:request-messages (into prior tail)
                        :model "gpt-4o"
                        :budget-tokens 6000
                        :count-messages-fn estimate
                        :canonical-base-messages-fn (fn []
                                                      (throw (ex-info "Unexpected lossy fold"
                                                                      {})))})))))
  (it "counts only the tail when provider usage already priced the prefix"
      (let [prior
            [{:role "user" :content "accepted input"}]

            tail
            [{:role "assistant" :content "new output"}]

            context
            {:id "same-route" :fixed-prefix-weight 20}

            counted
            (atom [])]

        (with-redefs [svar-router/count-messages (fn ^long [_ messages]
                                                   (swap! counted conj (vec messages))
                                                   (+ 3 (count messages)))]
          (let [estimate (#'lp/request-context-estimator
                          {[:openai "gpt-4o"]
                           {:messages prior :input-tokens 500 :prompt-cache-context context}}
                          :openai
                          "gpt-4o"
                          context)]
            (expect (= 501 (estimate (into prior tail))))
            (expect (= [[] tail] @counted)))))))

(defdescribe
  pre-request-context-projection-test
  (it "does no canonical work below budget, but acts at the boundary including pending user input"
      (let [base
            [{:role "user" :content "CURRENT REQUEST"}]

            messages
            (conj base {:role "assistant" :content (apply str (repeat 1000 "old work "))})

            n
            (svar-router/count-messages "gpt-4o" messages)

            calls
            (atom 0)

            opts
            {:request-messages messages
             :base-messages messages
             :trailer-iters []
             :summaries []
             :replay-target {:provider :openai :model "gpt-4o"}
             :model "gpt-4o"
             :canonical-base-messages-fn (fn []
                                           (swap! calls inc)
                                           base)
             :canonical-trailer-iters []}]

        (expect (nil? (#'lp/pre-request-context-projection (assoc opts :budget-tokens (inc n)))))
        (expect (zero? @calls))
        (let [projection (#'lp/pre-request-context-projection (assoc opts :budget-tokens n))]
          (expect (= 1 @calls))
          (expect (= base (:messages projection)))
          (expect (= :canonical-rebuild (:projection-kind projection))))))
  (it "will not drop immutable input or install a nonshrinking or still-oversized projection"
      (let [request
            [{:role "user" :content (apply str (repeat 1000 "CURRENT REQUEST "))}]

            opts
            {:request-messages request
             :base-messages request
             :trailer-iters []
             :summaries []
             :replay-target {:provider :openai :model "gpt-4o"}
             :model "gpt-4o"
             :budget-tokens 100}]

        (expect (nil? (#'lp/pre-request-context-projection opts)))
        (expect (nil? (#'lp/pre-request-context-projection
                       (assoc opts
                         :canonical-base-messages-fn (constantly request)
                         :canonical-trailer-iters []))))))
  (it
    "uses measured prefix pressure even while the local estimate and last-input hint are small"
    (let [base
          [{:role "user" :content "CURRENT REQUEST"}]

          target
          {:provider :openai :model "gpt-4o"}

          trailer
          (mapv #(stub-tool-iter {:id %
                                  :content [{:type "text"
                                             :text (apply str
                                                     (repeat (if (= 3 %) 20000 1000) "work "))}]})
                [1 2 3])

          prior
          (into base (conversation-suffix (subvec trailer 0 2) target))

          request
          (into base (conversation-suffix trailer target))

          context
          {:id "same-prefix" :fixed-prefix-weight 20}

          estimate
          (#'lp/request-context-estimator
           {[:openai "gpt-4o"] {:messages prior :input-tokens 30000 :prompt-cache-context context}}
           :openai
           "gpt-4o"
           context)

          projection
          (#'lp/pre-request-context-projection
           {:request-messages request
            :base-messages base
            :trailer-iters trailer
            :summaries []
            :replay-target target
            :model "gpt-4o"
            :budget-tokens 45000
            :count-messages-fn estimate})]

      (expect (< (svar-router/count-messages "gpt-4o" request) 45000))
      (expect (>= (:before-tokens projection) 45000))
      (expect (= 1 (:folded-scopes projection)))
      (expect (<= (estimate (:messages projection)) 45000))
      (expect (str/includes? (str (:messages projection)) "CURRENT REQUEST")))))

(defdescribe
  pre-request-tool-result-fold-test
  (it
    "folds large tool results while retaining semantic gists and recent tool pairs"
    (let [base
          [{:role "user" :content "CURRENT REQUEST"}]

          target
          {:provider :openai :model "gpt-4o"}

          raw
          [(stub-tool-iter {:id 1})
           (assoc-in (stub-tool-iter {:id 2})
             [1 :forms-vec 0 :stdout]
             (str "TOOL PAYLOAD " (apply str (repeat 10000 "old work "))))
           (assoc-in (stub-tool-iter {:id 3}) [1 :forms-vec 0 :stdout] "LIVE RESULT")]

          ledger
          [{"scopes" #{"t1/i1"} "gist" "MEANINGFUL FINDING" "at_turn" 1}]

          visible
          (#'lp/apply-summaries raw ledger)

          request
          (into base (conversation-suffix visible target))

          projection
          (#'lp/pre-request-context-projection
           {:request-messages request
            :base-messages base
            :trailer-iters visible
            :summaries ledger
            :replay-target target
            :model "gpt-4o"
            :budget-tokens 1500})

          text
          (str (:messages projection))]

      (expect (= #{"t1/i2"} (:scopes projection)))
      (expect (= :proactive-fold (:projection-kind projection)))
      (expect (<= (svar-router/count-messages "gpt-4o" (:messages projection)) 1500))
      (expect (str/includes? text "MEANINGFUL FINDING"))
      (expect (str/includes? text "LIVE RESULT"))
      (expect (str/includes? text "tc-3"))
      (expect (not (str/includes? text "TOOL PAYLOAD"))))))

(defdescribe
  canonical-overflow-projection-test
  (it
    "folds a canonical rebuild when rebuilding alone is still over budget"
    (let [base
          [{:role "system" :content "stable"} {:role "user" :content "CURRENT REQUEST"}]

          request
          (conj base {:role "assistant" :content (apply str (repeat 30000 "old work "))})

          trailer
          [(stub-tool-iter {:id 1
                            :content [{:type "text" :text (apply str (repeat 5000 "work "))}]})
           (stub-tool-iter {:id 2
                            :content [{:type "text" :text (apply str (repeat 5000 "work "))}]})]

          n
          (svar-router/count-messages "gpt-4o" request)

          recovery
          (context-overflow-recovery!
            {:error (ex-info "Context overflow"
                             {:type :svar.core/context-overflow
                              :source :preflight
                              :input-tokens n
                              :max-input-tokens 8000})
             :output-started? (atom false)
             :recovery-state (atom {:attempts 0})
             :ctx-atom (atom {})
             :turn-input-tokens 0
             :request-messages request
             :base-messages request
             :trailer-iters []
             :summaries []
             :canonical-base-messages-fn (constantly base)
             :canonical-trailer-iters trailer
             :replay-target {:provider :openai :model "gpt-4o"}
             :model "gpt-4o"})]

      (expect (= :emergency-fold (:projection-kind recovery)))
      (expect (= n (:before-tokens recovery)))
      (expect (< (:after-tokens recovery) 7201))
      (expect (= base (:canonical-base-messages recovery)))
      (expect (some? (:summary recovery)))))
  (it "never rebuilds or retries after output, for unrelated errors, or without shrinkage"
      (let [request
            [{:role "user" :content "CURRENT REQUEST"}]

            overflow
            (ex-info "Context overflow" {:type :svar.core/context-overflow})]

        (doseq [[error output?] [[overflow true] [(ex-info "Unrelated" {}) false] [overflow false]]]
          (let [calls (atom 0)
                state (atom {:attempts 0})
                result (context-overflow-recovery! {:error error
                                                    :output-started? (atom output?)
                                                    :recovery-state state
                                                    :ctx-atom (atom {})
                                                    :turn-input-tokens 0
                                                    :request-messages request
                                                    :base-messages request
                                                    :trailer-iters []
                                                    :summaries []
                                                    :canonical-base-messages-fn (fn []
                                                                                  (swap! calls inc)
                                                                                  request)
                                                    :canonical-trailer-iters []
                                                    :replay-target {:provider :openai
                                                                    :model "gpt-4o"}
                                                    :model "gpt-4o"})]

            (expect (nil? result))
            (expect (nil? (:last-after-tokens @state)))
            (expect (= (if (and (= error overflow) (not output?)) 1 0) @calls)))))))

(defdescribe attachment-reinspection-wire-test
             (it "renders a reinspection image as a canonical vision message"
                 (let [wired-images
                       (deref #'lp/iteration-wired-images)

                       image-messages
                       (deref #'lp/iteration-image-messages)

                       msg
                       (first (image-messages {:images (wired-images {:reinspect-attachments
                                                                      [{:id "att-1"
                                                                        :media-type "image/png"
                                                                        :base64
                                                                        replay-png-b64}]})}))]

                   (expect (= "user" (:role msg)))
                   (expect (= "image_url" (get-in msg [:content 0 :type])))
                   (expect (= (str "data:image/png;base64," replay-png-b64)
                              (get-in msg [:content 0 :image_url :url]))))))

(def ^:private env-gap-router-error (deref #'lp/env-gap-router-error))

(defdescribe
  env-gap-router-error-test
  (describe
    "an empty fleet caused by an unset ${NAME}"
    (it "names the variable instead of svar's generic 'at least one provider'"
        (let [cfg
              {:providers
               [{:id :rbi-genai :api-key "${VIS_TEST_UNSET_RBI_KEY}" :models [{:name "gpt-4o"}]}]}

              svar-err
              (ex-info "make-router requires at least one provider" {:type :svar/no-providers})

              restated
              (env-gap-router-error cfg svar-err)]

          (expect (str/includes? (ex-message restated) "VIS_TEST_UNSET_RBI_KEY"))
          (expect (str/includes? (ex-message restated) "rbi-genai"))
          ;; The TUI routes on the cause chain + `:type` — both must survive.
          (expect (= :svar/no-providers (:type (ex-data restated))))
          (expect (:vis/user-error (ex-data restated)))
          (expect (= {:rbi-genai ["VIS_TEST_UNSET_RBI_KEY"]} (:env-gaps (ex-data restated))))
          (expect (identical? svar-err (.getCause ^Throwable restated)))))
    (it "restates through a wrapping exception too"
        (let [cfg
              {:providers [{:id :rbi-genai :api-key "${VIS_TEST_UNSET_RBI_KEY}"}]}

              wrapped
              (ex-info "creating session"
                       {}
                       (ex-info "make-router requires at least one provider"
                                {:type :svar/no-providers}))]

          (expect (str/includes? (ex-message (env-gap-router-error cfg wrapped))
                                 "VIS_TEST_UNSET_RBI_KEY")))))
  (describe "anything else"
            (it "is returned untouched — no env gap, or not a no-providers failure"
                (let [gapped
                      {:providers [{:id :rbi-genai :api-key "${VIS_TEST_UNSET_RBI_KEY}"}]}

                      resolved
                      {:providers [{:id :ok :api-key "sk-literal"}]}

                      svar-err
                      (ex-info "make-router requires at least one provider"
                               {:type :svar/no-providers})

                      other
                      (ex-info "boom" {})]

                  (expect (identical? svar-err (env-gap-router-error resolved svar-err)))
                  (expect (identical? other (env-gap-router-error gapped other)))))))

(defdescribe
  non-correctable-provider-error-test
  "A provider failure the model cannot fix (rate limit, auth, spend cap) must END
   the turn instead of being fed back as a synthetic user message — feeding it
   back re-asks the SAME provider that just refused, which surfaced as repeated
   question/answer pairs before the consecutive-error limit killed the turn."
  (let [fatal? (fn [e]
                 (boolean (::lp/fatal-iteration-error
                            (lp/handle-iteration-exception!
                              e
                              {:iteration 2 :messages [{:role "user" :content "hi"}]}))))]
    (it "fails the turn on a rate limit"
        (expect (fatal? (ex-info "provider rate limited this request"
                                 {:status 429
                                  :provider :anthropic-coding-plan
                                  :body "{\"error\":{\"type\":\"rate_limit_error\"}}"}))))
    (it "fails the turn on an auth failure"
        (expect (fatal? (ex-info "unauthorized" {:status 401 :provider :openai-codex}))))
    (it "fails the turn when svar classifies an account limit as quota exhausted"
        (expect (fatal? (ex-info
                          "Exceptional status code: 400"
                          {:status 400
                           :provider :anthropic-coding-plan
                           :body (str "{\"type\":\"error\",\"error\":{\"type\":"
                                      "\"invalid_request_error\",\"message\":"
                                      "\"Third-party apps now draw from your extra usage.\"}}")}))))
    (it "still feeds a correctable model/code failure back for self-correction"
        (expect (not (fatal? (ex-info "Syntax error in generated code" {:type :vis/code-error})))))
    (it "still feeds a plain internal bug back for self-correction"
        (expect (not (fatal? (ex-info "assert failed" {}))))))
  (it "fails once when a wrapped rate-limit error reaches the gateway"
      ;; Reproduces the HTTP/client wrapper seen by the gateway: the outer
      ;; exception is untyped, while its cause carries the 429 provider data.
      ;; It must be terminal, otherwise iteration-loop adds synthetic user
      ;; feedback and asks the same rate-limited provider again.
      (let [result (lp/handle-iteration-exception!
                     (ex-info "HTTP client request failed"
                              {}
                              (ex-info "provider rate limited this request"
                                       {:status 429
                                        :provider :anthropic-coding-plan
                                        :body "{\"error\":{\"type\":\"rate_limit_error\"}}"}))
                     {:iteration 2 :messages [{:role "user" :content "hi"}]})]
        (expect (true? (::lp/fatal-iteration-error result)))))
  (it "fails once when a wrapped invalid-key error reaches the gateway"
      (let [result (lp/handle-iteration-exception!
                     (ex-info "HTTP client request failed"
                              {}
                              (ex-info "invalid API key" {:status 401 :provider :openai-codex}))
                     {:iteration 2 :messages [{:role "user" :content "hi"}]})]
        (expect (true? (::lp/fatal-iteration-error result))))))

;; Regression, issue #162: a provider-declared `max_output_tokens` cap was folded
;; into the terminal provider-failure path, so Vis ended the turn instead of making
;; one materially changed, compact recovery iteration.
(defdescribe
  output-budget-exhaustion-recovery-test
  (let [cap-data
        {:type :svar.core/stream-incomplete
         :stream? true
         :reason "max_output_tokens"
         :provider :openai-codex
         :content-acc-len 0
         :reasoning-acc-len 128}

        cap-error
        (ex-info "Stream ended with incomplete response, reason: max_output_tokens" cap-data)]

    (doseq [[label error] [["direct" cap-error]
                           ["wrapped" (ex-info "HTTP client request failed" {} cap-error)]]]
      (it (str "keeps the " label " output-cap signal recoverable and intact")
          (let [result (lp/handle-iteration-exception!
                         error
                         {:iteration 2 :messages [{:role "user" :content "finish the task"}]})
                iteration-error (::lp/iteration-error result)
                feedback (iteration-error-feedback 2 iteration-error "finish the task")]

            (expect (contains? result ::lp/iteration-error))
            (expect (not (::lp/fatal-iteration-error result)))
            (expect (= :svar.core/stream-incomplete (get-in iteration-error [:data :type])))
            (expect (= "max_output_tokens" (get-in iteration-error [:data :reason])))
            (expect (str/includes? feedback ":llm-provider/output-budget-exhausted"))
            (expect (str/includes? feedback "Use a compact path now"))
            (expect (str/includes? feedback "Original request: finish the task")))))))

;; Regression: a stream that ended before the provider's terminal marker reached
;; the log as class/message/type alone. Svar had already measured WHY — the last
;; SSE event, the finish reason, how much had streamed — and `format-exception-short`
;; dropped all of it, while the fatal line called the failure a rate limit /
;; auth / spend cap problem and sent the reader after a billing bug.
(defdescribe
  provider-failure-diagnostics-test
  "Svar refuses to resend a stream once output has been rendered, so a truncation
   is TERMINAL by design and the log line is the only record left. It must carry
   svar's bounded stream-finalization evidence and name the classified kind."
  (let [reasoning-transcript
        (apply str (repeat 200 "reasoning-transcript "))

        truncated
        (ex-info "Stream ended before terminal marker."
                 {:type :svar.core/stream-truncated
                  :stream? true
                  :url "https://gateway.example.com/v1/messages"
                  :stream-finalization {:terminal? false
                                        :terminal-kind nil
                                        :terminal-event-type nil
                                        :last-event-type "content_block_delta"
                                        :finish-reason nil
                                        :incomplete? false
                                        :incomplete-reason nil
                                        :content-acc-len 0
                                        :reasoning-acc-len 4200
                                        :http-status 200}
                  :content-acc-len 0
                  :reasoning-acc-len 4200
                  :partial-content nil
                  :reasoning reasoning-transcript})

        format-exception-short
        #'com.blockether.vis.internal.loop/format-exception-short

        log-message
        #'com.blockether.vis.internal.loop/non-correctable-log-message

        short-form
        (format-exception-short truncated)]

    (it "keeps svar's stream-finalization evidence in the logged short form"
        (expect (= {:terminal? false
                    :last-event-type "content_block_delta"
                    :incomplete? false
                    :content-acc-len 0
                    :reasoning-acc-len 4200
                    :http-status 200}
                   (:stream-finalization short-form))))
    (it "keeps a zero accumulator length — the fact that no content ever streamed"
        (expect (= 0 (:content-acc-len short-form)))
        (expect (= 4200 (:reasoning-acc-len short-form))))
    (it "never copies the streamed transcript into the log"
        (expect (nil? (:reasoning short-form)))
        (expect (nil? (:partial-content short-form)))
        (expect (not (str/includes? (pr-str short-form) "reasoning-transcript"))))
    (it "leaves an ordinary failure without stream keys"
        (let [plain (format-exception-short (ex-info "boom" {:type :vis/code-error}))]
          (expect (nil? (:stream-finalization plain)))
          (expect (nil? (:content-acc-len plain)))))
    (it "names the classified kind in the fatal line instead of a spend cap"
        (expect (= (str "Non-correctable provider error (stream-interrupted)"
                        " - failing turn instead of re-asking the same provider")
                   (log-message truncated)))
        (expect (str/includes? (log-message (ex-info "provider rate limited this request"
                                                     {:status 429
                                                      :provider :anthropic-coding-plan}))
                               "(rate-limit)")))
    (it "still ends the turn and carries the evidence onto the turn row"
        (let [result (lp/handle-iteration-exception! truncated
                                                     {:iteration 14
                                                      :messages [{:role "user" :content "hi"}]})]
          (expect (true? (::lp/fatal-iteration-error result)))
          (expect (= "content_block_delta"
                     (get-in (::lp/iteration-error result)
                             [:stream-finalization :last-event-type])))))))

(defdescribe
  user-configuration-error-test
  "An unset `${API_KEY}` env var (issues #51/#54) reaches the loop as a
   `:vis/user-error` / `:svar/no-providers` failure. The model cannot export a
   shell variable, so the turn must end ONCE with the actionable message —
   not be fed back for self-correction and not be flattened into the generic
   provider card."
  (let [handle
        (fn [e]
          (lp/handle-iteration-exception! e
                                          {:iteration 1 :messages [{:role "user" :content "hi"}]}))

        fatal?
        (fn [e]
          (boolean (::lp/fatal-iteration-error (handle e))))

        user-error-content
        #'com.blockether.vis.internal.loop/user-error-content

        env-gap
        (ex-info (str
                   "No usable provider — can't use rbi-genai: "
                   "RBI_GENAI_API_KEY is not set.\n"
                   "Set RBI_GENAI_API_KEY in your shell (export NAME=value) and start vis again.")
                 {:type :svar/no-providers
                  :vis/user-error true
                  :env-gaps {:rbi-genai ["RBI_GENAI_API_KEY"]}})]

    (it "fails the turn on an unset provider env var" (expect (fatal? env-gap)))
    (it "fails the turn when the user error is only on a cause"
        (expect (fatal? (ex-info "iteration failed" {} env-gap))))
    (it "fails the turn on a bare :svar/no-providers failure"
        (expect (fatal? (ex-info "make-router requires at least one provider"
                                 {:type :svar/no-providers}))))
    (it "still feeds a correctable model/code failure back for self-correction"
        (expect (not (fatal? (ex-info "Syntax error in generated code" {:type :vis/code-error})))))
    (it "renders the actionable env-var message instead of the generic provider card"
        (let [blocks
              (user-error-content (::lp/iteration-error (handle env-gap)))

              block
              (first blocks)]

          (expect (= 1 (count blocks)))
          (expect (= "error" (get block "type")))
          (expect (= "config_error" (get block "code")))
          (expect (str/includes? (get block "message") "RBI_GENAI_API_KEY is not set"))
          (expect (false? (get block "retryable")))))
    (it "leaves every other failure to the provider-card path"
        (expect (nil? (user-error-content {:message "boom" :data {:type :vis/code-error}}))))))

;; Regression, issue #105: Vis used to override Svar's terminal 402 quota
;; classification with its own legacy billing kind.
(defdescribe quota-error-is-terminal-test
             (it "ends a 402 turn once and preserves Svar's actionable quota card"
                 (let [result
                       (lp/handle-iteration-exception!
                         (ex-info "Exceptional status code: 402"
                                  {:status 402
                                   :provider :anthropic-coding-plan
                                   :body
                                   "{\"error\":{\"message\":\"Payment required: add credits\"}}"})
                         {:iteration 1 :messages [{:role "user" :content "hi"}]})

                       block
                       (first (perr/provider-error-content (::lp/iteration-error result)))]

                   (expect (true? (::lp/fatal-iteration-error result)))
                   (expect (= "provider_quota_exhausted" (get block "code")))
                   (expect (str/includes? (get block "message") "plan, usage limits")))))

(defdescribe reload-router-hook-test
             ;; `/reload` used to re-read vis.yml WITHOUT rebuilding the router, so a
             ;; changed `default_model` kept routing to the old model and the TUI footer
             ;; chip kept naming it until a restart.
             (describe "reload-router!"
                       (it "no-ops while the router was never built (lazy first use is preserved)"
                           (with-redefs [lp/router-initialized?
                                         (fn []
                                           false)

                                         lp/rebuild-router!
                                         (fn [_]
                                           (throw (ex-info "must not build" {})))

                                         lp/refresh-cached-routers!
                                         (fn [_]
                                           (throw (ex-info "must not reseat" {})))]

                             (expect (nil? (lp/reload-router!)))))
                       (it "rebuilds from the reloaded config and reseats cached session envs"
                           (let [built
                                 (atom nil)

                                 seated
                                 (atom nil)

                                 cfg
                                 {:providers [{:id :acme}] :default-model "new-model"}]

                             (with-redefs [lp/router-initialized?
                                           (fn []
                                             true)

                                           config/current-config
                                           (fn []
                                             cfg)

                                           lp/rebuild-router!
                                           (fn [c]
                                             (reset! built c)
                                             ::rebuilt)

                                           lp/refresh-cached-routers!
                                           (fn [r]
                                             (reset! seated r))]

                               (expect (nil? (lp/reload-router!))))
                             (expect (= cfg @built))
                             (expect (= ::rebuilt @seated)))))
             (describe
               "/reload wiring"
               (it
                 "is registered as a reload hook that rebuilds the router"
                 (let [hook
                       (get @@#'extension/reload-hooks
                            :com.blockether.vis.internal.loop/router-reload)

                       built
                       (atom nil)

                       seated
                       (atom nil)

                       cfg
                       {:providers [] :default-model "after-reload"}]

                   (expect (ifn? hook))
                   (with-redefs [lp/router-initialized?
                                 (fn []
                                   true)

                                 config/current-config
                                 (fn []
                                   cfg)

                                 lp/rebuild-router!
                                 (fn [c]
                                   (reset! built c)
                                   ::rebuilt)

                                 lp/refresh-cached-routers!
                                 (fn [r]
                                   (reset! seated r))]

                     (hook))
                   (expect (= cfg @built))
                   (expect (= ::rebuilt @seated))))))

(defdescribe human-input-parks-the-eval-wall-test
             ;; REGRESSION: HITL. Code that ASKS the operator blocks in
             ;; human-input/request!, and the enclosing wall used to bill that
             ;; thinking time — the call died with a timeout while the dialog was
             ;; still on screen and the answer was never applied.
             (it
               "a human-input pause parks the enclosing wall instead of timing out"
               (let [chan
                     (keyword "vis-test" (str "loop-hitl-" (random-uuid)))

                     events
                     (atom [])

                     ask
                     (fn []
                       (hi/request! {:title "Login"
                                     :session-id "loop-hitl-session"
                                     :fields [{:id "otp" :label "OTP"}]
                                     :timeout-ms 10000
                                     :channel-ids [chan]}))]

                 (ce/add-channel-event-listener! chan ::hitl-wall #(swap! events conj %))
                 (try (let [answerer
                            (future (loop [n 0]
                                      (if-let [request-id (some #(when (and (= :view/open (:op %))
                                                                            (= :input (:kind %)))
                                                                   (:view-id %))
                                                                @events)]
                                        ;; The operator takes MUCH longer than the 20ms wall.
                                        (do (Thread/sleep 1500)
                                            (hi/submit! request-id {"otp" "123456"}))
                                        (when (< n 400) (Thread/sleep 10) (recur (inc n))))))

                            {:keys [deadline park]}
                            (rt/parkable-wall (System/currentTimeMillis) 20)

                            result
                            (binding [rt/*blocking-wall-park* park]
                              (ask))]

                        (deref answerer 5000 nil)
                        (expect (true? (:is-submitted result)))
                        ;; the wall MOVED: it is no longer the 20ms one the call started with
                        (expect (> @deadline (+ (System/currentTimeMillis) -10))))
                      (finally (ce/remove-channel-event-listener! chan ::hitl-wall))))))

(def ^:private live-views-dir
  "The private var every view record hangs under, redefined per test so nothing
   here writes anywhere near the developer's own `~/.vis`."
  (requiring-resolve 'com.blockether.vis.internal.view.sink/views-dir))

(defn- bind-live-door!
  "The ONE host door these tests need, installed in a SANDBOX session.

   `python-extensions/bind-host!` binds the whole extension surface in the
   unconfined CHILD process, which is not where a sandbox block runs."
  ([pc] (bind-live-door! pc nil))
  ([pc installer]
   (python-host/install-sync-tools! pc
                                    {"__vis_host_live__" (fn [envelope]
                                                           (hi/live-json! envelope))}
                                    (or installer
                                        (fn [session name]
                                          (python-runtime/install-sync-tool! session name))))))

(defn- open-a-view
  "Guest code opening a live view through the host bridge an extension crosses,
   printing the id the engine minted for it."
  []
  (str "import json\n"
       "answer = json.loads(__vis_host_live__(json.dumps("
       "{'op': 'open', 'view': {'session_id': 'vis-test-wall', 'title': 'Watching', "
       "'nodes': [{'id': 'run', 'type': 'status', 'text': 'polling'}]}})))\n"
       "print(answer['view_id'])\n"))

(defn- watching-block
  "Run `code` in a python context of its own under the SHORTEST eval wall the
   engine allows (`rt/MIN_EVAL_TIMEOUT_MS`, 3s), records under a temp directory,
   and answer `[result ids-the-block-left-open]`. Anything left open is closed
   here, so one test cannot leak a view into the next."
  [code]
  (with-redefs-fn {live-views-dir (constantly (java.io.File. (System/getProperty "java.io.tmpdir")
                                                             (str "vis-views-" (random-uuid))))}
    (fn []
      (tpc/with-own [pc {}]
                    (let [;; The bridge an extension crosses for a view, on a context of its own.
                          _
                          (bind-live-door! pc)

                          before
                          (hi/open-live-ids)

                          left
                          #(remove before (hi/open-live-ids))]

                      (try [(binding [rt/*eval-timeout-ms* rt/MIN_EVAL_TIMEOUT_MS]
                              ((deref #'lp/run-python-code) pc code)) (vec (left))]
                           (finally (doseq [view-id (left)]
                                      (hi/close-live! view-id))
                                    (try (env/dispose-python-context! pc)
                                         (catch Throwable _ nil)))))))))

(defn- cancelled-watching-block
  "The same block as [[watching-block]], stopped the way a person's Cancel stops
   one: the turn's cancellation token is fired the moment the view is up, and the
   guest is killed from outside without ever reaching its own close."
  [code]
  (with-redefs-fn {live-views-dir (constantly (java.io.File. (System/getProperty "java.io.tmpdir")
                                                             (str "vis-views-" (random-uuid))))}
    (fn []
      (tpc/with-own
        [pc {} nil
         {:worker? true
          :jail-enabled? false
          :enabled? false
          :allowed-domains []
          :denied-domains []}]
        (let [_
              (bind-live-door! pc
                               (fn [session name]
                                 (python-worker/install-sync-tool! session session name)))

              before
              (hi/open-live-ids)

              left
              #(remove before (hi/open-live-ids))

              token
              (cancellation/cancellation-token)

              ;; The human watches for a moment, then presses Cancel.
              stopper
              (future (loop [n 0]
                        (if (seq (left))
                          (do (Thread/sleep 200) (cancellation/cancel! token :client-cancel-turn))
                          (when (< n 400) (Thread/sleep 10) (recur (inc n))))))]

          (try [(binding [rt/*eval-timeout-ms* rt/MIN_EVAL_TIMEOUT_MS]
                  ((deref #'lp/run-python-code) pc code :env {:cancel-token token})) (vec (left))]
               (finally (deref stopper 5000 nil)
                        (doseq [view-id (left)]
                          (hi/close-live! view-id))
                        (try (env/dispose-python-context! pc) (catch Throwable _ nil)))))))))

(defdescribe live-view-blocking-wait-bridge-test
             (it "waits through the guest host bridge and returns only a timeout envelope"
                 (let [[result left]
                       (watching-block
                         (str (open-a-view)
                              "before = __import__('time').monotonic()\n"
                              "waited = json.loads(__vis_host_live__(json.dumps({'op': 'state', "
                              "'view_id': answer['view_id'], 'after_seq': answer['view']['seq'], "
                              "'timeout_ms': 30})))\n"
                              "assert waited['timed_out'] and 'view' not in waited\n"
                              "assert __import__('time').monotonic() - before >= 0.03\n"
                              "print('waited without polling')\n"))]
                   (expect (nil? (:error result)))
                   (expect (str/includes? (str (:stdout result)) "waited without polling"))
                   (expect (= 1 (count left))))))

(defdescribe live-view-owns-the-eval-wall-test
             ;; Regression, reported from the app: watching a CI run died at `Timeout (300s)` with the
             ;; build still going — five minutes is the eval backstop, and a run worth
             ;; showing a human takes fifteen. Worse, the pane the wall left behind never
             ;; closed: hours later the phone still painted the last poll it had seen and
             ;; still offered a Stop nobody was listening to.
             (describe "a run SHOWING its work holds the wall, and the run's end ends the view"
                       (it "is not killed at the eval wall while a live view is open"
                           (let [[result left] (watching-block (str
                                                                 (open-a-view)
                                                                 "import time\n"
                                                                 "time.sleep(4.0)\n"
                                                                 "print('watched to the end')\n"))]
                             ;; 4s of watching under a 3s wall: without the hold this is `Timeout (3s)`.
                             (expect (nil? (:timeout? result)))
                             (expect (some? (re-find #"watched to the end" (str (:stdout result)))))
                             (expect (= 1 (count left)))))
                       (it "closes a view the block never closed, so no pane outlives its run"
                           (let [[result left] (watching-block
                                                 (str (open-a-view)
                                                      "raise RuntimeError('the poll blew up')\n"))]
                             (expect (some? (:error result)))
                             (expect (= [] left))
                             ;; Both halves reach the block that died holding it: the record
                             ;; as a row, the picture in what the block printed.
                             (expect (= 1 (count (:attachments result))))
                             (expect (str/includes? (str (:stdout result)) "# Watching"))))))

(defdescribe
  live-view-outlives-the-block-that-showed-it-test
  ;; Regression, reported from the app: a `gh` watch of a CI run was cancelled
  ;; two hours in. The pane went away, which is right — but nothing of it
  ;; reached the transcript. The block answered a bare
  ;; `java.lang.InterruptedException`, no attachment row was ever filed, and
  ;; everything the human had been watching survived only as an NDJSON record
  ;; on disk that nothing pointed at.
  (describe
    "a stopped block still hands over what it was SHOWING"
    (it
      "answers the record and the picture of the view a cancel killed"
      (let [outcomes
            (atom [])

            settle-running
            activity/settle-running

            [result left]
            (with-redefs [activity/settle-running (fn [state outcome summary]
                                                    (swap! outcomes conj outcome)
                                                    (settle-running state outcome summary))]
              (cancelled-watching-block
                (str (open-a-view)
                     "print('polled once')\n"
                     "__vis_host_live__(json.dumps({'op': 'state', 'view_id': answer['view_id'], "
                     "'after_seq': answer['view']['seq'], 'timeout_ms': 30000}))\n")))

            row
            (first (:attachments result))]

        (expect (= [] left))
        ;; Cancellation is presentation, not a Python/JVM failure: the
        ;; persisted form names it without leaking the host wait stack.
        (expect (= {:message "Python execution was interrupted" :type :vis/interrupted}
                   (:error result)))
        (expect (= [:cancelled] @outcomes))
        ;; The human's half: the record is a ROW, so the gallery and the
        ;; database hold what they watched.
        (expect (= 1 (count (:attachments result))))
        (expect (= "file" (:kind row)))
        (expect (str/ends-with? (str (:filename row)) ".live.ndjson"))
        ;; The model's half: the picture the view ended on, and the lines
        ;; the block printed before the stop.
        (expect (str/includes? (str (:stdout result)) "# Watching"))
        (expect (str/includes? (str (:stdout result)) "did not finish"))
        (expect (str/includes? (str (:stdout result)) "polled once"))))))

(defdescribe normalize-tool-input-strings-only-test
             (describe "model-drift and extension EDN are stringified, keys AND values"
                       (it "stringifies keyword/symbol values at every depth"
                           (let [normalize
                                 #'lp/normalize-tool-input

                                 normalized
                                 (normalize {:op :delete
                                             :paths ['a "b"]
                                             :edits [{:mode :replace/nested}]
                                             :count 3
                                             :is_overwrite true})]

                             (expect (= {"op" "delete"
                                         "paths" ["a" "b"]
                                         "edits" [{"mode" "replace/nested"}]
                                         "count" 3
                                         "is_overwrite" true}
                                        normalized))))))

(defdescribe tool-call-door-strings-only-test
             (describe
               "model drift is repaired once, at the door"
               (it "normalizes every tool call's :input at every depth"
                   (let [door
                         #'lp/normalize-tool-calls

                         calls
                         (door [{:id "t1"
                                 :name "patch"
                                 :input {:edits [{:path "a.clj" :from_anchor "1:aa" :replace "x"}]}}
                                {:id "t2" :name "delete" :input {:paths ["x"]}}])]

                     (expect (= [{"edits" [{"path" "a.clj" "from_anchor" "1:aa" "replace" "x"}]}
                                 {"paths" ["x"]}]
                                (mapv :input calls)))
                     (expect (= ["t1" "t2"] (mapv :id calls)))))
               (it "repairs a model-drift `\":path\"` key at every depth"
                   ;; svar hands the wire key over verbatim, so a model that writes a leading
                   ;; colon INTO its JSON is repaired here and nowhere else.
                   (let [[tc] (#'lp/normalize-tool-calls
                               [{:id "p"
                                 :name "patch"
                                 :input {":edits" [{":path" "a.clj" ":from_anchor" "1:aa"}]}}])]
                     (expect (= {"edits" [{"path" "a.clj" "from_anchor" "1:aa"}]} (:input tc)))))
               (it "lets downstream consumers read string keys only — no keyword fallback"
                   (let [[tc] (#'lp/normalize-tool-calls
                               [{:id "w" :name "python_execution" :input {:code "print(1)"}}])]
                     (expect (= {"code" "print(1)"} (:input tc)))))))

;; A tool call whose arguments were CORRUPTED inside the provider's own
;; tool-call encoding: the model's closing tag arrived mangled
;; (`</antmlutparameter>`, `</invoke>`) and the API handed that tag over as the
;; VALUE of an argument. Vis ran the call verbatim, so `apropos()` became
;; `apropos("</antmlutparameter>\n")` and answered "no unadvertised capabilities
;; match" to a question nobody asked, and a `cat` whose JSON the model
;; entity-escaped (`&quot;`) reached the tool as one garbage key.
(defdescribe
  tool-call-protocol-leak-test
  (describe
    "a leaked tool-call tag is not an argument value"
    (it "drops the mangled closing tag and runs the call the model meant"
        (let [[tc] (#'lp/normalize-tool-calls
                    [{:id "a" :name "python_execution" :input {"code" "</antmlutparameter>\n"}}])]
          (expect (= {} (:input tc)))))
    (it "drops a `</invoke>` value carried under an entity-escaped key"
        (let [[tc] (#'lp/normalize-tool-calls
                    [{:id "c"
                      :name "cat"
                      :input {"workflows/ci.yml&quot;, &quot;ranges&quot;: [[-1, -1]]}]"
                              "\n</invoke>\n"}}])]
          (expect (= {} (:input tc)))))
    (it "keeps a value that merely MENTIONS the tag"
        (let [[tc] (#'lp/normalize-tool-calls
                    [{:id "g" :name "grep" :input {"query" "who writes </parameter> here"}}])]
          (expect (= {"query" "who writes </parameter> here"} (:input tc)))))
    ;; The drop is the right repair, but it also ERASES the evidence: the second
    ;; instance of this fault was only ever found because the mangled tag had
    ;; been persisted in `session_turn_iteration.tool_calls`. Nothing corrupt
    ;; reaches engine data now, so the log line is the only trace left of a
    ;; provider that mangled its own tool-call encoding.
    (it "records the argument it dropped"
        (let [{:keys [signals]}
              (tel/with-signals
                (#'lp/normalize-tool-calls
                 [{:id "a" :name "apropos" :input {"query" "</antmlutparameter>\n"}}]))

              leak
              (first (filter #(= ::lp/tool-protocol-leak (:id %)) signals))]

          (expect (some? leak))
          (expect (= :warn (:level leak)))
          (expect (= "query"
                     (-> leak
                         :data
                         :argument)))
          (expect (= "</antmlutparameter>\n"
                     (-> leak
                         :data
                         :value))))))
  ;; The same wreckage one level up: the corrupted `arguments` payload never
  ;; decoded to an OBJECT at all. svar's tool-argument decode is strict and
  ;; FAITHFUL — it hands whatever JSON value it read straight back, so
  ;; `"\"</invoke>\""` arrives as a String and `"[1,2]"` as a vector — while
  ;; every consumer past this door (the sandbox program, persistence) reads a
  ;; string-keyed map.
  (describe "an arguments payload that is not an object at all"
            (it "drops a bare string payload and runs the call the model meant"
                (let [[tc] (#'lp/normalize-tool-calls
                            [{:id "a" :name "python_execution" :input "</invoke>"}])]
                  (expect (= {} (:input tc)))))
            (it "drops a vector payload"
                (let [[tc] (#'lp/normalize-tool-calls [{:id "a" :name "apropos" :input [1 2]}])]
                  (expect (= {} (:input tc)))))
            (it "drops a scalar payload"
                (let [[tc] (#'lp/normalize-tool-calls [{:id "a" :name "apropos" :input 42}])]
                  (expect (= {} (:input tc)))))
            (it "records the payload it refused"
                (let [{:keys [signals]}
                      (tel/with-signals (#'lp/normalize-tool-calls
                                         [{:id "a" :name "apropos" :input "</invoke>"}]))

                      leak
                      (first (filter #(= ::lp/tool-input-not-an-object (:id %)) signals))]

                  (expect (some? leak))
                  (expect (= :warn (:level leak)))
                  (expect (str/includes? (str (-> leak
                                                  :data
                                                  :value))
                                         "</invoke>"))))))

;; GitHub Copilot bills a request as a FULL premium interaction unless the
;; caller marks it `X-Initiator: agent` (a MISSING header means `user`), and
;; svar infers that header from message roles. Vis' background one-shots build a
;; fresh system+user pair, which reads exactly like a human prompt, so every
;; extension helper call used to be billed as a premium user interaction.
(defdescribe
  copilot-agent-initiator-defaults-test
  (it "marks one-shot ask-code! helper calls as agent initiated"
      (expect (= "agent"
                 (get-in (captured-ask-code-opts {:messages [{:role "user" :content "hi"}]})
                         [:opts :llm-headers "X-Initiator"]))))
  (it "marks llm-text! helper calls as agent initiated"
      (expect (= "agent"
                 (get-in (captured-llm-text-opts {:prompt "hi"})
                         [:opts :llm-headers "X-Initiator"]))))
  (it "still lets a caller pin the initiator explicitly"
      (expect (= "user"
                 (get-in (captured-ask-code-opts {:messages [] :llm-headers {"X-Initiator" "user"}})
                         [:opts :llm-headers "X-Initiator"]))))
  (it "applies the routed provider policy to both one-shot helpers"
      (expect (= 600000
                 (get-in (captured-ask-code-opts {:messages []}) [:opts :first-byte-timeout-ms])))
      (expect (= 600000
                 (get-in (captured-llm-text-opts {:prompt "hi"}) [:opts :first-byte-timeout-ms]))))
  (it "keeps explicit helper overrides above provider defaults"
      (expect (= 700000
                 (get-in (captured-ask-code-opts {:messages [] :first-byte-timeout-ms 700000})
                         [:opts :first-byte-timeout-ms]))))
  (it "leaves providers without policy on svar's first-byte default"
      (expect (not (contains? (:opts (captured-svar-ask-code-opts (helper-router :cloud nil)
                                                                  #(lp/ask-code! {:messages []})))
                              :first-byte-timeout-ms)))))

;; Regression: Copilot Claude capped `:deep` to `:balanced`. The cap was written
;; for the OPENAI-compatible chat wire, where `reasoning_effort` mis-routed the
;; proxy; on the native `/v1/messages` wire the cap only bought thinking
;; SHALLOWER than Anthropic's own default effort, which is how a `:deep` turn
;; came back with two-word thinking summaries.
(defdescribe copilot-claude-reasoning-level-test
             (it "sends the requested depth on EVERY Copilot plan"
                 (doseq [provider [:github-copilot :github-copilot-individual
                                   :github-copilot-business :github-copilot-enterprise]]
                   (expect (= :deep
                              (#'lp/copilot-claude-reasoning-level
                               {:provider provider :name "claude-opus-5"}
                               "please refactor the loop"
                               :deep)))))
             (it "leaves non-Copilot providers at the requested level"
                 (expect (= :deep
                            (#'lp/copilot-claude-reasoning-level
                             {:provider :anthropic-coding-plan :name "claude-opus-5"}
                             "please refactor the loop"
                             :deep))))
             (it "names no depth for casual Copilot chat, leaving it to adaptive thinking"
                 (expect (nil? (#'lp/copilot-claude-reasoning-level
                                {:provider :github-copilot-individual :name "claude-opus-5"}
                                "hey"
                                :deep)))))

;; Regression, issue #112: the `:provider-call` lifecycle marker carried only the iteration
;; and a start timestamp, so a stalled stream had nothing to name — the gateway failed the
;; turn without ever telling the human which provider and model went silent.
(defdescribe provider-call-chunk-test
             (it "names the provider and model the call is dispatched to"
                 (expect (= {:phase :provider-call
                             :iteration 3
                             :reason :tool-result
                             :started-at-ms 42
                             :provider "github-copilot-enterprise"
                             :model "claude-opus-5"}
                            (#'lp/provider-call-chunk
                             3
                             {:provider :github-copilot-enterprise :name "claude-opus-5"}
                             42))))
             (it "leaves out what the router could not resolve"
                 (expect (= {:phase :provider-call
                             :iteration 0
                             :reason :user-submit
                             :started-at-ms 1
                             :provider nil
                             :model nil}
                            (#'lp/provider-call-chunk 0 {} 1))))
             (it "carries the provider's bounded pre-output envelope to the gateway"
                 (expect (= {:first-output-timeout-ms 800000 :stall-timeout-ms 600000}
                            (#'lp/provider-watchdog-timeouts
                             {:timeout-ms 1800000
                              :first-byte-timeout-ms 600000
                              :idle-timeout-ms 600000
                              :semantic-timeout-ms 600000})))
                 (expect (= {:first-output-timeout-ms 700 :stall-timeout-ms 600}
                            (select-keys (#'lp/provider-call-chunk
                                          1
                                          {:provider :lmstudio :name "dense"}
                                          42
                                          {:first-output-timeout-ms 700 :stall-timeout-ms 600})
                                         [:first-output-timeout-ms :stall-timeout-ms])))))

(defdescribe providers-router-rebuild-hook-wiring-test
             ;; The picker's config-affecting saves fire `providers/rebuild-shared-router!`,
             ;; which only rebuilds the shared router because `loop` registered `reload-router!`
             ;; as that hook. Before the wiring the hook fired into nil and a default-model
             ;; change never reached the shared router — a new session's first turn kept the
             ;; OLD root until the model was re-pinned on the session.
             ;; The hook holds the VAR: `permission-config-snapshot-test` above reloads this
             ;; namespace, and a hook holding the FUNCTION was left pointing at the version
             ;; from the first load — dead wiring that only a full-suite run ever showed.
             (it "wires reload-router! as the providers router-rebuild hook"
                 (expect (identical? (providers/router-rebuild-hook-val) #'lp/reload-router!))))

;; Regression: `list_attachments()` located a TOOL artifact by its iteration
;; alone, so a descriptor for anything the model produced carried no turn id at
;; all — only a user image got `:turn-id` — and nothing on the rail could be
;; grouped by the turn it belongs to without a second lookup.
(defdescribe
  attachment-descriptor-test
  "Every `session_attachment` row carries `session_turn_soul_id`, so EVERY
   descriptor carries `:turn-id`. The iteration / tool-call grain is the FINER
   provenance a tool artifact also has, and a user image omits it rather than
   carrying nils."
  (it "gives a tool artifact its turn id, not only its iteration"
      (let [d (lp/attachment-descriptor {:id "a1"
                                         :source :tool
                                         :filename "chart.png"
                                         :version 2
                                         :media-type "image/png"
                                         :kind "image"
                                         :size 7
                                         :position 0
                                         :turn-soul-id "turn-1"
                                         :iteration-id "it-1"
                                         :tool-call-id "call-1"
                                         :base64 "PNGDATA"})]
        (expect (= "turn-1" (:turn-id d)))
        (expect (= "it-1" (:iteration-id d)))
        (expect (= "call-1" (:tool-call-id d)))
        (expect (= 2 (:version d)))
        ;; A descriptor is metadata: the payload never rides along.
        (expect (nil? (:base64 d)))))
  (it "gives a user image the same turn id and no tool grain"
      (let [d (lp/attachment-descriptor {:id "u1"
                                         :source :user
                                         :filename "photo.png"
                                         :version 1
                                         :media-type "image/png"
                                         :kind "image"
                                         :size 3
                                         :position 0
                                         :turn-soul-id "turn-1"})]
        (expect (= "turn-1" (:turn-id d)))
        (expect (not (contains? d :iteration-id)))
        (expect (not (contains? d :tool-call-id))))))

(def ^:private cache-key (deref #'lp/cache-key))

(def ^:private acquire-turn-lock! (deref #'lp/acquire-turn-lock!))

(defn- hold-lock-forever!
  "Start a thread that takes `lock` and keeps it until the returned `:release`
   promise is delivered — a turn wedged inside the engine, which is the only way
   to make an off-thread `tryLock` genuinely fail (the lock is reentrant)."
  [^java.util.concurrent.locks.ReentrantLock lock]
  (let [held
        (promise)

        release
        (promise)

        ghost
        (Thread. ^Runnable
                 (fn []
                   (.lock lock)
                   (deliver held true)
                   @release
                   (.unlock lock))
                 "wedged-engine-test-ghost")]

    (.setDaemon ghost true)
    (.start ghost)
    @held
    {:release release :thread ghost}))

;; Regression, session e8c9dbc9-388d-43a4-8264-9dd5adec4449: a turn wedged inside
;; the engine (parked on CPython's GIL, where `Thread.interrupt` never reaches it)
;; never released its session's `ReentrantLock`. The daemon's cancel backstop
;; synthesized `turn.cancelled` and reported the session idle — but `send!` took
;; that lock with a bare `.lock`, so the NEXT turn parked in `Unsafe.park`
;; forever: `turn.started` on the wire, not one event after it, and deaf to its
;; own cancel. The session was dead for the life of the daemon.
(defdescribe
  wedged-engine-lock-test
  (describe
    "a turn queued behind a wedged one"
    (it
      "waits while the lock is legitimately held, but stays interruptible"
      (let [id
            "wedge-test/queued"

            k
            (cache-key id)

            entry
            (new-cache-entry {:marker :ghost})

            ghost
            (hold-lock-forever! (:lock entry))

            outcome
            (promise)]

        (swap! env-cache assoc k entry)
        (try (let [queued (Thread.
                            ^Runnable
                            (fn []
                              (try (lp/send! id "hello" {})
                                   (deliver outcome :returned)
                                   (catch InterruptedException _ (deliver outcome :interrupted))
                                   (catch Throwable t (deliver outcome [:threw (str (class t))]))))
                            "wedged-engine-test-queued")]
               (.setDaemon queued true)
               (.start queued)
               ;; a running turn owns the lock: queueing is correct, so nothing resolves
               (Thread/sleep 400)
               (expect (not (realized? outcome)))
               ;; ...but a cancel must be able to take this turn OFF that queue
               (.interrupt queued)
               (expect (= :interrupted (deref outcome 5000 :parked-forever)))
               (.join queued 1000))
             (finally (deliver (:release ghost) true)
                      (.join ^Thread (:thread ghost) 2000)
                      (swap! env-cache dissoc k)))))
    (it
      "abandons a CONDEMNED engine and runs on a fresh context"
      (let [id
            "wedge-test/condemned"

            k
            (cache-key id)

            entry
            (new-cache-entry {:marker :ghost})

            ^java.util.concurrent.locks.ReentrantLock dead-lock
            (:lock entry)

            ghost
            (hold-lock-forever! dead-lock)

            got
            (promise)]

        (swap! env-cache assoc k entry)
        (try (with-redefs-fn {#'lp/open-env! (fn [_ _]
                                               {:marker :fresh})}
               (fn []
                 (let [waiter (Thread. ^Runnable
                                       (fn []
                                         (let [e (acquire-turn-lock! id)]
                                           (.unlock ^java.util.concurrent.locks.ReentrantLock
                                                    (:lock e))
                                           (deliver got e)))
                                       "wedged-engine-test-waiter")]
                   (.setDaemon waiter true)
                   (.start waiter)
                   ;; nothing has declared the holder dead yet, so we keep waiting
                   (Thread/sleep 400)
                   (expect (not (realized? got)))
                   ;; the daemon's cancel backstop declares the turn over
                   (expect (true? (lp/condemn-env! id)))
                   (let [fresh (deref got 5000 ::parked)]
                     (expect (not= ::parked fresh))
                     ;; a FRESH env under a FRESH lock — the ghost keeps the old one
                     (expect (= {:marker :fresh} (:environment fresh)))
                     (expect (not (identical? dead-lock (:lock fresh)))))
                   (.join waiter 1000))))
             (finally (deliver (:release ghost) true)
                      (.join ^Thread (:thread ghost) 2000)
                      (swap! env-cache dissoc k)))))
    ;; Regression, session 6e214dfd-6653-42a6-b45a-710864b0ccbd: the user pressed stop
    ;; on a live view. The cancelled turn unwound and released the engine lock, but the
    ;; guest thread it abandoned died OWNING CPython's GIL, so the context could never
    ;; be entered again. The NEXT turn took the free lock, walked into the engine and
    ;; parked in `PythonContext.acquireGil` before its first iteration — `turn.started`,
    ;; zero iterations, buried by the stall watchdog two minutes later, and the same for
    ;; every message sent afterwards. One stop bricked the session for the life of the
    ;; daemon.
    (it
      "abandons an engine whose Python context can no longer be ENTERED"
      (let [id
            "wedge-test/unenterable"

            k
            (cache-key id)

            entry
            (new-cache-entry {:marker :ghost})

            opened
            (atom 0)

            got
            (promise)]

        (swap! env-cache assoc k entry)
        (try (with-redefs-fn {#'lp/open-env! (fn [_ _]
                                               (swap! opened inc)
                                               {:marker :fresh})
                              ;; the leaked GIL: nothing will ever enter this context
                              #'env/context-enterable? (fn [_]
                                                         false)}
               (fn []
                 (let [waiter (Thread. ^Runnable
                                       (fn []
                                         (let [e (acquire-turn-lock! id)]
                                           (.unlock ^java.util.concurrent.locks.ReentrantLock
                                                    (:lock e))
                                           (deliver got e)))
                                       "wedged-context-test-waiter")]
                   (.setDaemon waiter true)
                   (.start waiter)
                   (let [fresh (deref got 5000 ::parked)]
                     ;; the turn RUNS instead of parking on a context nobody owns
                     (expect (not= ::parked fresh))
                     (expect (= {:marker :fresh} (:environment fresh)))
                     (expect (not (identical? (:lock entry) (:lock fresh))))
                     ;; ...and the rescue is taken ONCE: a fresh context that still
                     ;; refuses is a failure to report, not a reason to mint another
                     (expect (= 1 @opened)))
                   (.join waiter 1000))))
             (finally (swap! env-cache dissoc k)))))
    (it "keeps the SAME engine when its context still answers"
        (let [id
              "wedge-test/enterable"

              k
              (cache-key id)

              entry
              (new-cache-entry {:marker :live})

              opened
              (atom 0)]

          (swap! env-cache assoc k entry)
          (try (with-redefs-fn {#'lp/open-env! (fn [_ _]
                                                 (swap! opened inc)
                                                 {:marker :fresh})
                                #'env/context-enterable? (fn [_]
                                                           true)}
                 (fn []
                   (let [e (acquire-turn-lock! id)]
                     (.unlock ^java.util.concurrent.locks.ReentrantLock (:lock e))
                     ;; a healthy context is never thrown away: same entry, same globals
                     (expect (identical? entry e))
                     (expect (zero? @opened)))))
               (finally (swap! env-cache dissoc k)))))))

(defdescribe voice-projection-prompt-test
             (it "activates the voice projection instructions only for the requested turn"
                 (let [projected (#'lp/voice-system-prompt "base" {"voice_projection" true})]
                   (expect (str/includes? projected "base"))
                   (expect (str/includes? projected "vis-speech"))
                   (expect (str/includes? projected "text-to-speech"))
                   (expect (str/includes? projected "text-only turn"))
                   (expect (str/includes? projected "do not mention voice mode")))
                 (expect (= "base" (#'lp/voice-system-prompt "base" {})))
                 (expect (= "base" (#'lp/voice-system-prompt "base" {:voice_projection true})))))

(def ^:private failed-turn-outcome
  "A finished turn's terminal payload: content, structured error, counters, CTX."
  {:content [(content/error "python_runtime" "boom" true)]
   :error {"type" "error" "message" "boom"}
   :iteration-count 33
   :duration-ms 1234
   :status :error
   :prior-outcome :error
   :ctx {"fact" "value"}})

(defn- outcome-writes
  "Every payload `persist-turn-outcome!` hands the store, plus what it returned.
   `reject?` decides which payloads the store refuses."
  [reject? opts]
  (let [calls
        (atom [])

        result
        (atom nil)

        {:keys [signals]}
        (tel/with-signals (with-redefs-fn
                            {#'persistance/db-update-session-turn!
                             (fn [_db _id o]
                               (swap! calls conj o)
                               (when (reject? o)
                                 (throw (ex-info "[SQLITE_TOOBIG] String or BLOB exceeds size limit"
                                                 {})))
                               :written)}
                            #(reset! result (#'lp/persist-turn-outcome! {} "turn-1" opts))))]

    {:calls @calls :result @result :signals signals}))

;; Regression (session 4b6897d4): the write that RECORDS a turn's outcome was
;; itself unguarded, so a payload the store refused (`[SQLITE_TOOBIG]` on an
;; error message quoting the whole document that broke the turn) left the turn
;; `running` for good -- no status, no error, no iteration count -- inside a
;; session that had already finished it.
(defdescribe
  turn-outcome-guard-test
  "Persistence degrades from full outcome to no context, then minimal content.
   Snapshot failure alone never discards the answer or its structured error."
  (it "writes once and reports success when the store accepts the payload"
      (let [{:keys [calls result]} (outcome-writes (constantly false) failed-turn-outcome)]
        (expect (true? result))
        (expect (= [failed-turn-outcome] calls))))
  (it "degrades only after the answer itself is refused and logs each failed write"
      (let [{:keys [calls result signals]}
            (outcome-writes #(= (:content failed-turn-outcome) (:content %)) failed-turn-outcome)

            degraded
            (last calls)]

        (expect (true? result))
        (expect (= 3 (count calls)))
        (expect (= :error (:status degraded)))
        (expect (= 33 (:iteration-count degraded)))
        (expect (= 1234 (:duration-ms degraded)))
        (expect (= :error (:prior-outcome degraded)))
        (expect (nil? (:ctx degraded)))
        (expect (= 1 (count (:content degraded))))
        (expect (= "turn_outcome_persist_failed" (get (:error degraded) "code")))
        (expect (< (count (get (:error degraded) "message")) 256))
        (expect (= [::lp/turn-outcome-persist-failed ::lp/turn-outcome-without-context-failed]
                   (mapv :id signals)))
        (expect (every? #(= :warn (:level %)) signals))))
  (it "preserves the original structured error when only CTX is refused"
      (let [{:keys [calls result signals]} (outcome-writes #(contains? % :ctx) failed-turn-outcome)]
        (expect (true? result))
        (expect (= [failed-turn-outcome (dissoc failed-turn-outcome :ctx)] calls))
        (expect (= [::lp/turn-outcome-persist-failed] (mapv :id signals)))))
  (it "preserves a successful answer when only the context snapshot is refused"
      (let [outcome
            (assoc failed-turn-outcome
              :content [(content/prose "The completed answer")]
              :status :success
              :error nil
              :prior-outcome :complete)

            {:keys [calls result]}
            (outcome-writes #(some? (:ctx %)) outcome)]

        (expect (true? result))
        (expect (= 2 (count calls)))
        (expect (= (dissoc outcome :ctx) (second calls)))))
  (it "reports failure instead of throwing when even the minimal outcome is refused"
      (let [{:keys [calls result signals]} (outcome-writes (constantly true) failed-turn-outcome)]
        (expect (false? result))
        (expect (= 3 (count calls)))
        (expect (= ::lp/turn-outcome-lost (:id (last signals))))
        (expect (= :error (:level (last signals))))))
  (it "does not write after another terminal path owns the turn"
      (let [writes
            (atom [])

            claimed
            (atom 0)]

        (with-redefs-fn {#'persistance/db-update-session-turn! (fn [& args]
                                                                 (swap! writes conj args))}
          #(expect (false? (#'lp/persist-turn-outcome!
                            {}
                            "turn-1"
                            failed-turn-outcome
                            (fn []
                              (swap! claimed inc)
                              false)))))
        (expect (= 1 @claimed))
        (expect (empty? @writes)))))

 ;; Regression (vis session 26af5650): an upstream stream timeout killed a turn
 ;; and history showed no answer, error card, counters, or duration. Persisting a
 ;; raw provider fallback through answer validation masked the real failure.
(defdescribe
  failed-turn-persistence-test
  (it
    "records one terminal outcome with the provider diagnostic"
    (let [writes
          (atom [])

          trace
          [{:iteration 1}
           {:iteration 2
            :error {:message "Stream idle timeout (300000ms with no bytes): closed"
                    :data {:type :svar.core/stream-idle-timeout :idle-timeout-ms 300000}}}]

          env
          {:db-info ::db
           :session-id "session-1"
           :router {:providers [{:id :openai :models [{:name "gpt-canonical"}]}]}
           :turn-state-atom (ctx-loop/make-turn-state-atom)}]

      (with-redefs [persistance/db-store-session-turn!
                    (fn [_db _opts]
                      "turn-1")

                    persistance/db-update-session-turn!
                    (fn [_db turn-id opts]
                      (swap! writes conj [turn-id opts])
                      true)

                    lp/session-turn-position
                    (fn [_env _turn-id]
                      1)

                    lp/iteration-loop
                    (fn [_env _request _opts]
                      {:answer {:overloaded true :status 529}
                       :status :error
                       :iteration-count 2
                       :duration-ms 17
                       :trace trace
                       :tokens {}
                       :cost {"total_cost" 0.25}})]

        (let [result
              (run-normal-turn! env "explain the failure" {})

              [written-id written]
              (first @writes)

              card
              (first (:content written))]

          (expect (= 1 (count @writes)))
          (expect (= "turn-1" written-id))
          (expect (= :error (:status written)))
          (expect (= 2 (:iteration-count written)))
          (expect (= "error" (get card "type")))
          (expect (str/includes? (get card "message") "Stream went quiet"))
          (expect (= {"total_cost" 0.25 "model" "gpt-canonical" "provider" "openai"}
                     (:cost written)))
          (expect (= card (:error written)))
          (expect (= :error (:status result))))))))

;; Regression: `set-provider!` handed `save-config!` a map holding ONLY
;; `:providers`, so persisting one provider replaced the whole machine store —
;; toggles, the vision memory, the MCP servers and the selection tags were gone.
(defdescribe set-provider-keeps-the-rest-of-the-store-test
             "Persisting a provider is an update of the machine store, not a replacement."
             (it
               "leaves every unrelated machine key in place"
               (let [dir
                     (.toFile (java.nio.file.Files/createTempDirectory
                                "vis-set-provider"
                                (make-array java.nio.file.attribute.FileAttribute 0)))

                     home
                     (java.io.File. dir "home")

                     store-dir
                     (java.io.File. home ".vis")

                     old-home
                     (System/getProperty "user.home")]

                 (try (.mkdirs store-dir)
                      (spit (java.io.File. store-dir "state.yml")
                            "toggles:\n  introspection: true\ndefault_provider: keep-me\n")
                      (System/setProperty "user.home" (.getPath home))
                      (config/invalidate-config-cache!)
                      (with-redefs-fn {#'lp/rebuild-router! (fn [_]
                                                              :router)
                                       #'lp/refresh-cached-routers! (fn [_]
                                                                      nil)
                                       #'config/current-config (fn []
                                                                 {:providers []})}
                        (fn []
                          (lp/set-provider! {:id :probe :api-key "k" :models [{:name "m"}]})))
                      (let [store (config/load-global-config-raw)]
                        (expect (= {"introspection" true} (get store "toggles")))
                        (expect (= "keep-me" (get store "default_provider")))
                        (expect (= ["probe"] (mapv #(get % "id") (get store "providers")))))
                      (finally (System/setProperty "user.home" old-home)
                               (config/invalidate-config-cache!))))))

(defdescribe create-environment-failure-disposes-sandbox-test
             ;; The defs restore and the extension symbol sync run AFTER the sandbox exists.
             ;; A throw in that stretch must not abandon it — an abandoned sandbox is never
             ;; reclaimed: its Python namespace is a reference cycle through every function
             ;; defined in it, and the host half holds one closure per tool. So the FAILURE
             ;; path leaks worse than success can, on exactly the runs a caller retries.
             ;;
             ;; The sandbox is built lazily, so this stretch no longer runs inside
             ;; `create-environment`: it runs when something first enters Python, and the
             ;; guarantee moved there with it. Forcing is what can fail, and forcing is
             ;; what has to clean up.
             (it "closes the sandbox it built when a later step throws"
                 (let [disposed
                       (atom [])

                       boom
                       (RuntimeException. "workspace exploded")]

                   (with-redefs-fn {#'env/dispose-python-context! (fn [session]
                                                                    (swap! disposed conj session)
                                                                    nil)
                                    ;; A step that runs AFTER the sandbox exists, and fails.
                                    #'env/restore-session-defs! (fn [& _]
                                                                  (throw boom))}
                     (fn []
                       (let [environment
                             (lp/create-environment ::router {:db :memory})

                             _
                             (expect (empty? @disposed)
                                     "creating a session must not build a sandbox to dispose")

                             thrown
                             (try (env/python-context environment) nil (catch Throwable t t))]

                         (expect (identical? boom thrown)
                                 "the original failure must reach the caller")
                         (expect (= 1 (count @disposed))
                                 (str "the sandbox was abandoned on the failure path"
                                      " (dispose calls: "
                                      (count @disposed)
                                      ")"))
                         (expect (some? (first @disposed))
                                 "the disposed value must be the session it built")))))))

(defdescribe
  council-python-model-history-test
  (it
    "Projectless sessions: real Python publication reaches the next model input and history"
    (if-not (clojure.java.io/resource "com/blockether/vis/internal/council/core.clj")
      (expect false "Council has not been implemented")
      (let
        [router
         (helper-router :lmstudio nil)

         a
         (lp/create-environment router {:db :memory})

         db
         (:db-info a)

         b
         (lp/create-environment router {:db db})

         aid
         (str (:session-id a))

         bid
         (str (:session-id b))

         update!
         (requiring-resolve 'com.blockether.vis.internal.gateway.state/update-session!)

         drop!
         (ns-resolve 'com.blockether.vis.internal.gateway.state 'drop-session!)

         requests
         (atom [])

         source
         (str
           "assert session['id'] == '"
           aid
           "'\n"
           "assert set(session['council']) == {'default_group_id', 'pending_replies'}\n"
           "session['council']['default_group_id'] = 'tampered'\n"
           "await council.publish(kind='coordination', content='Boundary message ' + 'é' * 2000, title='API', ping=['"
           bid
           "'])\n" "await council.members()")]

        (try
          (doseq [sid [aid bid]]
            (expect (nil? (:project-id (persistance/db-get-session db sid))))
            (update! sid
                     (constantly {:current-turn "fixture"
                                  :turns {"fixture" {:status "running"
                                                     :cancel-token
                                                     (cancellation/cancellation-token)}}})))
          (with-redefs [toggles/enabled?
                        (fn [id]
                          (contains? #{"council" "introspection"} id))

                        vis/toggle-enabled?
                        (fn [id]
                          (contains? #{"council" "introspection"} id))]

            (doseq
              [[environment request codes]
               [[a "publish" [source]]
                [b "receive"
                 [:retry :error :empty
                  "page = await council.threads()\nentries = await council.read(thread_id=page['entries'][0]['thread_id'], limit=1)\nentry = await council.get(entry_id=entries['entries'][0]['entry_id'])\nprint(len(entry['content']))"
                  "before = await read_session()\nping = before['transcript']['turns'][0]['iterations'][0]['council_input']\nfold_session('-t1/i1', 'Council reviewed')\nafter = await read_session()\nassert ping == after['transcript']['turns'][0]['iterations'][0]['council_input']"]]]]
              (let [idx (atom -1)
                    loop-result (atom nil)
                    tid (persistance/db-store-session-turn!
                          db
                          {:parent-session-id (:session-id environment) :user-request request})]

                (with-redefs [svar/ask-code!
                              (fn [_ opts]
                                (swap! requests conj
                                  {:sid (str (:session-id environment)) :messages (:messages opts)})
                                (let [code (get codes (swap! idx inc))]
                                  (cond (= :retry code)
                                        (throw (ex-info "Retry fixture"
                                                        {:type :svar.llm/max-tokens-exceeded
                                                         :output-tokens 8192}))
                                        (= :error code) (throw (ex-info
                                                                 "Recoverable model-format fixture"
                                                                 {:type :fixture/format-error}))
                                        (= :empty code)
                                        {:stop-reason :tool-calls :tool-calls [] :tokens {}}
                                        code {:stop-reason :tool-calls
                                              :tool-calls [{:id "council-block"
                                                            :name "python_execution"
                                                            :input {:code code}}]
                                              :tokens {}}
                                        :else {:stop-reason :end
                                               :content "done"
                                               :tool-calls []
                                               :tokens {}})))]
                  (let [result (lp/iteration-loop environment request {:session-turn-id tid})]
                    (reset! loop-result
                      (assoc (select-keys result [:status-id :trace :iteration-count])
                        :worker-errors
                        (mapv (fn [path]
                                (with-open [reader (clojure.java.io/reader path)]
                                  (vec (take-last 30
                                                  (remove #(str/starts-with?
                                                             %
                                                             "Picked up JAVA_TOOL_OPTIONS:")
                                                    (line-seq reader))))))
                              (keep #(get-in % [:error :data :log]) (:trace result)))))
                    (expect (pos? @idx) (pr-str result))
                    ;; Council's input-state Atom belongs to the live activation,
                    ;; never to the context snapshot saved with the final answer.
                    (let [ctx @(:ctx-atom environment)
                          clean (eng/strip-ephemeral ctx)]

                      (expect (not (contains? (ctx-loop/read-turn-state environment) :council)))
                      (expect (not (contains? ctx :council-actor)))
                      (expect (not-any? #(str/starts-with? % "engine_council_")
                                        (filter string? (keys ctx))))
                      (expect (every? string? (keys clean)))
                      (expect (not-any? #(str/starts-with? % "engine_council_")
                                        (filter string? (keys clean))))
                      (expect (true? (#'lp/persist-turn-outcome!
                                      db
                                      tid
                                      {:status :success
                                       :content {:type "text" :text "done"}
                                       :ctx clean})))
                      (expect (nil? (:error (first (persistance/db-list-session-turns
                                                     db
                                                     (:session-id environment)))))))))
                (let [iterations (persistance/db-list-session-turn-iterations db tid)
                      rows (mapcat #(tree-seq (comp seq :children) :children %)
                                   (mapcat #(get-in % [:activity :rows])
                                           (mapcat :forms iterations)))
                      operations (filter #(str/starts-with? (:operation %) "council.") rows)]

                  (expect (= (if (= request "receive") 3 2) (count operations))
                          (pr-str {:request request
                                   :loop-result @loop-result
                                   :worker-policy (select-keys ((:jail-policy-fn environment))
                                                               [:disabled? :inbound-ports
                                                                :loopback-port])
                                   :forms (mapv #(select-keys % [:error :stdout :activity])
                                                (mapcat :forms iterations))}))
                  (doseq [row operations]
                    (expect (some #(= "council-group" (:type %)) (:resources row))))
                  (expect (= (if (= request "receive") 5 2) (count iterations)))
                  (expect (empty? (remove #(= :vis/preflight (get-in % [:block :phase]))
                                    (keep :error (mapcat :forms iterations)))))
                  (if (= request "receive")
                    (expect (seq (:entries (:council-input (first iterations)))))
                    (expect (seq (:council-publications (first iterations)))))))))
          (let [incoming
                (:messages (first (filter #(= bid (:sid %)) @requests)))

                system-text
                (pr-str (filter #(contains? #{"system" "developer"} (:role %)) incoming))

                data-text
                (pr-str (remove #(contains? #{"system" "developer"} (:role %)) incoming))]

            (expect (str/includes? system-text "council.publish"))
            (expect (not (str/includes? system-text "Boundary message")))
            (expect (str/includes? data-text "Boundary message"))
            (expect (str/includes? data-text "truncated"))
            ;; C13/C14: a transparent retry and a tool-free continuation do not consume
            ;; or duplicate the first input snapshot.
            (let [received
                  (mapv :messages (filter #(= bid (:sid %)) @requests))

                  previews
                  (mapv #(filterv (fn [message]
                                    (let [body
                                          (:content message)

                                          texts
                                          (if (string? body) [body] (keep :text body))]

                                      (some (fn [text]
                                              (str/starts-with? text "Council ping —"))
                                            texts)))
                           %)
                        received)]

              (expect (= 6 (count received)))
              (expect (= [1 1 1 1 1 0] (mapv count previews)))
              (expect (apply = (take 5 previews)))))
          (finally (doseq [sid [aid bid]]
                     (drop! sid))
                   (lp/dispose-environment! b)
                   (lp/dispose-environment! a)))))))

(defdescribe
  council-execution-state-test
  (it "collects publication references outside CTX and detaches them before persistence"
      (let [environment
            {:session-id "session"
             :ctx-atom (atom {})
             :turn-state-atom (ctx-loop/make-turn-state-atom)}

            publication
            {:entry_id 7 :thread_id 7 :group_id "group" :kind "informational"}

            result
            (#'lp/with-council-execution
             environment
             {:activation-id "active"}
             ["turn" 1]
             (fn []
               (expect (= "active"
                          (get-in (ctx-loop/read-turn-state environment)
                                  [:council :activation-id])))
               (expect (empty? @(:ctx-atom environment)))
               (ctx-loop/swap-turn-state! environment
                                          update-in
                                          [:council :publications]
                                          conj
                                          publication)
               {:status :success}))]

        (expect (= [publication] (:council-publications result)))
        (expect (not (contains? (ctx-loop/read-turn-state environment) :council)))
        (expect (empty? @(:ctx-atom environment)))))
  (it "does not lose a publication accepted while execution is being detached"
      (let [environment
            {:turn-state-atom (ctx-loop/make-turn-state-atom)}

            publication
            {:id 9}

            accepted?
            (atom false)

            swap-state!
            ctx-loop/swap-turn-state!

            result
            (with-redefs [ctx-loop/swap-turn-state! (fn [env f & args]
                                                      ;; Complete a host publication immediately before cleanup.
                                                      (when (:council (ctx-loop/read-turn-state
                                                                        env))
                                                        (reset! accepted? true)
                                                        (swap! (:turn-state-atom env) update-in
                                                          [:council :publications]
                                                          conj
                                                          publication))
                                                      (apply swap-state! env f args))]
              (#'lp/with-council-execution
               environment
               {:activation-id "active"}
               ["turn" 1]
               (constantly {})))]

        (expect (or (not @accepted?) (= [publication] (:council-publications result))))
        (expect (not (contains? (ctx-loop/read-turn-state environment) :council)))))
  (it "cleans execution state after an exception or cancellation"
      (doseq [error [(ex-info "fixture failure" {}) (InterruptedException. "fixture cancellation")]]
        (let [environment {:turn-state-atom (ctx-loop/make-turn-state-atom)}
              caught (try (#'lp/with-council-execution
                           environment
                           {:activation-id "active"}
                           ["turn" 1]
                           (fn []
                             (throw error)))
                          nil
                          (catch Throwable t t))]

          (expect (identical? error caught))
          (expect (not (contains? (ctx-loop/read-turn-state environment) :council))))))
  (it "does not collect or clear a newer execution when stale work returns"
      (let [environment
            {:session-id "session" :turn-state-atom (ctx-loop/make-turn-state-atom)}

            newer
            {:activation-id "new" :iteration-key ["turn" 2] :publications [{:id 8}]}

            result
            (atom nil)

            {:keys [signals]}
            (tel/with-signals
              (reset! result (#'lp/with-council-execution
                              environment
                              {:activation-id "old"}
                              ["turn" 1]
                              (fn []
                                (ctx-loop/set-turn-state! environment :council newer)
                                {}))))]

        (expect (nil? (:council-publications @result)))
        (expect (= newer (:council (ctx-loop/read-turn-state environment))))
        (expect (= [::lp/council-execution-superseded] (mapv :id signals))))))

(defdescribe
  unfreezable-context-preserves-answer-test
  (it
    "persists a successful answer in real SQLite when its snapshot cannot be frozen"
    (let [environment
          (lp/create-environment (helper-router :lmstudio nil) {:db :memory})

          db
          (:db-info environment)

          sid
          (:session-id environment)

          tid
          (persistance/db-store-session-turn! db
                                              {:parent-session-id sid
                                               :user-request "snapshot fixture"})

          answer
          [(content/prose "The completed answer")]]

      (try (let [written
                 (atom nil)

                 {:keys [signals]}
                 (tel/with-signals (reset! written (#'lp/persist-turn-outcome!
                                                    db
                                                    tid
                                                    {:status :success
                                                     :content answer
                                                     :iteration-count 2
                                                     :duration-ms 42
                                                     :ctx {"unsupported" (Object.)}})))

                 stored
                 (first (persistance/db-list-session-turns db sid))]

             (expect (true? @written))
             (expect (= answer (:content stored)))
             (expect (= :done (:status stored)))
             (expect (= 2 (:iteration-count stored)))
             (expect (= 42 (:duration-ms stored)))
             (expect (nil? (:error stored)))
             (expect (= [::lp/turn-outcome-persist-failed] (mapv :id signals))))
           (finally (lp/dispose-environment! environment))))))

(defdescribe
  python-autocomplain-boundary-test
  (it
    "records real failed Python calls once without Council, keeping execution provenance"
    (let [environment
          (lp/create-environment (helper-router :lmstudio nil) {:db :memory})

          db
          (:db-info environment)

          sid
          (str (:session-id environment))

          _
          (persistance/db-store-session-turn! db
                                              {:parent-session-id sid :user-request "Earlier turn"})

          tid
          (persistance/db-store-session-turn! db
                                              {:parent-session-id sid
                                               :user-request "Check failures"})

          requests
          (atom [])

          calls
          (atom 0)

          codes
          ["print('before failure'); 1 / 0"
           "try:\n    raise ValueError('caught')\nexcept ValueError:\n    print({'is_pass': False})"
           "if True print('syntax')" "raise RuntimeError('second failure')" " "
           "cat(project_root_path / 'missing-autocomplain-fixture')"]]

      (try
        (with-redefs [toggles/enabled?
                      (constantly false)

                      vis/toggle-enabled?
                      (constantly false)

                      svar/ask-code!
                      (fn [_ opts]
                        (swap! requests conj (:messages opts))
                        (if (= 1 (swap! calls inc))
                          {:stop-reason :tool-calls
                           :tokens {}
                           :tool-calls (mapv (fn [idx code]
                                               {:id (str "call-" idx)
                                                :name "python_execution"
                                                :input {:code code}})
                                             (range)
                                             codes)}
                          {:stop-reason :end :content "done" :tokens {}}))]

          (let [result
                (lp/iteration-loop environment "Check failures" {:session-turn-id tid})

                rows
                ((requiring-resolve
                   'com.blockether.vis.internal.persistance.sqlite.test-helpers/raw-query)
                  db
                  {:select [:*] :from [:improve] :order-by [:entry_id]})

                entries
                (mapv #(persistance/db-council-get db (:entry_id %)) rows)]

            (expect (= 2 (:iteration-count result)))
            (expect (= 5 (count rows)))
            (expect (= [1 3 4 5 6] (mapv :form rows)))
            (expect (every? #(= [sid (str tid) 2 1]
                                ((juxt :session_soul_id :session_turn_soul_id :turn :iteration) %))
                            rows))
            (expect (= 1 (count (distinct (map :session_turn_iteration_id rows)))))
            (expect (every? :session_turn_iteration_id rows))
            (expect (every? #(= ["complain" "autocomplain" []] ((juxt :kind :source :ping) %))
                            entries))
            (expect (not (str/includes? (pr-str entries) "before failure")))
            (expect (not (str/includes? (pr-str entries) "second failure")))
            (expect (str/includes? (pr-str (last @requests)) "t2/i1/f1"))
            (expect (str/includes? (pr-str (last @requests)) "autocomplain #"))))
        (finally (lp/dispose-environment! environment))))))
