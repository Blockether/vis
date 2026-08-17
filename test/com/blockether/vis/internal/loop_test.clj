(ns com.blockether.vis.internal.loop-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.ctx-loop :as ctx-loop]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.form :as form]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.ctx-engine :as eng]
            [com.blockether.vis.internal.foundation.editing.core :as ed]
            [com.blockether.vis.internal.titling :as titling]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [com.blockether.vis.internal.human-input :as hi]
            [com.blockether.vis.internal.channel-events :as ce]
            [com.blockether.vis.internal.provider-error :as perr]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.persistance :as persistance]
            [taoensso.telemere :as tel]
            [com.blockether.vis.internal.session-model :as session-model]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.vision-describe :as vision-describe]
            [lazytest.core :refer [defdescribe describe it expect throws?]]))

(defn- captured-svar-ask-code-opts
  "Opts a global-router helper hands to `svar/ask-code!`, with no network call."
  [invoke!]
  (let [seen (atom nil)]
    (with-redefs-fn {#'lp/get-router (fn []
                                       ::router)
                     #'svar/ask-code! (fn [router opts]
                                        (reset! seen {:router router :opts opts})
                                        {:blocks [] :raw ""})}
      invoke!)
    @seen))

(defn- captured-ask-code-opts
  [opts]
  (captured-svar-ask-code-opts #(lp/ask-code! opts)))

(defn- captured-llm-text-opts
  [opts]
  (captured-svar-ask-code-opts #(lp/llm-text! opts)))

(def ^:private provider-error-explanation perr/provider-error-explanation)

(def ^:private collect-iteration-start-hints (deref #'lp/collect-iteration-start-hints))

(def ^:private ask-result->api-usage (deref #'lp/ask-result->api-usage))

(def ^:private turn-eval-evidence (deref #'lp/turn-eval-evidence))

(def ^:private ask-code-block-observation (deref #'lp/ask-code-block-observation))

(def ^:private log-stage-level (deref #'lp/log-stage-level))

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
      (let
        [provider-calls
         (atom 0)

         env
         {:db-info ::db :environment-id ::environment :router (reasoning-effort-router)}

         thrown
         (try (with-redefs
                [lp/run-turn! (fn [& _]
                                (swap! provider-calls inc)
                                (throw (ex-info "should not run" {})))]
                (lp/turn! env [{:role "user" :content "task"}] {:reasoning-effort "medium"}))
              nil
              (catch clojure.lang.ExceptionInfo e e))]

        (expect (= 0 @provider-calls))
        (expect (= :vis/unsupported-reasoning-effort (:type (ex-data thrown))))
        (expect (= ["high" "max"] (:supported (ex-data thrown))))))
  (it "does not inject a prompt for models without native reasoning"
      (let
        [environment
         (lp/create-environment ::router {:db :memory})

         seen
         (atom nil)

         messages
         [{:role "system" :content "core"} {:role "user" :content "task"}]

         message-text
         (fn [{:keys [content]}]
           (if (string? content) content (apply str (keep :text content))))]

        (try (with-redefs
               [svar/ask-code! (fn [_router opts]
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
             (finally (lp/dispose-environment! environment)))))
  (it "builds valid evidence for same-model retries"
      (let
        [iteration
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
      (let
        [base
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
    (let
      [environment
       (lp/create-environment ::router {:db :memory})

       seen
       (atom nil)]

      (try (with-redefs
             [svar/ask-code! (fn [_router opts]
                               (reset! seen opts)
                               {:stop-reason :end
                                :tool-calls []
                                :content "done"
                                :tokens {}
                                :routed/reasoning-effort {:requested "max"
                                                          :effective "max"
                                                          :supported ["high" "max"]
                                                          :wire-style :zai-effort
                                                          :extra-body {:thinking {:type "enabled"}
                                                                       :reasoning_effort "max"}}})]
             (let
               [result (lp/run-iteration environment
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

(defdescribe
  copilot-action-service-headers-test
  (it "marks Copilot Enterprise requests with X-Initiator for the action service"
      (expect (= {"X-Initiator" "agent"}
                 (#'lp/copilot-llm-headers {:provider :github-copilot-enterprise} "agent"))))
  (it "does not add action-service headers for non-Copilot providers"
      (expect (nil? (#'lp/copilot-llm-headers {:provider :anthropic-coding-plan} "agent")))))

(defdescribe
  environment-lifecycle-test
  (it "closes the GraalPy context when disposing an environment"
      (let
        [environment
         (lp/create-environment ::router {:db :memory})

         python-context
         (:python-context environment)]

        (lp/dispose-environment! environment)
        (expect (try (env/run-python-block python-context "1") false (catch Throwable _ true))))))

(defdescribe
  permission-config-snapshot-test
  (it "keeps every process-jail and network grant immutable until environment rebuild"
      (require 'com.blockether.vis.internal.config-spec :reload)
      (require 'com.blockether.vis.internal.loop :reload)
      (let
        [cfg
         (atom {"workspace" {"filesystem"
                             [{"id" "full" "path" "/approved/full"}
                              {"id" "read" "path" "/approved/read" "access" "read-only"}
                              {"id" "cache" "path" "/approved/cache" "search" false}]}
                "jail" {"enabled" true
                        "filesystem" {"allow" ["full" "read" "cache"]}
                        "network" {"allowed_domains" ["approved.example"] "inbound_ports" [5273]}}})

         vis-home-root
         ;; Engine-level implicit grant: Vis's own session folder.
         (.getCanonicalPath (java.io.File. (System/getProperty "user.home") ".vis"))

         snapshot
         (with-redefs [config/load-config-raw #(deref cfg)]
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
  (it "fails closed when a child is created without its parent's snapshot"
      (expect (throws? clojure.lang.ExceptionInfo
                       #(lp/create-environment ::router {:child {:parent-db-info ::borrowed}})))))

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

(def ^:private max-tokens-exceeded-error? (deref #'lp/max-tokens-exceeded-error?))



(def ^:private next-retry-counters (deref #'lp/next-retry-counters))

(def ^:private emergency-fold-projection (deref #'lp/emergency-fold-projection))

(def ^:private context-overflow-recovery! (deref #'lp/context-overflow-recovery!))

(def ^:private estimator-undercount (deref #'lp/estimator-undercount))

(def ^:private overflow-fold-budget (deref #'lp/overflow-fold-budget))

(def ^:private provider-output-chunk? (deref #'lp/provider-output-chunk?))


(def ^:private bumped-max-tokens-extra-body (deref #'lp/bumped-max-tokens-extra-body))

(def ^:private llm-provider-error-context (deref #'lp/llm-provider-error-context))

(def ^:private previous-turn-context (deref #'lp/previous-turn-context))

(def ^:private previous-request-usage (deref #'lp/previous-request-usage))
(def ^:private run-normal-turn! (deref #'lp/run-normal-turn!))

(def ^:private maybe-auto-title! (deref #'titling/maybe-auto-title!))

;; Regression, issue #105: Vis retried a provider stream failure after Svar had already
;; finished its own policy, issuing the same user request more than once.
(defdescribe provider-stream-failure-is-terminal-test
  (it "does not retry a provider failure that escapes Svar"
      (let [env (lp/create-environment ::router {:db :memory})
            calls (atom 0)]
        (try
          (with-redefs [svar/ask-code! (fn [_router _opts]
                                         (swap! calls inc)
                                         (throw (ex-info "Stream connection error: closed"
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
    (let
      [env
       (lp/create-environment ::router {:db :memory})

       chunks
       (atom [])

       during
       (atom nil)]

      (try
        (with-redefs
          [svar/ask-code!
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
          (let
            [live (filterv #(= :provider-retry-reset (:phase %)) @during)
             ev (:event (first live))]

            (expect (= 1 (count live)))
            (expect (= {:iteration 1 :attempt 1 :max-retries 3 :delay-ms 2000}
                       (select-keys (first live) [:iteration :attempt :max-retries :delay-ms])))
            (expect (= :svar.llm/empty-content (get-in (first live) [:error :type])))
            (expect (= :empty-content (:reason ev)))
            (expect (= "anthropic" (:from-provider ev)))
            (expect (= "claude-x" (:from-model ev)))))
        (finally (lp/dispose-environment! env))))))

;; Regression, issue #120: every provider request looked identical in the TUI, so a
;; long tool-result loop was indistinguishable from the client re-sending on its
;; own — the spinner said "Vis is calling the provider (iter 12)" and never why.
(defdescribe
  provider-call-continuation-reason-test
  (let
    [reason-of
     (fn [iteration]
       (let
         [env
          (lp/create-environment ::router {:db :memory})

          chunks
          (atom [])]

         (try
           (with-redefs
             [svar/ask-code! (fn [_router _opts]
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

(defdescribe persisted-form-omits-a-derivable-render-test
             ;; Regression: every persisted form used to carry `:result-render` — a
             ;; rendered copy of the `:result`/`:stdout` sitting right beside it in the
             ;; same blob, 32% of `tool_calls`. The envelope must carry only what a
             ;; reader cannot re-derive.
             (it "drops a render the reader re-derives from the result"
                 (let [block {:id 0
                              :code "grep({})"
                              :result {"files" ["a.clj"]}
                              :result-render (form/result-render {:src "grep({})" :result {"files" ["a.clj"]}})}
                       [envelope] (eng/blocks->forms [block] {:turn 1 :iter 1})]
                   (expect (not (contains? envelope :result-render)))
                   (expect (= {"files" ["a.clj"]} (:result envelope)))
                   ;; …and the reader gets the very same body back
                   (expect (= (:result-render block) (form/result-render envelope)))))
             (it "keeps a render no projection reproduces"
                 ;; A `!cmd` bubble: its body is the shell layer's own card markdown.
                 (let [[envelope] (eng/blocks->forms [{:id 0
                                                       :code "await shell({\"command\": \"ls\"})"
                                                       :result {"ok" true}
                                                       :result-render "**SHELL**\nls"}]
                                                     {:turn 1 :iter 1})]
                   (expect (= "**SHELL**\nls" (:result-render envelope)))))
             (it "carries the timeout FLAG, and derives no card from it"
                 (let [[envelope] (eng/blocks->forms [{:id 0
                                                       :code "time.sleep(99)"
                                                       :timeout? true
                                                       :error {:message "Timeout (30s)"
                                                               :data {:timeout-ms 30000}}}]
                                                     {:turn 1 :iter 1})]
                   (expect (true? (:timeout? envelope)))
                   (expect (not (contains? envelope :result-render)))
                   (expect (nil? (form/result-display envelope))))))

(defdescribe guest-interrupt-on-eval-timeout-test
             ;; REGRESSION: an eval timeout (and Esc cancel) only did `Future.cancel(true)`.
             ;; GraalPy does NOT observe `Thread.interrupt` inside guest code, so a model
             ;; block that spins (`while True: ...`) kept burning a whole core FOREVER —
             ;; measured at 1.01 busy cores with BOTH worker futures already cancelled —
             ;; and pinned its virtual thread's carrier. Only a Truffle safepoint
             ;; interrupt unwinds the guest frame, and it must leave the context REUSABLE.
             (it
               "unwinds a runaway guest loop and keeps the context usable"
               (let
                 [pc
                  (:python-context (env/create-python-context {}))

                  cpu-ns
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
                  (busy-cores 250)]

                 (try (let
                        [result (binding [rt/*eval-timeout-ms* 400]
                                  ((deref #'lp/run-python-code) pc "while True:\n    pass"))]
                        (expect (true? (:timeout? result)))
                        ;; The guest is GONE: no EXTRA core is spinning after the timeout.
                        ;; Take the quieter of two samples so one unlucky GC/JIT burst
                        ;; cannot decide the verdict.
                        (expect (< (- (min (busy-cores 250) (busy-cores 250)) baseline) 0.75))
                        ;; ...and the interrupt did not poison the context.
                        (expect (= 42 (:result ((deref #'lp/run-python-code) pc "40 + 2")))))
                      (finally (try (.close ^org.graalvm.polyglot.Context pc true)
                                    (catch Throwable _ nil)))))))

(defdescribe eval-timeout-keeps-partial-stdout-test
             ;; REGRESSION: the wall-clock BACKSTOP answered with `{:result nil :error
             ;; "Timeout (120s)"}` and NOTHING else. The guest never reaches its own
             ;; `{:stdout}` outcome, so every line the block had already printed — the
             ;; progress log of a long fetch loop, results already computed — died with
             ;; the frame. The model then re-ran the whole block blind, which is how a
             ;; serial `httpx` sweep burned two turns instead of one.
             (it
               "surfaces what the block printed before the wall fired"
               (let
                 [pc
                  (:python-context (env/create-python-context {}))]

                 (try (let
                        [result (binding [rt/*eval-timeout-ms* 500]
                                  ((deref #'lp/run-python-code) pc "print('fetched 1')\nwhile True:\n    pass"))]

                        (expect (true? (:timeout? result)))
                        (expect (some? (re-find #"fetched 1" (str (:stdout result))))))
                      (finally (try (.close ^org.graalvm.polyglot.Context pc true)
                                    (catch Throwable _ nil)))))))

(defdescribe python-block-runs-in-the-session-context-test
             ;; REGRESSION: the eval worker thread bound only the per-block sinks, so the
             ;; block itself ran with NO session context. A sandbox SHIM bridge reads the
             ;; AMBIENT context (an extension SYMBOL installs its own around every call),
             ;; so `ls` saw an EMPTY `workspace/*filesystem-roots*` and refused every bound
             ;; extra filesystem root — "escapes the allowed workspace roots" — while
             ;; `cat`/`grep` on the very same path answered normally.
             (it
               "gives a shim bridge the filesystem roots the session actually bound"
               (let
                 [pc
                  (:python-context (env/create-python-context {}))

                  ;; Outside the primary cwd and outside every always-on root
                  ;; (temp dirs, `~/.vis`) — reachable ONLY as a bound root.
                  outside
                  (System/getProperty "user.home")

                  code
                  (str "try:\n"
                       "    rows = ls(" (pr-str outside) ")\n"
                       "    print('listed', isinstance(rows, list))\n"
                       "except Exception as e:\n"
                       "    print('refused', e)\n")

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
                    (str (:stdout ((deref #'lp/run-python-code) pc code :env (env-with roots)))))]

                 (try
                   ;; The path is genuinely outside what confinement grants by itself...
                   (expect (str/starts-with? (listing []) "refused"))
                   ;; ...and the moment the session binds it, the block's own `ls` reaches it.
                   (expect (str/starts-with? (listing [outside]) "listed True"))
                   (finally (try (.close ^org.graalvm.polyglot.Context pc true)
                                 (catch Throwable _ nil)))))))

(defdescribe parallel-sub-loops-child-error-isolation-test
             ;; REGRESSION: the settle loop only cancelled siblings on InterruptedException,
             ;; and nothing guarded the child body beyond `run-spec!`'s own catch. An Error
             ;; (or a throw while BUILDING the failed result) surfaced from `deref` as an
             ;; ExecutionException that (a) sank the whole batch, against the documented
             ;; "a child that throws does NOT sink the batch" contract, and (b) skipped the
             ;; cancel, leaving every sibling running as an orphaned full LLM turn whose
             ;; result nobody would ever read.
             (it "an escaping child error becomes one failed slot and never orphans a sibling"
                 (let
                   [sib
                    (atom {:started false :finished false})

                    sibling-started
                    (promise)

                    release-sibling
                    (promise)]

                   (with-redefs-fn {#'lp/run-spec!
                                    (fn [_ spec]
                                      (if (= "boom" (get spec "prompt"))
                                        (do (when-not (deref sibling-started 1000 false)
                                              (throw (ex-info "sibling never started" {})))
                                            (deliver release-sibling true)
                                            (throw (Error. "hard child failure")))
                                        (do (swap! sib assoc :started true)
                                            (deliver sibling-started true)
                                            @release-sibling
                                            (swap! sib assoc :finished true)
                                            {"status" "completed"})))}
                     (fn []
                       (let [res (lp/parallel-sub-loops! nil [{"prompt" "boom"} {"prompt" "ok"}])]
                         ;; input order preserved, one slot per spec
                         (expect (= 2 (count res)))
                         (expect (= "failed" (get (first res) "status")))
                         (expect (= "hard child failure" (get (first res) "error")))
                         ;; the sibling's work is RETURNED, not orphaned
                         (expect (= "completed" (get (second res) "status")))
                         (expect (true? (:finished @sib)))))))))

(defdescribe tool-call-execution-test
             ;; REGRESSION: tool calling once shipped 100% broken — `run-iteration`
             ;; synthesized `env* (assoc environment)` (a 1-arg assoc) before execute-code, so
             ;; EVERY tool-call iteration threw ArityException ("Provider unavailable / Wrong
             ;; number of args (1) passed to clojure.core/assoc"). 120+ loop tests stayed green
             ;; because none drove a real tool-call response through `run-iteration`. This does.
             (it
               "executes a python_execution tool call through run-iteration without throwing"
               (let
                 [env
                  (lp/create-environment ::router {:db :memory})

                  chunks
                  (atom [])]

                 (try (with-redefs
                        [svar/ask-code! (fn [_router _opts]
                                          {:stop-reason :tool-calls
                                           :tool-calls [{:id "call_1"
                                                         :name "python_execution"
                                                         :input {:code "print(6*7)"}}]
                                           :content nil
                                           :reasoning "computing"
                                           :tokens {}})]
                        ;; The bug threw HERE — a tool-call iteration reaching the execute path.
                        (let
                          [result (lp/run-iteration env
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
  previous-turn-context-test
  ;; Cross-process RESUME carry must be a pure function of the DB so the wire is
  ;; identical regardless of process (see DERIVED_WIRE.md). These pin: ALL prior
  ;; answered turns carried (not just the latest), each with its r[] scope index;
  ;; determinism; and summary-awareness (drop/summarize reshape uniformly).
  (it
    "carries ALL prior answered turns with their r[] scope index"
    (with-redefs
      [persistance/db-list-session-turns
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
             :forms [{:scope "t1/i1/f1" :src "cat(\"a\")" :result {:path "a"}}
                     {:scope "t1/i1/f2" :src "set_session_title(...)" :result "vis_silent"}]}]

           "t2"
           [{:status :done
             :position 1
             :forms [{:scope "t2/i1/f1" :src "rg({...})" :result {:hits []}}]}]

           []))]

      (let [out (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})} "t3")]
        (expect (= 2 (count out))) ; both answered turns, not just latest
        (expect (= "Read a" (:user-request (first out))))
        (expect (= [{:scope "t1/i1/f1" :src "cat(\"a\")"}] (:results (first out)))) ; sentinel f2 excluded
        (expect (= [{:scope "t2/i1/f1" :src "rg({...})"}] (:results (second out)))))))
  (it "keeps synthetic slash commands out of later provider context"
      (with-redefs
        [persistance/db-list-session-turns
         (constantly [{:id "t1"
                       :status :done
                       :user-request "/draft list"
                       :content [(content/prose "2 drafts")]}])

         persistance/db-list-session-turn-iterations
         (constantly [{:status :done
                       :forms [{:scope "t1/i1/f1"
                                :tag :user-slash
                                :src "/draft list"
                                :result {"drafts" [{"label" "secret-feature"}]}}]}])]

        (expect (nil? (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})}
                                             "t2")))))
  (it
    "is deterministic — same DB ⇒ identical output (process-invariant)"
    (with-redefs
      [persistance/db-list-session-turns
       (constantly
         [{:id "t1" :status :done :position 1 :user-request "q" :content [(content/prose "a")]}])

       persistance/db-list-session-turn-iterations
       (constantly
         [{:status :done :position 1 :forms [{:scope "t1/i1/f1" :src "cat(x)" :result {:k 1}}]}])]

      (let [env {:session-id "s1" :db-info ::db :ctx-atom (atom {})}]
        (expect (= (previous-turn-context env "t9") (previous-turn-context env "t9"))))))
  (it
    "is summary-aware at ITERATION granularity: session_drop leaves a dropped breadcrumb, fold_session collapses to one gist"
    ;; Folds are recorded at iteration scope (tN/iN) — what the prompt instructs
    ;; and what the live wire (apply-summaries) matches. Each form carries a FORM
    ;; scope (tN/iN/fN); prior-turn-scope-index normalizes form→iteration before
    ;; matching (the path-A fix). :drop? — not gist presence — picks the label. A
    ;; dropped iteration collapses to ONE `dropped` audit line (keeping the why);
    ;; a folded iteration with multiple forms collapses to ONE gist line.
    (with-redefs
      [persistance/db-list-session-turns
       (constantly
         [{:id "t1" :status :done :position 1 :user-request "q" :content [(content/prose "a")]}])

       persistance/db-list-session-turn-iterations
       (constantly [{:status :done
                     :position 1
                     :forms [{:scope "t1/i1/f1" :src "cat(a)" :result {:k 1}} ; iter i1 → dropped
                             {:scope "t1/i2/f1" :src "cat(b)" :result {:k 2}} ; iter i2 → folded
                             {:scope "t1/i2/f2" :src "cat(c)" :result {:k 3}}]}])]

      ; (same iter, 2nd form)
      (let
        [env
         {:session-id "s1"
          :db-info ::db
          :ctx-atom (atom {"session_summaries"
                           [{"scopes" #{"t1/i1"} "drop" true "gist" "wrong file"} ; drop i1
                            {"scopes" #{"t1/i2"} "gist" "b pinned"}]})}

         ; fold i2
         results
         (:results (first (previous-turn-context env "t9")))]

        (expect (= 2 (count results))) ; i1 dropped-line + i2 gist (each deduped)
        (let [by-scope (into {} (map (juxt :scope identity)) results)]
          (expect (= {:scope "t1/i1" :dropped? true :note "wrong file"} (get by-scope "t1/i1")))
          (expect (= {:scope "t1/i2" :gist "b pinned"} (get by-scope "t1/i2")))))))
  (it "returns nil when every prior turn is current/running/blank-answer"
      (with-redefs
        [persistance/db-list-session-turns
         (constantly
           [{:id "t1" :status :done :position 1 :user-request "old" :content [(content/prose "")]}
            {:id "t2" :status :running :user-request "now" :content [(content/prose "partial")]}])

         persistance/db-list-session-turn-iterations
         (constantly [])]

        (expect (nil? (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})}
                                             "t2")))))
  (it "carries prior provider-error turns as unfinished cross-turn context"
      (with-redefs
        [persistance/db-list-session-turns
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
      (with-redefs
        [persistance/db-list-session-turns
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
             :forms [{:scope (str id "/i1/f1") :src (str "cat(" id ")") :result {:ok true}}]}])]

        (let
          [env {:session-id "s1"
                :db-info ::db
                :ctx-atom (atom {"session_summaries" [{"scopes" #{"t1/i1"} "gist" "fine detail"}
                                                      {"through" "t2/i1"
                                                       "gist" "one durable checkpoint"}]})}]
          (expect (nil? (previous-turn-context env "t3"))))))
  (it "a gist-less whole-turn fold of a no-iteration turn leaves a visible tombstone checkpoint"
      ;; No done iterations → no trailer anchor exists anywhere, so previous-
      ;; turn-context must materialize the checkpoint itself instead of letting
      ;; the turn vanish without a trace.
      (with-redefs
        [persistance/db-list-session-turns
         (constantly [{:id "t1"
                       :status :done
                       :position 1
                       :user-request "spent request"
                       :content [(content/prose "spent answer")]}
                      {:id "t2" :status :running :position 2}])

         persistance/db-list-session-turn-iterations
         (constantly [])]

        (let
          [out (previous-turn-context {:session-id "s1"
                                       :db-info ::db
                                       :ctx-atom (atom {"session_summaries" [{"scopes" #{"t1"}}]})}
                                      "t2")]
          (expect (= 1 (count out)))
          (expect (:checkpoint? (first out)))
          (expect (= [1] (:turns (first out))))
          (expect (clojure.string/includes? (str (:gist (first out))) "dropped"))
          (expect (nil? (:user-request (first out)))))))
  (it
    "an enumerated iteration fold covering EVERY iteration still keeps the turn's Q/A recap"
    ;; Regression: 'all iterations folded' must NOT be inferred as whole-turn
    ;; intent — only a bare tN or a spanning range selector removes Q/A.
    (with-redefs
      [persistance/db-list-session-turns
       (constantly [{:id "t1"
                     :status :done
                     :position 1
                     :user-request "keep my question"
                     :content [(content/prose "keep my answer")]}
                    {:id "t2" :status :running :position 2}])

       persistance/db-list-session-turn-iterations
       (constantly
         [{:status :done :position 1 :forms [{:scope "t1/i1/f1" :src "cat(a)" :result {:k 1}}]}])]

      (let
        [out (previous-turn-context {:session-id "s1"
                                     :db-info ::db
                                     :ctx-atom (atom {"session_summaries" [{"scopes" #{"t1/i1"}
                                                                            "gist" "read a"}]})}
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
    (with-redefs
      [persistance/db-list-session-turns
       (constantly [{:id "t1"
                     :status :done
                     :position 1
                     :user-request "keep my question"
                     :content [(content/prose "keep my answer")]}
                    {:id "t2" :status :running :position 2}])

       persistance/db-list-session-turn-iterations
       (constantly
         [{:status :done :position 1 :forms [{:scope "t1/i1/f1" :src "cat(a)" :result {:k 1}}]}])]

      (let
        [out (previous-turn-context
               {:session-id "s1"
                :db-info ::db
                :ctx-atom (atom {"session_summaries"
                                 [{"scopes" #{"t1"} "issued_turn" 1 "gist" "folded so far"}]})}
               "t2")]
        (expect (= 1 (count out)))
        (expect (= "keep my question" (:user-request (first out))))
        (expect (= "keep my answer" (:answer (first out)))))))
  (it
    "a whole-turn fold ISSUED IN A LATER turn still removes the target turn's Q/A recap"
    ;; The normal prior-turn case: turn 2 folds turn 1 (issued_turn 2 > 1) — it
    ;; actually saw turn 1's answer, so removal is safe and the trailer owns the
    ;; one checkpoint. With only turn 1 present, the whole context collapses.
    (with-redefs
      [persistance/db-list-session-turns
       (constantly [{:id "t1"
                     :status :done
                     :position 1
                     :user-request "old q"
                     :content [(content/prose "old a")]} {:id "t2" :status :running :position 2}])

       persistance/db-list-session-turn-iterations
       (constantly
         [{:status :done :position 1 :forms [{:scope "t1/i1/f1" :src "cat(a)" :result {:k 1}}]}])]

      (expect (nil? (previous-turn-context {:session-id "s1"
                                            :db-info ::db
                                            :ctx-atom (atom {"session_summaries"
                                                             [{"scopes" #{"t1"}
                                                               "issued_turn" 2
                                                               "gist" "folded prior turn"}]})}
                                           "t2")))))
  (it "carries cancelled turns with settled work and an explicit cancellation boundary"
  (with-redefs
    [persistance/db-list-session-turns
     (constantly [{:id "t1"
                   :status :cancelled
                   :position 1
                   :user-request "inspect and fix"}
                  {:id "t2" :status :running :position 2 :user-request "continue"}])

     persistance/db-list-session-turn-iterations
     (constantly [{:status :done
                   :position 1
                   :forms [{:scope "t1/i1/f1" :src "cat(src)" :result {:path "src"}}]}
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
                 (with-redefs
                   [persistance/db-list-session-turns
                    (fn [_db-info session-id]
                      (expect (= "s1" session-id))
                      [{:id "t1" :position 1} {:id "t2" :position 2}
                       {:id "t3" :position 3 :status :running}])

                    persistance/db-list-session-turn-iterations
                    (fn [_db-info turn-id]
                      (case turn-id
                        "t2"
                        [{:position 1 :input-tokens 42000} {:position 2 :input-tokens 51000}]

                        "t1"
                        [{:position 1 :input-tokens 10000}]

                        []))]

                   (expect (= {:last-request-tokens 51000
                               :last-request-turn-id "t2"
                               :last-request-turn-position 2
                               :last-request-iteration 2}
                              (previous-request-usage {:session-id "s1" :db-info ::db} "t3")))))
             (it "returns nil when no prior iteration has input tokens"
                 (with-redefs
                   [persistance/db-list-session-turns
                    (constantly [{:id "t1" :position 1} {:id "t2" :position 2}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:position 1 :input-tokens 0}])]

                   (expect (nil? (previous-request-usage {:session-id "s1" :db-info ::db} "t2"))))))

(defdescribe stamp-utilization-monotonic-test
             ;; Regression: the stamp used to (dissoc "engine_utilization") on a nil
             ;; measurement, so a transient req=0 (iter-1 seed miss / errored iter)
             ;; BLANKED an already-shown "session_utilization" — the "sometimes works,
             ;; sometimes doesn't" flicker. The stamp must be monotonic.
             (let
               [stamp
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
  (let
    [scope-key
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
        (let
          [out (expand-through [{"through" "t1/i3" "gist" "g"}] ["t1/i1" "t1/i2" "t1/i3" "t1/i4"])]
          (expect (= #{"t1/i1" "t1/i2" "t1/i3"} (get (first out) "scopes")))
          (expect (nil? (get (first out) "through")))
          (expect (= "g" (get (first out) "gist")))))
    (it "expand-through leaves explicit-scope summaries untouched"
        (let [s [{"scopes" #{"t1/i2"} "gist" "g"}]]
          (expect (= s (expand-through s ["t1/i1" "t1/i2"])))))
    (it "apply-summaries collapses a through-range over the trailer, sparing later steps"
        (let
          [trailer
           [[0 {:forms-vec [{:scope "t1/i1/f1" :result "a"}]}]
            [1 {:forms-vec [{:scope "t1/i2/f1" :result "b"}]}]
            [2 {:forms-vec [{:scope "t1/i3/f1" :result "c"}]}]]

           out
           (apply-summaries trailer [{"through" "t1/i2" "gist" "early"}])]

          (expect (true? (:collapsed? (second (nth out 0)))))
          (expect (true? (:collapsed? (second (nth out 1)))))
          (expect (nil? (:collapsed? (second (nth out 2)))))))
    (it "prices only the projected wire while retaining the raw universe"
        (let
          [trailer
           [[0
             {:forms-vec [{:scope "t1/i1/f1"
                           :svar/tool-call-id "call-big"
                           :result (apply str (repeat 4000 "x"))}]}]
            [1 {:forms-vec [{:scope "t1/i2/f1" :result (apply str (repeat 400 "y"))}]}]]

           ca
           (atom {"session_summaries" [{"scopes" #{"t1/i1"} "gist" "already folded"}]})

           wire
           (apply-summaries trailer (get @ca "session_summaries"))]

          (stamp-iter-universe! ca trailer wire)
          ;; A collapsed iteration keeps its identity in the universe but no longer
          ;; contributes its historical 1k-token raw weight to a later broad fold.
          (expect (= ["t1/i1" "t1/i2"] (get @ca "engine_iter_universe")))
          (expect (= {"t1/i1" 0 "t1/i2" 100} (get @ca "engine_iter_weights")))
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
        (let
          [payload
           (apply str (repeat 4000 "x"))

           seed
           (fn [scope status]
             {:forms-vec [{:scope scope :result payload}]
              :cross-turn/turn-status status
              :preserved-thinking/replay? false})

           trailer
           [[0 (seed "t1/i1/f1" :done)]
            [1 (seed "t2/i1/f1" :cancelled)]
            [2 {:forms-vec [{:scope "t3/i1/f1" :result payload}]}]]

           ca
           (atom {})]

          (stamp-iter-universe! ca trailer)
          (expect (= {"t1/i1" 0 "t2/i1" 1000 "t3/i1" 1000}
                     (get @ca "engine_iter_weights")))))
    ;; Frozen-prompt regression (session 0cfd25a7…): a fold recorded under an
    ;; EARLIER/foreign turn numbering kept re-resolving its range cursor against
    ;; every later live turn, collapsing the whole trailer. The model then never
    ;; saw its own tool results and re-issued the same call for 60+ iterations.
    (it "a fold whose cursor outlives the live turn numbering never collapses the live turn"
        (let
          [trailer
           [[0 {:forms-vec [{:scope "t95/i1/f1" :result "a"}]}]
            [1 {:forms-vec [{:scope "t95/i2/f1" :result "b"}]}]
            [2 {:forms-vec [{:scope "t95/i3/f1" :result "c"}]}]]

           out
           (apply-summaries trailer [{"through" "t113/i9" "gist" "stale numbering"}])]

          (expect (every? (fn [[_ rec]]
                            (nil? (:collapsed? rec)))
                          out))
          (expect (= trailer out))))
    (it "a fold recorded in an EARLIER turn never collapses the live turn"
        (let
          [trailer
           [[0 {:forms-vec [{:scope "t96/i1/f1" :result "a"}]}]
            [1 {:forms-vec [{:scope "t96/i2/f1" :result "b"}]}]]

           out
           (apply-summaries trailer [{"scopes" #{"t96/i1" "t96/i2"} "gist" "old" "at_turn" 95}])]

          (expect (every? (fn [[_ rec]]
                            (nil? (:collapsed? rec)))
                          out))))
    (it "a fold recorded in THIS turn still collapses its own live iterations"
        (let
          [trailer
           [[0 {:forms-vec [{:scope "t96/i1/f1" :result "a"}]}]
            [1 {:forms-vec [{:scope "t96/i2/f1" :result "b"}]}]]

           out
           (apply-summaries trailer [{"through" "t96/i1" "gist" "in-turn" "at_turn" 96}])]

          (expect (true? (:collapsed? (second (nth out 0)))))
          (expect (nil? (:collapsed? (second (nth out 1)))))))
    (it "a stale-numbered fold still collapses PRIOR-turn scopes on the trailer"
        (let
          [trailer
           [[0 {:preserved-thinking/replay? false :forms-vec [{:scope "t94/i1/f1" :result "old"}]}]
            [1 {:forms-vec [{:scope "t95/i1/f1" :result "live"}]}]]

           out
           (apply-summaries trailer [{"scopes" #{"t94/i1" "t95/i1"} "gist" "g" "at_turn" 94}])]

          (expect (true? (:collapsed? (second (nth out 0)))))
          (expect (nil? (:collapsed? (second (nth out 1)))))))
    (it "prior-turn-scope-index: gist applies via form->iter normalization, ONE deduped entry"
        ;; The path-A regression: a fold recorded at iteration scope (t1/i1) must
        ;; apply to forms carrying FORM scopes (t1/i1/f1, t1/i1/f2) and collapse to
        ;; a SINGLE gist line, not repeat per form.
        (let
          [forms
           [{:scope "t1/i1/f1" :result "a" :src "(cat \"x\")"}
            {:scope "t1/i1/f2" :result "b" :src "(rg \"y\")"}
            {:scope "t1/i2/f1" :result "c" :src "(ls)"}]

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
      (let
        [forms
         (vec (for [i (range 1 21)]
                {:scope (str "t1/i" i "/f1") :result "r" :src "(cat)"}))

         out
         (prior-scope-index forms
                            [{"scopes" (into #{} (map #(str "t1/i" %)) (range 1 21))
                              "gist" "one big gist"}])]

        (expect (= [{:scope "t1/i1" :gist "one big gist"}] out))))
    (it "prior-turn-scope-index: distinct gists stay distinct while each collapses to one line"
        (let
          [forms
           [{:scope "t1/i1/f1" :result "a" :src "(cat)"} {:scope "t1/i2/f1" :result "b" :src "(rg)"}
            {:scope "t1/i3/f1" :result "c" :src "(ls)"} {:scope "t1/i4/f1" :result "d" :src "(ls)"}]

           out
           (prior-scope-index forms
                              [{"scopes" #{"t1/i1" "t1/i2"} "gist" "A"}
                               {"scopes" #{"t1/i3" "t1/i4"} "gist" "B"}])]

          (expect (= [{:scope "t1/i1" :gist "A"} {:scope "t1/i3" :gist "B"}] out))))
    (it
      "prior-turn-scope-index: a dropped iteration collapses to ONE dropped breadcrumb keeping the why"
      (let
        [forms
         [{:scope "t1/i1/f1" :result "a" :src "(cat)"} {:scope "t1/i1/f2" :result "b" :src "(rg)"} ; same dropped iter → still ONE line
          {:scope "t1/i2/f1" :result "c" :src "(ls)"}]

         out
         (prior-scope-index forms [{"scopes" #{"t1/i1"} "drop" true "gist" "misread"}])]

        ;; the iteration's forms collapse to a single audit line, not per-form, not vanished
        (expect (= {:scope "t1/i1" :dropped? true :note "misread"} (first (filter :dropped? out))))
        (expect (= 1 (count (filter :dropped? out))))
        (expect (not-any? #(re-find #"^t1/i1/" (str (:scope %))) out)) ; no raw forms from i1
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
    (let
      [seen
       (atom nil)

       env
       {:db-info ::db :session-id "s1" :turn-state-atom (ctx-loop/make-turn-state-atom)}]

      (with-redefs
        [persistance/db-store-session-turn!
         (fn [_db opts]
           (expect (= {:parent-session-id "s1" :user-request "follow up" :status :running} opts))
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

(defdescribe max-tokens-exceeded-retry-test
             (it "recognises :svar.llm/max-tokens-exceeded as retry-able"
                 (let
                   [e (ex-info "max_tokens hit"
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
                 (let
                   [iter-err
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
                 (let
                   [iter-err
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
      (let
        [target
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
      (let
        [target
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
  (it "drops iterations explicitly flagged :preserved-thinking/replay? false"
      ;; Cross-turn trailer seeds carry the opt-out flag so historical
      ;; iterations stay visible in transcripts but their opaque thinking
      ;; state is not replayed into a new user turn.
      (let
        [target
         {:provider :zai-coding-plan :model "glm-5.1"}

         trailer
         [(stub-iter {:id 1 :replay? false}) (stub-iter {:id 2 :replay? true})]

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
  (it "returns empty when no iteration has an :assistant-message"
      ;; Iterations that errored before the model produced a usable
      ;; assistant turn (e.g. provider HTTP 4xx mid-stream) lack
      ;; `:assistant-message`; the compatible filter drops them so the
      ;; replay never tries to send an empty/partial block.
      (let
        [target
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
   {:assistant-message
    {:role "assistant"
     :content
     (or content
         [{:type "thinking" :thinking (str "think-" id) :thinking-signature (str "sig-" id)}
          {:type "tool_use" :id (str "tc-" id) :name "python_execution" :input {"query" "lmstudio"}}])}
    :llm-provider provider
    :llm-model model
    :preserved-thinking/replay? replay?
    :attachments attachments
    :tool-calls [{:id (str "tc-" id) :name "python_execution" :input {"query" "lmstudio"}}]
    :forms-vec [{:scope (str "t1/i" id)
                 :svar/tool-call-id (str "tc-" id)
                 :result {"item_count" 2 "paths" ["a.clj" "b.clj"]}}]}])

(defdescribe conversation-suffix-mismatch-test
             ;; The session-c4b630c7 regression: the health gate demoted lmstudio so the
             ;; SELECTED model (target) was anthropic/opus while the ACTUAL server was
             ;; lmstudio/gemma. The old suffix dropped the whole [assistant, tool_result]
             ;; pair on that mismatch — the model never saw its own grep result and
             ;; re-issued the identical call every iteration.
             (it
               "replays [assistant sans thinking, tool_result] on provider/model mismatch"
               (let
                 [target
                  {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

                  suffix
                  (conversation-suffix [(stub-tool-iter {:id 1})] target)]

                 (expect (= 2 (count suffix)))
                 (let
                   [[assistant results]
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
                 (let
                   [target
                    {:provider :lmstudio :model "google/gemma-4-12b-qat"}

                    suffix
                    (conversation-suffix [(stub-tool-iter {:id 1})] target)]

                   (expect (= 2 (count suffix)))
                   (expect (= ["thinking" "tool_use"] (mapv :type (:content (first suffix)))))))
             (it "degrades to a plain-text results message when only thinking remains"
                 ;; No tool_use survives the strip → a tool_result would be orphaned
                 ;; (wire error on Anthropic), so the outputs ride as plain text.
                 (let
                   [target
                    {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

                    entry
                    (stub-tool-iter {:id 1
                                     :content [{:type "thinking"
                                                :thinking "only-thinking"
                                                :thinking-signature "sig"}]})

                    suffix
                    (conversation-suffix [entry] target)]

                   (expect (= 1 (count suffix)))
                   (let [[results] suffix]
                     (expect (= "user" (:role results)))
                     (expect (string? (:content results)))
                     (expect (str/includes? (:content results) "item_count")))))
             (it "still excludes successful cross-turn seeds entirely"
                 (let
                   [target
                    {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

                    suffix
                    (conversation-suffix [(stub-tool-iter {:id 1 :replay? false})] target)]

                   (expect (empty? suffix))))
             (it "replays terminal-incomplete cross-turn results as plain text without orphaned tool_result blocks"
                 (let
                   [target
                    {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

                    [pos rec]
                    (stub-tool-iter {:id 1 :replay? false})

                    suffix
                    (conversation-suffix [[pos (assoc rec :cross-turn/turn-status :cancelled)]]
                                         target)]

                   (expect (= 1 (count suffix)))
                   (let [[results] suffix]
                     (expect (= "user" (:role results)))
                     (expect (string? (:content results)))
                     (expect (str/includes? (:content results) "item_count"))
                     (expect (not (vector? (:content results))))))))

(defdescribe
  cancellation-continuity-provider-messages-test
  (it "assembles the cancelled request, abort boundary, settled call, and settled output without a tool protocol orphan"
      (let
        [target
         {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

         initial
         (prompt/assemble-initial-messages
           {:previous-turn-context [{:turn 1
                                     :user-request "inspect and fix"
                                     :cancelled? true
                                     :results [{:scope "t1/i1/f1" :src "cat(src)"}]}]
            :turn-context "session[\"turn\"] = 2"
            :initial-user-content "continue"})

         [pos rec]
         (stub-tool-iter {:id 1 :replay? false})

         messages
         (into initial
               (conversation-suffix
                 [[pos (assoc rec :cross-turn/turn-status :cancelled)]]
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
  (let
    [att {:tool-call-id "tc-1"
          :media-type "image/png"
          :base64 replay-png-b64
          :filename "plot.png"
          :size 67}]
    (it "appends a vision user message with the image AFTER results"
        (let
          [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
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
        (let
          [target {:provider :zai-coding-plan :model "glm-5-turbo"}
           suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})] target)]

          ;; back to the plain [assistant, results] pair, no image block
          (expect (= 2 (count suffix)))
          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            suffix))))
    (it "skips a non-image attach artifact — a csv never rides as an image block"
        (let
          [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
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
    (it "rasterizes an SVG figure to PNG on the way out"
        ;; No wire reads markup, so the vector is converted at SEND time — the
        ;; stored attachment stays SVG and is re-judged on every later turn.
        (let
          [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
           svg {:tool-call-id "tc-3"
                :media-type "image/svg+xml"
                :base64 (.encodeToString
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
        (let
          [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
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
                               (byte-array
                                 (concat (take 33
                                               (.decode (java.util.Base64/getDecoder)
                                                        ^String replay-png-b64))
                                         (repeat 24 0)))))
           blank (assoc broken-svg :media-type "" :filename "fig")
           only-bad (conversation-suffix [(stub-tool-iter {:id 1
                                                           :attachments [broken-svg corrupt blank]})]
                                         target)
           mixed (conversation-suffix [(stub-tool-iter {:id 1 :attachments [corrupt att]})] target)]

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
        ;; textual visibility. Once fold_session/session_drop collapses the
        ;; step, its image bytes leave the wire with it — never re-billed.
        (let
          [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
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
        (let
          [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
           [pos rec] (stub-tool-iter {:id 1 :replay? false :attachments [att]})
           suffix (conversation-suffix [[pos (assoc rec :collapsed? true)]] target)]

          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            suffix))))
    (it "still rides a NON-folded cross-turn seed's image to a vision target"
        ;; The reorder must not break the one path that legitimately emits a
        ;; seed's image: its bytes were never wired to any prior turn.
        (let
          [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
           suffix (conversation-suffix [(stub-tool-iter {:id 1 :replay? false :attachments [att]})]
                                       target)]

          (expect (= 1 (count suffix)))
          (expect (= ["image_url"] (mapv :type (:content (first suffix)))))))
    (it "emits no image message when an iteration produced no attachments"
        (let
          [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
           suffix (conversation-suffix [(stub-tool-iter {:id 1})] target)]

          (expect (= 2 (count suffix)))))))

(defdescribe
  conversation-suffix-blind-description-test
  "A target with NO vision used to lose every generated figure outright: the image
   was skipped and the model was told to open the file with PIL, which answers pixel
   size and never meaning. With a sighted model anywhere in the fleet, the same
   newest-first plan now replays each figure as that model's REPORT — text the blind
   model can actually read — while the pixels themselves stay off its wire."
  (let
    [att {:tool-call-id "tc-1"
          :media-type "image/png"
          :base64 replay-png-b64
          :filename "plot.png"
          :size 67}

     blind-target {:provider :zai-coding-plan :model "glm-5-turbo"}
     seeing-target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

     describer (fn [images]
                 (mapv (fn [img] {:text (str "a plot of " (:filename img)) :model "seer-1"}) images))

     suffix-for (fn [target opts]
                  (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})] target opts))]

    (it "replays the figure as text for a blind target"
        (let [suffix (suffix-for blind-target {:describe-images describer})
              note (last suffix)]
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
        (let [suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})] blind-target)]
          (expect (= 2 (count suffix)))))

    (it "never describes for a target that can SEE"
        (let [called (atom 0)
              suffix (suffix-for seeing-target
                                 {:describe-images (fn [images]
                                                     (swap! called inc)
                                                     (describer images))})]
          (expect (zero? @called))
          (expect (= 3 (count suffix)))
          (expect (= ["image_url"] (mapv :type (:content (last suffix)))))))

    (it "degrades to today's behaviour when the describer answers nothing"
        ;; Toggle off, blind fleet, refused ask, deadline — all arrive here as nil.
        (let [suffix (suffix-for blind-target {:describe-images (fn [_] nil)})]
          (expect (= 2 (count suffix)))))

    (it "describes only the images the replay budget kept"
        (let [seen (atom [])
              suffix (suffix-for blind-target
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
  (let
    [att {:tool-call-id "tc-1"
          :media-type "image/png"
          :base64 replay-png-b64
          :filename "plot.png"
          :size 67}

     seeing-router (svar/make-router [{:id :seeing
                                       :api-key "k"
                                       :base-url "http://seeing.invalid"
                                       :api-style :openai
                                       :models [{:name "seer" :capabilities #{:chat :vision}}]}])]

    (it "replays a figure a blind model cannot see as that fleet's report"
        (vision-describe/clear-cache!)
        (let
          [suffix
           (with-redefs-fn {#'svar/ask! (fn [_ _] {:result {:description "a red pixel on white"}})}
             #(conversation-suffix
                [(stub-tool-iter {:id 1 :attachments [att]})]
                {:provider :zai-coding-plan :model "glm-5-turbo"}
                {:describe-images ((deref #'lp/replay-image-describer)
                                   {:router seeing-router}
                                   "why is the plot empty?")}))

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
          (expect (nil? ((describer) {:router router} "ctx")))))

    (it "is a fn when the fleet has a seeing model"
        (let [router (svar/make-router [{:id :seeing
                                         :api-key "k"
                                         :base-url "http://seeing.invalid"
                                         :api-style :openai
                                         :models [{:name "seer" :capabilities #{:chat :vision}}]}])]
          (expect (fn? ((describer) {:router router} "ctx")))))

    ;; Regression: a router-SHAPED config map (no live provider state) made the sight
    ;; probe throw an NPE out of svar's resolver, so a plain text turn — no images
    ;; anywhere near it — died on the way into the request.
    (it "is nil for a router-shaped config map, and never throws"
        (expect (nil? ((describer)
                       {:router {:providers [{:id :zai-coding-plan
                                              :models [{:name "glm-5-turbo"}]}]}}
                       "ctx"))))

    (it "is nil while the vision_fallback_describe toggle is off"
        (let [router (svar/make-router [{:id :seeing
                                         :api-key "k"
                                         :base-url "http://seeing.invalid"
                                         :api-style :openai
                                         :models [{:name "seer" :capabilities #{:chat :vision}}]}])]
          (toggles/set-value! "vision_fallback_describe" false)
          (try
            (expect (nil? ((describer) {:router router} "ctx")))
            (finally (toggles/reset-to-default! "vision_fallback_describe")))))))
(defdescribe
  conversation-suffix-image-budget-test
  "An image is never REFERENCED by a later request, it is re-uploaded in full on
   every one of them — so an unbudgeted trailer eventually exceeds the provider's
   request limit and then EVERY turn fails, text-only ones included. Newest
   images ride; the rest are NAMED, with the id that brings them back."
  (let
    [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
     att (fn [id]
           {:id (str "att-" id)
            :tool-call-id (str "tc-" id)
            :media-type "image/png"
            :base64 replay-png-b64
            :filename (str "plot-" id ".png")
            :size 67})
     trailer (fn []
               (mapv (fn [id] (stub-tool-iter {:id id :attachments [(att id)]})) [1 2 3]))
     image-msgs (fn [suffix]
                  (filterv #(and (vector? (:content %))
                                 (some (fn [b] (= "image_url" (:type b))) (:content %)))
                           suffix))
     notes (fn [suffix]
             (filterv #(re-find #"show_attachment" (str (:content %))) suffix))]

    (it "replays every image when the budget is not under pressure"
        (let [suffix (conversation-suffix (trailer) target)]
          (expect (= 3 (count (image-msgs suffix))))
          (expect (empty? (notes suffix)))))
    (it "keeps the NEWEST image and names the older ones once the COUNT budget is spent"
        (with-redefs-fn {#'lp/max-replay-images 1}
          (fn []
            (let [suffix (conversation-suffix (trailer) target)
                  dropped (str/join " " (map (comp str :content) (notes suffix)))]
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
            (let [suffix (conversation-suffix (trailer) {:provider :zai-coding-plan
                                                        :model "glm-5-turbo"})]
              (expect (empty? (image-msgs suffix)))
              (expect (empty? (notes suffix)))))))
    (it "keeps an audience \"user\" artifact out of the budget and off the wire entirely"
        (let [suffix (conversation-suffix
                       [(stub-tool-iter {:id 1
                                         :attachments [(assoc (att 1) :audience "user")]})]
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
      (let [env {:turn-state-atom (atom {}) :ctx-atom (atom {})}
            value (with-redefs [ctx-loop/finalize-turn! (fn [_ _] nil)]
                    (#'lp/finalize-answer! env "Just prose."))]
        (expect (= "Just prose." (if (map? value) (:answer value) value)))
        (expect (= "Just prose."
                   (:answer-markdown (:best-answer @(:turn-state-atom env))))))))

;; multi-fence-hint / attach-multi-fence-hint / empty-code-error-with-observation
;; tests removed: those fns were deleted with the fenced-era machinery (lenient
;; mode yields <=1 block, so multi-fence merge + fence-dropped diagnostics are
;; unreachable). See refactor "remove dead fenced-era code-block machinery".

(defdescribe
  token-usage-normalization-test
  (let
    [canonical {:input-tokens 8889
                :output-tokens 69
                :input-tokens-details {:regular 112 :cache-write 8777 :cache-read 0}
                :total-tokens 8958}]
    (it "uses svar's canonical api-usage without reshaping it"
        (expect (= canonical (ask-result->api-usage {:api-usage canonical}))))
    (it "normalizes current svar keyword token maps when canonical usage is absent"
        (expect (= canonical
                   (ask-result->api-usage
                     {:tokens
                      {:input 8889 :output 69 :cached 0 :cache-created 8777 :input-regular 112}}))))
    (it "keeps the legacy string-key token fallback working"
        (expect (= canonical
                   (ask-result->api-usage {:tokens {"input" 8889
                                                    "output" 69
                                                    "cached" 0
                                                    "cache_created" 8777
                                                    "input_regular" 112}}))))
    (it "applies an explicit service-tier cost multiplier to every billed token class"
        (let [estimate (deref #'lp/estimate-token-cost)
              usage {:input-tokens 8298 :output-tokens 6}
              standard (estimate "gpt-5.6-sol" 8298 6 {:api-usage usage})
              priority (estimate "gpt-5.6-sol" 8298 6
                                 {:api-usage usage :cost-multiplier 2.0})]
          (doseq [k ["input_cost" "output_cost" "total_cost"]]
            (expect (< (Math/abs (- (* 2.0 (double (get standard k)))
                                    (double (get priority k))))
                       1.0E-12)))))
    (it "recognizes Priority pricing only for the Codex provider"
        (let [multiplier (deref (ns-resolve 'com.blockether.vis.internal.loop 'codex-fast-cost-multiplier))]
          (expect (= 2.0 (multiplier {:service_tier "priority"} :openai-codex)))
          (expect (= 2.0 (multiplier {"service_tier" "PRIORITY"} "openai-codex")))
          (expect (= 1.0 (multiplier {:service_tier "priority"} :openai)))
          (expect (= 1.0 (multiplier {} :openai-codex)))))))

(defdescribe ask-code-block-observation-test
             (it "reports the block count (lenient mode: only the count is meaningful)"
                 (expect (= {:form-count 1}
                            (ask-code-block-observation {:blocks [{:source "(def x 1)"
                                                                   :lang "clojure"}]})))
                 (expect (= {:form-count 0} (ask-code-block-observation {:blocks []})))
                 (expect (= {:form-count 0} (ask-code-block-observation {})))))

(defdescribe
  iteration-start-hook-test
  (it
    "collects active :turn.iteration/start hooks as hook-task descriptors (D12)"
    (let
      [seen
       (atom nil)

       ext
       {:ext/name "test.hooks"
        :ext/hooks [{:id :test/title
                     :doc "title"
                     :phase :turn.iteration/start
                     :fn (fn [ctx]
                           (reset! seen ctx)
                           {:title "set title" :importance :warn})}
                    {:id :test/answer
                     :doc "answer"
                     :phase :turn.answer/validate
                     :fn (fn [_]
                           {:reject true})}
                    {:id :test/no-title
                     :doc "missing title—rejected"
                     :phase :turn.iteration/start
                     :fn (fn [_]
                           {:importance :warn})}]}

       ctx
       {:session-title nil :title-refresh? true :turn-position 1}

       hits
       (collect-iteration-start-hints {} [ext] ctx)]

      ;; Only the title-bearing :turn.iteration/start hook materialises;
      ;; the :turn.answer/validate hook is the wrong phase and the
      ;; title-less hook is dropped. Self-asserted done means no
      ;; validator-fn and no :specs in the hook-task descriptor.
      (expect (= [{:id :test/title
                   :task {:title "set title"
                          :status :todo
                          :source :hook
                          :hook-id :test/title
                          :importance :warn}}]
                 hits))
      (expect (= ctx @seen))))
  (it "does NOT re-title when a real title already exists (generate once, never re-title)"
      (let [env (lp/create-environment {:providers []} {:db :memory :title "Old focus"})]
        (try
          ;; svar/ask! must NEVER fire — guard it so a regression to re-titling
          ;; throws instead of silently passing.
          (with-redefs
            [svar/ask! (fn [& _]
                         (throw (ex-info "must not re-title" {})))]
            (expect (nil? (maybe-auto-title! env "some unrelated new request")))
            (expect (= "Old focus" @(:session-title-atom env))))
          (finally (lp/dispose-environment! env)))))
  (it "auto-title treats Untitled placeholders as missing previous titles"
      (let
        [seen
         (atom nil)

         router-stub
         {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}

         env
         (lp/create-environment router-stub {:db :memory :title "Untitled"})]

        (try (with-redefs
               [svar/ask! (fn [_router opts]
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
    (let
      [router-stub
       {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}
                    {:id :openai-codex :models [{:name "gpt-5.3-codex"}]}]}

       seen
       (atom nil)

       env
       (lp/create-environment router-stub {:db :memory :title "Untitled"})]

      (try
        ;; svar owns the per-provider walk now; the host makes ONE call that
        ;; declares `:prefer-providers`. A thrown call → deterministic fallback.
        (with-redefs
          [svar/ask! (fn [_router opts]
                       (reset! seen opts)
                       (throw (ex-info "Exceptional status code: 400" {})))]
          (let
            [f (titling/after-turn-auto-title!
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
          (let
            [bad (env/run-python-block (:python-context env)
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
  (it "gives the first token 200s and the idle watchdog its own 300s by default"
      ;; 200s, not svar's two minutes: under Vis' pinned provider+model route
      ;; svar's router has no second candidate to cross to, and the first header
      ;; is the ONE wait Vis can retry for free — `pre-output-stream-retryable?`
      ;; does, so a slow queue gets three visible tries instead of one verdict.
      (expect (= 200000 rt/ASK_CODE_TTFT_TIMEOUT_MS))
      (expect (= 300000 rt/ASK_CODE_IDLE_TIMEOUT_MS))
      (let [{:keys [router opts]} (captured-ask-code-opts {:lang "clojure" :messages []})]
        (expect (= ::router router))
        (expect (= rt/ASK_CODE_TTFT_TIMEOUT_MS (:ttft-timeout-ms opts)))
        (expect (= rt/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts)))
        ;; Semantic silence can be legitimate encrypted reasoning while SSE
        ;; keepalives prove the transport is healthy, so it is opt-in.
        (expect (nil? rt/ASK_CODE_SEMANTIC_TIMEOUT_MS))
        (expect (not (contains? opts :semantic-timeout-ms)))))
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
        (expect (= rt/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts))))
      (let [opts (:opts (captured-ask-code-opts {:semantic-timeout-ms nil}))]
        (expect (contains? opts :semantic-timeout-ms))
        (expect (nil? (:semantic-timeout-ms opts))))))

(defdescribe python-eval-test
             (it "executes a Python assignment and the binding persists in the sandbox"
                 (let [env (lp/create-environment ::router {:db :memory})]
                   (try (let [result ((var-get #'lp/execute-code) env "x = 1")]
                          (expect (nil? (:error result))))
                        ;; Sandbox globals persist REPL-style across evals on the same context.
                        (let [read-back (env/run-python-block (:python-context env) "x")]
                          (expect (nil? (:error read-back)))
                          (expect (= 1 (:result read-back))))
                        (finally (lp/dispose-environment! env)))))
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
                            (eval-timeout-ms-for-code
                              120000
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
                 (expect (= (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000)
                            (eval-timeout-ms-for-code
                              120000
                              "secs = 600\nr = await shell(op=\"wait\", id=\"j\", timeout_secs=secs)")))
                 ;; A literal budget does not lower the floor either: it bounds ONE call, and
                 ;; the next call in the block can still own shell's default.
                 (expect (= (+ (* 1000 rt/MAX_SHELL_TIMEOUT_SECS) 10000)
                            (eval-timeout-ms-for-code
                              120000
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
                            (eval-timeout-ms-for-code
                              rt/DEFAULT_EVAL_TIMEOUT_MS
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
                            (eval-timeout-ms-for-code 120000 "for u in urls:\n    requests.get(u, timeout=5)")))
                 ;; …but a longer explicitly requested budget still wins outright.
                 (expect (= 610000 (eval-timeout-ms-for-code 120000 "requests.get(u, timeout=600)")))
                 ;; Prose that merely mentions a client must not widen anything.
                 (expect (= 120000 (eval-timeout-ms-for-code 120000 "print('requests are bounded')"))))
             (it "splits + evals multi-form blocks whose statements contain astral chars (emoji)"
                 ;; Regression: GraalPy's ast.get_source_segment truncates
                 ;; the per-form source when a statement carries a non-BMP char (emoji 👆),
                 ;; dropping the closing quotes -> the lone re-eval raised a spurious
                 ;; "unterminated triple-quoted string" SyntaxError, the (done ...) answer
                 ;; form errored, the turn never finalized, and the model looped re-emitting
                 ;; done(). Our pure-Python codepoint slice must keep every segment intact —
                 ;; including a MULTILINE triple-quoted string with emoji mid- and last-line.
                 (let
                   [{:keys [python-context]}
                    (env/create-python-context {})

                    ;; emoji on the first AND a later line; the second form re-reads the var.
                    code
                    (str "msg = \"\"\"# Heading 👆\n\n- bin/ 🚀\n\nPełne ł ó ż 🌳\"\"\"\n" "msg")

                    {:keys [error result]}
                    (env/run-python-block python-context code)]

                   (expect (nil? error))
                   ;; the final expression re-reads the multi-line emoji string unchanged
                   (expect (string? result))
                   (expect (clojure.string/includes? result "👆"))
                   (expect (clojure.string/includes? result "🌳"))
                   (expect (clojure.string/includes? result "Pełne ł ó ż")))))

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
                 (expect (nil? (lp/final-answer-gate-error
                                 {}
                                 1
                                 [{:id 0 :code "1 + 2" :result 3 :error nil}]
                                 {:answer "done"}
                                 nil)))))

;; def-sink -> vars-snapshot (per-var precise source extraction)

(defdescribe
  gather-builtin-test
  "maki-style in-program concurrency: `await gather(*awaitables)` runs each
   awaitable on a virtual thread and returns results IN ORDER. Guards the async
   runtime end-to-end through a real sandbox: the await path AST-wraps + drives
   the coroutine, gather dispatches awaitables to __vis_par__ (the host
   bounded platform pool). Concurrency itself is proven by GraalPy lock-release."
  (it
    "awaits gathered coroutines and returns their results in order"
    (let [environment (lp/create-environment ::router {:db :memory})]
      (try
        (let
          [r
           (env/run-python-block
             (:python-context environment)
             "async def work(n):\n    return n * n\nvals = await gather(work(2), work(3), work(4))\nprint(list(vals))"
             "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "[4, 9, 16]" (clojure.string/trim (str (:stdout r))))))
        (finally (try (lp/dispose-environment! environment) (catch Throwable _ nil))))))
  (it
    "a failing member surfaces the RIGHT slot + its OWN error (never mis-attributed)"
    ;; gather is all-or-nothing, but the error must name the EXACT failing index
    ;; and carry that call's real message — not a sibling's, not a generic one.
    ;; (Independent of the `__vis_par_isolated__` batch path, which never routes
    ;; through gather — this guards `gather`/`__vis_par__` behavior verbatim.)
    (let [environment (lp/create-environment ::router {:db :memory})]
      (try
        (let
          [r
           (env/run-python-block
             (:python-context environment)
             "async def ok(n):\n    return n\nasync def boom():\n    raise ValueError('DISTINCT_BOOM_42')\nawait gather(ok(1), boom(), ok(3))"
             "t1/i1")
           msg (str (:message (:error r)))]

          (expect (some? (:error r)))
          (expect (clojure.string/includes? msg "[1]"))               ;; the failing slot, not [0]/[2]
          (expect (clojure.string/includes? msg "DISTINCT_BOOM_42"))) ;; boom's own message
        (finally (try (lp/dispose-environment! environment) (catch Throwable _ nil)))))))

(defdescribe
  iteration-summarize-test
  "summarize/drop operate at ITERATION (tN/iN) granularity: a summarized step
   collapses entirely (its assistant+tool_result pair leaves the wire) to one
   gist line; a non-collapsed step renders as a tool_result tagged `# tN/iN`."
  (let
    [apply-summaries
     (var-get #'lp/apply-summaries)

     irm
     (var-get #'lp/iteration-results-message)]

    (it "summarize([tN/iN]) tags the iteration :collapsed? and swaps it for the gist"
        (let
          [tis
           [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big output"}]}]
            [2 {:forms-vec [{:scope "t1/i2/f1" :stdout "keep me"}]}]]

           out
           (apply-summaries tis [{"scopes" #{"t1/i1"} "gist" "did the thing"}])

           r1
           (second (first out))

           r2
           (second (second out))]

          (expect (true? (:collapsed? r1)))
          (expect (nil? (:collapsed? r2)))
          ;; collapsed → plain-text gist line (NOT a tool_result)
          (expect (= "# ⋯ folded t1/i1 · did the thing" (:content (irm r1))))))
    (it "session_drop collapses to a `⋯ dropped <scopes> · <why>` line (reason kept)"
        (let
          [out (apply-summaries [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big"}]}]]
                                [{"scopes" #{"t1/i1"} "drop" true "gist" "misread"}])]
          (expect (= "# ⋯ dropped t1/i1 · misread" (:content (irm (second (first out))))))))
    (it "a live step renders as a tool_result tagged with its # tN/iN handle"
        (let [m (irm {:forms-vec [{:scope "t1/i1/f1" :stdout "hello"}] :tool-calls [{:id "c1"}]})]
          (expect (= "c1" (get-in m [:content 0 :tool_use_id])))
          (expect (str/includes? (get-in m [:content 0 :content]) "# t1/i1"))
          (expect (str/includes? (get-in m [:content 0 :content]) "hello"))))
    (it "no summaries ⇒ trailer-iters unchanged"
        (let [tis [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "x"}]}]]]
          (expect (= tis (apply-summaries tis [])))))))


(defdescribe
  tool-result-pairing-test
  "REARCHITECTURE (same DB schema): an iteration is a LIST of `python_execution`
   tool-calls. Each tool_use gets its OWN tool_result, carrying ITS OWN forms'
   return — forms are grouped by `:svar/tool-call-id`. A call's return IS the
   text it print()s. No more 'first call carries everything, the rest get a
   stub'."
  (let
    [irm
     (var-get #'lp/iteration-results-message)

     pre
     (var-get #'lp/code-entries-preflight)]

    (it "each parallel tool_use is answered by its OWN result"
        (let
          [m
           (irm {:tool-calls [{:id "A" :name "python_execution"} {:id "B" :name "python_execution"}
                              {:id "P" :name "python_execution"}]
                 ;; A and B returned a value without printing (:result); P printed
                 ;; (:stdout) — the engine emits ONE channel per call, never both.
                 :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "A" :result "AAA"}
                             {:scope "t1/i1/f2" :svar/tool-call-id "B" :result "BBB"}
                             {:scope "t1/i1/f3" :svar/tool-call-id "P" :result nil :stdout "PPP"}]})

           by-id
           (into {} (map (juxt :tool_use_id :content)) (:content m))]

          (expect (= 3 (count (:content m))))
          ;; A/B → their RETURN value; not cross-contaminated
          (expect (str/includes? (by-id "A") "AAA"))
          (expect (not (str/includes? (by-id "A") "BBB")))
          (expect (str/includes? (by-id "B") "BBB"))
          ;; P → its PRINTED string
          (expect (str/includes? (by-id "P") "PPP"))))
    (it "no tool_result carries a result-recovery handle"
        ;; Regression guard for the `ntr` removal: nothing stores a call's return
        ;; any more, so no result may advertise a coordinate to re-read it by — a
        ;; handle here would promise a store that no longer exists.
        (let
          [m
           (irm {:tool-calls [{:id "toolu_A" :name "python_execution"} {:id "P" :name "python_execution"}]
                 :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "toolu_A" :result "AAA"}
                             {:scope "t1/i1/f2" :svar/tool-call-id "P" :result nil :stdout "PPP"}]})

           all
           (str/join "\n" (map :content (:content m)))]

          (expect (str/includes? all "AAA"))
          (expect (str/includes? all "PPP"))
          (expect (not (str/includes? all "ntr")))
          (expect (not (str/includes? all "# saved:")))))
    (it "a FAILED call's tool_result is flagged :is_error true; a successful one is not"
        (let
          [m
           (irm {:tool-calls [{:id "ok" :name "python_execution"} {:id "bad" :name "python_execution"}]
                 :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "ok" :result "FILE"}
                             {:scope "t1/i1/f2" :svar/tool-call-id "bad" :error "No such file"}]})

           by-id
           (into {} (map (juxt :tool_use_id identity)) (:content m))]

          ;; svar passes :is_error to Anthropic as `is_error: true`; on OpenAI/Gemini
          ;; the error TEXT carries the signal.
          (expect (nil? (:is_error (by-id "ok"))))
          (expect (true? (:is_error (by-id "bad"))))
          (expect (str/includes? (:content (by-id "bad")) "No such file"))))
    (it "a call that produced no output returns the no-return hint"
        ;; engine emitted neither :stdout nor :result (e.g. python_execution that
        ;; only did assignments and printed nothing)
        (let
          [m (irm {:tool-calls [{:id "P" :name "python_execution"}]
                   :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "P"}]})]
          (expect (str/includes? (get-in m [:content 0 :content]) "no return"))))
    (it "an unpaired/fold form folds onto the FIRST call (nothing lost)"
        (let
          [m
           (irm {:tool-calls [{:id "A" :name "python_execution"}]
                 :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "A" :result "body"}
                             {:summary? true :summary-iters ["t1/i0"] :summary-gist "ctx"}]})

           c
           (get-in m [:content 0 :content])]

          (expect (str/includes? c "body"))
          (expect (str/includes? c "folded t1/i0 · ctx"))))
    (it "code-entries-preflight keeps distinct tool-calls SEPARATE (no merge)"
        (let
          [entries
           (:code-entries
             (pre 1
                  [{:lang "python" :source "cat(\"a\")" :svar/tool-call-id "A" :vis/tool-name "cat"}
                   {:lang "python"
                    :source "rg({\"any\":[\"x\"]})"
                    :svar/tool-call-id "B"
                    :vis/tool-name "rg"}]))]
          (expect (= 2 (count entries)))
          (expect (= ["A" "B"] (mapv :svar/tool-call-id entries)))))
    (it "code-entries-preflight STILL merges legacy id-less blocks (provider stutter)"
        (let
          [entries (:code-entries
                     (pre 1 [{:lang "python" :source "x = 1"} {:lang "python" :source "y = 2"}]))]
          (expect (= 1 (count entries)))))))

(defdescribe
  csv-attachment-wire-test
  "A `attach`ed CSV is DATA for the HUMAN. The ````vis-table` fence reaches
   the TRANSCRIPT whole — the channel paints it as a live grid — while the model
   wire keeps only the `[Table: …]` headline and a pointer back to the stored
   attachment. Replayed rows are the most expensive thing a session can carry:
   they are re-uploaded on every later request for the rest of the session."
  (let
    [irm
     (var-get #'lp/iteration-results-message)

     display
      form/result-display

     fence
     (str "````vis-table\n"
          "[Table: fleet.csv 2 rows × 2 cols, 12 B] fleet counts\n"
          "fleet.csv\n"
          "text/csv\n"
          "2x2\n"
          "12 B\n"
          "machine,sessions\n"
          "studio,12\n"
          "rack-01,120\n"
          "````")

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
      (let
        [router-stub
         {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}

         env
         (lp/create-environment router-stub {:db :memory})

         calls
         (atom 0)]

        (try (with-redefs
               [svar/ask-code!
                (fn [_ _]
                  (if (<= (swap! calls inc) 4)
                    {:blocks [{:lang "clojure" :source "(def probe 1)"}]
                     :raw "```clojure\n(def probe 1)\n```"
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
          (let
            [router {:providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}
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
          (let
            [router {:providers [{:id :anthropic-coding-plan
                                  :models [{:name "claude-opus-4-8"} {:name "claude-sonnet-4-6"}]}]}
             config {:providers [{:id :anthropic-coding-plan :models ["claude-sonnet-4-6"]}]}
             routed (f router config)]

            (expect (= "claude-sonnet-4-6" (:name (lp/resolve-effective-model routed))))))
      (it "leaves the router intact when the explicit pair is not in its catalog"
          (let
            [router {:providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}]}
             config {:default-provider "anthropic-coding-plan"
                     :default-model "claude-fable-5"
                     :providers [{:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]}]

            (expect (= router (f router config)))))
      (it "a resolvable provider still wins when the model name does not match its catalog"
          (let
            [router {:providers [{:id :zai-coding-plan
                                  :models [{:name "glm-5.2"} {:name "glm-4.7"}]}
                                 {:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]}
             config {:default-provider "anthropic-coding-plan"
                     :default-model "typo-model"
                     :providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}
                                 {:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]}
             routed (f router config)]

            (expect (= :anthropic-coding-plan (:id (first (:providers routed)))))
            (expect (= "claude-fable-5" (:root (first (:providers routed)))))))
      (it "a slash INSIDE a model id is not a provider tag"
          ;; openrouter serves ids like `z-ai/glm-4.6v`. Splitting them on the
          ;; slash asked for provider `:z-ai`, which no fleet has, so the
          ;; default the user picked never became the router's root.
          (let
            [fleet [{:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}
                    {:id :openrouter :models [{:name "glm-5.2"} {:name "z-ai/glm-4.6v"}]}]
             router {:providers fleet}
             config {:default-provider "openrouter"
                     :default-model "z-ai/glm-4.6v"
                     :providers fleet}
             routed (f router config)
             p (first (:providers routed))]

            (expect (= :openrouter (:id p)))
            (expect (= "z-ai/glm-4.6v" (:root p)))
            (expect (= "z-ai/glm-4.6v" (:name (lp/resolve-effective-model routed))))))
      (it "default_model accepts the provider/model form and its provider wins"
          (let
            [router {:providers [{:id :zai-coding-plan
                                  :models [{:name "glm-5.2"} {:name "glm-4.7"}]}
                                 {:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]}
             config {:default-provider "anthropic-coding-plan"
                     :default-model "zai-coding-plan/glm-4.7"
                     :providers [{:id :zai-coding-plan :models [{:name "glm-5.2"}]}
                                 {:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]}
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
    (let
      [router {:providers [{:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"}]}
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
                       (vec (for
                              [p (:providers r)
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
   provider's `:root` NAME rather than the `:models` head. A coordinator's
   `models` preference (sub_loop) carries no forced `:routing`, so the child turn
   kept running the default provider's root model while the turn card and the
   cost row named the cheap model that never ran."
  (let
    [router (svar/make-router
              [{:id :prov-a
                :api-key "k"
                :base-url "https://a.example.com"
                :models [{:name "a-big"} {:name "a-small"}]}
               {:id :prov-b
                :api-key "k"
                :base-url "https://b.example.com"
                :models [{:name "b-cheap"}]}])

     ;; What svar ACTUALLY calls, not what Vis displays.
     selected (fn [r prefs]
                (let [[p m] (svar-router/select-provider r prefs)]
                  [(:id p) (:name m)]))

     seats (fn [r]
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
    (let
      [;; both providers expose "gpt-5.4" — the tie `router-for-model` cannot break
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
          (let
            [pinned
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

(defdescribe
  prepare-turn-model-preference-test
  (let [prepare #'lp/prepare-turn-context
        router {:providers [{:id :openai-codex
                             :models [{:name "shared"} {:name "gpt-explicit"}]}
                            {:id :lmstudio
                             :models [{:name "shared"} {:name "ornith"}]}]}
        env {:db-info ::db
             :session-id "session-1"
             :router router}
        messages [{:role "user" :content "hello"}]]
    (it "uses a persisted provider and model as one indivisible pin"
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    {:provider "lmstudio"
                                                     :model "ornith"})}
          #(let [ctx (prepare env messages {})]
             (expect (= :lmstudio (:root-provider ctx)))
             (expect (= "ornith" (:root-model ctx)))
             (expect (= {:provider :lmstudio :model "ornith"}
                        (:routing ctx))))))
    (it "trims the persisted pin so the display root names the model that RAN"
        ;; The routing helpers trim; the display/cost root did not. A pref with
        ;; stray whitespace (a hand-edited DB row, a client that pads the field)
        ;; therefore BOUND "ornith" while the turn card named LM Studio's FIRST
        ;; model — a model that turn never ran.
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    {:provider "  lmstudio  "
                                                     :model "  ornith  "})}
          #(let [ctx (prepare env messages {})]
             (expect (= :lmstudio (:root-provider ctx)))
             (expect (= "ornith" (:root-model ctx)))
             (expect (= {:provider :lmstudio :model "ornith"}
                        (:routing ctx))))))
    (it "honors a live-catalog model the pinned provider does not list statically"
        ;; What the pickers actually offer: `/v1/providers/:id/models` (the TUI's
        ;; "show all models", the companion's router dialog) lists the provider's
        ;; LIVE catalog, which is wider than vis.yml. Such a pick used to fall
        ;; through to the default model because the pinned provider's static
        ;; `:models` did not contain it.
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    {:provider "lmstudio"
                                                     :model "qwen3-next-80b"})}
          #(let [ctx (prepare env messages {})]
             (expect (= :lmstudio (:root-provider ctx)))
             (expect (= "qwen3-next-80b" (:root-model ctx)))
             (expect (= {:provider :lmstudio :model "qwen3-next-80b"}
                        (:routing ctx))))))
    (it "does not combine a caller model with the persisted provider"
        (with-redefs-fn {#'session-model/model-of (fn [& _]
                                                    {:provider "lmstudio"
                                                     :model "shared"})}
          #(let [ctx (prepare env messages {:model "shared"})]
             ;; Both providers offer this model. An explicit model retains the
             ;; router's normal provider choice instead of borrowing LM Studio
             ;; from an unrelated persisted pair.
             (expect (= :openai-codex (:root-provider ctx)))
             (expect (= "shared" (:root-model ctx)))
             (expect (= {:model "shared"}
                        (:routing ctx))))))))

(defdescribe
  sub-loop!-test
  (describe
    "sub-loop! assembly (stubbed env/turn — no LLM, no FS)"
    (let
      [child-ctx
       (atom {})

       captured
       (atom nil)

       router
       {:providers [{:id :anthropic-coding-plan :models [{:name "opus"}]}
                    {:id :anthropic :models [{:name "haiku"}]}]}

       parent
       {:router router
        :db-info :db
        :depth-atom (atom 0)
        :session/state-id "parent-state-123"
        :workspace {:id "parent-ws" :root "/parent"}}

       r
       (with-redefs
         [lp/child-workspace!
          (fn [_db _pw]
            {:id "child-ws" :root "/child" :fork-ms 0})

          lp/create-environment
          (fn [router opts]
            (reset! captured {:router router :opts opts})
            {:ctx-atom child-ctx :owns-db? false :db-info :db})

          lp/run-turn!
          (fn [_e _p _o]
            {:status :success :answer "did it"})

          lp/dispose-environment!
          (fn [_])]

         (lp/sub-loop! parent
                       {:prompt "implement oauth"
                        :subctx {"focus" "oauth" "tasks" {"oauth" {"status" "doing"}}}
                        :models ["haiku"]}))]

      (it "routes the child to the PROPOSED model"
          (expect (= "haiku" (:name (lp/resolve-effective-model (:router @captured))))))
      (it "passes parent-db-info + depth(parent+1) + seed-ctx as :child opts"
          (expect (= :db (get-in @captured [:opts :child :parent-db-info])))
          (expect (= 1 (get-in @captured [:opts :child :depth])))
          (expect (= "child-ws" (get-in @captured [:opts :workspace-id])))
          ;; child soul links to the PARENT's session_state (cross-soul) → hidden
          ;; from the top-level list, queryable as the parent's sub-tree
          (expect (= "parent-state-123" (get-in @captured [:opts :child :parent-state-id]))))
      (it "returns the focus result shape (task_id/status/answer)"
          (expect (= "oauth" (get r "task_id")))
          ;; status is the child turn's status, coerced to a python-facing STRING
          (expect (= "success" (get r "status")))
          (expect (= "did it" (get r "answer"))))))
  (describe "depth cap"
            (it "throws :vis/subloop-depth-exceeded past MAX-SUBLOOP-DEPTH"
                (with-redefs
                  [lp/child-workspace!
                   (fn [& _]
                     {:id "x" :root "/x"})

                   lp/create-environment
                   (fn [& _]
                     {:ctx-atom (atom {}) :owns-db? false})

                   lp/run-turn!
                   (fn [& _]
                     {})

                   lp/dispose-environment!
                   (fn [_])]

                  (expect (throws? clojure.lang.ExceptionInfo
                                   #(lp/sub-loop!
                                      {:depth-atom (atom 5) :router {} :workspace {} :db-info :db}
                                      {:prompt "x" :subctx {}}))))))
  (describe
    "child cleanup — clone trashed + env disposed (no leaks)"
    (let [stub-ws {:id "child-ws" :root "/child" :fork-ms 0}]
      (it "on success: disposes the env AND abandons the rift clone (after merging it back)"
          (let [events (atom [])]
            (with-redefs
              [lp/child-workspace! (fn [_ _]
                                     stub-ws)
               lp/create-environment (fn [_ _]
                                       {:ctx-atom (atom {}) :owns-db? false :db-info :db})
               lp/run-turn! (fn [_ _ _]
                              {:status :success :answer "ok"})
               workspace/apply! (fn [_ _]
                                  (swap! events conj :apply)
                                  {:changed []})
               workspace/abandon! (fn [_ a]
                                    (swap! events conj [:abandon (:workspace-id a)])
                                    a)
               lp/dispose-environment! (fn [_]
                                         (swap! events conj :dispose))]

              (lp/sub-loop! {:router {}
                             :db-info :db
                             :depth-atom (atom 0)
                             :workspace {:id "parent-ws" :root "/parent"}}
                            {:prompt "p" :subctx {"focus" "t"}}))
            ;; merge happens before dispose before abandon — and abandon names the clone
            (expect (= [:apply :dispose [:abandon "child-ws"]] @events))))
      (it "on a thrown turn: STILL disposes the env AND abandons the clone (finally), then rethrows"
          (let [events (atom [])]
            (with-redefs
              [lp/child-workspace! (fn [_ _]
                                     stub-ws)
               lp/create-environment (fn [_ _]
                                       {:ctx-atom (atom {}) :owns-db? false :db-info :db})
               lp/run-turn! (fn [_ _ _]
                              (throw (ex-info "turn blew up" {})))
               workspace/apply! (fn [_ _]
                                  (swap! events conj :apply)
                                  {:changed []})
               workspace/abandon! (fn [_ a]
                                    (swap! events conj [:abandon (:workspace-id a)])
                                    a)
               lp/dispose-environment! (fn [_]
                                         (swap! events conj :dispose))]

              (expect (throws? clojure.lang.ExceptionInfo
                               #(lp/sub-loop! {:router {}
                                               :db-info :db
                                               :depth-atom (atom 0)
                                               :workspace {:id "parent-ws" :root "/parent"}}
                                              {:prompt "p" :subctx {"focus" "t"}})))
              ;; no merge (turn failed), but BOTH cleanups ran
              (expect (= [:dispose [:abandon "child-ws"]] @events)))))))
  (describe
    "parallel-sub-loops! (stubbed sub-loop! — concurrency, ordering, failure isolation)"
    (let
      [live
       (atom 0)

       peak
       (atom 0)

       run
       (fn [parent specs]
         (with-redefs
           [lp/sub-loop! (fn [_parent {:keys [subctx]}]
                           (let [n (swap! live inc)]
                             (swap! peak max n))
                           (Thread/sleep 25)
                           (swap! live dec)
                           (let [focus (get subctx "focus")]
                             (when (= focus "boom") (throw (ex-info "child blew up" {})))
                             {"task_id" focus "status" "done" "changed_files" []}))]
           (lp/parallel-sub-loops! parent specs)))

       specs
       (mapv (fn [i]
               {"prompt" (str "t" i) "subctx" {"focus" (str "task" i)} "models" ["haiku"]})
             (range 8))

       results
       (run {:depth-atom (atom 0)} specs)]

      (it "returns one result per spec, in INPUT ORDER"
          (expect (= 8 (count results)))
          (expect (= (mapv #(str "task" %) (range 8)) (mapv #(get % "task_id") results))))
      (it "bounds concurrency to the cap (peak in-flight never exceeds 4)"
          (expect (<= @peak 4))
          (expect (pos? @peak)))
      (it "a child that throws surfaces as a failed slot, not a batch-killing exception"
          (let
            [r (run {:depth-atom (atom 0)}
                    [{"prompt" "ok" "subctx" {"focus" "good"}}
                     {"prompt" "bad" "subctx" {"focus" "boom"}}
                     {"prompt" "ok2" "subctx" {"focus" "fine"}}])]
            (expect (= ["good" "boom" "fine"] (mapv #(get % "task_id") r)))
            (expect (= ["done" "failed" "done"] (mapv #(get % "status") r)))
            (expect (= "child blew up" (get (second r) "error")))))))
  (describe
    "sequence-sub-loops! (:sequence composite — in order, fail-fast)"
    ;; stub sub-loop! to succeed/fail based on the spec's focus key
    (let
      [run (fn [focuses fail-set]
             (let [ran (atom [])]
               (with-redefs
                 [lp/sub-loop! (fn [_ {:keys [subctx]}]
                                 (let [f (get subctx "focus")]
                                   (swap! ran conj f)
                                   {"task_id" f "status" (if (fail-set f) "failed" "done")}))]
                 {:results (lp/sequence-sub-loops! {}
                                                   (mapv (fn [f]
                                                           {"prompt" f "subctx" {"focus" f}})
                                                         focuses))
                  :ran @ran})))]
      (it "all succeed → runs every child in order, returns all"
          (let [{:keys [results ran]} (run ["a" "b" "c"] #{})]
            (expect (= ["a" "b" "c"] ran))
            (expect (= ["a" "b" "c"] (mapv #(get % "task_id") results)))
            (expect (= ["done" "done" "done"] (mapv #(get % "status") results)))))
      (it "stops at the FIRST failure — later children never run; result includes the failure"
          (let [{:keys [results ran]} (run ["a" "b" "c"] #{"b"})]
            (expect (= ["a" "b"] ran)) ; "c" never ran
            (expect (= ["a" "b"] (mapv #(get % "task_id") results)))
            (expect (= ["done" "failed"] (mapv #(get % "status") results)))))))
  (describe "selector-sub-loops! (:selector composite — alternatives until one succeeds)"
            (let
              [run
               (fn [focuses fail-set]
                 (let [ran (atom [])]
                   (with-redefs
                     [lp/sub-loop! (fn [_ {:keys [subctx]}]
                                     (let [f (get subctx "focus")]
                                       (swap! ran conj f)
                                       {"task_id" f "status" (if (fail-set f) "failed" "done")}))]
                     {:results (lp/selector-sub-loops! {}
                                                       (mapv (fn [f]
                                                               {"prompt" f "subctx" {"focus" f}})
                                                             focuses))
                      :ran @ran})))]
              (it "first child succeeds → stops immediately, no alternatives tried"
                  (let [{:keys [results ran]} (run ["a" "b" "c"] #{})]
                    (expect (= ["a"] ran))
                    (expect (= ["a"] (mapv #(get % "task_id") results)))
                    (expect (= ["done"] (mapv #(get % "status") results)))))
              (it "tries alternatives in order until one succeeds; later ones skipped"
                  (let [{:keys [results ran]} (run ["a" "b" "c"] #{"a"})]
                    (expect (= ["a" "b"] ran)) ; "c" never tried
                    (expect (= ["failed" "done"] (mapv #(get % "status") results)))))
              (it "all alternatives fail → returns every attempt (all failures)"
                  (let [{:keys [results ran]} (run ["a" "b"] #{"a" "b"})]
                    (expect (= ["a" "b"] ran))
                    (expect (= ["failed" "failed"] (mapv #(get % "status") results)))))))
  (describe "retry-sub-loop! (stubbed sub-loop! — selector: re-run until success)"
            (it "succeeds on the first attempt — no re-run"
                (let [calls (atom 0)]
                  (with-redefs
                    [lp/sub-loop! (fn [_ _]
                                    (swap! calls inc)
                                    {"task_id" "t" "status" "done"})]
                    (let [r (lp/retry-sub-loop! {} {"prompt" "p" "subctx" {"focus" "t"}} 3)]
                      (expect (= "done" (get r "status")))
                      (expect (= 1 (get r "attempts")))
                      (expect (= 1 @calls))))))
            (it "re-runs a failing child until it succeeds, stamping the winning attempt"
                (let [calls (atom 0)]
                  (with-redefs
                    [lp/sub-loop! (fn [_ _]
                                    (let [n (swap! calls inc)]
                                      (if (< n 3)
                                        {"task_id" "t" "status" "failed"}
                                        {"task_id" "t" "status" "done"})))]
                    (let [r (lp/retry-sub-loop! {} {"prompt" "p" "subctx" {"focus" "t"}} 5)]
                      (expect (= "done" (get r "status")))
                      (expect (= 3 (get r "attempts")))
                      (expect (= 3 @calls))))))
            (it "exhausts n attempts and returns the last failure (status in the failure set)"
                (let [calls (atom 0)]
                  (with-redefs
                    [lp/sub-loop! (fn [_ _]
                                    (swap! calls inc)
                                    {"task_id" "t" "status" "rejected"})]
                    (let [r (lp/retry-sub-loop! {} {"prompt" "p" "subctx" {"focus" "t"}} 2)]
                      (expect (= "rejected" (get r "status")))
                      (expect (= 2 (get r "attempts")))
                      (expect (= 2 @calls))))))
            (it "treats a THROWN child as a failure and retries; defaults to 2 attempts"
                (let [calls (atom 0)]
                  (with-redefs
                    [lp/sub-loop! (fn [_ _]
                                    (swap! calls inc)
                                    (throw (ex-info "blew up" {})))]
                    (let [r (lp/retry-sub-loop! {} {"prompt" "p" "subctx" {"focus" "t"}} nil)]
                      (expect (= "failed" (get r "status")))
                      (expect (= "blew up" (get r "error")))
                      (expect (= 2 (get r "attempts")))
                      (expect (= 2 @calls)))))))
  (describe
    "SINGULAR DB connection (child reuses the parent's; never opens its own)"
    (it
      "a child env shares the EXACT parent db-info and disposing it leaves the parent connection alive"
      ;; A sub_loop child (and every parallel child) must run on the parent's ONE
      ;; DB connection — `:parent-db-info` short-circuits `db-create-connection!`,
      ;; so no new pool/datasource is opened. Critical for `:memory` (per-connection
      ;; — a fresh one would be a SEPARATE empty DB) and to avoid connection sprawl.
      (let [parent (lp/create-environment ::router {:db :memory})]
        (try (let
               [child (lp/create-environment ::router
                                             {:child {:parent-db-info (:db-info parent)
                                                      :depth 1
                                                      :security-policy (:security-policy parent)}})]
               ;; SAME connection object — not a second pool
               (expect (identical? (:db-info parent) (:db-info child)))
               (expect (identical? (:security-policy parent) (:security-policy child)))
               (expect (false? (:owns-db? child)))
               (expect (not (false? (:owns-db? parent))))
               ;; disposing the child must NOT close the shared connection
               (lp/dispose-environment! child))
             ;; parent's db-info is still usable after the child was disposed
             (expect (= [] (persistance/db-list-session-turns (:db-info parent) (random-uuid))))
             (finally (lp/dispose-environment! parent)))))))
(defdescribe
  sub-loop-shell-toggle-test
  (it
    "honors a disabled shell toggle in the child turn"
    (let
      [before
       (toggles/enabled? "shell")

       seen
       (atom nil)

       parent
       {:router {:providers [{:id :anthropic :models [{:name "haiku"}]}]}
        :db-info :db
        :depth-atom (atom 0)
        :workspace {:id "parent-ws" :root "/parent"}}]

      (try (toggles/set-enabled! "shell" false)
           (with-redefs
             [lp/child-workspace!
              (fn [& _]
                {:id "child-ws" :root "/child" :fork-ms 0})

              lp/create-environment
              (fn [& _]
                {:ctx-atom (atom {}) :owns-db? false :db-info :db})

              lp/run-turn!
              (fn [& _]
                (reset! seen (toggles/enabled? "shell"))
                {:status :success :answer "disabled"})

              lp/dispose-environment!
              (fn [_])]

             (lp/sub-loop! parent {:prompt "no shell" :subctx {"focus" "no-shell"}}))
           (expect (false? @seen))
           (finally (toggles/set-enabled! "shell" before))))))


(defdescribe
  context-overflow-terminal-breaker-test
  "Typed context overflow must never be fed back into an unreachable next model call."
  (let
    [overflow-ex
     (fn overflow-ex
       ([input max-input source]
        (overflow-ex :svar.tokens/context-overflow input max-input source))
       ([type input max-input source]
        (ex-info "Context overflow"
                 {:type type
                  :source source
                  :model "claude-fable-5"
                  :input-tokens input
                  :max-input-tokens max-input
                  :overflow (when (and input max-input) (- input max-input))})))

     ctx
     {:iteration 1 :messages [] :routing {} :reasoning-level nil}]

    (doseq
      [[label type input max-input source]
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
        (let
          [result
           (lp/handle-iteration-exception! (overflow-ex 210000 200000 :provider) ctx)

           data
           (get-in result [:com.blockether.vis.internal.loop/iteration-error :data])]

          (expect (= :svar.tokens/context-overflow (:type data)))
          (expect (= 210000 (:input-tokens data)))
          (expect (= 200000 (:max-input-tokens data)))
          (expect (= :provider (:source data)))))
    (it "does not make unrelated model errors terminal"
        (let
          [result
           (lp/handle-iteration-exception! (ex-info "NameError: nope" {:type :vis/eval-error}) ctx)]
          (expect (not (:com.blockether.vis.internal.loop/fatal-iteration-error result)))))))

;; Regression: a pinned provider accepted the POST and sent no response header for
;; the whole TTFT budget. svar declined the retry (:no-retry-path), its router had
;; no second candidate under Vis' sticky provider+model pin, and the turn died with
;; ten iterations of finished work — the human had to type "Continue".
(defdescribe
  pre-output-stream-retry-test
  "A stream watchdog that fires before ANY output is the one provider failure Vis
   re-issues itself: no header, no byte, no token, nothing billed, nothing painted."
  (let
    [retryable? @#'lp/pre-output-stream-retryable?

     backoff @#'lp/pre-output-stream-backoff-ms

     next-counters @#'lp/next-retry-counters

     ttft (ex-info "Stream TTFT timeout (60000 ms)" {:type :svar.core/stream-ttft-timeout})]

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
        (expect (= [1 1] (next-counters :com.blockether.vis.internal.loop/retry-pre-output-stream
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
    (doseq
      [error-type [:svar.core/stream-cancelled :svar.core/stream-idle-timeout
                   :svar.core/stream-semantic-timeout]]
      (it (str error-type " is fatal and cannot create a duplicate next iteration")
          (let
            [result (lp/handle-iteration-exception! (ex-info "Terminal stream watchdog failure"
                                                             {:type error-type})
                                                    ctx)]
            (expect (contains? result :com.blockether.vis.internal.loop/iteration-error))
            (expect (true? (:com.blockether.vis.internal.loop/fatal-iteration-error result))))))))

(defdescribe
  provider-unavailable-is-terminal-test
  ;; Regression, issue #105: Vis used to retry svar's terminal provider-unavailable
  ;; result three more times, stacking a second retry ladder above svar.
  (it "surfaces svar's provider-unavailable result without an outer Vis retry"
      (let [ctx {:iteration 1 :messages [] :routing {} :reasoning-level nil}
            result (lp/handle-iteration-exception!
                     (ex-info "Provider unavailable"
                              {:type :svar.llm/provider-unavailable :status 503})
                     ctx)]
        (expect
          (contains? result :com.blockether.vis.internal.loop/iteration-error))
        (expect
          (true? (:com.blockether.vis.internal.loop/fatal-iteration-error result))))))
;; The reply is ONE program: multiple fences a provider splits out collapse to a
;; single code-entry so the form cap spans the whole reply and r["…/fF"] numbers
;; continuously (no per-fence f1 collision).
(defdescribe code-entries-preflight-merge-test
             (it "collapses multiple fenced blocks into ONE code-entry = the normalized concat"
                 (let
                   [pre
                    (@#'lp/code-entries-preflight
                     2
                     [{:source "rg(1)" :lang "python"} {:source "cat(2)" :lang "python"}])

                    entries
                    (:code-entries pre)]

                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)\n\ncat(2)" (:expr (first entries))))))
             (it "leaves a single fenced block untouched"
                 (let
                   [entries (:code-entries
                              (@#'lp/code-entries-preflight 1 [{:source "rg(1)" :lang "python"}]))]
                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)" (:expr (first entries))))))
             (it "dedups identical stutter-fences first (one survivor, not merged with itself)"
                 (let
                   [entries (:code-entries (@#'lp/code-entries-preflight
                                            2
                                            [{:source "rg(1)" :lang "python"}
                                             {:source "rg(1)" :lang "python"}]))]
                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)" (:expr (first entries)))))))

;; The model-facing disclosure: a trimmed iteration tells the model what dropped.
(defdescribe
  literal-code-block-error-test
  ;; The guard's comment-only branch parses inside the SESSION's own GraalPy
  ;; context (post one-context consolidation), so the test threads a real
  ;; context through — built lazily ONCE for the block.
  (let
    [ctx
     (delay (:python-context (env/create-python-context {})))

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

(defdescribe
  strip-echo-diffs-test
  ;; A struct_patch result carries a per-file unified `"diff"`. On a
  ;; successful edit that diff merely re-describes the bytes the model supplied,
  ;; so it is stripped from the MODEL wire. The human card keeps it.
  (let [strip @#'lp/strip-echo-diffs]
    (it "drops the diff from a byte-exact edit summary"
        (expect (= [{"path" "a.clj" "op" "update" "changed" true}]
                   (strip [{"path" "a.clj" "op" "update" "changed" true "diff" "--- x"}]))))
    (it "strips a single-map summary too"
        (expect (= {"path" "a.clj" "op" "add" "changed" true}
                   (strip {"path" "a.clj" "op" "add" "changed" true "diff" "--- x"}))))
    ;; Drive the shared summary builder so struct_patch cannot silently diverge.
    (it
      "strips the diff from a real struct_patch summary"
      (let
        [summary ((deref #'ed/patch-result-file-summary)
                   {:path "a.clj"
                    :op :update
                    :before "(defn foo [a] (+ a 1))\n"
                    :after "(defn foo [a] (* a 2))\n"})]
        ;; the un-stripped summary DOES carry the diff (human card keeps it)
        (expect (contains? summary "diff"))
        ;; struct_patch wraps it as `[summary]` — the echo diff is stripped on the
        ;; model wire, but the line COUNTS survive (they are then the only
        ;; statement of how big the edit was).
        (expect (= [{"path" "a.clj" "op" "update" "changed" true
                     "lines" {"added" 0 "removed" 0 "modified" 1}}]
                   (strip [summary])))))
    (it "leaves a non-edit result untouched"
        (let [r {"hit_count" 3 "matches" {}}]
          (expect (= r (strip r)))))
    (it "leaves a mixed vector (not all file summaries) untouched"
        (let [r [{"path" "a.clj" "op" "update" "changed" true "diff" "--- x"} {"text" "hi"}]]
          (expect (= r (strip r)))))))

(defdescribe
  only-python-execution-is-advertised-test
  ;; ONE tool reaches the provider. Every other capability is already a bare Python
  ;; name inside that sandbox — found with `apropos(text)`, read with `doc(name)`,
  ;; called from inside a block — so a second JSON schema advertises a door the
  ;; model can open anyway and charges for it on every single request.
  (it
    "advertises exactly one tool, and it is python_execution"
    (let
      [tools
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
      (doseq
        [fact ["project packages need a project REPL" "plain Python" "errors surface"
               ;; With no result store left, the description states the one rule that
               ;; replaces it: what you did not print is gone when the block ends.
               "gone from the transcript once the block ends"
               ;; The sleep/poll prohibition lives HERE and nowhere else: the core
               ;; prompt deliberately dropped its duplicate copy.
               "`sh.logs()`" "no tool waits for you"
               ;; Sandbox Python does NOT close a dropped file handle, so an
               ;; unclosed `open(...)` leaks a PROCESS descriptor until a GC —
               ;; enough of them and no `shell` child can be spawned at
               ;; all. The sandbox reclaims and caps them
               ;; (`env-python-fd-test`); the description says so, because the
               ;; cheapest fix is the block never leaking in the first place.
               "Close what you open" "with open(...)" "leaked descriptors"
               "VIS_PY_MAX_OPEN_FILES"]]
        (expect (str/includes? (:description tool) fact))))))

(def ^:private settle-gather-futures! (deref #'lp/settle-gather-futures!))

(def ^:private gather-executor @@#'lp/gather-executor)

(def ^:private gather-max-threads @@#'lp/gather-max-threads)

(defdescribe
  gather-executor-resource-safety-test
  "GraalPy pins a carrier across Value.execute, so gather must never use an
   unbounded virtual-thread-per-task executor. The production pool has a hard
   platform-thread ceiling, no backlog, reclaimable idle workers, and the
   cancellation tests below prove children do not outlive their coordinator."
  (it "is a bounded, self-reclaiming platform pool with no queued-task retention"
      (let
        [^java.util.concurrent.ThreadPoolExecutor exec
         gather-executor

         worker-virtual?
         (.get (.submit exec
                        ^java.util.concurrent.Callable
                        (fn []
                          (.isVirtual (Thread/currentThread)))))]

        (expect (instance? java.util.concurrent.ThreadPoolExecutor exec))
        (expect (= 0 (.getCorePoolSize exec)))
        (expect (= (int gather-max-threads) (.getMaximumPoolSize exec)))
        (expect (instance? java.util.concurrent.SynchronousQueue (.getQueue exec)))
        (expect (.allowsCoreThreadTimeOut exec))
        (expect (= 30 (.getKeepAliveTime exec java.util.concurrent.TimeUnit/SECONDS)))
        (expect (false? worker-virtual?))
        (expect (<= (.getPoolSize exec) (int gather-max-threads)))))
  (it
    "backpressures a saturated virtual submitter instead of pinning it with guest work"
    (let
      [^java.util.concurrent.ThreadPoolExecutor exec
       gather-executor

       release
       (java.util.concurrent.CountDownLatch. 1)

       started
       (java.util.concurrent.CountDownLatch. (int gather-max-threads))

       blockers
       (mapv (fn [_]
               (.submit exec
                        ^java.util.concurrent.Callable
                        (fn []
                          (.countDown started)
                          (.await release)
                          nil)))
             (range gather-max-threads))]

      (try (expect (.await started 5 java.util.concurrent.TimeUnit/SECONDS))
           (let
             [result
              (promise)

              submitter
              (Thread/startVirtualThread
                ^Runnable
                (fn []
                  (try (deliver result
                                (.get (.submit exec
                                               ^java.util.concurrent.Callable
                                               (fn []
                                                 (.isVirtual (Thread/currentThread))))))
                       (catch Throwable e (deliver result e)))))]

             (Thread/sleep 25)
             (expect (not (realized? result)))
             (.countDown release)
             (.join submitter 5000)
             (expect (false? (deref result 5000 ::timeout))))
           (finally (.countDown release)
                    (doseq [^java.util.concurrent.Future blocker blockers]
                      (try (.get blocker 5 java.util.concurrent.TimeUnit/SECONDS)
                           (catch Throwable _))))))))

(defdescribe
  settle-gather-futures-test
  ;; The gather settle loop `(.get f)`-ed each child in order and swallowed
  ;; InterruptedException as that slot's `:err` — so an eval-timeout/cancel
  ;; `.cancel(true)` on the worker never reached the CHILD futures: cancelled
  ;; `gather(rg(...), rg(...))` calls left orphaned virtual threads grinding
  ;; at 100% CPU each until process exit. Now an interrupt during settle
  ;; hard-cancels every child and propagates.
  (it "settles every slot in order — value OR error, a failing slot never aborts siblings"
      (let [exec (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)]
        (try (let
               [futs [(.submit exec
                               ^java.util.concurrent.Callable
                               (fn []
                                 1))
                      (.submit exec
                               ^java.util.concurrent.Callable
                               (fn []
                                 (throw (ex-info "boom" {}))))
                      (.submit exec
                               ^java.util.concurrent.Callable
                               (fn []
                                 3))]
                outcomes (settle-gather-futures! futs)]

               (expect (= {:ok 1} (nth outcomes 0)))
               (expect (= "boom" (ex-message (:err (nth outcomes 1)))))
               (expect (= {:ok 3} (nth outcomes 2))))
             (finally (.shutdownNow exec)))))
  (it
    "hard-cancels still-running children and rethrows when the settling thread is interrupted"
    (let
      [exec
       (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)

       child-started
       (promise)

       child-interrupted
       (promise)]

      (try (let
             [futs
              [(.submit exec
                        ^java.util.concurrent.Callable
                        (fn []
                          (deliver child-started true)
                          (try (Thread/sleep 60000)
                               :never
                               (catch InterruptedException _
                                 (deliver child-interrupted true)
                                 :interrupted))))]

              _
              (when (= ::timeout (deref child-started 5000 ::timeout))
                (throw (ex-info "child did not start" {})))

              _
              (.interrupt (Thread/currentThread))

              thrown
              (try (settle-gather-futures! futs)
                   nil
                   (catch InterruptedException e e)
                   (finally
                     ;; clear the flag before any further test plumbing
                     (Thread/interrupted)))]

             (expect (some? thrown))
             (expect (.isCancelled ^java.util.concurrent.Future (first futs)))
             (expect (= true (deref child-interrupted 2000 ::timeout))))
           (finally (Thread/interrupted) (.shutdownNow exec))))))

(defdescribe
  parallel-sub-loops-cancel-test
  ;; `(mapv deref futs)` propagated an interrupt from the COORDINATING thread
  ;; but never cancelled active child futures, orphaning full LLM turns.
  (it
    "hard-cancels child sub-loops and rethrows when the coordinator is interrupted"
    (let
      [child-started
       (promise)

       child-interrupted
       (promise)

       coordinator-outcome
       (promise)]

      (with-redefs-fn {#'lp/run-spec! (fn [_ _]
                                        (deliver child-started true)
                                        (try (Thread/sleep 60000)
                                             :never
                                             (catch InterruptedException _
                                               (deliver child-interrupted true)
                                               :interrupted)))}
        #(let
           [coordinator
            (Thread/startVirtualThread (fn []
                                         (deliver coordinator-outcome
                                                  (try (lp/parallel-sub-loops! nil [{:prompt "x"}])
                                                       nil
                                                       (catch InterruptedException e e)))))]

           (try (when (= ::timeout (deref child-started 5000 ::timeout))
                  (throw (ex-info "child did not start" {})))
                (.interrupt coordinator)
                (expect (instance? InterruptedException (deref coordinator-outcome 5000 ::timeout)))
                (expect (= true (deref child-interrupted 2000 ::timeout)))
                (finally (.interrupt coordinator) (.join coordinator 5000))))))))

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
      (let
        [fallback
         @#'lp/auth-fallback-routing

         error
         (ex-info "OAuth access token has been revoked" {:type :svar.core/http-error :status 401})

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
    (let
      [fallback
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
      (let
        [fallback
         @#'lp/auth-fallback-routing

         error
         (ex-info "Unauthorized" {:status 401})]

        (expect (nil?
                  (fallback error {:on-auth-error :fallback-provider} {:provider :openai-codex})))
        (expect (= nil (fallback error {} {})))))
  (it "threads the auth-fallback retry without consuming another retry budget"
      (let
        [next-counters
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
      (let
        [cooldown @#'lp/provider-auth-cooldown

         note! @#'lp/note-provider-auth-cooldown!

         request-ok! @#'lp/note-provider-request-ok!

         apply-cooldown @#'lp/apply-auth-cooldown-routing

         base {:on-transient-error :fallback-model-in-the-same-provider}]

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
  (it "expires with the window and releases even an explicitly pinned provider"
      (let
        [cooldown @#'lp/provider-auth-cooldown

         apply-cooldown @#'lp/apply-auth-cooldown-routing

         now (System/currentTimeMillis)]

        (try
          ;; Expired entries are pruned, and routing passes through untouched.
          (reset! cooldown {:rbi-genai {:until (- now 1) :since (- now 60000) :hits 3}})
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
               (expect (= 1 (:hits (get (:cooldowns metrics) :rbi-genai)))))
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
  (let
    [cooldown @#'lp/provider-auth-cooldown

     note! @#'lp/note-provider-auth-cooldown!

     request-ok! @#'lp/note-provider-request-ok!

     apply-cooldown @#'lp/apply-auth-cooldown-routing

     released {:on-auth-error :fallback-provider
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
          (expect (= released
                     (apply-cooldown {:force-provider :rbi-genai :force-model "gpt-5"})))
          (finally (reset! cooldown {}))))
    (it "keeps the cooldown armed when a PEER served the turn"
        (try
          (reset! cooldown {})
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
  (let
    [wrapper
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
     (ex-info "Provider unavailable"
              {:type :svar.llm/provider-unavailable
               :attempts [{:provider :rbi-genai :status 401 :reason :authentication :error "bad key"}
                          {:provider :openai :status 503 :reason :transient-error :error "upstream down"}]})

     shaped? @#'lp/auth-error-shaped?

     fallback-routing @#'lp/auth-fallback-routing

     resolved {:provider :rbi-genai :name "gpt-5"}]

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

(defdescribe
  router-with-pinned-model-test
  "A session pick naming a model only the provider's LIVE catalog lists must still
   BIND. It used to validate away to `{}` and the turn silently ran the default
   model while the picker showed the pick as applied (issue #81)."
  (let
    [router
     {:providers [{:id :openai-codex :models [{:name "gpt-5.4"}]}
                  {:id :zai-coding-plan :models [{:name "glm-4.7"}]}]}

     materialise @#'lp/router-with-pinned-model

     forced @#'lp/forced-routing-for-pref]

    (it "a config-unknown model alone forces NOTHING — the regression"
        (expect (= {} (forced router :zai-coding-plan "glm-4.8"))))
    (it "materialising the pin makes the pick force routing and own the display root"
        (let
          [pinned (materialise router :zai-coding-plan "glm-4.8")]

          (expect (= [{:name "glm-4.7"} {:name "glm-4.8"}]
                     (:models (second (:providers pinned)))))
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
        (with-redefs
          [config/baked-token (fn [_]
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
        (with-redefs
          [config/baked-token
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

(def ^:private AUTH_REFRESH_WINDOW_MS (deref #'lp/AUTH_REFRESH_WINDOW_MS))

(def ^:private AUTH_REFRESH_WINDOW_MAX (deref #'lp/AUTH_REFRESH_WINDOW_MAX))

(defdescribe
  auth-refresh-circuit-breaker-test
  "The breaker budgets forced refreshes per window and drains once they stop."
  (it "grants exactly the per-window budget, reports open, then denies without recording"
      (reset! auth-refresh-events {})
      (expect (every? true?
                      (repeatedly AUTH_REFRESH_WINDOW_MAX
                                  #(auth-refresh-allowed? :ap))))
      (expect (= #{:ap} (:breaker-open (lp/auth-refresh-metrics))))
      (dotimes [_ 25] (expect (false? (auth-refresh-allowed? :ap))))
      (expect (= (long AUTH_REFRESH_WINDOW_MAX)
                 (long (count (get @auth-refresh-events :ap))))))

  (it "closes again once the recorded refreshes age out of the window"
      (let [stale (- (System/currentTimeMillis) (long AUTH_REFRESH_WINDOW_MS) 1)]
        (reset! auth-refresh-events
                {:ap (vec (repeat AUTH_REFRESH_WINDOW_MAX stale))})
        (expect (true? (auth-refresh-allowed? :ap)))
        (expect (= 1 (count (get @auth-refresh-events :ap))))))

  (it "budgets each provider independently"
      (reset! auth-refresh-events {})
      (dotimes [_ AUTH_REFRESH_WINDOW_MAX] (auth-refresh-allowed? :ap))
      (expect (false? (auth-refresh-allowed? :ap)))
      (expect (true? (auth-refresh-allowed? :other)))
      (reset! auth-refresh-events {})))

(defdescribe
  request-bound-credential-hydration-test
  "Every provider attempt reads dynamic auth fields without rebuilding router state."
  (it "replaces all dynamic credential fields while preserving router state"
      (let [state (atom {:health :warm})
            router {:providers [{:id :ap
                                 :api-key "old"
                                 :base-url "https://old.example"
                                 :llm-headers {"old" "header"}
                                 :responses-path "/old"}]
                    :state state
                    :budget {:spent 42}}]
        (with-redefs
          [registry/provider-by-id
           (fn [_]
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

  (it "retains the exact old provider snapshot when token lookup fails"
      (let [provider {:id :ap :api-key "still-usable" :base-url "https://old.example"}
            router {:providers [provider]}]
        (with-redefs
          [registry/provider-by-id
           (fn [_]
             {:provider/get-token-fn (fn [] (throw (ex-info "disk race" {})))})]
          (expect (= provider (first (:providers (hydrate-router-credentials router))))))))

  (it "leaves static providers untouched"
      (let [router {:providers [{:id :static :api-key "configured"}]}]
        (with-redefs [registry/provider-by-id (constantly nil)]
          (expect (= router (hydrate-router-credentials router)))))))

(defdescribe
  request-bound-auth-refresh-test
  "401 recovery adopts peer rotations first and refreshes only the exact rejected token."
  (it "adopts a peer token without spending breaker budget or calling refresh"
      (let [refreshes (atom [])
            attempt-router {:providers [{:id :ap :api-key "rejected"}]}]
        (reset! auth-refresh-events {})
        (with-redefs
          [registry/provider-by-id
           (fn [_]
             {:provider/get-token-fn (fn [] {:token "peer-fresh"})
              :provider/refresh-token-fn (fn [rejected] (swap! refreshes conj rejected))})]
          (expect (true? (try-refresh-provider-token! attempt-router {:provider :ap})))
          (expect (= [] @refreshes))
          (expect (= {} @auth-refresh-events)))))

  (it "passes the exact attempt token to refresh, not a process-global baked token"
      (let [refreshes (atom [])
            attempt-router {:providers [{:id :ap :api-key "attempt-rejected"}]}]
        (reset! auth-refresh-events {})
        (with-redefs
          [config/baked-token (constantly "wrong-global-token")
           registry/provider-by-id
           (fn [_]
             {:provider/get-token-fn (fn [] {:token "attempt-rejected"})
              :provider/refresh-token-fn (fn [rejected] (swap! refreshes conj rejected))})]
          (expect (true? (try-refresh-provider-token! attempt-router {:provider :ap})))
          (expect (= ["attempt-rejected"] @refreshes))
          (expect (= 1 (count (get @auth-refresh-events :ap)))))))

  (it "an open breaker neither refreshes nor mistakes the rejected token for a peer"
      (let [refreshes (atom 0)
            attempt-router {:providers [{:id :ap :api-key "same"}]}]
        (reset! auth-refresh-events
                {:ap (vec (repeat AUTH_REFRESH_WINDOW_MAX (System/currentTimeMillis)))})
        (with-redefs
          [registry/provider-by-id
           (fn [_]
             {:provider/get-token-fn (fn [] {:token "same"})
              :provider/refresh-token-fn (fn [& _] (swap! refreshes inc))})]
          (expect (false? (try-refresh-provider-token! attempt-router {:provider :ap})))
          (expect (= 0 @refreshes))
          (expect (= AUTH_REFRESH_WINDOW_MAX
                     (count (get @auth-refresh-events :ap))))))
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

(defdescribe
  env-reaper-test
  ;; The idle-env reaper is the authoritative backstop against unbounded
  ;; GraalPy Context growth. An empty {} env is safe to dispose:
  ;; dispose-environment! no-ops with no :python-context / :db-info.
  (describe
    "evict-if-idle!"
    (it "disposes + evicts an idle, unlocked entry"
        (let
          [k
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
      (let
        [k
         "reaper-test/busy"

         entry
         (new-cache-entry {})

         ^java.util.concurrent.locks.ReentrantLock lock
         (:lock entry)]

        (swap! env-cache assoc k entry)
        ;; A running turn holds the lock on ANOTHER thread;
        ;; ReentrantLock is reentrant, so the lock MUST be
        ;; held off-thread for tryLock to genuinely fail.
        (let
          [held
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
               (finally (deliver release true) (.join holder 1000) (swap! env-cache dissoc k))))))
    (it "keeps a freshly-touched (not-yet-idle) entry"
        (let
          [k
           "reaper-test/warm"

           entry
           (new-cache-entry {})]

          (swap! env-cache assoc k entry)
          (try (touch-entry! entry)
               (expect (false? (evict-if-idle! k 60000)))
               (expect (contains? @env-cache k))
               (finally (swap! env-cache dissoc k)))))))

(def ^:private reap-idle-envs! (deref #'lp/reap-idle-envs!))

(def ^:private heap-pressure? (deref #'lp/heap-pressure?))

(defdescribe
  env-heap-watermark-test
  ;; Layer 3: under JVM heap pressure the reaper force-evicts EVERY
  ;; idle (unlocked) env this sweep, ignoring the idle TTL, to shed
  ;; GraalPy Contexts fast. A running turn (lock held off-thread) is
  ;; still skipped; the transcript reloads from the DB.
  (describe
    "reap-idle-envs! under heap pressure"
    (it "force-evicts fresh, unlocked entries when pressured"
        (let [k "watermark-test/fresh"]
          (swap! env-cache assoc k (new-cache-entry {}))
          (try
            ;; not idle (just touched) + default 15m TTL: a
            ;; normal sweep keeps it ...
            (with-redefs [lp/heap-pressure? (constantly false)]
              (reap-idle-envs!)
              (expect (contains? @env-cache k)))
            ;; ... but under pressure it is evicted now.
            (with-redefs [lp/heap-pressure? (constantly true)]
              (expect (pos? (reap-idle-envs!)))
              (expect (not (contains? @env-cache k))))
            (finally (swap! env-cache dissoc k)))))
    (it "still skips a locked entry (a running turn) under pressure"
        (let
          [k
           "watermark-test/busy"

           entry
           (new-cache-entry {})

           ^java.util.concurrent.locks.ReentrantLock lock
           (:lock entry)]

          (swap! env-cache assoc k entry)
          (let
            [held
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
                 (with-redefs [lp/heap-pressure? (constantly true)]
                   (reap-idle-envs!)
                   (expect (contains? @env-cache k)))
                 (finally (deliver release true) (.join holder 1000) (swap! env-cache dissoc k))))))
    (it "heap-pressure? is disabled when BOTH gates are off"
        (expect (false? (with-redefs
                          [lp/env-heap-watermark-pct
                           (delay 0)

                           lp/env-heap-budget-mb
                           (delay 0)

                           lp/env-rss-budget-mb
                           (delay 0)]

                          (heap-pressure?)))))
    (it "heap-pressure? fires on the absolute MB budget when the percent watermark can't reach"
        (expect (true? (with-redefs
                         [lp/env-heap-watermark-pct
                          (delay 0)

                          lp/env-heap-budget-mb
                          (delay 1)

                          lp/env-rss-budget-mb
                          (delay 0)]

                         (heap-pressure?)))))))

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
                 (let
                   [k
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
                   (try (with-redefs
                          [lp/open-env!
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

;; Regression, issue #106: a Settings flip only reached live tool bindings when it
;; arrived through the gateway HTTP handler, which called
;; `sync-cached-extension-symbols!` inline. A flip made anywhere else — the TUI
;; settings dialog calls `toggles/set-enabled!` directly, as does any extension —
;; persisted to state.yml and refreshed nothing: every other cached session kept
;; its stale tool surface (no `shell`, no `subprocess`) until a restart.
(defdescribe toggle-change-refreshes-cached-envs-test
             (describe
               "a toggle change from ANY channel"
               (it
                 "refreshes extension bindings in every idle cached env"
                 (toggles/register-toggle!
                   {:id "loop_test_fanout" :label "Fan-out" :default false})
                 (let
                   [k
                    "toggle-fanout-test/idle"

                    entry
                    (new-cache-entry {:marker :idle})

                    synced
                    (atom [])]

                   (swap! env-cache assoc k entry)
                   (try (with-redefs
                          [lp/sync-active-extension-symbols!
                           (fn [e]
                             (swap! synced conj e))]

                          ;; NOT the HTTP handler: the bare toggles API the TUI
                          ;; dialog and every extension flip goes through.
                          (toggles/set-enabled! "loop_test_fanout" true))
                        (expect (some #(= {:marker :idle} %) @synced))
                        (finally (swap! env-cache dissoc k)
                                 (toggles/set-enabled! "loop_test_fanout" false)))))
               (it
                 "leaves a session that is mid-turn to its own next-turn sync"
                 (toggles/register-toggle!
                   {:id "loop_test_fanout_busy" :label "Fan-out busy" :default false})
                 (let
                   [k
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
                    (doto (Thread. ^Runnable (fn []
                                               (.lock lock)
                                               (deliver started true)
                                               (try @release (finally (.unlock lock)))))
                      (.setDaemon true)
                      (.start))

                    synced
                    (atom [])]

                   (swap! env-cache assoc k entry)
                   (try @started
                        (with-redefs
                          [lp/sync-active-extension-symbols!
                           (fn [e]
                             (swap! synced conj e))]

                          (toggles/set-enabled! "loop_test_fanout_busy" true))
                        (expect (not-any? #(= {:marker :busy} %) @synced))
                        (finally (deliver release true)
                                 (.join holder 1000)
                                 (swap! env-cache dissoc k)
                                 (toggles/set-enabled! "loop_test_fanout_busy" false)))))))

(defdescribe env-reaper-enablement-test
             (it "starts for the absolute heap budget even when every older policy is off"
                 (let [enabled? (deref #'lp/env-reaper-enabled?)]
                   (expect (true? (with-redefs
                                    [lp/env-reaper-interval-ms (delay 1000)
                                     lp/env-idle-ttl-ms (delay 0)
                                     lp/env-cache-max (delay 0)
                                     lp/env-heap-watermark-pct (delay 0)
                                     lp/env-heap-budget-mb (delay 1)
                                     lp/env-rss-budget-mb (delay 0)]

                                    (enabled?))))
                   (expect (false? (with-redefs
                                     [lp/env-reaper-interval-ms (delay 1000)
                                      lp/env-idle-ttl-ms (delay 0)
                                      lp/env-cache-max (delay 0)
                                      lp/env-heap-watermark-pct (delay 0)
                                      lp/env-heap-budget-mb (delay 0)
                                      lp/env-rss-budget-mb (delay 0)]

                                     (enabled?))))))
             (it "samples bounded runtime metrics without mutating the cache"
                 (let
                   [before
                    (count @env-cache)

                    snapshot
                    (lp/gateway-runtime-metrics)]

                   (expect (= before (:env-cache-size snapshot)))
                   (expect (pos? (:jvm-heap-max-bytes snapshot)))
                   (expect (pos? (:process-rss-bytes snapshot)))
                   (expect (not (neg? (:jvm-gc-count-total snapshot))))
                   (expect (pos? (:jvm-thread-count snapshot))))))

(defdescribe env-rss-pressure-test
             (it "detects native/process memory when JVM heap gates are disabled"
                 (let [pressure? (deref #'lp/heap-pressure?)]
                   (with-redefs-fn {#'lp/env-heap-watermark-pct (delay 0)
                                    #'lp/env-heap-budget-mb (delay 0)
                                    #'lp/env-rss-budget-mb (delay 1)
                                    #'lp/process-rss-bytes (constantly (* 2 1024 1024))}
                     (fn []
                       (expect (true? (pressure?))))))))

(defdescribe
  emergency-context-fold-projection-test
  (describe
    "one-shot overflow rescue"
    (it
      "folds the OLDEST settled work only, shrinks wire input, and leaves canonical history unchanged"
      (let
        [large
         (apply str (repeat 20000 "x"))

         content
         [{:type "text" :text large} {:type "tool_use" :id "tc" :name "grep" :input {"query" "x"}}]

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
        (expect (some #(re-find #"1 tool call" (str (:content %)))
                      (:messages recovery)))))
    (it "measures the estimator's undercount from the refused request, never a constant"
        (let
          [;; Session cd24926e: the provider priced the very same 132-iteration seed at
           ;; 1,437,952 where the local estimator read 963,503.
           factor
           (estimator-undercount 1437952 963503)

           budget
           (overflow-fold-budget {:provider-tokens 1437952 :provider-limit 1000000 :margin 0.9}
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
          (expect (= 5000 (overflow-fold-budget {:provider-limit 999 :cut 0.5} 10000)))
          (expect (nil? (overflow-fold-budget {} 10000)))))
    (it "escalates: each rescue folds strictly more, then goes terminal"
        (let
          [content
           [{:type "text" :text (apply str (repeat 5000 "x"))}
            {:type "tool_use" :id "tc" :name "grep" :input {"query" "x"}}]

           trailer
           (mapv (fn [i] (stub-tool-iter {:id i :content content})) (range 1 13))

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
           (mapv (fn [_] (rescue)) (range 4))]

          (expect (= [1 2 3 nil] (mapv :attempt rescues)))
          (expect (nil? (last rescues)))
          ;; Every rescue keeps recent work; only the oldest prefix collapses.
          (expect (every? #(< (count (:scopes %)) (count trailer)) (butlast rescues)))
          (expect (apply < (mapv #(count (:scopes %)) (butlast rescues))))
          (expect (apply > (mapv :after-tokens (butlast rescues))))
          (expect (every? #(<= (long (:after-tokens %)) (long (:budget-tokens %)))
                          (butlast rescues)))
          ;; Every rescue reports the undercount it measured, and its projection priced
          ;; through that measurement stays under the limit the provider refused.
          (expect (every? #(some? (:estimator-undercount %)) (butlast rescues)))
          (expect (every? #(< (* (double (:after-tokens %)) (double (:estimator-undercount %)))
                              (double (:provider-limit %)))
                          (butlast rescues)))
          (expect (not-any? #(contains? (:scopes %) "t1/i12") (butlast rescues)))))
    (it "preserves existing semantic fold gists"
        (let
          [large
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
        (let
          [content
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
        (doseq [phase [:reasoning :content :assistant-prose :form-start :tool-start :form-result]]
          (expect (true? (provider-output-chunk? {:phase phase})))))
    (it
      "performs exactly one smaller retry, preserves live and canonical input, then terminates"
      (let
        [large
         (apply str (repeat 30000 "x"))

         content
         [{:type "text" :text large} {:type "tool_use" :id "tc" :name "grep" :input {"query" "x"}}]

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
         (loop
           [messages (into base
                           (conversation-suffix canonical {:provider :openai :model "gpt"}))]
           (swap! calls conj messages)
           (let [result (try (throw overflow) (catch Exception e e))]
             (if-let
               [recovery (context-overflow-recovery! {:error result
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

(defdescribe attachment-reinspection-wire-test
             (it "renders a reinspection image as a canonical vision message"
                 (let
                   [wired-images
                    (deref #'lp/iteration-wired-images)

                    image-messages
                    (deref #'lp/iteration-image-messages)

                    msg
                    (first (image-messages
                             {:images (wired-images {:reinspect-attachments
                                                     [{:id "att-1"
                                                       :media-type "image/png"
                                                       :base64 replay-png-b64}]})}))]

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
        (let
          [cfg
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
        (let
          [cfg
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
                (let
                  [gapped
                   {:providers [{:id :rbi-genai :api-key "${VIS_TEST_UNSET_RBI_KEY}"}]}

                   resolved
                   {:providers [{:id :ok :api-key "sk-literal"}]}

                   svar-err
                   (ex-info "make-router requires at least one provider" {:type :svar/no-providers})

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
  (let
    [fatal? (fn [e]
              (boolean (::lp/fatal-iteration-error (lp/handle-iteration-exception!
                                                     e
                                                     {:iteration 2
                                                      :messages [{:role "user" :content "hi"}]}))))]
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
                                      "\"Third-party apps now draw from your extra usage.\"}}") }))))
    (it "still feeds a correctable model/code failure back for self-correction"
        (expect (not (fatal? (ex-info "Syntax error in generated code" {:type :vis/code-error})))))
    (it "still feeds a plain internal bug back for self-correction"
        (expect (not (fatal? (ex-info "assert failed" {}))))))
  (it "fails once when a wrapped rate-limit error reaches the gateway"
      ;; Reproduces the HTTP/client wrapper seen by the gateway: the outer
      ;; exception is untyped, while its cause carries the 429 provider data.
      ;; It must be terminal, otherwise iteration-loop adds synthetic user
      ;; feedback and asks the same rate-limited provider again.
      (let
        [result (lp/handle-iteration-exception!
                  (ex-info "HTTP client request failed"
                           {}
                           (ex-info "provider rate limited this request"
                                    {:status 429
                                     :provider :anthropic-coding-plan
                                     :body "{\"error\":{\"type\":\"rate_limit_error\"}}"}))
                  {:iteration 2 :messages [{:role "user" :content "hi"}]})]
        (expect (true? (::lp/fatal-iteration-error result)))))
  (it "fails once when a wrapped invalid-key error reaches the gateway"
      (let
        [result (lp/handle-iteration-exception!
                  (ex-info "HTTP client request failed"
                           {}
                           (ex-info "invalid API key" {:status 401 :provider :openai-codex}))
                  {:iteration 2 :messages [{:role "user" :content "hi"}]})]
        (expect (true? (::lp/fatal-iteration-error result))))))

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
  (let
    [reasoning-transcript
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
                                                     {:status 429 :provider :anthropic-coding-plan}))
                               "(rate-limit)")))
    (it "still ends the turn and carries the evidence onto the turn row"
        (let [result (lp/handle-iteration-exception!
                       truncated
                       {:iteration 14 :messages [{:role "user" :content "hi"}]})]
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
  (let
    [handle
     (fn [e]
       (lp/handle-iteration-exception! e {:iteration 1 :messages [{:role "user" :content "hi"}]}))

     fatal?
     (fn [e]
       (boolean (::lp/fatal-iteration-error (handle e))))

     user-error-content
     #'com.blockether.vis.internal.loop/user-error-content

     env-gap
     (ex-info (str "No usable provider — can't use rbi-genai: "
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
        (let
          [blocks
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
                   (expect (= "provider_quota-exhausted" (get block "code")))
                   (expect (str/includes? (get block "message") "plan, usage limits")))))

(defdescribe
  reload-router-hook-test
  ;; `/reload` used to re-read vis.yml WITHOUT rebuilding the router, so a
  ;; changed `default_model` kept routing to the old model and the TUI footer
  ;; chip kept naming it until a restart.
  (describe
    "reload-router!"
    (it "no-ops while the router was never built (lazy first use is preserved)"
        (with-redefs [lp/router-initialized? (fn [] false)
                      lp/rebuild-router! (fn [_] (throw (ex-info "must not build" {})))
                      lp/refresh-cached-routers! (fn [_] (throw (ex-info "must not reseat" {})))]
          (expect (nil? (lp/reload-router!)))))
    (it "rebuilds from the reloaded config and reseats cached session envs"
        (let [built (atom nil)
              seated (atom nil)
              cfg {:providers [{:id :acme}] :default-model "new-model"}]
          (with-redefs [lp/router-initialized? (fn [] true)
                        config/current-config (fn [] cfg)
                        lp/rebuild-router! (fn [c] (reset! built c) ::rebuilt)
                        lp/refresh-cached-routers! (fn [r] (reset! seated r))]
            (expect (nil? (lp/reload-router!))))
          (expect (= cfg @built))
          (expect (= ::rebuilt @seated)))))
  (describe
    "/reload wiring"
    (it "is registered as a reload hook that rebuilds the router"
        (let [hook (get @@#'extension/reload-hooks
                        :com.blockether.vis.internal.loop/router-reload)
              built (atom nil)
              seated (atom nil)
              cfg {:providers [] :default-model "after-reload"}]
          (expect (ifn? hook))
          (with-redefs [lp/router-initialized? (fn [] true)
                        config/current-config (fn [] cfg)
                        lp/rebuild-router! (fn [c] (reset! built c) ::rebuilt)
                        lp/refresh-cached-routers! (fn [r] (reset! seated r))]
            (hook))
          (expect (= cfg @built))
          (expect (= ::rebuilt @seated))))))

(defdescribe human-input-parks-the-eval-wall-test
             ;; REGRESSION: HITL. Code that ASKS the operator blocks in
             ;; human-input/request!, and the enclosing wall used to bill that
             ;; thinking time — the call died with a timeout while the dialog was
             ;; still on screen and the answer was never applied.
             (it "a human-input pause parks the enclosing wall instead of timing out"
                 (let
                   [chan
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
                   (try
                     (let
                       [answerer
                        (future
                          (loop [n 0]
                            (if-let [request-id (some #(when (= :human-input/request (:op %))
                                                         (:request-id %))
                                                      @events)]
                              ;; The operator takes MUCH longer than the 20ms wall.
                              (do (Thread/sleep 1500) (hi/submit! request-id {"otp" "123456"}))
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

(defdescribe normalize-tool-input-strings-only-test
  (describe "model-drift and extension EDN are stringified, keys AND values"
    (it "stringifies keyword/symbol values at every depth"
      (let [normalize #'lp/normalize-tool-input
            normalized (normalize {:op :delete
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
  (describe "model drift is repaired once, at the door"
    (it "normalizes every tool call's :input at every depth"
      (let [door #'lp/normalize-tool-calls
            calls (door [{:id "t1" :name "patch"
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
                   [{:id "p" :name "patch"
                     :input {":edits" [{":path" "a.clj" ":from_anchor" "1:aa"}]}}])]
        (expect (= {"edits" [{"path" "a.clj" "from_anchor" "1:aa"}]} (:input tc)))))
    (it "lets downstream consumers read string keys only — no keyword fallback"
      (let [[tc] (#'lp/normalize-tool-calls [{:id "w" :name "python_execution"
                                              :input {:code "print(1)"}}])]
        (expect (= {"code" "print(1)"} (:input tc)))))))

;; A tool call whose arguments were CORRUPTED inside the provider's own
;; tool-call encoding: the model's closing tag arrived mangled
;; (`</antmlutparameter>`, `</invoke>`) and the API handed that tag over as the
;; VALUE of an argument. Vis ran the call verbatim, so `apropos()` became
;; `apropos("</antmlutparameter>\n")` and answered "no unadvertised capabilities
;; match" to a question nobody asked, and a `cat` whose JSON the model
;; entity-escaped (`&quot;`) reached the tool as one garbage key.
(defdescribe tool-call-protocol-leak-test
  (describe "a leaked tool-call tag is not an argument value"
    (it "drops the mangled closing tag and runs the call the model meant"
      (let [[tc] (#'lp/normalize-tool-calls
                   [{:id "a" :name "python_execution" :input {"code" "</antmlutparameter>\n"}}])]
        (expect (= {} (:input tc)))))
    (it "drops a `</invoke>` value carried under an entity-escaped key"
      (let [[tc] (#'lp/normalize-tool-calls
                   [{:id "c" :name "cat"
                     :input {"workflows/ci.yml&quot;, &quot;ranges&quot;: [[-1, -1]]}]" "\n</invoke>\n"}}])]
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
      (let [{:keys [signals]} (tel/with-signals
                                (#'lp/normalize-tool-calls
                                  [{:id "a" :name "apropos"
                                    :input {"query" "</antmlutparameter>\n"}}]))
            leak (first (filter #(= ::lp/tool-protocol-leak (:id %)) signals))]
        (expect (some? leak))
        (expect (= :warn (:level leak)))
        (expect (= "query" (-> leak :data :argument)))
        (expect (= "</antmlutparameter>\n" (-> leak :data :value))))))
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
      (let [{:keys [signals]} (tel/with-signals
                                (#'lp/normalize-tool-calls
                                  [{:id "a" :name "apropos" :input "</invoke>"}]))
            leak (first (filter #(= ::lp/tool-input-not-an-object (:id %)) signals))]
        (expect (some? leak))
        (expect (= :warn (:level leak)))
        (expect (str/includes? (str (-> leak :data :value)) "</invoke>"))))))

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
                 (get-in (captured-ask-code-opts {:messages []
                                                  :llm-headers {"X-Initiator" "user"}})
                         [:opts :llm-headers "X-Initiator"])))))

;; Regression: Copilot Claude capped `:deep` to `:balanced`. The cap was written
;; for the OPENAI-compatible chat wire, where `reasoning_effort` mis-routed the
;; proxy; on the native `/v1/messages` wire the cap only bought thinking
;; SHALLOWER than Anthropic's own default effort, which is how a `:deep` turn
;; came back with two-word thinking summaries.
(defdescribe
  copilot-claude-reasoning-level-test
  (it "sends the requested depth on EVERY Copilot plan"
      (doseq [provider [:github-copilot :github-copilot-individual :github-copilot-business
                        :github-copilot-enterprise]]
        (expect (= :deep
                   (#'lp/copilot-claude-reasoning-level {:provider provider
                                                         :name "claude-opus-5"}
                                                        "please refactor the loop"
                                                        :deep)))))
  (it "leaves non-Copilot providers at the requested level"
      (expect (= :deep
                 (#'lp/copilot-claude-reasoning-level {:provider :anthropic-coding-plan
                                                       :name "claude-opus-5"}
                                                      "please refactor the loop"
                                                      :deep))))
  (it "names no depth for casual Copilot chat, leaving it to adaptive thinking"
      (expect (nil? (#'lp/copilot-claude-reasoning-level {:provider :github-copilot-individual
                                                          :name "claude-opus-5"}
                                                         "hey"
                                                         :deep)))))

;; Regression, issue #112: the `:provider-call` lifecycle marker carried only the iteration
;; and a start timestamp, so a stalled stream had nothing to name — the gateway failed the
;; turn without ever telling the human which provider and model went silent.
(defdescribe
  provider-call-chunk-test
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
                 (#'lp/provider-call-chunk 0 {} 1)))))

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
      (expect (identical? (providers/router-rebuild-hook-val)
                          #'lp/reload-router!))))

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
  (let [held (promise)
        release (promise)
        ghost (Thread. ^Runnable (fn []
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
;; the engine (parked on GraalPy's GIL, where `Thread.interrupt` never reaches it)
;; never released its session's `ReentrantLock`. The daemon's cancel backstop
;; synthesized `turn.cancelled` and reported the session idle — but `send!` took
;; that lock with a bare `.lock`, so the NEXT turn parked in `Unsafe.park`
;; forever: `turn.started` on the wire, not one event after it, and deaf to its
;; own cancel. The session was dead for the life of the daemon.
(defdescribe wedged-engine-lock-test
  (describe
    "a turn queued behind a wedged one"
    (it
      "waits while the lock is legitimately held, but stays interruptible"
      (let [id "wedge-test/queued"
            k (cache-key id)
            entry (new-cache-entry {:marker :ghost})
            ghost (hold-lock-forever! (:lock entry))
            outcome (promise)]
        (swap! env-cache assoc k entry)
        (try
          (let [queued (Thread. ^Runnable
                                (fn []
                                  (try (lp/send! id "hello" {})
                                       (deliver outcome :returned)
                                       (catch InterruptedException _
                                         (deliver outcome :interrupted))
                                       (catch Throwable t
                                         (deliver outcome [:threw (str (class t))]))))
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
      (let [id "wedge-test/condemned"
            k (cache-key id)
            entry (new-cache-entry {:marker :ghost})
            ^java.util.concurrent.locks.ReentrantLock dead-lock (:lock entry)
            ghost (hold-lock-forever! dead-lock)
            got (promise)]
        (swap! env-cache assoc k entry)
        (try
          (with-redefs-fn {#'lp/open-env! (fn [_ _]
                                            {:marker :fresh})}
            (fn []
              (let [waiter (Thread. ^Runnable
                                    (fn []
                                      (let [e (acquire-turn-lock! id)]
                                        (.unlock ^java.util.concurrent.locks.ReentrantLock (:lock e))
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
                   (swap! env-cache dissoc k)))))))
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
  (let [calls (atom [])
        result (atom nil)]
    (with-redefs-fn {#'persistance/db-update-session-turn!
                     (fn [_db _id o]
                       (swap! calls conj o)
                       (when (reject? o)
                         (throw (ex-info "[SQLITE_TOOBIG] String or BLOB exceeds size limit" {})))
                       :written)}
      #(reset! result (#'lp/persist-turn-outcome! {} "turn-1" opts)))
    {:calls @calls :result @result}))

;; Regression (session 4b6897d4): the write that RECORDS a turn's outcome was
;; itself unguarded, so a payload the store refused (`[SQLITE_TOOBIG]` on an
;; error message quoting the whole document that broke the turn) left the turn
;; `running` for good -- no status, no error, no iteration count -- inside a
;; session that had already finished it.
(defdescribe
  turn-outcome-guard-test
  "The write that records HOW a turn ended must never be the write that loses
   it: on refusal the outcome is re-written from a minimal payload that carries
   neither CTX nor the content the store would not take."
  (it "writes once and reports success when the store accepts the payload"
      (let [{:keys [calls result]} (outcome-writes (constantly false) failed-turn-outcome)]
        (expect (true? result))
        (expect (= [failed-turn-outcome] calls))))
  (it "degrades to a recorded outcome when the full payload is refused"
      (let [{:keys [calls result]} (outcome-writes #(some? (:ctx %)) failed-turn-outcome)
            degraded (second calls)]
        (expect (true? result))
        (expect (= 2 (count calls)))
        ;; The turn ENDS: same status and counters, no CTX, no unstorable answer.
        (expect (= :error (:status degraded)))
        (expect (= 33 (:iteration-count degraded)))
        (expect (= 1234 (:duration-ms degraded)))
        (expect (= :error (:prior-outcome degraded)))
        (expect (nil? (:ctx degraded)))
        (expect (= 1 (count (:content degraded))))
        (expect (= "turn_outcome_persist_failed" (get (:error degraded) "code")))
        (expect (str/includes? (get (:error degraded) "message") "SQLITE_TOOBIG"))))
  (it "keeps a successful turn's own status when only its answer is refused"
      (let [{:keys [calls result]} (outcome-writes #(some? (:ctx %))
                                                  (assoc failed-turn-outcome
                                                    :status :success
                                                    :error nil
                                                    :prior-outcome :complete))]
        (expect (true? result))
        (expect (= :success (:status (second calls))))
        (expect (= "turn_outcome_persist_failed" (get (:error (second calls)) "code")))))
  (it "reports failure instead of throwing when even the minimal outcome is refused"
      (let [{:keys [calls result]} (outcome-writes (constantly true) failed-turn-outcome)]
        (expect (false? result))
        (expect (= 2 (count calls))))))

;; Regression (vis session 26af5650): an upstream stream timeout killed a turn
;; and the UI showed NOTHING - no answer, no error card, no counters, duration 0.
;; The failure branch ran the non-answer-shaped fallback through `answer-content`
;; unguarded, so its validation throw escaped `send!` BEFORE the terminal write
;; ran: the turn was never recorded and "Final answer must be canonical content
;; or Markdown prose" masked the provider failure that actually killed it.
(defdescribe
  failed-turn-finalization-test
  "A failed turn ALWAYS records its outcome: content rebuilt from the trace's
   provider failure, never a throw out of the finalizer."
  (it "records the outcome and the provider failure when the fallback is not answer-shaped"
      (let [writes (atom [])

            trace [{:iteration 1}
                   {:iteration 2
                    :error {:message "Stream idle timeout (300000ms with no bytes): closed"
                            :data {:type :svar.core/stream-idle-timeout
                                   :idle-timeout-ms 300000}}}]

            result (with-redefs-fn {#'persistance/db-update-session-turn!
                                    (fn [_db _id o] (swap! writes conj o) :written)}
                     #(#'lp/finalize-turn-result
                       {:db-info {} :root-model "m" :root-provider :p}
                       {:session-turn-id "turn-1"
                        :start-time (System/nanoTime)
                        :iteration-count 2
                        :status :error
                        :trace trace
                        :answer {:overloaded true :status 529}
                        :total-tokens-atom (atom {})
                        :total-cost-atom (atom {})}))

            written (first @writes)

            card (first (:content written))]
        ;; the terminal write happened at all - the whole point
        (expect (= 1 (count @writes)))
        (expect (= :error (:status written)))
        (expect (= 2 (:iteration-count written)))
        ;; content is the real provider failure, never the validation string
        (expect (= "error" (get card "type")))
        (expect (nil? (re-find #"(?i)canonical content" (pr-str (:content written)))))
        (expect (some? (:error written)))
        (expect (= :error (:status result)))))
  (it "keeps the answer's own blocks when the fallback IS answer-shaped"
      (let [writes (atom [])

            answer [(content/error "provider_error" "upstream refused" false)]

            _ (with-redefs-fn {#'persistance/db-update-session-turn!
                               (fn [_db _id o] (swap! writes conj o) :written)}
                #(#'lp/finalize-turn-result
                  {:db-info {}}
                  {:session-turn-id "turn-2"
                   :start-time (System/nanoTime)
                   :iteration-count 1
                   :status :error
                   :trace []
                   :answer answer
                   :total-tokens-atom (atom {})
                   :total-cost-atom (atom {})}))

            written (first @writes)]
        (expect (= 1 (count @writes)))
        (expect (= "upstream refused" (get (first (:content written)) "message"))))))
