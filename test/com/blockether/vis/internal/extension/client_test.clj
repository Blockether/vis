(ns com.blockether.vis.internal.extension.client-test
  (:require [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.contract.document :as contract]
            [com.blockether.vis.internal.extension.client :as client]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.python.host :as python-host])
  (:import [java.util UUID]))

(defn- tool
  [n]
  {"name" n
   "tag" "observation"
   "hidden" false
   "doc" "Return application data."
   "params" []
   "varargs" true
   "contract" {"version" 1
               "name" n
               "tag" "observation"
               "description" "Return application data."
               "signature" "(*args, **kwargs)"
               "parameters" []
               "returns" {"kind" "any" "name" "Any"}}
   "activity" {"presenter" "observation" "label" "Read application data" "show_start" false}})

(defn- payload
  [& names]
  {"extensions" [{"name" "application"
                  "description" "Application tools"
                  "alias" "app"
                  "version" "1"
                  "kind" "python"
                  "prompt" "Use application data."
                  "symbols" (mapv tool names)}]})

(defn- with-registration
  [run]
  (let [sid
        (str (UUID/randomUUID))

        env
        {:extensions (atom [])}

        entries
        (client/register! sid "owner" (payload "lookup") (constantly true) env)]

    (try (run sid env entries) (finally (client/detach! sid)))))

(defn- invoke-fn [entries] (:ext.symbol/fn (first (extension/ext-symbols (first entries)))))

(defn- await-call
  [sid]
  (loop [attempt 0]
    (if-let [call (first (:calls (client/pending sid "owner")))]
      call
      (if (< attempt 200)
        (do (Thread/sleep 5) (recur (inc attempt)))
        (throw (ex-info "No queued application call" {}))))))

(defn- failure-code [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:code (ex-data e)))))

(defdescribe
  declaration-reuses-portable-contract-and-survives-environment-recreation
  (it "declaration reuses portable contract and survives environment recreation"
      (with-registration
        (fn [sid env entries]
          (let [entry (first (extension/ext-symbols (first entries)))]
            (expect (= 'lookup (:ext.symbol/symbol entry)))
            (expect (= (get (tool "lookup") "contract") (:ext.symbol/contract entry)))
            (expect (= false (get-in entry [:ext.symbol/activity :show-start])))
            (expect (= entries (client/extensions-for sid)))
            (expect (= entries
                       (client/register! sid "owner" (payload "lookup") (constantly true) env)))
            (client/detach! sid)
            (expect (nil? (client/extensions-for sid)))
            (expect (false? ((:ext/activation-fn (first entries)) env))))))))

(defdescribe callback-preserves-positional-maps-and-keyword-metadata
             (it "callback preserves positional maps and keyword metadata"
                 (with-registration
                   (fn [sid _ entries]
                     (let [f
                           (future ((invoke-fn entries)
                                     {"positional" 7}
                                     (with-meta {"flag" true}
                                       {::python-host/keyword-arguments true})))

                           call
                           (await-call sid)]

                       (expect (= [{"positional" 7}] (:args call)))
                       (expect (= {"flag" true} (:kwargs call)))
                       (expect (= "lookup" (:name call)))
                       (client/complete! sid "owner" (:id call) {"status" "success" "result" nil})
                       (expect (nil? (:result (deref f 2000 ::timeout))))
                       (expect (empty? (:calls (client/pending sid "owner")))))))))

(defdescribe
  result-retries-are-idempotent-and-conflicts-refused
  (it "result retries are idempotent and conflicts refused"
      (with-registration
        (fn [sid _ entries]
          (let [f
                (future ((invoke-fn entries) 3))

                id
                (:id (await-call sid))

                result
                {"status" "success" "result" {"answer" 3}}]

            (expect (= {} (client/complete! sid "owner" id result)))
            (expect (= {"answer" 3} (:result (deref f 2000 ::timeout))))
            (expect (= {} (client/complete! sid "owner" id result)))
            (expect (= :client_call_conflict
                       (failure-code
                         #(client/complete! sid "owner" id {"status" "success" "result" 4})))))))))

(defdescribe owner-and-session-boundaries-are-enforced
             (it "owner and session boundaries are enforced"
                 (with-registration
                   (fn [sid _ entries]
                     (let [f
                           (future ((invoke-fn entries)))

                           id
                           (:id (await-call sid))]

                       (doseq [action
                               [#(client/pending sid "other")
                                #(client/complete! sid "other" id {"status" "success" "result" 1})
                                #(client/detach-owned! sid "other")
                                #(client/pending (str (UUID/randomUUID)) "owner")]]
                         (expect (= :client_extension_owner (failure-code action))))
                       (client/complete! sid "owner" id {"status" "success" "result" 1})
                       (expect (= 1 (:result (deref f 2000 ::timeout)))))))))

(defdescribe presentation-and-python-failure-reach-the-observed-invocation
             (it "presentation and python failure reach the observed invocation"
                 (with-registration
                   (fn [sid _ entries]
                     (let [seen
                           (atom [])

                           f
                           (future (binding [extension/*activity-content-sink* #(swap! seen conj %)]
                                     ((invoke-fn entries))))

                           id
                           (:id (await-call sid))

                           presentation
                           {"headline" "Read application data"
                            "summary" "No matching record"
                            "content" []
                            "sections" []}]

                       (client/activity! sid "owner" id presentation)
                       (client/complete! sid
                                         "owner"
                                         id
                                         {"status" "failure"
                                          "error" {"type" "ValueError" "message" "Missing record"}})
                       (let [result (deref f 2000 ::timeout)]
                         (expect (= [presentation] @seen))
                         (expect (re-find #"ValueError: Missing record" (str result)))))))))

(defdescribe
  collisions-and-malformed-declarations-are-atomic
  (it "collisions and malformed declarations are atomic"
      (let [sid
            (str (UUID/randomUUID))

            env
            {:extensions (atom [])}]

        (try (doseq [body [(payload "read" "read.child") (payload "same" "same")
                           (assoc-in (payload "lookup") ["extensions" 0 "symbols" 0 "fn"] "code")
                           (assoc-in (payload "lookup") ["extensions" 0 "providers"] [])]]
               (expect (some? (failure-code
                                #(client/register! sid "owner" body (constantly true) env))))
               (expect (nil? (client/extensions-for sid))))
             (let [entries (client/register! sid "owner" (payload "lookup") (constantly true) env)]
               (reset! (:extensions env) entries)
               (expect (= :client_extension_conflict
                          (failure-code #(client/register! (str (UUID/randomUUID))
                                                           "other"
                                                           (payload "lookup.child")
                                                           (constantly true)
                                                           env)))))
             (finally (client/detach! sid))))))

(defdescribe
  disconnect-lease-loss-and-deadline-unblock-callers
  (it "disconnect lease loss and deadline unblock callers"
      (doseq [mode [:detach :lease :timeout]]
        (let [sid (str (UUID/randomUUID))
              live? (atom true)
              entries (client/register! sid
                                        "owner"
                                        (payload "lookup")
                                        #(deref live?)
                                        {:extensions (atom [])})]

          (try (let [f (binding [client/*call-timeout-ms* (if (= :timeout mode) 50 300000)]
                         (future (failure-code #((invoke-fn entries)))))
                     id (:id (await-call sid))]

                 (case mode
                   :detach
                   (client/detach-owner! "owner")

                   :lease
                   (reset! live? false)

                   nil)
                 (expect (contains? #{:client_call_gone :client_extension_owner}
                                    (deref f 2000 ::timeout)))
                 (when (= mode :timeout)
                   (expect
                     (= :client_call_gone
                        (failure-code
                          #(client/complete! sid "owner" id {"status" "success" "result" 1}))))))
               (finally (client/detach! sid)))))))

(defdescribe
  result-size-and-canonical-activity-are-validated
  (it "result size and canonical activity are validated"
      (with-registration
        (fn [sid _ entries]
          (let [f
                (future ((invoke-fn entries)))

                id
                (:id (await-call sid))]

            (expect (= :invalid_client_activity
                       (failure-code
                         #(client/activity! sid "owner" id {"headline" "Bad" "status" "success"}))))
            (expect (= :client_call_result_limit
                       (failure-code #(client/complete! sid
                                                        "owner"
                                                        id
                                                        {"status" "success"
                                                         "result" (apply str
                                                                    (repeat 1048577 "x"))}))))
            (client/complete! sid "owner" id {"status" "success" "result" []})
            (expect (= [] (:result (deref f 2000 ::timeout)))))))))

(defdescribe
  idle-owner-replaces-only-the-static-prompt
  (it "idle owner replaces only the static prompt"
      (with-registration
        (fn [sid env _]
          (let [changed
                (assoc-in (payload "lookup") ["extensions" 0 "prompt"] "Use fresh data.")

                [entry]
                (client/register! sid "owner" changed (constantly true) env)]

            (expect (= "Use fresh data." ((:ext/prompt-fn entry) env)))
            (expect (= [entry] (client/extensions-for sid)))
            (expect (= :client_extension_conflict
                       (failure-code
                         #(client/register! sid "owner" (payload "other") (constantly true) env))))
            (expect (= [entry] (client/extensions-for sid))))))))

;; #520: an application had no way to keep the prompt of its extension from a subagent.
(defdescribe
  client-prompt-audience-test
  (it "gives the prompt of each audience of the schema its own extension key"
      (let [audiences
            (get-in (contract/schema-document "gateway")
                    ["$defs" "client_extensions" "properties" "extensions" "items" "properties"
                     "prompt_audience" "enum"])

            prompt-keys
            (mapv extension/prompt-key audiences)]

        (expect (apply distinct? prompt-keys))
        (doseq [audience audiences]
          (let [sid
                (str (UUID/randomUUID))

                env
                {:extensions (atom [])}

                [entry]
                (client/register! sid
                                  "owner"
                                  (assoc-in (payload "lookup")
                                            ["extensions" 0 "prompt_audience"]
                                            audience)
                                  (constantly true)
                                  env)]

            (try (expect (= [(extension/prompt-key audience)] (filterv #(contains? entry %) prompt-keys)))
                 (expect (= "Use application data."
                            ((get entry (extension/prompt-key audience)) env)))
                 (finally (client/detach! sid))))))))

(defdescribe detached-adapters-stay-inactive-after-owner-registers-again
             (it "detached adapters stay inactive after owner registers again"
                 (with-registration
                   (fn [sid env entries]
                     (client/detach! sid)
                     (client/register! sid "owner" (payload "lookup") (constantly true) env)
                     (expect (false? ((:ext/activation-fn (first entries)) env)))
                     (expect (= :client_call_gone (failure-code #((invoke-fn entries)))))
                     (expect (empty? (:calls (client/pending sid "owner"))))))))

(defdescribe
  interrupt-preserves-cancellation-and-removes-pending-call
  (it "interrupt preserves cancellation and removes pending call"
      (with-registration
        (fn [sid _ entries]
          (let [result
                (promise)

                worker
                (Thread. ^Runnable
                         (fn []
                           (try ((invoke-fn entries))
                                (deliver result :unexpected-success)
                                (catch InterruptedException _ (deliver result :interrupted)))))]

            (.start worker)
            (let [id (:id (await-call sid))]
              (.interrupt worker)
              (expect (= :interrupted (deref result 2000 ::timeout)))
              (.join worker 2000)
              (expect (empty? (:calls (client/pending sid "owner"))))
              (expect
                (= :client_call_gone
                   (failure-code
                     #(client/complete! sid "owner" id {"status" "success" "result" nil}))))))))))

(defn- gated
  "A registration whose `guard` callable gates the `lookup` tool. Runs `run` with
   the session id and a function that calls `lookup` on another thread."
  [run]
  (let [sid
        (str (UUID/randomUUID))

        env
        {:extensions (atom [])}

        entries
        (client/register! sid
                          "owner"
                          (assoc-in (payload "lookup")
                            ["extensions" 0 "gates"]
                            [{"op" "app/lookup" "symbol" "guard"}])
                          (constantly true)
                          env)

        ext
        (first entries)

        call
        (fn [timeout-ms]
          (future (binding [client/*call-timeout-ms* timeout-ms]
                    (try (extension/invoke-symbol-wrapper ext
                                                          (first (extension/ext-symbols ext))
                                                          ["secret"]
                                                          env)
                         (catch clojure.lang.ExceptionInfo e (ex-message e))))))]

    (reset! (:extensions env) entries)
    (try (run sid call) (finally (client/detach! sid)))))

(defn- answer-gate!
  "Answer the pending gate call with `outcome` and return that call."
  [sid outcome]
  (let [gate (await-call sid)]
    (client/complete! sid "owner" (:id gate) outcome)
    gate))

(defdescribe client-gate-asks-before-the-tool-runs
             (it "allows the call on null and refuses it with the reason of a text answer"
                 (gated
                   (fn [sid call]
                     (let [allowed
                           (call 300000)

                           gate
                           (answer-gate! sid {"status" "success" "result" nil})

                           tool
                           (await-call sid)]

                       (expect (= "guard" (:name gate)))
                       (expect (= [{"op" "app/lookup" "args" ["secret"]}] (:args gate)))
                       (expect (= "lookup" (:name tool)))
                       (client/complete! sid "owner" (:id tool) {"status" "success" "result" 7})
                       (expect (= 7 (deref allowed 2000 ::timeout))))
                     (let [refused (call 300000)]
                       (answer-gate! sid {"status" "success" "result" "Secrets stay private."})
                       (expect (= "app/lookup blocked: Secrets stay private."
                                  (deref refused 2000 ::timeout)))
                       (expect (empty? (:calls (client/pending sid "owner")))))))))

(defdescribe client-gate-fails-closed
             (it "refuses the call when the gate fails, gives no text, times out or detaches"
                 (doseq [mode [:failure :number :timeout :detach]]
                   (gated
                     (fn [sid call]
                       (let [refused (call (if (= :timeout mode) 50 300000))]
                         (case mode
                           :failure
                           (answer-gate! sid
                                         {"status" "failure"
                                          "error" {"type" "ValueError" "message" "Broken"}})

                           :number
                           (answer-gate! sid {"status" "success" "result" 1})

                           :timeout
                           nil

                           :detach
                           (do (await-call sid) (client/detach! sid)))
                         (let [message (deref refused 2000 ::timeout)]
                           (expect (re-find #"^app/lookup blocked: the client gate guard " message)
                                   [mode message])
                           (expect (re-find #"fails closed" message) [mode message]))))))))

(defdescribe
  client-gate-refuses-a-pattern-for-every-operation
  (it
    "refuses a match-all pattern and a gate callable that is also a tool"
    (doseq [[op code] [["*" :client_gate_too_broad] ["*.read" :client_gate_too_broad]
                       ["app*" :client_gate_too_broad] ["lookup" :invalid_client_gate]]]
      (let [sid (str (UUID/randomUUID))
            gate-name (if (= :invalid_client_gate code) "lookup" "guard")
            body
            (assoc-in (payload "lookup") ["extensions" 0 "gates"] [{"op" op "symbol" gate-name}])]

        (expect (=
                  code
                  (failure-code
                    #(client/register! sid "owner" body (constantly true) {:extensions (atom [])})))
                op)
        (expect (nil? (client/extensions-for sid)))))
    (let [sid (str (UUID/randomUUID))]
      (try (expect (= 1
                      (count (client/register! sid
                                               "owner"
                                               (assoc-in (payload "lookup")
                                                 ["extensions" 0 "gates"]
                                                 [{"op" "linear.*" "symbol" "guard"}])
                                               (constantly true)
                                               {:extensions (atom [])}))))
           (finally (client/detach! sid))))))
