(ns com.blockether.vis.internal.extension.client-test
  (:require [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
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

(deftest declaration-reuses-portable-contract-and-survives-environment-recreation
  (with-registration
    (fn [sid env entries]
      (let [entry (first (extension/ext-symbols (first entries)))]
        (is (= 'lookup (:ext.symbol/symbol entry)))
        (is (= (get (tool "lookup") "contract") (:ext.symbol/contract entry)))
        (is (= false (get-in entry [:ext.symbol/activity :show-start])))
        (is (= entries (client/extensions-for sid)))
        (is (= entries (client/register! sid "owner" (payload "lookup") (constantly true) env)))
        (client/detach! sid)
        (is (nil? (client/extensions-for sid)))
        (is (false? ((:ext/activation-fn (first entries)) env)))))))

(deftest callback-preserves-positional-maps-and-keyword-metadata
  (with-registration (fn [sid _ entries]
                       (let [f
                             (future ((invoke-fn entries)
                                       {"positional" 7}
                                       (with-meta {"flag" true}
                                         {::python-host/keyword-arguments true})))

                             call
                             (await-call sid)]

                         (is (= [{"positional" 7}] (:args call)))
                         (is (= {"flag" true} (:kwargs call)))
                         (is (= "lookup" (:name call)))
                         (client/complete! sid "owner" (:id call) {"status" "success" "result" nil})
                         (is (nil? (:result (deref f 2000 ::timeout))))
                         (is (empty? (:calls (client/pending sid "owner"))))))))

(deftest result-retries-are-idempotent-and-conflicts-refused
  (with-registration
    (fn [sid _ entries]
      (let [f
            (future ((invoke-fn entries) 3))

            id
            (:id (await-call sid))

            result
            {"status" "success" "result" {"answer" 3}}]

        (is (= {} (client/complete! sid "owner" id result)))
        (is (= {"answer" 3} (:result (deref f 2000 ::timeout))))
        (is (= {} (client/complete! sid "owner" id result)))
        (is (= :client_call_conflict
               (failure-code
                 #(client/complete! sid "owner" id {"status" "success" "result" 4}))))))))

(deftest owner-and-session-boundaries-are-enforced
  (with-registration (fn [sid _ entries]
                       (let [f
                             (future ((invoke-fn entries)))

                             id
                             (:id (await-call sid))]

                         (doseq [action
                                 [#(client/pending sid "other")
                                  #(client/complete! sid "other" id {"status" "success" "result" 1})
                                  #(client/detach-owned! sid "other")
                                  #(client/pending (str (UUID/randomUUID)) "owner")]]
                           (is (= :client_extension_owner (failure-code action))))
                         (client/complete! sid "owner" id {"status" "success" "result" 1})
                         (is (= 1 (:result (deref f 2000 ::timeout))))))))

(deftest presentation-and-python-failure-reach-the-observed-invocation
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
          (is (= [presentation] @seen))
          (is (re-find #"ValueError: Missing record" (str result))))))))

(deftest collisions-and-malformed-declarations-are-atomic
  (let [sid
        (str (UUID/randomUUID))

        env
        {:extensions (atom [])}]

    (try (doseq [body [(payload "read" "read.child") (payload "same" "same")
                       (assoc-in (payload "lookup") ["extensions" 0 "symbols" 0 "fn"] "code")
                       (assoc-in (payload "lookup") ["extensions" 0 "providers"] [])]]
           (is (some? (failure-code #(client/register! sid "owner" body (constantly true) env))))
           (is (nil? (client/extensions-for sid))))
         (let [entries (client/register! sid "owner" (payload "lookup") (constantly true) env)]
           (reset! (:extensions env) entries)
           (is (= :client_extension_conflict
                  (failure-code #(client/register! (str (UUID/randomUUID))
                                                   "other"
                                                   (payload "lookup.child")
                                                   (constantly true)
                                                   env)))))
         (finally (client/detach! sid)))))

(deftest disconnect-lease-loss-and-deadline-unblock-callers
  (doseq [mode [:detach :lease :timeout]]
    (let [sid (str (UUID/randomUUID))
          live? (atom true)
          entries
          (client/register! sid "owner" (payload "lookup") #(deref live?) {:extensions (atom [])})]

      (try (let [f (binding [client/*call-timeout-ms* (if (= :timeout mode) 50 300000)]
                     (future (failure-code #((invoke-fn entries)))))
                 id (:id (await-call sid))]

             (case mode
               :detach
               (client/detach-owner! "owner")

               :lease
               (reset! live? false)

               nil)
             (is (contains? #{:client_call_gone :client_extension_owner} (deref f 2000 ::timeout)))
             (when (= mode :timeout)
               (is (= :client_call_gone
                      (failure-code
                        #(client/complete! sid "owner" id {"status" "success" "result" 1}))))))
           (finally (client/detach! sid))))))

(deftest result-size-and-canonical-activity-are-validated
  (with-registration
    (fn [sid _ entries]
      (let [f
            (future ((invoke-fn entries)))

            id
            (:id (await-call sid))]

        (is (= :invalid_client_activity
               (failure-code
                 #(client/activity! sid "owner" id {"headline" "Bad" "status" "success"}))))
        (is (= :client_call_result_limit
               (failure-code #(client/complete! sid
                                                "owner"
                                                id
                                                {"status" "success"
                                                 "result" (apply str (repeat 1048577 "x"))}))))
        (client/complete! sid "owner" id {"status" "success" "result" []})
        (is (= [] (:result (deref f 2000 ::timeout))))))))

(deftest detached-adapters-stay-inactive-after-owner-registers-again
  (with-registration (fn [sid env entries]
                       (client/detach! sid)
                       (client/register! sid "owner" (payload "lookup") (constantly true) env)
                       (is (false? ((:ext/activation-fn (first entries)) env)))
                       (is (= :client_call_gone (failure-code #((invoke-fn entries)))))
                       (is (empty? (:calls (client/pending sid "owner")))))))

(deftest interrupt-preserves-cancellation-and-removes-pending-call
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
          (is (= :interrupted (deref result 2000 ::timeout)))
          (.join worker 2000)
          (is (empty? (:calls (client/pending sid "owner"))))
          (is (= :client_call_gone
                 (failure-code
                   #(client/complete! sid "owner" id {"status" "success" "result" nil})))))))))
