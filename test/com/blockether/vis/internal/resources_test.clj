(ns com.blockether.vis.internal.resources-test
  "Health contract of the session resource registry: `list-resources` probes
   every health-capable resource (`:health-fn`, parallel with a hard timeout)
   and flips the stored `status` to reality; a throwing or wedged health-fn can
   neither hang a render nor corrupt the stored status."
  (:require [com.blockether.vis.internal.resources :as resources]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- fresh-sid [] (str "res-test-" (java.util.UUID/randomUUID)))

(defdescribe health-fn-test
             (it "advertises can_health and flips status via the health probe on list"
                 (let
                   [sid
                    (fresh-sid)

                    health
                    (atom :up)]

                   (try (let
                          [data (resources/register! sid
                                                     {:id "r1" :kind :thing :status :up}
                                                     {:health-fn (fn []
                                                                   @health)})]
                          (expect (true? (get data "can_health")))
                          (expect (= "up" (get (resources/get-resource sid "r1") "status")))
                          ;; the resource degrades -> the NEXT list reflects it
                          (reset! health :failed)
                          (let [[r] (resources/list-resources sid)]
                            (expect (= "failed" (get r "status"))))
                          ;; ...and recovers the same way
                          (reset! health :up)
                          (let [[r] (resources/list-resources sid)]
                            (expect (= "up" (get r "status")))))
                        (finally (resources/unregister! sid "r1")))))
             (it "a resource without a health-fn keeps its stored status (can_health false)"
                 (let [sid (fresh-sid)]
                   (try (let [data (resources/register! sid {:id "r2" :status :running} nil)]
                          (expect (false? (get data "can_health")))
                          (let [[r] (resources/list-resources sid)]
                            (expect (= "running" (get r "status")))))
                        (finally (resources/unregister! sid "r2")))))
             (it "a THROWING health-fn means UNKNOWN — the stored status is left alone"
                 (let [sid (fresh-sid)]
                   (try (resources/register! sid
                                             {:id "r3" :status :starting}
                                             {:health-fn (fn []
                                                           (throw (ex-info "boom" {})))})
                        (let [[r] (resources/list-resources sid)]
                          (expect (= "starting" (get r "status"))))
                        (finally (resources/unregister! sid "r3")))))
             (it "a WEDGED health-fn is cut off by the hard timeout and cannot hang the list"
                 (let [sid (fresh-sid)]
                   (try (resources/register! sid
                                             {:id "r4" :status :up}
                                             {:health-fn (fn []
                                                           (Thread/sleep 10000)
                                                           :down)})
                        (let
                          [t0 (System/currentTimeMillis)
                           [r] (resources/list-resources sid)
                           elapsed (- (System/currentTimeMillis) t0)]

                          (expect (= "up" (get r "status")))
                          (expect (< elapsed 5000)))
                        (finally (resources/unregister! sid "r4")))))
             (it "a failed-but-alive resource survives pruning and lists with its health status"
                 (let [sid (fresh-sid)]
                   (try (resources/register! sid
                                             ;; bogus pid — WOULD be pruned without the alive-fn
                                             {:id "r5" :status :up :pid 999999999}
                                             {:alive-fn (fn []
                                                          true)
                                              :health-fn (fn []
                                                           :failed)})
                        (let [[r] (resources/list-resources sid)]
                          (expect (some? r))
                          (expect (= "failed" (get r "status"))))
                        (finally (resources/unregister! sid "r5"))))))

(defdescribe model-view-test
             (it "indexes REPL state by language and workspace-relative dir without a flat mirror"
                 (let
                   [view (resources/model-view
                           [{"id" "main"
                             "kind" "nrepl"
                             "language" "clojure"
                             "status" "up"
                             "detail" {"cwd" "/repo" "port" 7888}}
                            {"id" "api"
                             "kind" "repl"
                             "language" "python"
                             "status" "starting"
                             "detail" {"cwd" "/repo/apps/api" "cmd" "python -i"}}]
                           {:root "/repo" :languages ["clojure" "python" "typescript"]})]
                   (expect (= "up" (get-in view ["repls" "clojure" "." "status"])))
                   (expect (= 7888 (get-in view ["repls" "clojure" "." "port"])))
                   (expect (= "starting" (get-in view ["repls" "python" "apps/api" "status"])))
                   (expect (= {} (get-in view ["repls" "typescript"])))
                   (expect (not (vector? view)))))
             (it "groups non-REPL resources without reviving the flat legacy shape"
                 (let
                   [resource
                    {"id" "server" "kind" "process" "status" "up"}

                    view
                    (resources/model-view [resource] {:root "/repo"})]

                   (expect (= resource (get-in view ["other" "process" "server"])))
                   (expect (nil? (get view "repls"))))))

(defdescribe
  lifecycle-race-test
  (it
    "does not unregister a replacement created while the old stop callback is blocked"
    (let
      [sid
       (fresh-sid)

       entered
       (promise)

       release
       (promise)

       old-stops
       (atom 0)

       replacement-stops
       (atom 0)]

      (try (resources/register! sid
                                {:id "same" :kind :process :status :running :label "old"}
                                {:stop-fn (fn []
                                            (swap! old-stops inc)
                                            (deliver entered true)
                                            @release)})
           (let [stopping (future (resources/stop! sid "same"))]
             ;; Deterministically put registration inside the old implementation's
             ;; vulnerable window: it had read the old record but had not yet run
             ;; its unconditional post-callback `unregister!`.
             @entered
             (resources/register! sid
                                  {:id "same" :kind :process :status :running :label "replacement"}
                                  {:stop-fn #(swap! replacement-stops inc)})
             (deliver release true)
             (expect (= :stopped (:result @stopping)))
             (expect (= 1 @old-stops))
             (expect (= 0 @replacement-stops))
             (let [[survivor] (resources/list-resources sid)]
               (expect (some? survivor))
               (expect (= "replacement" (get survivor "label")))))
           (finally (deliver release true) (resources/stop-all! sid)))))
  (it
    "cannot let an older delayed persistence write erase a newer registration"
    (let
      [sid
       (fresh-sid)

       writer-var
       (ns-resolve 'com.blockether.vis.internal.resources 'write-persisted!)

       writer-entered
       (promise)

       release-writer
       (promise)

       writer-calls
       (atom 0)

       writes
       (atom [])]

      (try (resources/register! sid
                                {:id "same" :kind :process :status :running :label "old"}
                                {:stop-fn (fn [])})
           (with-redefs-fn {writer-var (fn [snapshot]
                                         (if (= 1 (swap! writer-calls inc))
                                           (do (deliver writer-entered true)
                                               @release-writer
                                               (swap! writes conj snapshot))
                                           (swap! writes conj snapshot)))}
             (fn []
               (let [stopping (future (resources/stop! sid "same"))]
                 ;; Freeze the old stop's persistence after it captured the empty
                 ;; state. Registration mutates the atom before trying to persist.
                 @writer-entered
                 (let
                   [registering
                    (future (resources/register!
                              sid
                              {:id "same" :kind :process :status :running :label "replacement"}
                              {:stop-fn (fn [])}))]
                   (loop [tries 1000]
                     (when-not (= "replacement" (get (resources/get-resource sid "same") "label"))
                       (when (zero? tries) (throw (ex-info "Replacement was not registered" {})))
                       (Thread/sleep 1)
                       (recur (dec tries))))
                   (deliver release-writer true)
                   (expect (= :stopped (:result (deref stopping 5000 {:result :timeout}))))
                   (expect (not= ::timeout (deref registering 5000 ::timeout)))
                   ;; The last disk write must reflect the latest registry state.
                   ;; Without snapshot/write serialization these two writes land
                   ;; in the opposite order and the replacement disappears on disk.
                   (expect (= "replacement" (get-in (last @writes) [sid "same" "label"])))))))
           (finally (deliver release-writer true) (resources/stop-all! sid)))))
  (it
    "does not apply a delayed update to a replacement generation"
    (let
      [sid
       (fresh-sid)

       normalize-var
       (ns-resolve 'com.blockether.vis.internal.resources 'normalize-patch)

       original-normalize
       (var-get normalize-var)

       entered
       (promise)

       release
       (promise)]

      (try (resources/register! sid {:id "same" :kind :process :status :running :label "old"})
           (with-redefs-fn {normalize-var (fn [patch]
                                            (deliver entered true)
                                            @release
                                            (original-normalize patch))}
             (fn []
               (let [updating (future (resources/update! sid "same" {:status :failed}))]
                 @entered
                 (resources/unregister! sid "same")
                 (resources/register!
                   sid
                   {:id "same" :kind :process :status :running :label "replacement"})
                 (deliver release true)
                 (expect (nil? @updating))
                 (let [survivor (resources/get-resource sid "same")]
                   (expect (= "replacement" (get survivor "label")))
                   (expect (= "running" (get survivor "status")))))))
           (finally (deliver release true) (resources/unregister! sid "same")))))
  (it "does not prune a replacement installed while the old liveness probe is blocked"
      (let
        [sid
         (fresh-sid)

         entered
         (promise)

         release
         (promise)]

        (try (resources/register! sid
                                  {:id "same" :kind :process :status :running :label "old"}
                                  {:alive-fn (fn []
                                               (deliver entered true)
                                               @release
                                               false)})
             (let [pruning (future (resources/prune! sid))]
               @entered
               (resources/register!
                 sid
                 {:id "same" :kind :process :status :running :label "replacement"})
               (deliver release true)
               (expect (empty? @pruning))
               (expect (= "replacement" (get (resources/get-resource sid "same") "label"))))
             (finally (deliver release true) (resources/unregister! sid "same")))))
  (it "does not apply an old delayed health result to a replacement generation"
      (let
        [sid
         (fresh-sid)

         entered
         (promise)

         release
         (promise)]

        (try (resources/register! sid
                                  {:id "same" :kind :process :status :running :label "old"}
                                  {:health-fn (fn []
                                                (deliver entered true)
                                                @release
                                                :failed)})
             (let [listing (future (resources/list-resources sid))]
               @entered
               (resources/register!
                 sid
                 {:id "same" :kind :process :status :running :label "replacement"})
               (deliver release true)
               @listing
               (let [survivor (resources/get-resource sid "same")]
                 (expect (= "replacement" (get survivor "label")))
                 (expect (= "running" (get survivor "status")))))
             (finally (deliver release true) (resources/unregister! sid "same")))))
  (it
    "stop-all chases a replacement installed while the old callback is blocked"
    (let
      [sid
       (fresh-sid)

       entered
       (promise)

       release
       (promise)

       old-stops
       (atom 0)

       replacement-stops
       (atom 0)]

      (try (resources/register! sid
                                {:id "same" :kind :process :status :running :label "old"}
                                {:stop-fn (fn []
                                            (swap! old-stops inc)
                                            (deliver entered true)
                                            @release)})
           (let [stopping-all (future (resources/stop-all! sid))]
             @entered
             (resources/register! sid
                                  {:id "same" :kind :process :status :running :label "replacement"}
                                  {:stop-fn #(swap! replacement-stops inc)})
             (deliver release true)
             (let [results (deref stopping-all 5000 ::timed-out)]
               (expect (not= ::timed-out results))
               (expect (= 2 (count results)))
               (expect (every? #(= :stopped (:result %)) results))
               (expect (= 1 @old-stops))
               (expect (= 1 @replacement-stops))
               (expect (nil? (resources/get-resource sid "same")))))
           (finally (deliver release true) (resources/stop-all! sid)))))
  (it "restores a failed stop generation so its cleanup can be retried"
      (let
        [sid
         (fresh-sid)

         calls
         (atom 0)]

        (try (resources/register! sid
                                  {:id "flaky" :kind :process :status :running :label "retryable"}
                                  {:stop-fn (fn []
                                              (when (= 1 (swap! calls inc))
                                                (throw (ex-info "transient stop failure" {}))))})
             (let [first-result (resources/stop! sid "flaky")]
               (expect (= :error (:result first-result)))
               (expect (= "transient stop failure" (:message first-result)))
               (expect (= "retryable" (get (resources/get-resource sid "flaky") "label"))))
             (expect (= :stopped (:result (resources/stop! sid "flaky"))))
             (expect (= 2 @calls))
             (expect (nil? (resources/get-resource sid "flaky")))
             (finally (resources/stop-all! sid)))))
  (it "never restores a failed old stop over the replacement its callback installed"
      (let
        [sid
         (fresh-sid)

         replacement-stops
         (atom 0)]

        (try (resources/register!
               sid
               {:id "same" :kind :process :status :running :label "old"}
               {:stop-fn (fn []
                           (resources/register!
                             sid
                             {:id "same" :kind :process :status :running :label "replacement"}
                             {:stop-fn #(swap! replacement-stops inc)})
                           (throw (ex-info "old stop failed late" {})))})
             (expect (= :error (:result (resources/stop! sid "same"))))
             (expect (= "replacement" (get (resources/get-resource sid "same") "label")))
             (expect (= 0 @replacement-stops))
             (finally (resources/stop-all! sid)))))
  (it "stop-all reports an always-failing generation once instead of hot-looping it"
      (let
        [sid
         (fresh-sid)

         calls
         (atom 0)]

        (try (resources/register! sid
                                  {:id "broken" :kind :process :status :running}
                                  {:stop-fn #(do (swap! calls inc) (throw (ex-info "nope" {})))})
             (let [results (deref (future (resources/stop-all! sid)) 2000 ::timed-out)]
               (expect (not= ::timed-out results))
               (expect (= 1 (count results)))
               (expect (= :error (:result (first results))))
               (expect (= 1 @calls))
               (expect (some? (resources/get-resource sid "broken"))))
             (finally (resources/unregister! sid "broken")))))
  (it
    "tears down a session that first appears while an earlier stop callback is blocked"
    (let
      [teardown
       (var-get (ns-resolve 'com.blockether.vis.internal.resources 'teardown-sessions!))

       sid-a
       (fresh-sid)

       sid-b
       (fresh-sid)

       entered
       (promise)

       release
       (promise)

       a-stops
       (atom 0)

       b-stops
       (atom 0)]

      (try
        (resources/register! sid-a
                             {:id "a" :kind :process :status :running}
                             {:stop-fn (fn []
                                         (swap! a-stops inc)
                                         (deliver entered true)
                                         @release)})
        (let [tearing (future (teardown #{sid-a sid-b}))]
          @entered
          ;; A teardown callback (or any concurrent owner) can put a brand new
          ;; SESSION into the registry; a one-shot key snapshot would leave it running.
          (resources/register! sid-b
                               {:id "b" :kind :process :status :running}
                               {:stop-fn #(swap! b-stops inc)})
          (deliver release true)
          (let [result (deref tearing 5000 ::timed-out)]
            (expect (not= ::timed-out result))
            (expect (= #{sid-a sid-b} (set (keys result))))
            (expect (= 1 @a-stops))
            (expect (= 1 @b-stops))
            (expect (nil? (resources/get-resource sid-a "a")))
            (expect (nil? (resources/get-resource sid-b "b")))))
        (finally (deliver release true) (resources/stop-all! sid-a) (resources/stop-all! sid-b))))))
