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
                             "detail" {"dir" "/repo" "port" 7888}}
                            {"id" "api"
                             "kind" "repl"
                             "language" "python"
                             "status" "starting"
                             "detail" {"dir" "/repo/apps/api" "cmd" "python -i"}}]
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
