(ns com.blockether.vis.ext.language-clojure.nrepl-ctx-test
  "Unit tests for the `:ext/ctx-fn` nREPL contribution. The session's owned REPLs
   (`repl-manager/session-repls`) and liveness (`nrepl-client/probe!`) are stubbed
   so the assertions are deterministic and offline; the real `describe`/eval
   round-trip is covered in `nrepl-client-test`."
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nc]
            [com.blockether.vis.ext.language-clojure.nrepl-ctx :as nx]
            [com.blockether.vis.ext.language-clojure.repl-manager :as rm]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- reset-cache! [] (reset! @#'nx/liveness-cache {:key nil :statuses {}}))

(defn- no-resource-mirror
  "Stub the resource-mirror IO so contribute never touches the real registry."
  [f]
  (with-redefs
    [nx/ensure-resource! (fn [& _]
                           nil)]
    (f)))

(defdescribe
  canonical-resource-sync-test
  (it "returns no legacy env contribution and sends one probed REPL to the resource mirror"
      (reset-cache!)
      (let [mirrored (atom nil)]
        (with-redefs
          [rm/session-repls
           (fn [_]
             [{:id "nrepl:/proj" :dir "/proj" :port 7001 :tool :clj :aliases [:dev :test]}])
           nc/probe! (fn [_]
                       {:status :up :versions {:clojure "1.12.4"} :dialect :clj})
           nc/health-check! (fn [_]
                              {:status :up :ms 3})
           nx/ensure-resource! (fn [sid statuses repl]
                                 (reset! mirrored {:sid sid :statuses statuses :repl repl}))]

          (expect (= {}
                     (nx/contribute {:workspace/root "/proj"
                                     :session-id "s1"
                                     :ctx-atom (atom {:session/turn 1})})))
          (expect (= "s1" (:sid @mirrored)))
          (expect (= :up (get-in @mirrored [:statuses 7001 :status])))
          (expect (= :clj (get-in @mirrored [:statuses 7001 :dialect])))
          (expect (= "nrepl:/proj" (get-in @mirrored [:repl :id])))))))

(defdescribe
  eval-health-block-test
  (it "passes unresponsive eval diagnostics to the canonical resource mirror"
      (reset-cache!)
      (let [mirrored (atom nil)]
        (with-redefs
          [rm/session-repls (fn [_]
                              [{:id "nrepl:/proj" :dir "/proj" :port 7001 :tool :clj}])
           nc/probe! (fn [_]
                       {:status :up :versions {:clojure "1.12.4"} :dialect :clj})
           nc/health-check!
           (fn [_]
             {:status :unresponsive :form "(+ 1 1)" :hint "UNRESPONSIVE — restart or reprobe"})
           nx/ensure-resource! (fn [_ statuses _]
                                 (reset! mirrored (get statuses 7001)))]

          (nx/contribute
            {:workspace/root "/proj" :session-id "s1" :ctx-atom (atom {:session/turn 1})})
          (expect (= :unresponsive (:status @mirrored)))
          (expect (= "(+ 1 1)" (:form @mirrored)))
          (expect (re-find #"(?i)restart|reprobe" (:hint @mirrored)))
          (expect (= :clj (:dialect @mirrored)))))))

(defdescribe
  resource-mirror-logs-test
  (it "registers nREPL mirrors as log-capable resources when the manager has a log path"
      (let
        [sid
         (str "nrepl-ctx-logs-" (System/nanoTime))

         rid
         "nrepl:/proj"

         log
         (java.io.File/createTempFile "vis-nrepl-ctx-" ".log")]

        (try
          (spit log "starting\nready\n")
          (@#'nx/ensure-resource!
           sid
           {7001 {:status :up}}
           {:id rid :dir "/proj" :port 7001 :tool :clj :aliases [:dev] :log (.getAbsolutePath log)})
          (let [r (vis/get-resource sid rid)]
            (expect (= true (get r "can_logs")))
            (expect (= (.getAbsolutePath log) (get-in r ["detail" "log"])))
            (expect (= ["starting" "ready"] (vis/resource-logs sid rid))))
          (finally (vis/unregister-resource! sid rid) (.delete log))))))

(defdescribe
  resource-mirror-health-test
  (it "does not mark a classpath-resolving nREPL healthy before its port answers"
      (let
        [sid
         (str "nrepl-ctx-health-" (System/nanoTime))

         rid
         "nrepl:/proj"]

        (try (with-redefs
               [rm/session-repls
                (fn [_]
                  [{:id rid :dir "/proj" :port 7001 :tool :clj}])

                nc/probe!
                (fn [_]
                  {:status :starting})]

               (nx/contribute
                 {:workspace/root "/proj" :session-id sid :ctx-atom (atom {:session/turn 42})})
               (expect (= "starting" (get (vis/get-resource sid rid) "status"))))
             (finally (vis/unregister-resource! sid rid)))))
  (it "refreshes an existing mirror from healthy to failed when liveness changes"
      (let
        [sid
         (str "nrepl-ctx-health-refresh-" (System/nanoTime))

         rid
         "nrepl:/proj"]

        (try (@#'nx/ensure-resource! sid {7001 {:status :up}} {:id rid :dir "/proj" :port 7001})
             (@#'nx/ensure-resource! sid {7001 {:status :failed}} {:id rid :dir "/proj" :port 7001})
             (expect (= "failed" (get (vis/get-resource sid rid) "status")))
             (finally (vis/unregister-resource! sid rid))))))

(defdescribe robustness-test
             (it "returns {} when no workspace root is on env" (expect (= {} (nx/contribute {}))))
             (it "never throws — degrades to {} when session-repls blows up"
                 (reset-cache!)
                 (with-redefs
                   [rm/session-repls (fn [_]
                                       (throw (ex-info "boom" {})))]
                   (expect (= {}
                              (nx/contribute {:workspace/root "/proj"
                                              :session-id "s1"
                                              :ctx-atom (atom {:session/turn 4})}))))))

(defdescribe
  per-turn-cache-test
  (it "probes once per (turn, port-set); re-probes when the turn advances"
      (reset-cache!)
      (no-resource-mirror
        (fn []
          (let [calls (atom 0)]
            (with-redefs
              [rm/session-repls (fn [_]
                                  [{:id "nrepl:/p" :dir "/p" :port 9999 :tool :clj}])
               nc/probe! (fn [_]
                           (swap! calls inc)
                           {:status :up})]

              (let
                [e1 {:workspace/root "/p" :session-id "s1" :ctx-atom (atom {:session/turn 5})}
                 e2 {:workspace/root "/p" :session-id "s1" :ctx-atom (atom {:session/turn 6})}]

                (nx/contribute e1)
                (nx/contribute e1)
                (nx/contribute e1)
                (expect (= 1 @calls))
                (nx/contribute e2)
                (expect (= 2 @calls)))))))))
