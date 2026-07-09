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

(defn- nrepl-of
  [contribution]
  ;; The contribution is STRING-keyed from the top: the contract key is
  ;; "session_env" (ctx_loop drops a keyword :session/env), and everything under
  ;; it crosses the strings-only boundary.
  (get-in contribution ["session_env" "languages" "clojure" "nrepl"]))

(defn- no-resource-mirror
  "Stub the resource-mirror IO so contribute never touches the real registry."
  [f]
  (with-redefs [nx/ensure-resource! (fn [_ _]
                                      nil)]
    (f)))

(defdescribe
  contribute-shape-test
  (it "surfaces ONE owned REPL with id/port/status/aliases + it is the default"
      (reset-cache!)
      (no-resource-mirror
        (fn []
          (with-redefs [rm/session-repls
                        (fn [_]
                          [{:id "nrepl:/proj"
                            :dir "/proj"
                            :port 7001
                            :tool :clj
                            :aliases [:dev :test]}])

                        nc/probe!
                        (fn [_]
                          {:status :up :versions {:clojure "1.12.4"} :dialect :clj})]

            (let [b
                  (nrepl-of (nx/contribute {:workspace/root "/proj"
                                            :session-id "s1"
                                            :ctx-atom (atom {:session/turn 1})}))

                  p
                  (first (get b "repls"))]

              ;; the SINGLE owned REPL's id is the implicit default
              (expect (= "nrepl:/proj" (get b "default")))
              (expect (= "nrepl:/proj" (get p "id")))
              (expect (= "/proj" (get p "dir")))
              (expect (= 7001 (get p "port")))
              (expect (= "up" (get p "status")))
              (expect (true? (get p "managed")))
              (expect (= "clj" (get p "tool")))
              (expect (= "clj" (get p "dialect")))
              (expect (= ["dev" "test"] (get p "aliases")))
              (expect (= {"clojure" "1.12.4"} (get p "versions")))))))))

(defdescribe resource-mirror-logs-test
             (it "registers nREPL mirrors as log-capable resources when the manager has a log path"
                 (let [sid (str "nrepl-ctx-logs-" (System/nanoTime))
                       rid "nrepl:/proj"
                       log (java.io.File/createTempFile "vis-nrepl-ctx-" ".log")]
                   (try
                     (spit log "starting\nready\n")
                     (@#'nx/ensure-resource!
                      sid
                      {:id rid
                       :dir "/proj"
                       :port 7001
                       :tool :clj
                       :aliases [:dev]
                       :log (.getAbsolutePath log)})
                     (let [r (vis/get-resource sid rid)]
                       (expect (= true (get r "can_logs")))
                       (expect (= (.getAbsolutePath log) (get-in r ["detail" "log"])))
                       (expect (= ["starting" "ready"] (vis/resource-logs sid rid))))
                     (finally
                       (vis/unregister-resource! sid rid)
                       (.delete log))))))

(defdescribe default-selection-test
             (it "no default when MORE THAN ONE REPL is owned (id must be specified)"
                 (reset-cache!)
                 (no-resource-mirror
                   (fn []
                     (with-redefs [rm/session-repls
                                   (fn [_]
                                     [{:id "nrepl:/a" :dir "/a" :port 1 :tool :clj}
                                      {:id "nrepl:/b" :dir "/b" :port 2 :tool :clj}])

                                   nc/probe!
                                   (fn [_]
                                     {:status :up})]

                       (let [b (nrepl-of (nx/contribute {:workspace/root "/a"
                                                         :session-id "s1"
                                                         :ctx-atom (atom {:session/turn 2})}))]
                         (expect (nil? (get b "default")))
                         (expect (= 2 (count (get b "repls"))))
                         (expect (= #{"nrepl:/a" "nrepl:/b"}
                                    (set (map #(get % "id") (get b "repls"))))))))))
             (it "emits an empty block (no default, no repls) when the session owns none"
                 (reset-cache!)
                 (no-resource-mirror (fn []
                                       (with-redefs [rm/session-repls (fn [_]
                                                                        [])]
                                         (let [b (nrepl-of (nx/contribute
                                                             {:workspace/root "/proj"
                                                              :session-id "s1"
                                                              :ctx-atom (atom {:session/turn 3})}))]
                                           (expect (nil? (get b "default")))
                                           (expect (= [] (get b "repls")))))))))

(defdescribe robustness-test
             (it "returns {} when no workspace root is on env" (expect (= {} (nx/contribute {}))))
             (it "never throws — degrades to {} when session-repls blows up"
                 (reset-cache!)
                 (with-redefs [rm/session-repls (fn [_]
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
            (with-redefs [rm/session-repls (fn [_]
                                             [{:id "nrepl:/p" :dir "/p" :port 9999 :tool :clj}])
                          nc/probe! (fn [_]
                                      (swap! calls inc)
                                      {:status :up})]

              (let [e1 {:workspace/root "/p" :session-id "s1" :ctx-atom (atom {:session/turn 5})}
                    e2 {:workspace/root "/p" :session-id "s1" :ctx-atom (atom {:session/turn 6})}]

                (nx/contribute e1)
                (nx/contribute e1)
                (nx/contribute e1)
                (expect (= 1 @calls))
                (nx/contribute e2)
                (expect (= 2 @calls)))))))))
