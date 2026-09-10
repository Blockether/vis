(ns com.blockether.vis.internal.config.agent-name-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.validation :as validation]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.workspace.git :as git]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [java.nio.file Files]))

(deftest validates-agent-name
  (doseq [value ["Ada" "  Ada  " "助手" (apply str (repeat 80 "a"))]]
    (is (validation/valid? {"agent_name" value})))
  (doseq [value [nil 42 true "" "   " "Ada\nOther" "Ada\tOther" (str "Ada" (char 27))
                 (apply str (repeat 81 "a"))]]
    (is (not (validation/valid? {"agent_name" value})))
    (with-redefs [config/load-config-raw (constantly {"agent_name" value})
                  config/load-global-config-raw (constantly nil)]

      (is (= "Vis" (config/agent-name))))))

(deftest yaml-name-crosses-gateway-and-jvm-boundaries
  (let [dir
        (.toFile (Files/createTempDirectory "vis-agent-name"
                                            (make-array java.nio.file.attribute.FileAttribute 0)))

        store
        (io/file dir "store")

        project
        (io/file dir "project")

        other
        (io/file dir "other")

        sid
        (random-uuid)]

    (try
      (.mkdirs store)
      (.mkdirs project)
      (.mkdirs other)
      (with-redefs [config/config-dir
                    (constantly (.getPath store))

                    lp/db-info
                    (constantly {})

                    lp/by-id
                    (constantly {:id sid :channel :api :title "Task"})

                    git/workspace-status
                    (constantly nil)]

        (with-redefs-fn {#'state/resolve-workspace (fn [_ _]
                                                     {:id "workspace" :root (.getPath project)})
                         #'prompt/system-prompt-file-overrides (constantly nil)}
          (fn []
            (is (= "Vis" (config/agent-name (.getPath project))))
            (spit (io/file store "config.yml") "agent_name: Global\n")
            (is (= "Global" (config/agent-name (.getPath project))))
            (spit (io/file store "state.yml") "agent_name: Personal\n")
            (is (= "Personal" (config/agent-name (.getPath project))))
            (.delete (io/file store "state.yml"))
            (spit (io/file project "vis.yml") "agent_name: '  Ada  '\n")
            (spit (io/file other "vis.yml") "agent_name: Grace\n")
            (is (= "Grace" (config/agent-name (.getPath other))))
            (is (= "Ada" (config/agent-name (.getPath project))))
            (is (= "Ada" (get (state/soul sid) "agent_name")))
            (is (= "Ada" (get (state/session-workspace-info sid) "agent_name")))
            (let [response (#'server/soul-handler {:path-params {:sid (str sid)}})]
              (is (= 200 (:status response)))
              (is (str/includes? (:body response) "\"agent_name\":\"Ada\"")))
            (is (str/starts-with? (prompt/build-system-prompt {:workspace-root (.getPath project)})
                                  "You are Ada. Complete the task autonomously."))
            (.mkdirs (io/file project ".vis"))
            (spit (io/file project ".vis/config.yml") "agent_name: Overlay\n")
            (is (= "Overlay" (state/session-agent-name sid)))
            (spit (io/file project ".vis/config.yml") "agent_name: Updated\n")
            (is (= "Updated" (state/session-agent-name sid)))
            (spit (io/file store "state.yml") "toggles:\n  shell: true\n")
            (let [events
                  (atom [])

                  request
                  {:query-params {"id" "agent_name" "action" "value" "value" "  Grace  "}}]

              (with-redefs-fn {#'state/other-session-ids (constantly [sid "second-client"])
                               #'state/append-event! (fn [& args]
                                                       (swap! events conj args))}
                (fn []
                  (is (= 200 (:status (#'server/set-setting-handler request))))))
              (is (= 2 (count @events)))
              (is (every? #(= ["session.agent_name_updated" {:agent-name "Grace"} {:store? false}]
                              (vec (rest %)))
                          @events)))
            (is (= {"toggles" {"shell" true} "agent_name" "Grace"} (config/load-global-config-raw)))
            (is (= "Grace" (state/session-agent-name sid)))
            (is (= "Grace" (config/agent-name (.getPath other))))
            (is (= "Grace" (get (state/soul sid) "agent_name")))
            (is (= "Grace" (get (state/session-workspace-info sid) "agent_name")))
            (let [out (java.io.ByteArrayOutputStream.)]
              (#'server/sse-ready! out sid 0 [])
              (is (str/includes? (.toString out "UTF-8") "\"agent_name\":\"Grace\"")))
            (is (str/starts-with? (prompt/build-system-prompt {:workspace-root (.getPath project)})
                                  "You are Grace."))
            (is (str/includes? (:body (#'server/get-setting-handler
                                       {:path-params {:id "agent_name"}}))
                               "\"value\":\"Grace\""))
            (is (str/includes? (:body (#'server/list-settings-handler {})) "\"id\":\"agent_name\""))
            (doseq [value [nil "" "   " "Ada\nOther" 42 (apply str (repeat 81 "a"))]]
              (is (= 400
                     (:status (#'server/set-setting-handler
                               {:query-params
                                {"id" "agent_name" "action" "value" "value" value}})))))
            (is (= 400
                   (:status (#'server/set-setting-handler
                             {:query-params {"id" "agent_name" "action" "toggle"}}))))
            (is (= "Grace" (state/session-agent-name sid)))
            (with-redefs [config/load-config-raw (constantly {"system_prompt" {"text"
                                                                               "Custom identity"
                                                                               "is_replace" true}})]
              (is (= "Custom identity"
                     (prompt/build-system-prompt {:workspace-root (.getPath project)})))))))
      (finally (doseq [file (reverse (file-seq dir))]
                 (.delete ^java.io.File file))))))
