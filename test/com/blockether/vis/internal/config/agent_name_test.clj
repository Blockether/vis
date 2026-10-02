(ns com.blockether.vis.internal.config.agent-name-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.validation :as validation]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.server.sessions :as sessions-api]
            [com.blockether.vis.internal.gateway.server.settings :as settings-api]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.workspace.git :as git]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]))

(defdescribe validates-agent-name
             (it "validates agent name"
                 (doseq [value ["Ada" "  Ada  " "助手" (apply str (repeat 80 "a"))]]
                   (expect (validation/valid? {"agent_name" value})))
                 (doseq [value [nil 42 true "" "   " "Ada\nOther" "Ada\tOther" (str "Ada" (char 27))
                                (apply str (repeat 81 "a"))]]
                   (expect (not (validation/valid? {"agent_name" value})))
                   (with-redefs [config/load-config-raw (constantly {"agent_name" value})
                                 config/load-global-config-raw (constantly nil)]

                     (expect (= "Vis" (config/agent-name)))))))

(defdescribe
  yaml-name-crosses-gateway-and-jvm-boundaries
  (it
    "yaml name crosses gateway and jvm boundaries"
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
              (expect (= "Vis" (config/agent-name (.getPath project))))
              (spit (io/file store "config.yml") "agent_name: Global\n")
              (expect (= "Global" (config/agent-name (.getPath project))))
              (spit (io/file store "state.yml") "agent_name: Personal\n")
              (expect (= "Personal" (config/agent-name (.getPath project))))
              (.delete (io/file store "state.yml"))
              (spit (io/file project "vis.yml") "agent_name: '  Ada  '\n")
              (spit (io/file other "vis.yml") "agent_name: Grace\n")
              (expect (= "Grace" (config/agent-name (.getPath other))))
              (expect (= "Ada" (config/agent-name (.getPath project))))
              (expect (= "Ada" (get (state/soul sid) "agent_name")))
              (expect (= "Ada" (get (state/session-workspace-info sid) "agent_name")))
              (let [response (#'sessions-api/soul-handler {:path-params {:sid (str sid)}})]
                (expect (= 200 (:status response)))
                (expect (str/includes? (:body response) "\"agent_name\":\"Ada\"")))
              (expect (str/starts-with? (prompt/build-system-prompt {:workspace-root (.getPath
                                                                                       project)})
                                        "You are Ada. Complete the task on your own."))
              (.mkdirs (io/file project ".vis"))
              (spit (io/file project ".vis/config.yml") "agent_name: Overlay\n")
              (expect (= "Overlay" (state/session-agent-name sid)))
              (spit (io/file project ".vis/config.yml") "agent_name: Updated\n")
              (expect (= "Updated" (state/session-agent-name sid)))
              (spit (io/file store "state.yml") "toggles:\n  shell: true\n")
              (let [events
                    (atom [])

                    request
                    {:query-params {"id" "agent_name" "action" "value" "value" "  Grace  "}}]

                (with-redefs-fn {#'state/other-session-ids (constantly [sid "second-client"])
                                 #'state/append-event! (fn [& args]
                                                         (swap! events conj args))}
                  (fn []
                    (expect (= 200 (:status (#'settings-api/set-setting-handler request))))))
                (expect (= 2 (count @events)))
                (expect (every? #(= ["session.agent_name_updated" {:agent-name "Grace"}
                                     {:store? false}]
                                    (vec (rest %)))
                                @events)))
              (expect (= {"toggles" {"shell" true} "agent_name" "Grace"}
                         (config/load-global-config-raw)))
              (expect (= "Grace" (state/session-agent-name sid)))
              (expect (= "Grace" (config/agent-name (.getPath other))))
              (expect (= "Grace" (get (state/soul sid) "agent_name")))
              (expect (= "Grace" (get (state/session-workspace-info sid) "agent_name")))
              (let [out (java.io.ByteArrayOutputStream.)]
                (#'server/sse-ready! out sid 0 [] (state/soul sid))
                (expect (str/includes? (.toString out "UTF-8") "\"agent_name\":\"Grace\"")))
              (expect (str/starts-with? (prompt/build-system-prompt {:workspace-root (.getPath
                                                                                       project)})
                                        "You are Grace."))
              (expect (str/includes? (:body (#'settings-api/get-setting-handler
                                             {:path-params {:id "agent_name"}}))
                                     "\"value\":\"Grace\""))
              (expect (str/includes? (:body (#'settings-api/list-settings-handler {}))
                                     "\"id\":\"agent_name\""))
              (doseq [value [nil "" "   " "Ada\nOther" 42 (apply str (repeat 81 "a"))]]
                (expect (= 400
                           (:status (#'settings-api/set-setting-handler
                                     {:query-params
                                      {"id" "agent_name" "action" "value" "value" value}})))))
              (expect (= 400
                         (:status (#'settings-api/set-setting-handler
                                   {:query-params {"id" "agent_name" "action" "toggle"}}))))
              (expect (= "Grace" (state/session-agent-name sid)))
              (with-redefs [config/load-config-raw (constantly {"system_prompt"
                                                                {"text" "Custom identity"
                                                                 "is_replace" true}})]
                (expect (= "Custom identity"
                           (prompt/build-system-prompt {:workspace-root (.getPath project)})))))))
        (finally (doseq [file (reverse (file-seq dir))]
                   (.delete ^java.io.File file)))))))
