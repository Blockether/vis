(ns com.blockether.vis.internal.config.scoped-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [charred.api :as json]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.gateway.server.settings :as settings-api]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]
            [com.blockether.vis.internal.foundation.harness.core :as harness]
            [com.blockether.vis.internal.docs.corpus :as corpus]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [com.blockether.vis.contract.toggle :as contract]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.sandbox.scoped-policy :as policy]
            [com.blockether.vis.internal.foundation.mcp.core :as mcp]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.core :as store]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [lazytest.core :refer [defdescribe it expect]]))

(h/use-mem-store!)

(defmacro with-empty-config
  [& body]
  `(with-redefs [config/load-global-yaml-config-raw
                 (constantly {})

                 config/load-global-config-raw
                 (constantly {})

                 config/load-project-tiers-raw
                 (constantly {})

                 config/load-project-config-raw
                 (constantly {})]

     ~@body))

(defdescribe
  scope-resolution
  (it "resolves per key, preserving false and inheriting omitted values"
      (let [specs
            [{:id "a" :default true :scopes contract/scopes}
             {:id "b" :default "deep" :scopes contract/scopes}]

            layers
            [{:scope "global" :values {"a" true "b" "quick"}} {:scope "project" :values {"a" false}}
             {:scope "group" :values {"b" "balanced"}} {:scope "session" :values {"b" "deep"}}]

            rows
            (scoped/resolve-layers specs layers "session" {"b" "deep"})]

        (expect (= [false "deep"] (mapv :value rows)))
        (expect (= ["project" "session"] (mapv :source rows)))
        (expect (= [false true] (mapv :is-override rows)))
        (expect (= [false "balanced"]
                   (mapv :value (scoped/resolve-layers specs (butlast layers) "session" {}))))))
  (it "enforces every allowed-scope combination even for YAML declarations"
      (doseq [mask (range 1 16)]
        (let [allowed (vec (keep-indexed #(when (bit-test mask %1) %2) contract/scopes))
              layers (mapv #(hash-map :scope % :values {"a" %}) contract/scopes)
              row (first (scoped/resolve-layers [{:id "a" :default false :scopes allowed}]
                                                layers
                                                "session"
                                                {}))]

          (expect (= (last allowed) (:value row)))
          (expect (= (last allowed) (:source row))))))
  (it "requires a nonempty unique allowed-scope set"
      (doseq [scopes [[] ["session" "session"] ["device"]]]
        (expect (not (contract/contribution-valid?
                       {:id "scope_test" :label "Test" :default true :scopes scopes})))))
  (it "keeps undeclared extension settings global"
      (toggles/register-toggle! {:id "scope_test_default" :label "Test" :default true})
      (expect (= ["global"] (:scopes (toggles/toggle-spec "scope_test_default"))))))

(defdescribe
  durable-scopes
  (it
    "keeps false, deletes only the selected override, and observes ancestor edits"
    (with-empty-config
      (let [db
            (h/store)

            project
            (store/db-create-project! db
                                      {:name "Scoped"
                                       :workspace-root (.getCanonicalPath
                                                         (java.io.File. "/tmp/scoped-project"))})

            group
            (store/db-create-session-group! db (:id project) {:name "Work"})

            sid
            (h/store-session! db {:title "A"})

            other
            (h/store-session! db {:title "B"})

            id
            "scope_isolation"]

        (toggles/register-toggle! {:id id :label "Isolation" :default true :scopes contract/scopes})
        (doseq [session [sid other]]
          (store/db-set-session-group! db session (:id group)))
        (let [gt
              (scoped/target db "group" (:id group))

              st
              (scoped/target db "session" sid)]

          (scoped/set-setting! db gt id "value" false)
          (expect (false? (get (scoped/values db sid) id)))
          (expect (false? (get (scoped/values db other) id)))
          (scoped/set-setting! db st id "value" true)
          (expect (true? (get (scoped/values db sid) id)))
          (expect (false? (get (scoped/values db other) id)))
          (expect (= {id false} (store/db-scoped-settings db "group" (:id group))))
          (scoped/set-setting! db st id "inherit" nil)
          (expect (= {} (store/db-scoped-settings db "session" sid)))
          (expect (false? (get (scoped/values db sid) id)))
          (expect (= (str (:id project))
                     (:target-id (scoped/target db "project" "/tmp/scoped-project"))))))))
  (it "keeps own overrides through moves and never copies them into forks"
      (with-empty-config
        (let [db
              (h/store)

              project
              (store/db-create-project! db {:name "Move"})

              a
              (store/db-create-session-group! db (:id project) {:name "A"})

              b
              (store/db-create-session-group! db (:id project) {:name "B"})

              sid
              (h/store-session! db {:title "Source"})

              id
              "scope_move"]

          (toggles/register-toggle! {:id id :label "Move" :default true :scopes contract/scopes})
          (store/db-set-session-group! db sid (:id a))
          (scoped/set-setting! db (scoped/target db "session" sid) id "value" false)
          (let [fork (h/fork-session! db sid {:title "Fork"})]
            (expect (= {} (store/db-scoped-settings db "session" fork))))
          (store/db-set-session-group! db sid (:id b))
          (expect (false? (get (scoped/values db sid) id)))
          (scoped/set-setting! db (scoped/target db "session" sid) id "inherit" nil)
          (expect (true? (get (scoped/values db sid) id))))))
  (it "rejects unsupported scopes before writing and retains different concurrent keys"
      (with-empty-config
        (let [db
              (h/store)

              sid
              (h/store-session! db {:title "Concurrent"})

              target
              (scoped/target db "session" sid)]

          (expect (= 400
                     (try (scoped/set-setting! db target "scope_test_default" "value" false)
                          nil
                          (catch clojure.lang.ExceptionInfo e (:status (ex-data e))))))
          (let [writes
                (mapv #(future (store/db-set-scoped-setting! db "session" sid (str "key_" %) false))
                      (range 12))]
            (run! deref writes)
            (expect (= 12 (count (store/db-scoped-settings db "session" sid)))))))))

(defdescribe
  scoped-resources
  (it
    "persists MCP definitions, isolates namesakes, and gates already-known names live"
    (with-empty-config
      (let [db
            (h/store)

            sid
            (h/store-session! db {:title "MCP"})

            other
            (h/store-session! db {:title "Other"})

            target
            (scoped/target db "session" sid)

            env
            {:db-info db :session-id sid}]

        (mcp/save-scoped-server! db target "local" {"transport" "stdio" "command" "echo"})
        (expect (= "echo"
                   (get-in (first (scoped/definitions db target ["mcp" "servers"]))
                           [:value "command"])))
        (expect (empty?
                  (scoped/definitions db (scoped/target db "session" other) ["mcp" "servers"])))
        (expect (scoped/resource-enabled? env :mcp "local"))
        (mcp/set-scoped-server-enabled! db target "local" false)
        (expect (false? (scoped/resource-enabled? env :mcp "local")))
        (expect (scoped/resource-enabled? {:db-info db :session-id other} :mcp "local"))
        (expect (= :mcp/invalid-server
                   (try (mcp/save-scoped-server! db
                                                 target
                                                 "secret"
                                                 {"url" "https://gateway.example.com/mcp"
                                                  "headers" {"Authorization" "test"}})
                        nil
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
        (mcp/delete-scoped-server! db target "local")
        (expect (empty? (scoped/definitions db target ["mcp" "servers"]))))))
  (it "denies a saved raw extension handle after the engine is switched off"
      (with-empty-config
        (let [db
              (h/store)

              sid
              (h/store-session! db {:title "Engine"})

              ext
              {:ext/name "scoped-helper"
               :ext/engine {:ext.engine/symbols [{:ext.symbol/symbol 'answer
                                                  :ext.symbol/raw? true
                                                  :ext.symbol/fn (constantly 42)}]}}

              target
              (scoped/target db "session" sid)

              handle
              (get (extension/wrap-extension-thunked ext (constantly {:db-info db :session-id sid}))
                   'answer)

              id
              (scoped/engine-setting! ext)]

          (expect (= 42 (handle)))
          (scoped/set-setting! db target id "value" "off")
          (expect
            (= :extension/disabled
               (try (handle) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))))

(defdescribe access-ceiling
             (it "rejects disabling confinement or removing a host network deny"
                 (let [host {"jail" {"enabled" true
                                     "network" {"denied_domains" ["gateway.example.com"]}}}]
                   (doseq [candidate [(assoc-in host ["jail" "enabled"] false)
                                      (assoc-in host ["jail" "network"] {})]]
                     (expect (= :settings/host-policy
                                (try (policy/assert-bounded! host candidate "/tmp")
                                     nil
                                     (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
                   (expect (:jail-enabled (policy/assert-bounded! host host "/tmp"))))))

(defdescribe
  scoped-storage-boundary
  (it
    "persists explicit global access without importing project grants on other writes"
    (let [dir
          (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-global-access"
                     (make-array java.nio.file.attribute.FileAttribute 0)))

          path
          (str (io/file dir "state.yml"))

          filesystem
          [{"id" "global-access" "path" (.getCanonicalPath dir)}]]

      (try
        (with-redefs [config/config-dir
                      (constantly (.getCanonicalPath dir))

                      config/state-path
                      (constantly path)

                      config/load-global-yaml-config-raw
                      (constantly {})]

          (let [db
                (h/store)

                target
                (scoped/target db "global" nil)]

            (policy/set-setting! db target "workspace_filesystem" "value" filesystem)
            (policy/set-setting! db target "jail_enabled" "value" false)
            (config/invalidate-config-cache!)
            (expect (= filesystem
                       (get-in (config/load-global-config-raw) ["workspace" "filesystem"])))
            (expect (false? (get-in (config/load-global-config-raw) ["jail" "enabled"])))
            ;; Whole-store callers may still hold a merged project configuration.
            (config/save-config! {"providers" [{"id" "prov-a" "api_key" "key-a"}]
                                  "workspace" {"filesystem" [{"id" "project-access"
                                                              "path" "/tmp/project-access"}]}
                                  "jail" {"enabled" true}
                                  "environment" {"FROM_PROJECT" "yes"}})
            (config/update-machine-config! #(assoc-in % ["toggles" "plans"] false))
            (expect (= filesystem
                       (get-in (config/load-global-config-raw) ["workspace" "filesystem"])))
            (expect (false? (get-in (config/load-global-config-raw) ["jail" "enabled"])))
            (expect (nil? (get (config/load-global-config-raw) "environment")))
            (expect (not (str/includes? (slurp path) "project-access")))
            (policy/set-setting! db target "workspace_filesystem" "inherit" nil)
            (policy/set-setting! db target "jail_enabled" "inherit" nil)
            (config/invalidate-config-cache!)
            (expect (nil? (get-in (config/load-global-config-raw) ["workspace" "filesystem"])))
            (expect (nil? (get-in (config/load-global-config-raw) ["jail" "enabled"])))
            (expect (= "prov-a" (get-in (config/load-global-config-raw) ["providers" 0 "id"])))))
        (finally (config/invalidate-config-cache!)
                 (doseq [file (reverse (file-seq dir))]
                   (io/delete-file file true))))))
  (it
    "creates the local project overlay atomically without modifying authored YAML"
    (let [dir
          (.toFile (java.nio.file.Files/createTempDirectory
                     "vis-scoped-project"
                     (make-array java.nio.file.attribute.FileAttribute 0)))

          root
          (.getCanonicalPath dir)

          authored
          (io/file dir "vis.yml")]

      (try (spit authored "toggles:
  plans: true
")
           (binding [workspace/*workspace-root* root]
             (with-redefs [config/load-global-yaml-config-raw (constantly {})
                           config/load-global-config-raw (constantly {})]

               (let [db (h/store)
                     project (store/db-create-project! db {:name "Disk" :workspace-root root})
                     target (scoped/target db "project" (:id project))]

                 (scoped/set-setting! db target "plans" "value" false)
                 (expect (false? (get-in (config/load-project-config-raw) ["toggles" "plans"])))
                 (let [writes (mapv (fn [n]
                                      (future
                                        (config/update-project-config!
                                          #(assoc-in % ["toggles" (str "scoped_key_" n)] false))))
                                    (range 8))]
                   (run! deref writes)
                   (expect (= 9 (count (get (config/load-project-config-raw) "toggles")))))
                 (scoped/set-setting! db target "plans" "inherit" nil)
                 (expect (true? (:value (first (filter #(= "plans" (:id %))
                                                       (scoped/settings db target)))))))))
           (expect (= "toggles:
  plans: true
" (slurp authored)))
           (finally (doseq [file (reverse (file-seq dir))]
                      (io/delete-file file true))))))
  (it "retains sparse false values after reopening the database and cascades owner deletion"
      (let [file
            (.toFile (java.nio.file.Files/createTempDirectory
                       "vis-scoped-store"
                       (make-array java.nio.file.attribute.FileAttribute 0)))

            db
            (store/db-create-connection! (.getPath file))

            sid
            (h/store-session! db {:title "Persist"})]

        (try (store/db-set-scoped-setting! db "session" sid "plans" false)
             (store/db-dispose-connection! db)
             (let [reopened (store/db-create-connection! (.getPath file))]
               (try (expect (= {"plans" false} (store/db-scoped-settings reopened "session" sid)))
                    (store/db-delete-session-tree! reopened sid)
                    (expect (empty? (store/db-scoped-settings reopened "session" sid)))
                    (finally (store/db-dispose-connection! reopened))))
             (finally (store/db-dispose-connection! db)
                      (doseq [f (reverse (file-seq file))]
                        (io/delete-file f true)))))))

(defdescribe
  scoped-http-boundary
  (it
    "round-trips false, provenance and inheritance using the portable wire schema"
    (with-empty-config
      (let [db
            (h/store)

            sid
            (h/store-session! db {:title "HTTP"})

            target
            {"scope" "session" "target_id" (str sid)}

            id
            "http_scoped_feature"

            write!
            (fn [body]
              (#'settings-api/set-setting-handler
               {:body (java.io.StringReader. (json/write-json-str body))}))]

        (toggles/register-toggle! {:id id :label "Feature" :default true :scopes contract/scopes})
        (with-redefs [lp/db-info
                      (constantly db)

                      discovery/all-skills
                      (constantly [])]

          (let [response
                (write! (merge target {"id" id "action" "value" "value" false}))

                row
                (json/read-json (:body response))]

            (expect (= 200 (:status response)))
            (expect (document/valid? "gateway" "setting" row))
            (expect (false? (get row "enabled")))
            (expect (= "session" (get row "source")))
            (expect (true? (get row "is_override"))))
          (let [response
                (write! (merge target {"id" id "action" "inherit"}))

                row
                (json/read-json (:body response))]

            (expect (= 200 (:status response)))
            (expect (document/valid? "gateway" "setting" row))
            (expect (true? (get row "enabled")))
            (expect (= "default" (get row "source")))
            (expect (false? (get row "is_override"))))
          (expect (= 400 (:status (write! {"id" id "scope" "unknown"}))))
          (expect (= 400 (:status (write! {"id" id "scope" "session"}))))
          (expect (= 404 (:status (write! (assoc target "id" "missing_setting")))))
          (expect (= 400
                     (:status (write! (assoc target
                                        "id" "agent_name"
                                        "value" "Local")))))
          (toggles/register-toggle! {:id "http_global_feature" :label "Global" :default true})
          (expect (= 400 (:status (write! (assoc target "id" "http_global_feature")))))))))
  (it
    "marks rows a more specific scope decides for the context session and nothing else"
    (with-empty-config
      (let [db
            (h/store)

            project
            (store/db-create-project! db
                                      {:name "Context"
                                       :workspace-root (.getCanonicalPath
                                                         (java.io.File. "/tmp/context-project"))})

            sid
            (h/store-session! db {:title "In project"})

            loose
            (h/store-session! db {:title "Outside the project"})

            id
            "context_feature"

            row
            (fn [response]
              (let [body (json/read-json (:body response))]
                (or (some #(when (= id (get % "id")) %)
                          (mapcat #(get % "toggles") (get body "groups")))
                    body)))

            list!
            (fn [params]
              (row (#'settings-api/list-settings-handler {:query-params params})))]

        (toggles/register-toggle! {:id id :label "Context" :default true :scopes contract/scopes})
        (store/db-set-session-project! db sid (:id project))
        (with-redefs [lp/db-info
                      (constantly db)

                      discovery/all-skills
                      (constantly [])

                      config/load-project-tiers-raw
                      (constantly {"toggles" {id false}})]

          (let [locked (list! {"context_session_id" (str sid)})]
            (expect (document/valid? "gateway" "setting" locked))
            (expect (true? (get locked "enabled")))
            (expect (= {"scope" "project" "enabled" false} (get locked "overridden_by"))))
          (expect (= {"scope" "project" "enabled" false}
                     (get (row (#'settings-api/get-setting-handler
                                {:path-params {:id id}
                                 :query-params {"context_session_id" (str sid)}}))
                          "overridden_by")))
          (doseq [params [{} {"context_session_id" "missing-session"} {"context_session_id" " "}]]
            (expect (not (contains? (list! params) "overridden_by"))))
          (scoped/set-setting! db (scoped/target db "session" sid) id "value" true)
          (let [project-params {"scope" "project" "target_id" (str (:id project))}]
            (expect (= {"scope" "session" "enabled" true}
                       (get (list! (assoc project-params "context_session_id" (str sid)))
                            "overridden_by")))
            (expect (not (contains? (list! (assoc project-params "context_session_id" (str loose)))
                                    "overridden_by")))))))))

(defdescribe
  scoped-skill-visibility
  (it "removes disabled skills from prompt, docs and slash menus and denies saved expansions"
      (with-empty-config
        (let [db
              (h/store)

              sid
              (h/store-session! db {:title "Skills"})

              env
              {:db-info db :session-id sid}

              skill
              {:name "scoped_fixture" :description "Scoped skill" :body "Fixture body" :dir "/tmp"}

              id
              (scoped/register-resource! :skills (:name skill))]

          (with-redefs [discovery/all-skills (constantly [skill])]
            (binding [extension/*current-environment* env]
              (let [expand (:expand-fn (first (#'harness/skill-template-entries)))]
                (expect (some #(= "scoped_fixture" (:name %)) (corpus/entries)))
                (scoped/set-setting! db (scoped/target db "session" sid) id "value" false)
                (expect (empty? (discovery/skills)))
                (expect (empty? (#'harness/skill-template-entries)))
                (expect (not (some #(= "scoped_fixture" (:name %)) (corpus/entries))))
                (expect (not (re-find #"scoped_fixture" (str (#'harness/skills-prompt env)))))
                (expect (= :skill/unavailable
                           (try (expand env "task")
                                nil
                                (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
                (scoped/set-setting! db (scoped/target db "session" sid) id "inherit" nil)
                (expect (= [skill] (discovery/skills))))))))))

(defdescribe
  scoped-live-values
  (it "reads a resource registered after a batch snapshot from the store"
      (with-empty-config
        (let [db
              (h/store)

              sid
              (h/store-session! db {:title "Live"})

              env
              {:db-info db :session-id sid}

              live
              (scoped/live-values env)

              id
              (scoped/register-resource! :skills "late_fixture")]

          (scoped/set-setting! db (scoped/target db "session" sid) id "value" false)
          (expect (not (contains? live id)))
          (expect (false? (scoped/resource-enabled? env :skills "late_fixture" live)))
          (expect
            (false?
              (scoped/resource-enabled? env :skills "late_fixture" (scoped/live-values env)))))))
  (it "resolves no session settings unless a checked extension owns an engine setting"
      (with-empty-config
        (let [db
              (h/store)

              ;; A hook environment can name a session the store does not hold.
              env
              {:db-info db :session-id "missing-session"}

              live
              (delay (scoped/live-values env))]

          (expect (= "auto" (scoped/engine-mode env {:ext/name "infrastructure_fixture"} live)))
          (expect (not (realized? live))))))
  (it "titles groups in plain words and hides unloaded engines and scoped MCP rows"
      (with-empty-config
        (let [db
              (h/store)

              sid
              (h/store-session! db {:title "Titles"})

              target
              (scoped/target db "session" sid)]

          (scoped/engine-setting! {:ext/name "title-fixture"
                                   :ext/engine {:ext.engine/symbols ['fixture]}})
          (scoped/set-definition! db target ["mcp" "servers"] "title_fixture" {"command" "true"})
          (with-redefs [lp/db-info
                        (constantly db)

                        discovery/all-skills
                        (constantly [])]

            (let [groups
                  (-> (#'settings-api/list-settings-handler
                       {:query-params {"scope" "session" "target_id" (str sid)}})
                      :body
                      json/read-json
                      (get "groups"))

                  titles
                  (into {} (map (juxt #(get % "id") #(get % "title"))) groups)]

              (expect (not (contains? titles "engines")))
              (expect (= "Response" (get titles "provider")))
              (expect (not (contains? titles "mcp"))))))))
  (it
    "puts an extension's engine choice, settings and packaged skills in its own section"
    (with-empty-config
      (let [db
            (h/store)

            ext
            {:ext/name "section-fixture"
             :ext/engine {:ext.engine/symbols ['fixture]}
             :ext/toggles [{:id "section_fixture_flag"}]
             :ext/skills [{:name "section-fixture/guide"}]}]

        (toggles/register-toggle! {:id "section_fixture_flag"
                                   :label "Flag"
                                   :default true
                                   :owner "section-fixture"
                                   :scopes contract/scopes})
        (try (with-redefs [lp/db-info
                           (constantly db)

                           discovery/all-skills
                           (constantly [{:name "section-fixture/guide"}])

                           extension/registered-extensions
                           (constantly [ext])]

               (let [engine
                     (scoped/engine-setting! ext)

                     groups
                     (-> (#'settings-api/list-settings-handler {:query-params {}})
                         :body
                         json/read-json
                         (get "groups"))

                     section
                     (last groups)]

                 (expect (= "extension:section-fixture" (get section "id")))
                 (expect (= "section-fixture" (get section "title")))
                 (expect (= [engine "section_fixture_flag"
                             (scoped/resource-id :skills "section-fixture/guide")]
                            (mapv #(get % "id") (get section "toggles"))))
                 (expect (not-any? #(#{"engines" "skills"} (get % "id")) groups))))
             (finally (toggles/unregister-owner! "section-fixture"))))))
  (it
    "lists a project extension's settings only in that project"
    ;; #302: project extension settings never reached a settings catalog.
    (with-empty-config
      (let [db
            (h/store)

            root
            (workspace/normalize-root "target/project-settings-catalog")

            project
            (store/db-create-project! db {:name "Project settings" :workspace-root root})

            sid
            (h/store-session! db {:title "In project"})

            rows
            (fn [params]
              (into {}
                    (for [group
                          (-> (#'settings-api/list-settings-handler {:query-params params})
                              :body
                              json/read-json
                              (get "groups"))

                          row
                          (get group "toggles")]

                      [(get row "id") (assoc row "group" (get group "id"))])))]

        (store/db-set-session-project! db sid (:id project))
        (try (extension/set-project-extensions! root
                                                [{:ext/name "project-settings-fixture"
                                                  :ext/description "Project settings fixture"
                                                  :ext/toggles [{:id "project_settings_flag"
                                                                 :label "Flag"
                                                                 :default false
                                                                 :scopes ["global" "project"
                                                                          "session"]}]}])
             (with-redefs [lp/db-info
                           (constantly db)

                           discovery/all-skills
                           (constantly [])]

               (let [row (get (rows {"scope" "project" "target_id" (str (:id project))})
                              "project_settings_flag")]
                 (expect (= "extension:project-settings-fixture" (get row "group")))
                 (expect (= ["project" "session"] (get row "scopes"))))
               (expect (not (contains? (rows {}) "project_settings_flag")))
               (scoped/set-setting! db
                                    (scoped/target db "session" sid)
                                    "project_settings_flag"
                                    "toggle"
                                    nil)
               (expect (true? (get-in (rows {"scope" "session" "target_id" (str sid)})
                                      ["project_settings_flag" "enabled"])))
               (expect (= 404
                          (try (scoped/set-setting! db
                                                    (scoped/target db "global" nil)
                                                    "project_settings_flag"
                                                    "toggle"
                                                    nil)
                               nil
                               (catch clojure.lang.ExceptionInfo e (:status (ex-data e)))))))
             (finally (extension/set-project-extensions! root []))))))
  (it "leaves global MCP availability to the MCP servers section"
      (with-empty-config
        (let [db (h/store)]
          (with-redefs [config/load-global-config-raw
                        (constantly {"mcp" {"servers" {"global_fixture" {"command" "true"}}}})
                        lp/db-info (constantly db)
                        discovery/all-skills (constantly [])]

            (let [groups (-> (#'settings-api/list-settings-handler {:query-params {}})
                             :body
                             json/read-json
                             (get "groups"))]
              (expect (some #(= "access" (get % "id")) groups))
              (expect (not-any? #(= "mcp" (get % "id")) groups))))))))
