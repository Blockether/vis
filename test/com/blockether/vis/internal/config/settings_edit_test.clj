(ns com.blockether.vis.internal.config.settings-edit-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.toggle :as contract]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.settings-edit :as edit]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.server.settings :as api]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as store]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.sandbox.scoped-policy :as policy]
            [lazytest.core :refer [defdescribe describe it expect]]))

(h/use-mem-store!)

(defmacro with-empty-config
  [& body]
  `(with-redefs [config/load-global-yaml-config-raw
                 (constantly {})

                 config/load-global-config-raw
                 (constantly {})

                 config/load-project-tiers-raw
                 (constantly {})

                 config/load-project-root-config-raw
                 (constantly {})

                 config/load-project-config-raw
                 (constantly {})]

     ~@body))

(defn- scope-fixture
  []
  (let [db
        (h/store)

        project
        (store/db-create-project! db
                                  {:name "Settings"
                                   :workspace-root (.getCanonicalPath (java.io.File.
                                                                        "/tmp/settings-editor"))})

        group
        (store/db-create-session-group! db (:id project) {:name "Work"})

        sid
        (h/store-session! db {:title "Selected"})

        other
        (h/store-session! db {:title "Sibling"})

        id
        "settings_batch_flag"]

    (toggles/register-toggle! {:id id :label "Batch flag" :default true :scopes contract/scopes})
    (doseq [session [sid other]]
      (store/db-set-session-group! db session (:id group)))
    {:db db
     :sid sid
     :other other
     :id id
     :group (scoped/target db "group" (:id group))
     :target (scoped/target db "session" sid)}))

(defn- failure-data [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe
  settings-editor-contract
  (describe "typed access and catalog"
            (it "exposes switches and structured editors instead of JSON text for every field"
                (with-empty-config (let [rows (into {}
                                                    (map (juxt :id identity))
                                                    (policy/settings (h/store) {:scope "global"}))]
                                     (expect (= "boolean" (:type (get rows "jail_enabled"))))
                                     (expect (= "filesystem"
                                                (:editor (get rows "jail_filesystem"))))
                                     (expect (= "network" (:editor (get rows "jail_network"))))
                                     (expect (false? (:enabled (get rows "jail_keychain")))))))
            (it "returns a revision and the actual inherited value with the shared catalog"
                (with-empty-config
                  (with-redefs [lp/db-info h/store]
                    (let [response ((get api/handlers [:get "/v1/settings"]) {:query-params {}})
                          body (json/read-json (:body response))]

                      (expect (= 200 (:status response)))
                      (expect (boolean (re-matches #"[a-f0-9]{64}" (get body "revision"))))
                      (expect (contains? (first (get-in body ["groups" 0 "toggles"]))
                                         "inherited_value"))))))
            (it "persists booleans, arrays and objects as JSON values at only the selected owner"
                (with-empty-config
                  (let [{:keys [db target sid other id]}
                        (scope-fixture)

                        network
                        {"allowed_domains" ["gateway.example.com"]
                         "allow_private" false
                         "inbound_ports" [8080]}

                        result
                        (edit/apply! db
                                     target
                                     (edit/revision db target)
                                     [{"id" id "action" "value" "value" false}
                                      {"id" "jail_deny_exec" "action" "value" "value" ["visgw"]}
                                      {"id" "jail_network" "action" "value" "value" network}])

                        own
                        (store/db-scoped-settings db "session" sid)]

                    (expect (false? (get own id)))
                    (expect (= ["visgw"] (get own "config:jail:deny_exec")))
                    (expect (= network (get own "config:jail:network")))
                    (expect (= {} (store/db-scoped-settings db "session" other)))
                    (expect (= (:revision result) (edit/revision db target)))
                    (expect (true? (get (scoped/values db other) id)))))))
  (describe
    "atomic validation and rollback"
    (it "writes no key when a later toggle is invalid"
        (with-empty-config
          (let [{:keys [db target sid id]}
                (scope-fixture)

                before
                (edit/revision db target)

                failure
                (failure-data #(edit/apply!
                                 db
                                 target
                                 before
                                 [{"id" id "action" "value" "value" false}
                                  {"id" "missing_setting" "action" "value" "value" true}]))]

            (expect (= 400 (:status failure)))
            (expect (= "missing_setting" (:id failure)))
            (expect (= {} (store/db-scoped-settings db "session" sid)))
            (expect (= before (edit/revision db target))))))
    (it "validates the complete access candidate before a valid toggle can be written"
        (with-empty-config (let [{:keys [db target sid id]}
                                 (scope-fixture)

                                 failure
                                 (failure-data #(edit/apply!
                                                  db
                                                  target
                                                  (edit/revision db target)
                                                  [{"id" id "action" "value" "value" false}
                                                   {"id" "jail_network"
                                                    "action" "value"
                                                    "value" {"inbound_ports" [0]}}]))]

                             (expect (= 400 (:status failure)))
                             (expect (contains? (:field-errors failure) "jail_network"))
                             (expect (= {} (store/db-scoped-settings db "session" sid))))))
    (it "rejects malformed batches, duplicate ids and null without mutation"
        (with-empty-config
          (let [{:keys [db target sid id]}
                (scope-fixture)

                value
                {"id" id "action" "value" "value" false}

                revision
                (edit/revision db target)]

            (doseq [changes [[] [value value] [(dissoc value "value")] [(assoc value "value" nil)]
                             [(assoc value "action" "toggle")]
                             (mapv #(assoc value "id" (str "setting_" %)) (range 257))]]
              (expect (= 400 (:status (failure-data #(edit/apply! db target revision changes)))))
              (expect (= {} (store/db-scoped-settings db "session" sid)))))))
    (it "rolls back writes performed by a failed transactional callback"
        (with-empty-config
          (let [{:keys [db sid]} (scope-fixture)]
            (expect (= :test/rollback
                       (:type (failure-data
                                #(store/db-edit-scoped-settings!
                                   db
                                   "session"
                                   sid
                                   (fn [tx _raw]
                                     (store/db-set-scoped-setting! tx "session" sid "first" false)
                                     (throw (ex-info "Abort transaction"
                                                     {:type :test/rollback}))))))))
            (expect (= {} (store/db-scoped-settings db "session" sid)))
            (store/db-edit-scoped-settings! db
                                            "session"
                                            sid
                                            (fn [_ raw]
                                              (assoc raw "after" false)))
            (expect (= {"after" false} (store/db-scoped-settings db "session" sid))))))
    (it
      "enforces the host ceiling before writing any local setting"
      (with-empty-config
        (with-redefs [config/load-global-config-raw (constantly {"jail" {"enabled" true
                                                                         "keychain" false}})]
          (let [{:keys [db target sid id]} (scope-fixture)
                failure (failure-data #(edit/apply!
                                         db
                                         target
                                         (edit/revision db target)
                                         [{"id" id "action" "value" "value" false}
                                          {"id" "jail_keychain" "action" "value" "value" true}]))]

            (expect (= 400 (:status failure)))
            (expect (= :settings/host-policy (:type failure)))
            (expect (= {} (store/db-scoped-settings db "session" sid))))))))
  (describe
    "revisions and inheritance"
    (it "rejects an old revision and allows an explicit retry with the latest revision"
        (with-empty-config
          (let [{:keys [db target sid id]}
                (scope-fixture)

                before
                (edit/revision db target)

                first-result
                (edit/apply! db target before [{"id" id "action" "value" "value" false}])

                inherit
                [{"id" id "action" "inherit"}]]

            (expect (not= before (:revision first-result)))
            (expect (= 409 (:status (failure-data #(edit/apply! db target before inherit)))))
            (expect (false? (get (store/db-scoped-settings db "session" sid) id)))
            (edit/apply! db target (:revision first-result) inherit)
            (expect (= {} (store/db-scoped-settings db "session" sid)))
            (expect (true? (get (scoped/values db sid) id))))))
    (it "accepts exactly one of two simultaneous batches from the same revision"
        (with-empty-config
          (let [{:keys [db target sid id]}
                (scope-fixture)

                revision
                (edit/revision db target)

                start
                (promise)

                writes
                (mapv (fn [setting]
                        (future @start
                                (or (:status (failure-data #(edit/apply! db
                                                                         target
                                                                         revision
                                                                         [{"id" setting
                                                                           "action" "value"
                                                                           "value" false}])))
                                    200)))
                      [id "jail_keychain"])]

            (deliver start true)
            (expect (= [200 409] (sort (mapv deref writes))))
            (expect (= 1 (count (store/db-scoped-settings db "session" sid)))))))
    (it
      "detects ancestor changes and removes only the selected override"
      (with-empty-config
        (let [{:keys [db target group sid other id]}
              (scope-fixture)

              initial
              (edit/revision db target)]

          (edit/apply! db group (edit/revision db group) [{"id" id "action" "value" "value" false}])
          (expect
            (= 409
               (:status
                 (failure-data
                   #(edit/apply! db target initial [{"id" id "action" "value" "value" true}])))))
          (edit/apply! db
                       target
                       (edit/revision db target)
                       [{"id" id "action" "value" "value" true}])
          (expect (true? (get (scoped/values db sid) id)))
          (expect (false? (get (scoped/values db other) id)))
          (edit/apply! db target (edit/revision db target) [{"id" id "action" "inherit"}])
          (expect (false? (get (scoped/values db sid) id)))
          (expect (= {id false} (store/db-scoped-settings db "group" (:target-id group)))))))
    (it "retains authored machine YAML when the writable override inherits"
        (with-empty-config
          (let [{:keys [db id]}
                (scope-fixture)

                raw
                (atom {"toggles" {id true} "unrelated" {"kept" true}})

                target
                {:scope "global"}]

            (with-redefs [config/load-global-yaml-config-raw
                          (constantly {"toggles" {id false}})

                          config/load-global-config-raw
                          (fn []
                            @raw)

                          config/update-machine-config!
                          (fn [f]
                            (swap! raw f))]

              (let [row (first (filter #(= id (:id %)) (scoped/settings db target)))]
                (expect (true? (:own-value row)))
                (expect (false? (:inherited-value row)))
                (edit/apply! db target (edit/revision db target) [{"id" id "action" "inherit"}])
                (expect (= {"toggles" {} "unrelated" {"kept" true}} @raw))
                (expect (false? (:value (first (filter #(= id (:id %))
                                                       (scoped/settings db target)))))))))))))

(defdescribe
  settings-batch-api
  (it
    "applies one typed PATCH and rejects stale, invalid-context and invalid-envelope retries without writes"
    (with-empty-config
      (let [{:keys [db sid id]}
            (scope-fixture)

            get-handler
            (get api/handlers [:get "/v1/settings"])

            patch-handler
            (get api/handlers [:patch "/v1/settings"])

            request
            (fn [body]
              {:body (java.io.StringReader. (json/write-json-str body))})]

        (with-redefs [lp/db-info (constantly db)]
          (let [initial (json/read-json (:body (get-handler {:query-params {"scope" "session"
                                                                            "target_id" sid
                                                                            "channel" "tui"}})))
                batch {"scope" "session"
                       "target_id" sid
                       "channel" "tui"
                       "context_session_id" sid
                       "revision" (get initial "revision")
                       "changes" [{"id" id "action" "value" "value" false}]}
                saved (patch-handler (request batch))
                body (json/read-json (:body saved))]

            (expect (= 200 (:status saved)))
            (expect (false? (get (store/db-scoped-settings db "session" sid) id)))
            (expect (not= (get initial "revision") (get body "revision")))
            (expect (= 409 (:status (patch-handler (request batch)))))
            (let [fresh (assoc batch
                          "revision" (get body "revision")
                          "changes" [{"id" id "action" "inherit"}])]
              (expect (= 400 (:status (patch-handler (request (assoc fresh "unknown" true))))))
              (expect (= 404
                         (:status (patch-handler (request (assoc fresh
                                                            "context_session_id" "missing"))))))
              (expect (= {id false} (store/db-scoped-settings db "session" sid)))
              (expect (= 200 (:status (patch-handler (request fresh)))))
              (expect (= {} (store/db-scoped-settings db "session" sid)))))))))
  (it "keeps every prior value when a later PATCH field is invalid"
      (with-empty-config
        (let [{:keys [db target sid id]} (scope-fixture)]
          (with-redefs [lp/db-info (constantly db)]
            (let [response ((get api/handlers [:patch "/v1/settings"])
                             {:body (java.io.StringReader.
                                      (json/write-json-str
                                        {"scope" "session"
                                         "target_id" sid
                                         "revision" (edit/revision db target)
                                         "changes" [{"id" id "action" "value" "value" false}
                                                    {"id" "jail_network"
                                                     "action" "value"
                                                     "value" {"inbound_ports" [70000]}}]}))})]
              (expect (= 400 (:status response)))
              (expect (= {} (store/db-scoped-settings db "session" sid)))
              (expect (str/includes? (:body response) "field_errors"))))))))
