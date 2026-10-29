(ns com.blockether.vis.internal.config.toggles-test
  "Feature-toggle registry contract: register, lookup, override,
   listener fan-out, persistence snapshot + hydrate, and the canonical
   internal toggles. Toggle ids are plain strings (no namespaces)."
  (:require [com.blockether.vis.contract.toggle :as toggle-contract]
            [com.blockether.vis.internal.config.toggles :as t]
            [com.blockether.vis.internal.config.validation :as validation]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.telemere :as tel]))

(defn- with-clean-state
  [f]
  (let [snapshot (t/snapshot)]
    (try (t/clear-state!)
         (f)
         (finally
           ;; Restore via `set-value!` so enum toggles round-trip too;
           ;; `set-enabled!` would refuse them.
           (doseq [[id v] snapshot]
             (try (t/set-value! id v) (catch Throwable _ nil)))))))

(defdescribe registry-contract-test
             (it "register-toggle! normalizes the spec and exposes it via registered-toggles"
                 (with-clean-state
                   (fn []
                     (t/register-toggle! {:id "test_alpha" :label "Alpha" :default true})
                     (let [spec (t/toggle-spec "test_alpha")]
                       (expect (= "test_alpha" (:id spec)))
                       (expect (= "Alpha" (:label spec)))
                       (expect (true? (:default spec)))
                       (expect (false? (:persist? spec)))
                       (expect (= :vis (:owner spec)))
                       (expect (contains? (set (map :id (t/registered-toggles))) "test_alpha"))))))
             (it "re-registering the same id is idempotent and preserves the live value"
                 (with-clean-state
                   (fn []
                     (t/register-toggle! {:id "test_beta" :label "Beta" :default false})
                     (t/set-enabled! "test_beta" true)
                     (t/register-toggle!
                       {:id "test_beta" :label "Beta v2" :default false :description "updated"})
                     (expect (= "Beta v2" (:label (t/toggle-spec "test_beta"))))
                     (expect (= "updated" (:description (t/toggle-spec "test_beta"))))
                     ;; Live override survived the re-register.
                     (expect (true? (t/enabled? "test_beta"))))))
             (it "keeps the parent of a nested setting"
                 (with-clean-state
                   (fn []
                     (t/register-toggle! {:id "test_parent" :label "Parent" :default true})
                     (t/register-toggle!
                       {:id "test_child" :label "Child" :default true :parent "test_parent"})
                     (expect (= "test_parent" (:parent (t/toggle-spec "test_child"))))
                     (expect (nil? (:parent (t/toggle-spec "test_parent")))))))
             (it "rejects invalid contributions with :vis.toggles/invalid-contribution"
                 (let [thrown? (try (t/register-toggle! {:label "no-id"})
                                    false
                                    (catch clojure.lang.ExceptionInfo e
                                      (= :vis.toggles/invalid-contribution (:type (ex-data e)))))]
                   (expect thrown?)))
             (it "rejects every non-string or non-snake_case id shape"
                 (doseq [id [:ns/kw "ns/name" "kebab-case" "UPPER_CASE" "_leading" "trailing_"
                             "double__underscore" "" " "]]
                   (let [thrown? (try (t/register-toggle! {:id id :label "bad" :default false})
                                      false
                                      (catch clojure.lang.ExceptionInfo e
                                        (= :vis.toggles/invalid-contribution (:type (ex-data e)))))]
                     (expect thrown?)))))

(defdescribe value-resolution-test
             (it "unknown ids resolve to false (fail-closed)"
                 (expect (false? (t/enabled? "test_never_registered")))))

(defdescribe listener-test
             (it "listener fires on value transitions and a disposer detaches it"
                 (with-clean-state
                   (fn []
                     (t/register-toggle! {:id "test_epsilon" :label "Epsilon" :default false})
                     (let [events
                           (atom [])

                           dispose
                           (t/add-listener! #(swap! events conj %))]

                       (t/set-enabled! "test_epsilon" true)
                       (t/set-enabled! "test_epsilon" true) ;; no-op transition
                       (t/set-enabled! "test_epsilon" false)
                       (dispose)
                       (t/set-enabled! "test_epsilon" true) ;; listener detached
                       (expect (= 2 (count @events)))
                       (expect (= [{:id "test_epsilon" :old false :new true}
                                   {:id "test_epsilon" :old true :new false}]
                                  (mapv #(select-keys % [:id :old :new]) @events))))))))

;; Regression, issue #106: `clear-state!` wiped LISTENERS as well as overrides,
;; so this test helper silently detached the engine's toggle -> tool-binding
;; fan-out for the rest of the JVM (and, in a suite that shares one process,
;; for every namespace that ran after it).
(defdescribe clear-state-keeps-listeners-test
             (it "drops overrides but leaves listeners attached"
                 (with-clean-state (fn []
                                     (t/register-toggle!
                                       {:id "test_zeta" :label "Zeta" :default false})
                                     (let [events
                                           (atom [])

                                           dispose
                                           (t/add-listener! #(swap! events conj %))]

                                       (try (t/set-enabled! "test_zeta" true)
                                            (t/clear-state!)
                                            (expect (false? (t/enabled? "test_zeta")))
                                            (t/set-enabled! "test_zeta" true)
                                            (expect (= 2 (count @events)))
                                            (finally (dispose))))))))

(defdescribe
  persistence-test
  (it "snapshot omits non-persistent toggles and includes effective values for persistent ones"
      (with-clean-state
        (fn []
          (t/register-toggle! {:id "test_persist" :label "P" :default false :persist? true})
          (t/register-toggle! {:id "test_transient" :label "T" :default true :persist? false})
          (t/set-enabled! "test_persist" true)
          (let [snap (t/snapshot)]
            (expect (contains? snap "test_persist"))
            (expect (true? (get snap "test_persist")))
            (expect (not (contains? snap "test_transient")))))))
  (it "snapshot is SORTED so the serialised block is stable, not a hash jumble"
      (with-clean-state
        (fn []
          (t/register-toggle! {:id "test_zulu" :label "Z" :default true :persist? true})
          (t/register-toggle! {:id "test_alpha" :label "A" :default true :persist? true})
          (t/register-toggle! {:id "test_mike" :label "M" :default true :persist? true})
          (let [ks (keys (t/snapshot))]
            (expect (= (vec ks) (vec (sort ks))))))))
  (it "has-orphan-keys? flags stale ids no longer registered (e.g. legacy `enabled`)"
      (with-clean-state (fn []
                          (t/register-toggle!
                            {:id "test_live" :label "L" :default true :persist? true})
                          (expect (true? (t/has-orphan-keys? {"test_live" true "enabled" true})))
                          (expect (false? (t/has-orphan-keys? {"test_live" true})))
                          (expect (false? (t/has-orphan-keys? nil))))))
  (it "hydrate-from-config! applies persisted values; orphaned ids are skipped"
      (with-clean-state
        (fn []
          (t/register-toggle! {:id "test_zeta" :label "Zeta" :default false :persist? true})
          (t/hydrate-from-config! {"toggles" {"test_zeta" true "test_orphan_unknown" true}})
          (expect (true? (t/enabled? "test_zeta")))
          (expect (false? (t/enabled? "test_orphan_unknown")))))))

(defdescribe
  config-hydration-test
  "Feature toggles declared in hand-written `vis.yml` apply exactly like a UI
   flip: `coerce-config-value` maps the YAML string/boolean onto each toggle's
   registered type, and `hydrate-from-config!` routes through `set-value!`."
  (it "coerce-config-value maps YAML strings onto boolean and enum toggles"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_b" :label "B" :default false})
                          (t/register-toggle! {:id "test_e"
                                               :label "E"
                                               :type :enum
                                               :choices [:quick :balanced :deep]
                                               :default :balanced})
                          (expect (true? (t/coerce-config-value "test_b" "on")))
                          (expect (false? (t/coerce-config-value "test_b" "off")))
                          (expect (true? (t/coerce-config-value "test_b" true)))
                          (expect (= "deep" (t/coerce-config-value "test_e" "DEEP")))
                          (expect (= "quick" (t/coerce-config-value "test_e" :quick)))
                          (expect (= "x" (t/coerce-config-value "test_unknown_id" "x"))))))
  (it "hydrate-from-config! applies YAML string toggles (the vis.yml path)"
      (with-clean-state
        (fn []
          (t/register-toggle! {:id "test_flag" :label "Flag" :default false :persist? true})
          (t/register-toggle! {:id "test_mode"
                               :label "Mode"
                               :type :enum
                               :choices [:quick :balanced :deep]
                               :default :balanced
                               :persist? true})
          (t/hydrate-from-config! {"toggles" {"test_flag" "true" "test_mode" "deep"}})
          (expect (true? (t/enabled? "test_flag")))
          (expect (= "deep" (t/value-of "test_mode")))
          ;; the same canonical string id updates the same toggle
          (t/hydrate-from-config! {"toggles" {"test_mode" "quick"}})
          (expect (= "quick" (t/value-of "test_mode")))
          ;; keyword and namespaced aliases are ignored, never coerced
          (t/hydrate-from-config! {"toggles" {:test_mode "deep" "ns/test_mode" "deep"}})
          (expect (= "quick" (t/value-of "test_mode")))
          ;; an out-of-set enum string is dropped, leaving the prior value
          (t/hydrate-from-config! {"toggles" {"test_mode" "nonsense"}})
          (expect (= "quick" (t/value-of "test_mode")))))))

(defdescribe host-defaults-test
             ;; Regression, td-410a36: a Companion-only wording change replaced the shared
             ;; reasoning choice with verbosity's low value, contaminating later TUI tests.
             (it "keeps reasoning choices and listener events independent from verbosity"
                 (let [reasoning
                       (t/toggle-spec "reasoning_level")

                       verbosity
                       (t/toggle-spec "verbosity")

                       events
                       (atom [])

                       dispose
                       (t/add-listener! #(swap! events conj %))]

                   (try (expect
                          (= {:type :enum :choices ["quick" "balanced" "deep"] :default "balanced"}
                             (select-keys reasoning [:type :choices :default])))
                        (expect (= {:type :enum :choices ["low" "medium" "high"] :default "low"}
                                   (select-keys verbosity [:type :choices :default])))
                        (t/reset-to-default! "reasoning_level")
                        (t/reset-to-default! "verbosity")
                        (t/cycle-value! "verbosity")
                        (expect (= "balanced" (t/value-of "reasoning_level")))
                        (expect (= ["verbosity"] (mapv :id @events)))
                        (finally (dispose)
                                 (t/reset-to-default! "reasoning_level")
                                 (t/reset-to-default! "verbosity"))))))

(defdescribe
  settings-description-test
  (it "a settings row is never a paragraph: over-long copy is refused"
      ;; A settings row is a control plus ONE line of help; a paragraph of
      ;; rationale buried the toggle it described in the TUI and the app.
      (let [thrown? (try (t/register-toggle!
                           {:id "test_wordy"
                            :label "Wordy"
                            :default false
                            :description
                            (apply str (repeat (inc toggle-contract/max-description-length) "x"))})
                         false
                         (catch clojure.lang.ExceptionInfo e
                           (= :vis.toggles/invalid-contribution (:type (ex-data e)))))]
        (expect (true? thrown?))
        (expect (nil? (t/toggle-spec "test_wordy")))))
  (it "every registered toggle's description fits a settings row"
      (doseq [{:keys [description]}
              (t/registered-toggles)

              :when description]

        (expect (toggle-contract/settings-description? description)))))

(defdescribe
  provider-fallback-toggle-registry-test
  "`provider_fallback` decides whether a failed turn may be answered by a model the human
   did not pick. It ships ON — today's rescue — and every route off the pick reads it."
  (it "shows up in the Settings list of BOTH channels from that one declaration"
      ;; No `:channels` set and no `:settings? false`, so the TUI dialog and the
      ;; companion sheet (`/v1/settings`) each render it without their own wiring.
      (let [ids (fn [specs]
                  (set (map :id specs)))]
        (expect (contains? (ids (t/toggles-for-channel :tui)) "provider_fallback"))
        (expect (contains? (ids (t/toggles-for-channel :web)) "provider_fallback")))))

(defdescribe
  refusal-fallback-toggle-registry-test
  "`refusal_fallback` decides whether a request Anthropic's safety classifier declined may be
   re-asked of a sibling model of the SAME provider. It is separate from `provider_fallback`
   because nothing about the credential, the provider or the wire failed."
  (it "is a DIFFERENT switch from provider fallback, not an alias of it"
      ;; One human wants no peer credentials; another wants no model they did not pick.
      ;; Collapsing the two would make the cheaper answer decide the safer one.
      (t/set-value! "provider_fallback" false)
      (try (expect (true? (t/enabled? "refusal_fallback")))
           (finally (t/reset-to-default! "provider_fallback")))
      (t/set-value! "refusal_fallback" false)
      (try (expect (true? (t/enabled? "provider_fallback")))
           (finally (t/reset-to-default! "refusal_fallback")))))

;; Regression: `POST /v1/settings {"value" "false"}` turned a boolean setting ON,
;; because a wire string was cast by truthiness — the human asked for OFF and the
;; gateway answered 200 with the opposite stored.
(defdescribe
  wire-value-test
  (it "reads every boolean spelling a client may send"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_wire_bool" :label "Wire" :default true})
                          (expect (= {:value true} (t/wire-value "test_wire_bool" true)))
                          (expect (= {:value false} (t/wire-value "test_wire_bool" false)))
                          (doseq [token ["true" "TRUE" " on " "yes" "1"]]
                            (expect (= {:value true} (t/wire-value "test_wire_bool" token))))
                          (doseq [token ["false" "FALSE" " off " "no" "0"]]
                            (expect (= {:value false} (t/wire-value "test_wire_bool" token)))))))
  (it "answers a MAP, so the legal value false is not read as nothing"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_wire_bool" :label "Wire" :default true})
                          (expect (some? (t/wire-value "test_wire_bool" false)))
                          (expect (false? (:value (t/wire-value "test_wire_bool" false)))))))
  (it "refuses a token the registered type cannot name"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_wire_bool" :label "Wire" :default true})
                          (doseq [junk ["maybe" "" "  " 1 0 nil {} :true]]
                            (expect (nil? (t/wire-value "test_wire_bool" junk)))))))
  (it "matches an enum choice by name, case-insensitively, and refuses the rest"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_wire_enum"
                                               :label "Wire enum"
                                               :type :enum
                                               :choices ["quick" "deep"]
                                               :default "quick"})
                          (expect (= {:value "deep"} (t/wire-value "test_wire_enum" " DEEP ")))
                          (expect (= {:value "quick"} (t/wire-value "test_wire_enum" :quick)))
                          (expect (nil? (t/wire-value "test_wire_enum" "banana")))
                          (expect (nil? (t/wire-value "test_wire_enum" true))))))
  (it "never resolves an unregistered id"
      (with-clean-state (fn []
                          (expect (nil? (t/wire-value "test_wire_absent" true))))))
  (it "set-value! refuses a non-boolean instead of casting it to its opposite"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_wire_bool" :label "Wire" :default true})
                          (let [refused (try (t/set-value! "test_wire_bool" "false")
                                             ::stored
                                             (catch clojure.lang.ExceptionInfo e
                                               (:type (ex-data e))))]
                            (expect (= :vis.toggles/invalid-value refused)))
                          (expect (true? (t/enabled? "test_wire_bool")))
                          (t/set-value! "test_wire_bool" false)
                          (expect (false? (t/enabled? "test_wire_bool")))))))

(defdescribe plans-toggle-test
             (it "is retired, and a value saved by an earlier build stays silent"
                 (expect (nil? (t/toggle-spec "plans")))
                 (doseq [channel [:tui :web]]
                   (expect (not-any? #(= "plans" (:id %)) (t/toggles-for-channel channel))))
                 (let [saved {"toggles" {"plans" true}}]
                   (expect (validation/valid? saved))
                   (expect (nil? (tel/with-signal (t/hydrate-from-config! saved)))))
                 (expect (not (contains? (t/snapshot) "plans")))))

(defdescribe
  project-toggles-test
  ;; #302: a project extension's settings exist only where its project runs.
  (it
    "keeps project settings in their project and replaces the settings of overridden extensions"
    (let [root
          "/tmp/vis-project-toggles"

          ids
          (fn [root]
            (set (keep #{"test_project_global" "test_project_shared" "test_project_local"}
                       (map :id (t/target-toggles root)))))]

      (try (t/register-toggle!
             {:id "test_project_global" :label "Global" :default true :owner "test.overridden"})
           (t/register-toggle!
             {:id "test_project_shared" :label "Shared" :default true :owner "test.overridden"})
           (t/set-project-toggles! root
                                   ["test.overridden"]
                                   [{:id "test_project_shared"
                                     :label "Project shared"
                                     :default false
                                     :owner "test.overridden"}
                                    {:id "test_project_local"
                                     :label "Local"
                                     :type :enum
                                     :choices ["fast" "slow"]
                                     :default "fast"
                                     :owner "test.overridden"}])
           (expect (= #{"test_project_global" "test_project_shared"} (ids nil)))
           (expect (= #{"test_project_shared" "test_project_local"} (ids root)))
           (expect (= "Project shared" (:label (t/target-toggle-spec root "test_project_shared"))))
           (expect (= "Shared" (:label (t/target-toggle-spec nil "test_project_shared"))))
           (expect (nil? (t/target-toggle-spec root "test_project_global")))
           (expect (nil? (t/toggle-spec "test_project_local")))
           (expect (= {:value "slow"} (t/wire-value root "test_project_local" "SLOW")))
           (expect (nil? (t/wire-value "test_project_local" "slow")))
           (finally (t/set-project-toggles! root [] []) (t/unregister-owner! "test.overridden"))))))

(defdescribe
  string-toggle-test
  (it "stores one trimmed line and refuses other values"
      (with-clean-state (fn []
                          (try (t/register-toggle!
                                 {:id "test_string" :label "Text" :type :string :default ""})
                               (expect (= :string (t/type-of "test_string")))
                               (expect (= "" (t/value-of "test_string")))
                               (expect (= {:value "https://kb.example.com"}
                                          (t/wire-value "test_string" "  https://kb.example.com ")))
                               (expect (= {:value "8080"} (t/wire-value "test_string" 8080)))
                               (doseq [junk ["a\nb" (apply str (repeat 4097 "x")) true nil {}]]
                                 (expect (nil? (t/wire-value "test_string" junk))))
                               (expect (= "x" (t/set-value! "test_string" "x")))
                               (expect (= "42" (t/coerce-config-value "test_string" 42)))
                               (finally (t/unregister-toggle! "test_string"))))))
  (it "lists only non-empty secret values, and an error never carries the value"
      (with-clean-state
        (fn []
          (try (t/register-toggle!
                 {:id "test_secret" :label "Token" :type :string :default "" :secret? true})
               (t/register-toggle!
                 {:id "test_plain" :label "URL" :type :string :default "https://kb.example.com"})
               (expect (true? (:secret? (t/toggle-spec "test_secret"))))
               (expect (= [] (t/secret-values)))
               (t/set-value! "test_secret" "kb-token-12345678")
               (expect (= ["kb-token-12345678"] (t/secret-values)))
               (let [data (try (t/set-value! "test_secret" "bad\nvalue")
                               nil
                               (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                 (expect (= :vis.toggles/invalid-value (:type data)))
                 (expect (not (contains? data :value))))
               (finally (t/unregister-toggle! "test_secret")
                        (t/unregister-toggle! "test_plain")))))))
