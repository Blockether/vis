(ns com.blockether.vis.internal.toggles-test
  "Feature-toggle registry contract: register, lookup, override,
   listener fan-out, persistence snapshot + hydrate, and the canonical
   internal toggles. Toggle ids are plain strings (no namespaces)."
  (:require [com.blockether.vis.internal.toggles :as t]
            [lazytest.core :refer [defdescribe expect it]]))

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
             (it "rejects invalid specs with :vis.toggles/invalid-spec"
                 (let [thrown? (try (t/register-toggle! {:label "no-id"})
                                    false
                                    (catch clojure.lang.ExceptionInfo e
                                      (= :vis.toggles/invalid-spec (:type (ex-data e)))))]
                   (expect thrown?)))
             (it "rejects every non-string or non-snake_case id shape"
                 (doseq [id [:ns/kw "ns/name" "kebab-case" "UPPER_CASE" "_leading" "trailing_"
                             "double__underscore" "" " "]]
                   (let [thrown? (try (t/register-toggle! {:id id :label "bad" :default false})
                                      false
                                      (catch clojure.lang.ExceptionInfo e
                                        (= :vis.toggles/invalid-spec (:type (ex-data e)))))]
                     (expect thrown?)))))

(defdescribe
  value-resolution-test
  (it "enabled? falls back to the registered default"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_gamma" :label "Gamma" :default true})
                          (expect (true? (t/enabled? "test_gamma"))))))
  (it "set! overrides the default and reset-to-default! restores it"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_delta" :label "Delta" :default true})
                          (t/set-enabled! "test_delta" false)
                          (expect (false? (t/enabled? "test_delta")))
                          (t/reset-to-default! "test_delta")
                          (expect (true? (t/enabled? "test_delta"))))))
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
          (t/hydrate-from-config! {:toggles {"test_zeta" true "test_orphan_unknown" true}})
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
          (t/hydrate-from-config! {:toggles {"test_flag" "true" "test_mode" "deep"}})
          (expect (true? (t/enabled? "test_flag")))
          (expect (= "deep" (t/value-of "test_mode")))
          ;; the same canonical string id updates the same toggle
          (t/hydrate-from-config! {:toggles {"test_mode" "quick"}})
          (expect (= "quick" (t/value-of "test_mode")))
          ;; keyword and namespaced aliases are ignored, never coerced
          (t/hydrate-from-config! {:toggles {:test_mode "deep" "ns/test_mode" "deep"}})
          (expect (= "quick" (t/value-of "test_mode")))
          ;; an out-of-set enum string is dropped, leaving the prior value
          (t/hydrate-from-config! {:toggles {"test_mode" "nonsense"}})
          (expect (= "quick" (t/value-of "test_mode")))))))

(defdescribe host-defaults-test
             (it "the reasoning_level toggle is registered as an enum"
                 (let [spec (t/toggle-spec "reasoning_level")]
                   (expect (some? spec))
                   (expect (= :enum (:type spec)))
                   (expect (= ["low" "balanced" "deep"] (:choices spec)))
                   (expect (= "balanced" (:default spec)))))
             (it "retired display toggles do not exist (code always shows)"
                 ;; Render-fn op cards were removed — tool output is now stdout, and the TUI
                 ;; renders the model's raw :code unconditionally (the canonical contract,
                 ;; identical to web's `block-code`). Both display gates were retired.
                 (expect (nil? (t/toggle-spec "show_raw_code")))
                 (expect (nil? (t/toggle-spec "show_tool_results")))))

(defdescribe
  settings-description-test
  (it "a one-line sentence within the cap registers"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_short"
                                               :label "Short"
                                               :default false
                                               :description "Expose the thing this row turns on."})
                          (expect (= "Expose the thing this row turns on."
                                     (:description (t/toggle-spec "test_short")))))))
  (it "settings-description? draws the line at one line within the cap"
      (expect (t/settings-description? (apply str (repeat t/max-description-length "x"))))
      (expect (not (t/settings-description? (apply str
                                              (repeat (inc t/max-description-length) "x")))))
      (expect (not (t/settings-description? "two\nlines")))
      (expect (not (t/settings-description? "   ")))
      (expect (not (t/settings-description? nil))))
  (it "a settings row is never a paragraph: over-long copy is refused"
      ;; A settings row is a control plus ONE line of help; a paragraph of
      ;; rationale buried the toggle it described in the TUI and the app.
      (let [thrown? (try (t/register-toggle!
                           {:id "test_wordy"
                            :label "Wordy"
                            :default false
                            :description (apply str (repeat (inc t/max-description-length) "x"))})
                         false
                         (catch clojure.lang.ExceptionInfo e
                           (= :vis.toggles/invalid-spec (:type (ex-data e)))))]
        (expect (true? thrown?))
        (expect (nil? (t/toggle-spec "test_wordy")))))
  (it "every registered toggle's description fits a settings row"
      (doseq [{:keys [description]}
              (t/registered-toggles)

              :when description]

        (expect (t/settings-description? description)))))

(defdescribe
  vision-fallback-toggle-test
  "`vision_fallback_describe` gates the borrowed-eyes side-channel. It ships ON: a
   fleet with no vision model pays nothing for it, and a fleet that has one would
   otherwise throw the image away and tell the model to open it with PIL."
  (it "is registered as a persisted boolean that defaults on"
      (let [spec (t/toggle-spec "vision_fallback_describe")]
        (expect (some? spec))
        (expect (= :boolean (:type spec)))
        (expect (true? (:default spec)))
        (expect (true? (:persist? spec)))
        (expect (= :vis (:owner spec)))
        (expect (= :provider (:group spec)))
        (expect (t/settings-description? (:description spec)))))
  (it "reads true by default and follows an override"
      (expect (true? (t/enabled? "vision_fallback_describe")))
      (t/set-value! "vision_fallback_describe" false)
      (try (expect (false? (t/enabled? "vision_fallback_describe")))
           (finally (t/reset-to-default! "vision_fallback_describe")))
      (expect (true? (t/enabled? "vision_fallback_describe")))))

(defdescribe
  provider-fallback-toggle-registry-test
  "`provider_fallback` decides whether a failed turn may be answered by a model the human
   did not pick. It ships ON — today's rescue — and every route off the pick reads it."
  (it "is registered as a persisted boolean that defaults on"
      (let [spec (t/toggle-spec "provider_fallback")]
        (expect (some? spec))
        (expect (= :boolean (:type spec)))
        (expect (true? (:default spec)))
        (expect (true? (:persist? spec)))
        (expect (= :vis (:owner spec)))
        (expect (= :provider (:group spec)))
        (expect (t/settings-description? (:description spec)))))
  (it "shows up in the Settings list of BOTH channels from that one declaration"
      ;; No `:channels` set and no `:settings? false`, so the TUI dialog and the
      ;; companion sheet (`/v1/settings`) each render it without their own wiring.
      (let [ids (fn [specs]
                  (set (map :id specs)))]
        (expect (contains? (ids (t/toggles-for-channel :tui)) "provider_fallback"))
        (expect (contains? (ids (t/toggles-for-channel :web)) "provider_fallback"))))
  (it "reads true by default and follows an override"
      (expect (true? (t/enabled? "provider_fallback")))
      (t/set-value! "provider_fallback" false)
      (try (expect (false? (t/enabled? "provider_fallback")))
           (finally (t/reset-to-default! "provider_fallback")))
      (expect (true? (t/enabled? "provider_fallback")))))

(defdescribe
  refusal-fallback-toggle-registry-test
  "`refusal_fallback` decides whether a request Anthropic's safety classifier declined may be
   re-asked of a sibling model of the SAME provider. It is separate from `provider_fallback`
   because nothing about the credential, the provider or the wire failed."
  (it "is registered as a persisted boolean that defaults on"
      (let [spec (t/toggle-spec "refusal_fallback")]
        (expect (some? spec))
        (expect (= :boolean (:type spec)))
        (expect (true? (:default spec)))
        (expect (true? (:persist? spec)))
        (expect (= :vis (:owner spec)))
        (expect (= :provider (:group spec)))
        (expect (t/settings-description? (:description spec)))))
  (it "shows up in the Settings list of BOTH channels from that one declaration"
      (let [ids (fn [specs]
                  (set (map :id specs)))]
        (expect (contains? (ids (t/toggles-for-channel :tui)) "refusal_fallback"))
        (expect (contains? (ids (t/toggles-for-channel :web)) "refusal_fallback"))))
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
