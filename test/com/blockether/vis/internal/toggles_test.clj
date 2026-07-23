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
                 (let
                   [thrown? (try (t/register-toggle! {:label "no-id"})
                                 false
                                 (catch clojure.lang.ExceptionInfo e
                                   (= :vis.toggles/invalid-spec (:type (ex-data e)))))]
                   (expect thrown?)))
             (it "rejects every non-string or non-snake_case id shape"
                 (doseq
                   [id [:ns/kw "ns/name" "kebab-case" "UPPER_CASE" "_leading" "trailing_"
                        "double__underscore" "" " "]]
                   (let
                     [thrown? (try (t/register-toggle! {:id id :label "bad" :default false})
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

(defdescribe
  forced-on-test
  ;; sub_loop children bind *forced-on* so a default-OFF toggle reads ON for the
  ;; child turn, while the global value (settings/persistence) is untouched.
  (it "*forced-on* makes enabled? true for an OFF toggle, in dynamic scope only"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_forced" :label "Forced" :default false})
                          (expect (false? (t/enabled? "test_forced")))
                          (binding [t/*forced-on* #{"test_forced"}]
                            (expect (true? (t/enabled? "test_forced")))
                            ;; value-of (settings UI / persistence) stays the GLOBAL truth
                            (expect (false? (boolean (t/value-of "test_forced")))))
                          (expect (false? (t/enabled? "test_forced"))))))
  (it "the binding conveys into a future (parallel sub_loop children inherit it)"
      (with-clean-state (fn []
                          (t/register-toggle! {:id "test_forced2" :label "Forced2" :default false})
                          (expect (true? @(binding [t/*forced-on* #{"test_forced2"}]
                                            (future (t/enabled? "test_forced2")))))))))

(defdescribe listener-test
             (it "listener fires on value transitions and a disposer detaches it"
                 (with-clean-state
                   (fn []
                     (t/register-toggle! {:id "test_epsilon" :label "Epsilon" :default false})
                     (let
                       [events
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
                          (expect (= :deep (t/coerce-config-value "test_e" "DEEP")))
                          (expect (= :quick (t/coerce-config-value "test_e" :quick)))
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
          (expect (= :deep (t/value-of "test_mode")))
          ;; the same canonical string id updates the same toggle
          (t/hydrate-from-config! {:toggles {"test_mode" "quick"}})
          (expect (= :quick (t/value-of "test_mode")))
          ;; keyword and namespaced aliases are ignored, never coerced
          (t/hydrate-from-config! {:toggles {:test_mode "deep" "ns/test_mode" "deep"}})
          (expect (= :quick (t/value-of "test_mode")))
          ;; an out-of-set enum string is dropped, leaving the prior value
          (t/hydrate-from-config! {:toggles {"test_mode" "nonsense"}})
          (expect (= :quick (t/value-of "test_mode")))))))

(defdescribe host-defaults-test
             (it "the reasoning_level toggle is registered as an enum"
                 (let [spec (t/toggle-spec "reasoning_level")]
                   (expect (some? spec))
                   (expect (= :enum (:type spec)))
                   (expect (= :balanced (:default spec)))))
             (it "retired display toggles do not exist (code always shows)"
                 ;; Render-fn op cards were removed — tool output is now stdout, and the TUI
                 ;; renders the model's raw :code unconditionally (the canonical contract,
                 ;; identical to web's `block-code`). Both display gates were retired.
                 (expect (nil? (t/toggle-spec "show_raw_code")))
                 (expect (nil? (t/toggle-spec "show_tool_results")))))
