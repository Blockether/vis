(ns com.blockether.vis.internal.toggles-test
  "Feature-toggle registry contract: register, lookup, override,
   listener fan-out, persistence snapshot + hydrate, and the canonical
   internal toggles (`:vis/show-raw-code`, `:vis/show-tool-results`)
   are registered at load time."
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
                     (t/register-toggle! {:id :test/alpha :label "Alpha" :default true})
                     (let [spec (t/toggle-spec :test/alpha)]
                       (expect (= :test/alpha (:id spec)))
                       (expect (= "Alpha" (:label spec)))
                       (expect (true? (:default spec)))
                       (expect (false? (:persist? spec)))
                       (expect (= :vis (:owner spec)))
                       (expect (contains? (set (map :id (t/registered-toggles))) :test/alpha))))))
             (it "re-registering the same id is idempotent and preserves the live value"
                 (with-clean-state
                   (fn []
                     (t/register-toggle! {:id :test/beta :label "Beta" :default false})
                     (t/set-enabled! :test/beta true)
                     (t/register-toggle!
                       {:id :test/beta :label "Beta v2" :default false :description "updated"})
                     (expect (= "Beta v2" (:label (t/toggle-spec :test/beta))))
                     (expect (= "updated" (:description (t/toggle-spec :test/beta))))
                     ;; Live override survived the re-register.
                     (expect (true? (t/enabled? :test/beta))))))
             (it "rejects invalid specs with :vis.toggles/invalid-spec"
                 (let [thrown? (try (t/register-toggle! {:label "no-id"})
                                    false
                                    (catch clojure.lang.ExceptionInfo e
                                      (= :vis.toggles/invalid-spec (:type (ex-data e)))))]
                   (expect thrown?))))

(defdescribe
  value-resolution-test
  (it "enabled? falls back to the registered default"
      (with-clean-state (fn []
                          (t/register-toggle! {:id :test/gamma :label "Gamma" :default true})
                          (expect (true? (t/enabled? :test/gamma))))))
  (it "set! overrides the default and reset-to-default! restores it"
      (with-clean-state (fn []
                          (t/register-toggle! {:id :test/delta :label "Delta" :default true})
                          (t/set-enabled! :test/delta false)
                          (expect (false? (t/enabled? :test/delta)))
                          (t/reset-to-default! :test/delta)
                          (expect (true? (t/enabled? :test/delta))))))
  (it "unknown ids resolve to false (fail-closed)"
      (expect (false? (t/enabled? :test/never-registered)))))

(defdescribe
  forced-on-test
  ;; sub_loop children bind *forced-on* so a default-OFF toggle reads ON for the
  ;; child turn, while the global value (settings/persistence) is untouched.
  (it "*forced-on* makes enabled? true for an OFF toggle, in dynamic scope only"
      (with-clean-state (fn []
                          (t/register-toggle! {:id :test/forced :label "Forced" :default false})
                          (expect (false? (t/enabled? :test/forced)))
                          (binding [t/*forced-on* #{:test/forced}]
                            (expect (true? (t/enabled? :test/forced)))
                            ;; value-of (settings UI / persistence) stays the GLOBAL truth
                            (expect (false? (boolean (t/value-of :test/forced)))))
                          (expect (false? (t/enabled? :test/forced))))))
  (it "the binding conveys into a future (parallel sub_loop children inherit it)"
      (with-clean-state (fn []
                          (t/register-toggle! {:id :test/forced2 :label "Forced2" :default false})
                          (expect (true? @(binding [t/*forced-on* #{:test/forced2}]
                                            (future (t/enabled? :test/forced2)))))))))

(defdescribe listener-test
             (it "listener fires on value transitions and a disposer detaches it"
                 (with-clean-state
                   (fn []
                     (t/register-toggle! {:id :test/epsilon :label "Epsilon" :default false})
                     (let [events
                           (atom [])

                           dispose
                           (t/add-listener! #(swap! events conj %))]

                       (t/set-enabled! :test/epsilon true)
                       (t/set-enabled! :test/epsilon true) ;; no-op transition
                       (t/set-enabled! :test/epsilon false)
                       (dispose)
                       (t/set-enabled! :test/epsilon true) ;; listener detached
                       (expect (= 2 (count @events)))
                       (expect (= [{:id :test/epsilon :old false :new true}
                                   {:id :test/epsilon :old true :new false}]
                                  (mapv #(select-keys % [:id :old :new]) @events))))))))

(defdescribe
  persistence-test
  (it "snapshot omits non-persistent toggles and includes effective values for persistent ones"
      (with-clean-state
        (fn []
          (t/register-toggle! {:id :test/persist :label "P" :default false :persist? true})
          (t/register-toggle! {:id :test/transient :label "T" :default true :persist? false})
          (t/set-enabled! :test/persist true)
          (let [snap (t/snapshot)]
            (expect (contains? snap :test/persist))
            (expect (true? (get snap :test/persist)))
            (expect (not (contains? snap :test/transient)))))))
  (it "hydrate-from-config! applies persisted values; orphaned ids are skipped"
      (with-clean-state
        (fn []
          (t/register-toggle! {:id :test/zeta :label "Zeta" :default false :persist? true})
          (t/hydrate-from-config! {:toggles {:test/zeta true :test/orphan-unknown true}})
          (expect (true? (t/enabled? :test/zeta)))
          (expect (false? (t/enabled? :test/orphan-unknown)))))))

(defdescribe host-defaults-test
             (it "neither :vis/show-raw-code nor :vis/show-tool-results exist (code always shows)"
                 ;; Render-fn op cards were removed — tool output is now stdout, and the TUI
                 ;; renders the model's raw :code unconditionally (the canonical contract,
                 ;; identical to web's `block-code`). Both display gates were retired.
                 (expect (nil? (t/toggle-spec :vis/show-raw-code)))
                 (expect (nil? (t/toggle-spec :vis/show-tool-results)))))

