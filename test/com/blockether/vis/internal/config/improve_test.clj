(ns com.blockether.vis.internal.config.improve-test
  (:require [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.improve :as improve]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.config.validation :as validation]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- with-settings
  [f]
  (let [old
        (toggles/value-of "improve_mode")

        enabled?
        (toggles/enabled? "improve")

        raw
        (atom {"toggles" {"plans" true}})]

    (try (toggles/set-enabled! "improve" true)
         (toggles/set-value! "improve_mode" "human")
         (with-redefs [config/load-config-raw
                       (fn []
                         @raw)

                       config/update-machine-config!
                       (fn [update-fn _]
                         (swap! raw update-fn))]

           (f raw))
         (finally (toggles/set-value! "improve_mode" old)
                  (toggles/set-enabled! "improve" enabled?)))))

(defn- status [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:status (ex-data e)))))

(defdescribe default-and-persisted-settings
             (it "default and persisted settings"
                 (with-settings
                   (fn [raw]
                     (expect (= {:mode "human" :provider nil :model nil :interval_minutes 60}
                                (improve/settings)))
                     (expect (= "automatic"
                                (:mode (improve/update-settings! {:mode "automatic"
                                                                  :provider "chosen"
                                                                  :model "exact"
                                                                  :interval_minutes 2}))))
                     (expect (true? (get-in @raw ["toggles" "plans"])))
                     (expect (= "automatic" (get-in @raw ["toggles" "improve_mode"])))
                     (expect (validation/valid? @raw))
                     (expect (= "chosen" (get-in @raw ["improve" "provider"])))))))

(defdescribe provider-and-model-editing-is-automatic-only
             (it "provider and model editing is automatic only"
                 (with-settings
                   (fn [raw]
                     (doseq [attrs [{:provider "x"} {:model nil} {:mode "off" :provider "x"}
                                    {:mode "human" :model "x"} {:mode "unknown"} {:unexpected true}
                                    {:interval_minutes 0} {:interval_minutes 1441}
                                    {:interval_minutes 1.5} {:mode "automatic" :provider " "}]]
                       (expect (= 400 (status #(improve/update-settings! attrs)))))
                     (expect (= {"toggles" {"plans" true}} @raw))))))

(defdescribe route-and-mode-round-trips-invalidate-snapshots
             (it "route and mode round trips invalidate snapshots"
                 (with-settings (fn [_]
                                  (improve/update-settings!
                                    {:mode "automatic" :provider "p" :model "m"})
                                  (let [snapshot (improve/snapshot)]
                                    (expect (improve/current? snapshot))
                                    (toggles/set-value! "improve_mode" "human")
                                    (toggles/set-value! "improve_mode" "automatic")
                                    (expect (not (improve/current? snapshot))))
                                  (let [snapshot (improve/snapshot)]
                                    (improve/update-settings! {:model "other"})
                                    (expect (not (improve/current? snapshot))))))))

(defdescribe config-schema-rejects-invalid-review-settings
             (it "config schema rejects invalid review settings"
                 (doseq [block [{"interval_minutes" 0} {"interval_minutes" "60"} {"provider" ""}
                                {"unknown" true}]]
                   (expect (not (validation/valid? {"improve" block}))))
                 (expect (validation/valid? {"improve"
                                             {"provider" nil "model" nil "interval_minutes" 60}}))))
