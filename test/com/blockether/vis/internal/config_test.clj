(ns com.blockether.vis.internal.config-test
  "Coverage for the small set of Vis-side config helpers that don't touch
   disk. Filesystem-bound helpers (`load-config`, `save-config!`, ...) are
   exercised end-to-end through the iteration loop integration tests."
  (:require
   [com.blockether.vis.internal.config :as config]
   [lazytest.core :refer [defdescribe it expect]]))

(defdescribe router-opts-test
  "`router-opts` extracts the `:router` block from a Vis config map and
   trims it to the keys `svar/make-router`'s opts arity understands.
   Without this passthrough, every Vis-built router would silently fall
   back to svar defaults — the user's `:rate-limit` config block would
   be inert."

  (it "returns {} when no `:router` block is present"
    (expect (= {} (config/router-opts {})))
    (expect (= {} (config/router-opts {:providers []}))))

  (it "returns {} when `:router` is non-map"
    (expect (= {} (config/router-opts {:router nil})))
    (expect (= {} (config/router-opts {:router "string"}))))

  (it "passes through `:rate-limit` verbatim"
    (let [block {:same-provider-delays-ms [2000 3000 6000]
                 :fallback-after-ms 30000
                 :respect-retry-after? true
                 :fallback-provider? true}]
      (expect (= {:rate-limit block}
                (config/router-opts {:router {:rate-limit block}})))))

  (it "passes through `:network`, `:budget`, `:tokens`, and CB knobs"
    (let [cfg {:router {:network {:timeout-ms 600000
                                  :idle-timeout-ms 60000}
                        :budget  {:max-tokens 1000000 :max-cost 5.0}
                        :tokens  {:check-context? false}
                        :failure-threshold 10
                        :recovery-ms 30000}}]
      (expect (= (:router cfg) (config/router-opts cfg)))))

  (it "drops unknown keys so future config additions don't crash make-router"
    (let [cfg {:router {:rate-limit {:fallback-after-ms 1}
                        :totally-made-up-key :whatever
                        :another :nope}}]
      (expect (= {:rate-limit {:fallback-after-ms 1}}
                (config/router-opts cfg)))))

  (it "ignores top-level config keys outside `:router`"
    (let [cfg {:providers [{:id :p1}]
               :db-spec {:backend :sqlite}
               :router {:rate-limit {:fallback-after-ms 1}}}]
      (expect (= {:rate-limit {:fallback-after-ms 1}}
                (config/router-opts cfg))))))
