(ns com.blockether.vis.internal.config-test
  "Coverage for the Vis-side config helpers. Pure helpers (`router-opts`) plus a
   disk-isolated round-trip for `save-config!`/`load-config` — the persistence
   the first-run welcome and the provider manager both rely on (a connected
   provider must survive a restart)."
  (:require [clojure.java.io :as io]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config :as config]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe
  router-opts-test
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
        (expect (= {:rate-limit block} (config/router-opts {:router {:rate-limit block}})))))
  (it "passes through `:network`, `:budget`, `:tokens`, and CB knobs"
      (let [cfg {:router {:network {:timeout-ms 600000 :idle-timeout-ms 60000}
                          :budget {:max-tokens 1000000 :max-cost 5.0}
                          :tokens {:check-context? false}
                          :failure-threshold 10
                          :recovery-ms 30000}}]
        (expect (= (:router cfg) (config/router-opts cfg)))))
  (it "drops unknown keys so future config additions don't crash make-router"
      (let [cfg {:router {:rate-limit {:fallback-after-ms 1}
                          :totally-made-up-key :whatever
                          :another :nope}}]
        (expect (= {:rate-limit {:fallback-after-ms 1}} (config/router-opts cfg)))))
  (it "ignores top-level config keys outside `:router`"
      (let [cfg {:providers [{:id :p1}]
                 :db-spec {:backend :sqlite}
                 :router {:rate-limit {:fallback-after-ms 1}}}]
        (expect (= {:rate-limit {:fallback-after-ms 1}} (config/router-opts cfg))))))

(defdescribe
  ->svar-provider-test
  "`->svar-provider` coerces a vis provider map to svar-native shape. Local
   no-auth presets (ollama, lmstudio) carry no user credential, but svar's
   `models!` sends the catalog api-key as an HTTP header — a nil value throws
   a null-header NPE, which surfaced as empty LM Studio / Ollama model lists.
   So the coercion must forward svar's catalog dummy key for these presets."
  (it "forwards svar's catalog api-key for local no-auth presets (lmstudio/ollama)"
      (expect (= "lmstudio"
                 (:api-key (config/->svar-provider {:id :lmstudio
                                                    :base-url "http://localhost:1234/v1"
                                                    :models [{:name "probe"}]}))))
      (expect (= "ollama"
                 (:api-key (config/->svar-provider {:id :ollama
                                                    :base-url "http://localhost:11434/v1"
                                                    :models [{:name "probe"}]})))))
  (it "prefers an explicitly configured api-key over the catalog fallback"
      (expect (= "user-key"
                 (:api-key (config/->svar-provider
                             {:id :lmstudio :api-key "user-key" :models [{:name "probe"}]})))))
  (it "leaves cloud presets keyless when none is configured (no catalog dummy)"
      (expect (nil? (:api-key (config/->svar-provider {:id :openrouter
                                                       :models [{:name "probe"}]}))))))

(defdescribe
  svar-model-metadata-test
  (it "lets svar retain GLM-5.2's catalog-native effort style and values"
      (let [provider
            (config/->svar-provider
              {:id :zai-coding-plan :api-key "test" :models [{:name "glm-5.2"}]})

            model
            (-> (svar/make-router [provider])
                :providers
                first
                :models
                first)]

        (expect (= :zai-effort (:reasoning-style model)))
        (expect (= [{:type "effort" :values ["high" "max"]}] (:reasoning-options model)))))
  (it "does not stamp a broad Z.ai reasoning override in Vis"
      (expect (= {:name "glm-5.2"} (config/->svar-model :zai-coding-plan {:name "glm-5.2"})))))

(defn- rm-rf! [^java.io.File f] (when (.exists f) (run! rm-rf! (.listFiles f)) (.delete f)))

(defdescribe
  provider-persistence-test
  "save-config! / load-config round-trip backing onboarding. The first-run
   welcome and the provider manager BOTH write ~/.vis/config.edn so a connected
   provider survives a restart; adding a second provider must preserve the first
   and any unrelated global keys (e.g. :router). Isolated to a temp config dir."
  (it
    "first-run connect persists; adding a second provider keeps both + globals"
    (let [tmp
          (str (System/getProperty "java.io.tmpdir") "/vis-cfg-test-" (System/nanoTime))

          cfg-path
          (str tmp "/config.edn")]

      (try
        (with-redefs [config/config-dir
                      tmp

                      config/config-path
                      cfg-path

                      ;; isolate from any real project-local .vis overlay
                      config/project-config-path
                      (constantly (str tmp "/none/.vis/config.edn"))]

          ;; (0) genuine first run — nothing on disk
          (expect (config/first-run?))
          (expect (not (config/provider-configured?)))
          ;; (1) welcome connects provider A (mirrors show-welcome!'s persist:
          ;;     merge into raw global config, then save-config!)
          (config/save-config! (assoc (or (config/load-config-raw) {})
                                 :router {:budget {:max-cost 5.0}}
                                 :providers [{:id :prov-a :api-key "key-a"}]))
          (expect (not (config/first-run?)))
          (expect (config/provider-configured?))
          (expect (= [:prov-a] (mapv :id (:providers (config/load-config)))))
          ;; (2) provider manager adds provider B (mirrors manage-providers:
          ;;     seed from existing config, append, save the full list)
          (let [raw (config/load-config-raw)]
            (config/save-config!
              (assoc raw :providers (conj (vec (:providers raw)) {:id :prov-b :api-key "key-b"}))))
          ;; (3) reload from disk: BOTH providers survive (in order) and the
          ;;     unrelated global :router key is preserved
          (let [loaded (config/load-config)]
            (expect (= [:prov-a :prov-b] (mapv :id (:providers loaded))))
            (expect (= 5.0 (get-in loaded [:router :budget :max-cost])))))
        (finally (rm-rf! (io/file tmp)))))))
