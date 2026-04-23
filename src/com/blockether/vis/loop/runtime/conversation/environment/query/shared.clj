(ns com.blockether.vis.loop.runtime.conversation.environment.query.shared
  "Reusable query-level helpers.

   - Router lifecycle: get-router, resolve-effective-model, provider-has-reasoning?
   - Var snapshots: restorable-var-snapshots, extract-def-names
   - System var lifecycle: update-system-vars!, inject-system-var-snapshots"
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis.config :as config]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as rlm-tools]
   [com.blockether.vis.loop.runtime.shared :as rt-shared :refer [realize-value]]
   [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iterate]
   [edamame.core :as edamame]
   [taoensso.trove :as trove]))

;; =============================================================================
;; Router lifecycle (singleton)
;; =============================================================================

(defonce ^:private router-atom (atom nil))

(defn get-router
  "Get or create the shared LLM router."
  []
  (or @router-atom
    (let [cfg (config/resolve-config)
          r   (llm/make-router (:providers cfg))]
      (reset! router-atom r)
      r)))

(defn reset-router! [] (reset! router-atom nil))

(defn ask! [opts] (llm/ask! (get-router) opts))

(def resolve-effective-model
  "Resolves the effective model from the router. Returns map with
   :name, :reasoning?, etc. or nil. Accepts optional overrides map
   with :optimize, :provider, :model, :reasoning."
  svar-router/resolve-effective-model)

(defn provider-has-reasoning?
  "True when the root model supports reasoning. Convenience wrapper."
  [router]
  (:reasoning? (resolve-effective-model router)))

;; =============================================================================
;; Var snapshot helpers
;; =============================================================================

(defn extract-def-names
  "Extracts var names from code blocks via EDN parsing of def-like forms."
  [executions]
  (->> executions
    (mapcat (fn [{:keys [code error]}]
              (when-not error
                (try
                  (->> (edamame/parse-string-all (or code "") {:all true})
                    (keep (fn [form]
                            (when (seq? form)
                              (let [[op name & _] form]
                                (when (and (contains? '#{def defn defn- defonce defmulti defmacro} op)
                                        (symbol? name))
                                  name)))))
                    distinct)
                  (catch Exception _ [])))))
    (map str) vec))

(defn restorable-var-snapshots
  "Returns serializable snapshots of user vars introduced by this iteration."
  [rlm-env executions]
  (let [execution->defs (mapv (fn [{:keys [error] :as execution}]
                                [execution (when-not error (set (map symbol (extract-def-names [execution]))))])
                          executions)
        defined (into #{} (mapcat second) execution->defs)
        sym->exec (reduce (fn [acc [{:keys [code execution-time-ms]} defs]]
                            (if (and code (seq defs))
                              (reduce #(assoc %1 %2 {:expr code :time-ms execution-time-ms}) acc defs)
                              acc))
                    {} execution->defs)
        locals (iterate/get-locals rlm-env)]
    (->> locals
      (keep (fn [[sym value]]
              (when (contains? defined sym)
                (let [realized (realize-value value)
                      exec-info (get sym->exec sym)]
                  (when (or (nil? realized) (string? realized) (number? realized)
                          (keyword? realized) (boolean? realized) (symbol? realized)
                          (map? realized) (vector? realized) (set? realized)
                          (list? realized) (sequential? realized))
                    (cond-> {:name (str sym) :value realized :code (:expr exec-info)}
                      (:time-ms exec-info) (assoc :time-ms (:time-ms exec-info))))))))
      vec)))

;; =============================================================================
;; System var lifecycle
;; =============================================================================

(defn update-system-vars!
  "Bind *reasoning* and *answer* into the SCI sandbox after an iteration."
  [rlm-env {:keys [thinking final-result final-answer]}]
  (when (seq thinking)
    (rlm-tools/bind-and-bump! rlm-env '*reasoning* thinking))
  (when final-result
    (rlm-tools/bind-and-bump! rlm-env '*answer* final-answer)))

(defn inject-system-var-snapshots
  "Append SYSTEM var entries to a vars-snapshot vec for persistence."
  [vars-snapshot {:keys [iteration query thinking final-result final-answer]}]
  (cond-> vars-snapshot
    (zero? iteration)   (conj {:name "*query*" :value query :code ";; SYSTEM var"})
    (seq thinking)      (conj {:name "*reasoning*" :value thinking :code ";; SYSTEM var"})
    final-result        (conj {:name "*answer*" :value final-answer :code ";; SYSTEM var"})))
