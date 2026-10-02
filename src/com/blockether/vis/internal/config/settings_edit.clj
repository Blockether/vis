(ns com.blockether.vis.internal.config.settings-edit
  "Versioned configuration edits shared by the Companion and terminal UI.
   A batch belongs to one owner. Validation and revision checks precede one write."
  (:require [clojure.walk :as walk]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.config.validation :as validation]
            [com.blockether.vis.internal.sandbox.scoped-policy :as policy]
            [com.blockether.vis.internal.util :as util]))

(defn revision
  "Content revision includes ancestors and explicit values, not volatile service state."
  [db target]
  (util/sha256-hex (pr-str (walk/postwalk #(if (map? %) (into (sorted-map) %) %)
                                          {:target target
                                           :toggles (scoped/settings db target)
                                           :access (policy/settings db target)
                                           :agent (when (= "global" (:scope target))
                                                    (config/agent-name))}))))

(defn- prepare-edit
  [target {:strs [id action value]}]
  (when-not (and (string? id) (#{"value" "inherit"} action))
    (throw (ex-info "Use an explicit value or inherit" {:status 400 :id id})))
  (cond (policy/setting? id)
        (let [{:keys [section key]} (first (filter #(= id (:id %)) policy/fields))]
          (when (and (= action "value") (nil? value))
            (throw (ex-info "Use inherit instead of null" {:status 400 :id id})))
          {:id id :section section :name key :value (when (= action "value") value)})
        (= id "agent_name")
        (do (when-not (= "global" (:scope target))
              (throw (ex-info "Agent name belongs to the machine" {:status 400 :id id})))
            (when (and (= action "value") (not (validation/valid? {"agent_name" value})))
              (throw (ex-info "Enter a name of 1–80 characters without control characters"
                              {:status 400 :id id})))
            {:id id :section [] :name id :value (when (= action "value") (str/trim value))})
        :else (let [spec
                    (toggles/toggle-spec id)

                    chosen
                    (when (= action "value") (toggles/wire-value id value))]

                (when-not (and spec (some #{(:scope target)} (:scopes spec)))
                  (throw (ex-info "Setting is not available in this scope" {:status 400 :id id})))
                (when (and (= action "value") (nil? chosen))
                  (throw (ex-info "Choose a valid setting value" {:status 400 :id id})))
                {:id id :section ["toggles"] :name id :value (:value chosen)})))

(defn apply!
  "Validate and apply explicit edits with compare-and-swap. Failed batches write nothing."
  [db target expected changes]
  (when-not (and (document/valid-json?
                   "gateway"
                   "settings_batch"
                   (cond-> {"scope" (:scope target) "revision" expected "changes" changes}
                     (:target-id target)
                     (assoc "target_id" (str (:target-id target)))))
                 (= (count changes) (count (distinct (map #(get % "id") changes)))))
    (throw (ex-info "Supply a revision and a valid batch of distinct setting edits" {:status 400})))
  (scoped/edit-settings!
    db
    target
    (fn [current-db]
      (when-not (= expected (revision current-db target))
        (throw (ex-info
                 "Settings changed in another client. Review the latest values before applying."
                 {:status 409 :type :settings/conflict})))
      (let [prepared
            (mapv #(prepare-edit target %) changes)

            access
            (filter #(policy/setting? (:id %)) prepared)

            inherited
            (into {} (map (juxt :id :inherited-value)) (policy/settings current-db target))

            candidate
            (reduce (fn [raw {:keys [id section name value]}]
                      (assoc-in raw (conj section name) (if (nil? value) (get inherited id) value)))
                    (policy/effective-config current-db target)
                    access)]

        (when (seq access)
          (try (policy/validate-candidate! target candidate)
               (catch clojure.lang.ExceptionInfo e
                 (throw (ex-info (ex-message e)
                                 (assoc (ex-data e)
                                   :field-errors
                                   (into {} (map #(vector (:id %) (ex-message e))) access)))))))
        prepared)))
  {:revision (revision db target)})
