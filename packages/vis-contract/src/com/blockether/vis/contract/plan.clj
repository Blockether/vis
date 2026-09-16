(ns com.blockether.vis.contract.plan
  "Version-addressed planning artifacts and explicit review actions for both clients."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]))

(def ^:private schema (delay (document/schema-document "plans")))

(defn plan-name
  "Return kind and feature only for a canonical planning filename."
  [filename]
  (when (string? filename)
    (when-let [[_ kind feature]
               (re-matches (re-pattern (get-in @schema ["$defs" "filename" "pattern"])) filename)]
      {:kind (if (= kind "PLAN") :plan :implementation) :feature feature})))

(defn document-info
  "Read the header before the first section. Reject mismatched or ambiguous headers."
  [filename text]
  (when-let [info (plan-name filename)]
    (let [header (take-while #(not (str/starts-with? % "## ")) (take 16 (str/split-lines text)))
          values (fn [pattern]
                   (keep #(second (re-matches pattern %)) header))
          features (values #"\*\*Feature:\*\* (\S+)")
          statuses (values #"\*\*Status:\*\* (\S+)")]

      (when (and (= [(:feature info)] (vec features))
                 (= 1 (count statuses))
                 (some #{(first statuses)} (get-in @schema ["$defs" "status" "enum"])))
        (assoc info :status (first statuses))))))

(defn available-actions
  "One action per review: send pending remarks, or approve and start a complete specification."
  [info pending-comments?]
  (cond (nil? info) []
        pending-comments? [:revise]
        (and (= :plan (:kind info)) (#{"ready" "accepted"} (:status info))) [:approve]
        :else []))

(defn action-request
  "A user turn naming the exact saved version, never an unversioned latest pointer."
  [filename version action]
  (let [declaration (some #(when (= (name action) (get % "const")) %)
                          (get-in @schema ["$defs" "action" "oneOf"]))]
    (when-not (and (plan-name filename) (integer? version) (pos? version) declaration)
      (throw (ex-info "Invalid plan action or version"
                      {:filename filename :version version :action action})))
    (str "Read `" filename
         "` v" version
         " with read_attachment(" (pr-str filename)
         ", version=" version
         ").\n" (get declaration "x-vis-request"))))
