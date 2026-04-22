(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.shared
  "Shared logging and tiny pure helpers used across every step
   of the iteration pipeline. No business logic lives here — only
   cross-cutting infrastructure that every step needs."
  (:require
   [com.blockether.vis.loop.storage.schema :refer [*rlm-ctx*]]
   [com.blockether.svar.internal.router :as router]
   [taoensso.trove :as trove]))

(defn rlm-debug!
  "Logs at :info level only when :rlm-debug? is true in *rlm-ctx*.
   Includes :rlm-phase from context automatically in data."
  [data msg]
  (when (:rlm-debug? *rlm-ctx*)
    (trove/log! {:level :info :data (assoc data :rlm-phase (:rlm-phase *rlm-ctx*)) :msg msg})))

(defn rlm-stage!
  "Always-on structured stage log for RLM pipeline visibility.
   Emits structured data — no custom formatting."
  [stage iteration data]
  (trove/log! {:level :info
               :id    ::rlm-stage
               :data  (merge {:stage     stage
                              :iteration iteration
                              :env-id    (:rlm-env-id *rlm-ctx*)}
                        data)}))

(defn normalize-reasoning-level
  "Returns canonical :quick | :balanced | :deep, or nil for unknown input."
  [v]
  (router/normalize-reasoning-level v))

(defn reasoning-level-for-errors
  "Maps consecutive error count to an effective reasoning level."
  [base consecutive-errors]
  (cond
    (<= (long consecutive-errors) 0) base
    (= 1 (long consecutive-errors)) (if (= base :quick) :balanced :deep)
    :else :deep))

(defn answer-str
  "Extracts the answer string from an RLM answer map.
   Answer is {:result string :type :text}. The :result is always a string
   by construction (LLM response or var-resolve). Falls back to str for
   defensive safety but never pr-str — the user sees this verbatim."
  [answer]
  (let [v (:result answer answer)]
    (if (string? v) v (str v))))
