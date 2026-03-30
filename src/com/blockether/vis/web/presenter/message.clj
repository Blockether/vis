(ns com.blockether.vis.web.presenter.message
  "Message rendering — user/assistant bubbles, iterations, executions."
  (:require [com.blockether.vis.trace :as trace]
            [clojure.string :as str])
  (:import [java.util Locale]))

(defn- fmt-dur [ms]
  (cond (nil? ms) "" (< ms 1000) (str ms "ms")
        :else (String/format Locale/US "%.1fs" (into-array Object [(double (/ ms 1000.0))]))))

(defn- exec-badge [code]
  (when code
    (let [t (str/trim code)
          b (if (str/starts-with? t "(") (subs t 1) t)]
      (first (str/split b #"[\s\)\(\"']" 2)))))

(defn- render-exec [{:keys [code result error stdout]}]
  (let [clean     (trace/clean-result result)
        is-final? (and (map? result) (:rlm/final result))
        badge     (exec-badge code)]
    (cond
      is-final? nil
      error
      [:div.exec.exec-errored
       [:div.exec-error (str error)]]
      :else
      [:div.exec
       [:div.exec-code code]
       (when (and stdout (not (str/blank? stdout)))
         [:div.exec-stdout stdout])
       [:div.exec-result
        (cond
          (nil? clean) "nil"
          (string? clean) clean
          :else [:pre.exec-data (pr-str clean)])]])))

(defn- render-iteration [{:keys [iteration thinking executions final? error]}]
  [:div.iteration {:class (cond final? "iteration-final" error "iteration-error")}
   [:div.iter-header (str "Iteration " (inc iteration))
    (when final? [:span.final " FINAL"])
    (when error [:span.iter-error-tag " ERROR"])]
   (when error
     [:div.iter-error (str (:message error) (when (:type error) (str " [" (:type error) "]")))])
   (when (and thinking (not (str/blank? thinking)))
     [:div.thinking thinking])
   (when (seq executions) (keep render-exec executions))])

(defn render-msg [idx {:keys [role text result]}]
  (case role
    :user [:div.msg.user-msg [:div.bubble.user-bubble [:span text]]]
    :assistant
    [:div.msg.ai-msg
     [:div.bubble.ai-bubble
      (when-let [trace (:trace result)]
        (map render-iteration trace))
      (when-let [a (:answer result)]
        [:div.answer.md-content (let [v (if (map? a) (:result a) a)] (if (string? v) v (pr-str v)))])
      (let [{:keys [iterations duration-ms tokens cost]} result]
        [:div.meta
         (str/join " · "
                   (cond-> []
                     (:model cost) (conj (:model cost))
                     iterations (conj (str iterations " iter"))
                     duration-ms (conj (fmt-dur duration-ms))
                     (:input tokens) (conj (str (:input tokens) "↓ " (:output tokens) "↑"))
                     (:total-cost cost) (conj (String/format Locale/US "$%.4f"
                                                             (into-array Object [(double (:total-cost cost))])))))])]]))
