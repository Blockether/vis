(ns com.blockether.vis.web.presenter.message
  "Message rendering — user/assistant bubbles, iterations, executions."
  (:require [com.blockether.vis.web.presenter.tool-render :as tool-render]
            [clojure.string :as str])
  (:import [java.util Locale]))

(defn- fmt-dur [ms]
  (cond (nil? ms) "" (< ms 1000) (str ms "ms")
    :else (String/format Locale/US "%.1fs" (into-array Object [(double (/ ms 1000.0))]))))

(defn- clean-result
  "Extract the meaningful value from RLM internal result maps.
   Strips :rlm/final, :rlm/answer wrappers to show just the value."
  [result]
  (cond
    (and (map? result) (:rlm/final result))
    (let [answer (:rlm/answer result)]
      (if (and (map? answer) (:result answer))
        (:result answer)
        (or answer result)))

    (and (map? result) (contains? result :rlm/answer))
    (let [answer (:rlm/answer result)]
      (if (and (map? answer) (:result answer))
        (:result answer)
        answer))

    :else result))

(defn- exec-badge [code]
  (when code
    (let [t (str/trim code)
          b (if (str/starts-with? t "(") (subs t 1) t)
          first-sym (first (str/split b #"[\s\)\(\"']" 2))]
      ;; If wrapped in (def name (tool ...)), extract the inner tool name
      (if (= first-sym "def")
        (when-let [inner (second (re-find #"\(def\s+\S+\s+\((\S+)" t))]
          inner)
        first-sym))))

(defn- render-exec [{:keys [code result error stdout]}]
  (let [clean     (clean-result result)
        is-final? (and (map? result) (:rlm/final result))
        badge     (exec-badge code)]
    (cond
      is-final? nil
      error
      [:div.exec.exec-errored
       (when code [:div.exec-code code])
       [:div.exec-error (str error)]]
      :else
      (or
        ;; Try tool-specific renderer first
        (when badge
          (when-let [rendered (tool-render/render-tool badge clean code)]
            [:div.exec rendered]))
        ;; Fallback to default rendering
        [:div.exec
         [:div.exec-code code]
         (when (and stdout (not (str/blank? stdout)))
           [:div.exec-stdout stdout])
         (when-not (nil? clean)
           [:div.exec-result
            (cond
              (string? clean) clean
              :else [:pre.exec-data (pr-str clean)])])]))))

(defn- render-iteration [{:keys [iteration thinking executions final? error]}]
  ;; Skip FINAL iterations entirely — the answer is rendered separately
  (when-not final?
    [:div.iteration {:class (when error "iteration-error")}
     [:div.iter-header (str "Iteration " (inc iteration))
      (when error [:span.iter-error-tag " ERROR"])]
     (when error
       [:div.iter-error (str (:message error) (when (:type error) (str " [" (:type error) "]")))])
     (when (and thinking (not (str/blank? thinking)))
       [:div.thinking.md-content thinking])
     (when (seq executions) (keep render-exec executions))]))

(defn render-msg [_idx {:keys [role text result]}]
  (case role
    :user [:div.msg.user-msg [:div.bubble.user-bubble [:span text]]]
    :assistant
    [:div.msg.ai-msg
     [:div.bubble.ai-bubble
      (when-let [trace (:trace result)]
        (map render-iteration trace))
      (when-let [a (:answer result)]
        (let [v (if (map? a) (:result a) a)
              raw (cond (string? v)     v
                    (sequential? v) (str/join "\n" (map str v))
                    :else           (pr-str v))
              cleaned (-> raw str/trim (str/replace #"\n{3,}" "\n\n"))]
          (when-not (str/blank? cleaned)
            [:div.answer.md-content cleaned])))
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
