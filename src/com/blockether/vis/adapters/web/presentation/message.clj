(ns com.blockether.vis.adapters.web.presentation.message
  "Message rendering — user/assistant bubbles, iterations, executions."
  (:require [com.blockether.vis.adapters.web.presentation.tool-render :as tool-render]
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

(defn- normalize-thinking
  "Trim + collapse runs of 3+ newlines down to a single paragraph break so
   marked.js doesn't stretch reasoning over huge vertical gaps."
  [s]
  (some-> s str/trim (str/replace #"\n{3,}" "\n\n")))

(defn- render-iteration [{:keys [iteration thinking executions final? error]}]
  (let [thinking      (normalize-thinking thinking)
        has-thinking? (not (str/blank? thinking))
        has-execs?    (seq executions)]
    (cond
      ;; Final iteration: only surface its thinking (answer renders separately).
      ;; Skip entirely when there's nothing to show.
      final?
      (when has-thinking?
        [:div.iteration.iteration-thinking-only
         [:div.thinking.md-content thinking]])

      ;; Non-final iteration: header + optional thinking/executions/error.
      :else
      [:div.iteration {:class (when error "iteration-error")}
       [:div.iter-header (str "Iteration " (inc iteration))
        (when error [:span.iter-error-tag " ERROR"])]
       (when error
         (let [{:keys [message type class data cause stack]} error]
           [:div.iter-error
            [:div.iter-error-headline
             (str (or message "<no message>")
               (when type (str " [" type "]")))]
            (when class
              [:div.iter-error-class (str class)])
            (when (seq data)
              [:details.iter-error-details
               [:summary "ex-data"]
               [:pre.iter-error-data (pr-str data)]])
            (when cause
              [:details.iter-error-details
               [:summary (str "cause: " (:class cause))]
               [:pre.iter-error-data (str (:message cause))]])
            (when (seq stack)
              [:details.iter-error-details
               [:summary "stack"]
               [:pre.iter-error-data (str/join "\n" stack)]])]))
       (when has-thinking?
         [:div.thinking.md-content thinking])
       (when has-execs? (keep render-exec executions))])))

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
