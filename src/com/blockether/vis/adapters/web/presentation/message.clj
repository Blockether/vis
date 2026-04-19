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

(defn- render-vars-table
  "Portal-style compact table listing vars (re)defined in this iteration.
   Columns: name · type · preview. Skipped when no vars were written."
  [vars]
  (when (seq vars)
    [:table.var-table
     [:thead
      [:tr
       [:th.var-th-name "var"]
       [:th.var-th-type "type"]
       [:th.var-th-preview "preview"]]]
     [:tbody
      (for [{:keys [name type preview]} vars]
        [:tr.var-row
         [:td.var-td-name [:code name]]
         [:td.var-td-type [:span.var-type-chip type]]
         [:td.var-td-preview [:code preview]]])]]))

(defn- render-iteration
  "Render one iteration row.

   Distilled to essentials: ITER N header + optional status badge +
   code/result rows per execution + var writes table + error banner.

   The LLM's `:thinking` narrative is intentionally NOT shown — it used to
   render as a \"Thinking\"-labeled callout that felt like the system prompt
   was drifting turn-to-turn. The main chat now shows only WHAT the model
   did (code) and WHAT came back (result). The full thinking log still lives
   in the query entity tree for anyone who wants to dig."
  [{:keys [iteration executions final? error vars]}]
  (let [has-execs? (seq executions)
        has-vars?  (seq vars)]
    (when (or has-execs? error has-vars?)
      [:div.iteration {:class (str/join " "
                                (cond-> []
                                  error (conj "iteration-error")
                                  final? (conj "iteration-final-ok")))}
       [:div.iter-header
        (str "Iteration " (inc iteration))
        (when final? [:span.iter-final-tag " FINAL"])
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
       (when has-execs? (keep render-exec executions))
       (render-vars-table vars)])))

(defn- has-final-answer?
  "True when the result has a non-blank :answer AND a :final? iteration
   exists in the trace. Triggers the collapsed-trace presentation."
  [{:keys [trace answer]}]
  (and (some :final? trace)
    (let [v (if (map? answer) (:result answer) answer)
          s (cond (string? v) v
                  (nil? v)    ""
                  :else       (pr-str v))]
      (not (str/blank? s)))))

(defn- trace-summary-label
  "Short `N steps · M tool calls` label for the collapsed reasoning toggle."
  [trace]
  (let [steps (count (remove :final? trace))
        calls (reduce + 0 (map (fn [t] (count (:executions t))) trace))]
    (str steps " step" (when (not= 1 steps) "s")
      (when (pos? calls)
        (str " · " calls " tool call" (when (not= 1 calls) "s"))))))

(defn- status-banner
  "Visible banner when a turn ended on a non-:success status. Without this,
   :max-iterations / :error-budget-exhausted turns rendered as normal
   answers and the user had no idea the agent gave up."
  [status]
  (case status
    :max-iterations
    [:div.status-banner.status-warn
     [:i {:data-lucide "alert-triangle"}]
     [:span "Agent stopped at the iteration limit without finalizing. The answer below is a partial summary."]]
    :error-budget-exhausted
    [:div.status-banner.status-err
     [:i {:data-lucide "octagon-alert"}]
     [:span "Agent gave up after repeated errors. The answer below lists the recent failures."]]
    :cancelled
    [:div.status-banner
     [:i {:data-lucide "circle-stop"}]
     [:span "Turn cancelled."]]
    nil))

(defn render-msg [_idx {:keys [role text result]}]
  (case role
    :user [:div.msg.user-msg [:div.bubble.user-bubble [:span text]]]
    :assistant
    (let [trace        (:trace result)
          final-ready? (has-final-answer? result)]
      [:div.msg.ai-msg
       [:div.bubble.ai-bubble
        (status-banner (:status result))
        ;; Reasoning trace — collapsed by default when a final answer exists,
        ;; so the user sees the answer first and can expand to inspect steps.
        (when (seq trace)
          (if final-ready?
            [:details.trace-details
             [:summary.trace-summary
              [:i {:data-lucide "chevron-right"}]
              [:span.trace-summary-label
               (str "Reasoning — " (trace-summary-label trace))]]
             [:div.trace-body (map render-iteration trace)]]
            (map render-iteration trace)))
        ;; Final answer — unchanged prose rendering path.
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
                                          (into-array Object [(double (:total-cost cost))])))))])]])))
