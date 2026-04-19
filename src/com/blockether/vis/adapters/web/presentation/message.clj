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
          list-form? (str/starts-with? t "(")]
      (when list-form?
        (let [b (subs t 1)
              first-sym (first (str/split b #"[\s\)\(\"']" 2))]
          ;; If wrapped in (def name (tool ...)), extract the inner tool name
          (if (= first-sym "def")
            (when-let [inner (second (re-find #"\(def\s+\S+\s+\((\S+)" t))]
              inner)
            first-sym))))))

(defn- fmt-exec-time
  "Short human label for per-exec elapsed time."
  [ms]
  (when (number? ms)
    (cond
      (< ms 1000) (str (long ms) "ms")
      :else       (String/format Locale/US "%.2fs" (into-array Object [(double (/ ms 1000.0))])))))

(defn- render-exec-meta
  "Row of per-execution metadata chips (timeout badge, elapsed, auto-repair).
   Rendered at the top-right of every non-final exec card so the user knows
   which calls were slow, timed out, or got patched up."
  [{:keys [timeout? repaired? time-ms]}]
  (let [chips (cond-> []
                timeout?  (conj [:span.exec-chip.exec-chip-timeout "timeout"])
                repaired? (conj [:span.exec-chip.exec-chip-repaired "auto-repaired"])
                (and time-ms (not timeout?)) (conj [:span.exec-chip.exec-chip-time (fmt-exec-time time-ms)]))]
    (when (seq chips)
      ;; Splat chips as children, not as a single nested vector —
      ;; hiccup's `[:div.foo chips]` where chips is a vec-of-hiccup
      ;; tries to treat the inner vec's first element as a tag and
      ;; throws `... is not a valid element name`.
      (into [:div.exec-meta] chips))))

(defn- render-exec-streams
  "Render stdout/stderr (if present). Separate from :result because these
   are side-effects the tool/code emitted independently of its return value."
  [{:keys [stdout stderr]}]
  (list
    (when (and stdout (not (str/blank? stdout)))
      [:div.exec-stdout stdout])
    (when (and stderr (not (str/blank? stderr)))
      [:div.exec-stderr stderr])))

(defn- render-exec [{:keys [code result error stdout stderr timeout?] :as exec}]
  (let [clean     (clean-result result)
        is-final? (and (map? result) (:rlm/final result))
        badge     (exec-badge code)]
    (cond
      is-final? nil
      error
      [:div.exec.exec-errored
       (render-exec-meta exec)
       (when code [:details.exec-code-toggle
                   [:summary.exec-code-summary [:code (or badge "code")]]
                   [:pre.exec-code-full code]])
       (render-exec-streams exec)
       [:div.exec-error {:class (when timeout? "exec-error-timeout")} (str error)]]
      :else
      (or
        ;; Tool-specific renderer — shows the pretty tool output AND a
        ;; click-to-expand `source` toggle below it with the exact SCI
        ;; expression that produced the result. Answers "what code ran to
        ;; produce this?" without bloating the main view.
        (when badge
          (when-let [rendered (tool-render/render-tool badge clean code)]
            [:div.exec
             (render-exec-meta exec)
             rendered
             (render-exec-streams exec)
             (when code
               [:details.exec-code-toggle
                [:summary.exec-code-summary "source"]
                [:pre.exec-code-full code]])]))
        ;; Fallback — no tool-specific renderer. Show the raw code + result.
        [:div.exec
         (render-exec-meta exec)
         (when code [:pre.exec-code code])
         (render-exec-streams exec)
         (when-not (nil? clean)
           [:div.exec-result
            (cond
              (string? clean) clean
              :else [:pre.exec-data (pr-str clean)])])]))))

(defn- system-var?
  "Agent-loop SYSTEM vars are rebound every iteration regardless of what
   the LLM wrote, so surfacing them in the per-iteration var list makes
   every card look like it wrote the same three vars. They get their own
   collapsed sub-section instead."
  [var-name]
  (let [n (str var-name)]
    (or (= n "*query*") (= n "*reasoning*") (= n "*answer*"))))

(defn- markdown-system-var?
  [var-name]
  (let [n (str var-name)]
    (or (= n "*query*") (= n "*reasoning*") (= n "*answer*"))))

(defn- display-var-name
  [name]
  (case (str name)
    "*query*" "query"
    "*reasoning*" "thinking"
    "*answer*" "answer"
    (str name)))

(defn- render-var-delta [{:keys [version prev-version prev-preview]}]
  (when (and (number? version) (number? prev-version))
    [:div.var-delta
     [:span.var-delta-chip (str "v" prev-version " -> v" version)]
     (when (and prev-preview (not (str/blank? prev-preview)))
       [:span.var-delta-from
        [:span.var-delta-from-label "was"]
        [:code prev-preview]])]))

(defn- render-one-var-row [{:keys [name type preview code] :as v}]
  (let [is-system? (system-var? name)]
    [:details.var-row-details
     [:summary.var-row
      [:span.var-td-name
       [:code (display-var-name name)]
       (when (number? (:version v))
         [:span.var-version-chip (str "v" (:version v))])]
      [:span.var-td-type [:span.var-type-chip type]]
      [:span.var-td-preview [:code preview]]]
     [:div.var-row-expanded
      (render-var-delta v)
      (when (and code (not is-system?))
        [:div
         [:div.var-row-label "source"]
         [:pre.var-row-code code]])
      [:div.var-row-label "value"]
      (if (markdown-system-var? name)
        [:div {:class (str "var-row-value-md md-content"
                      (when (= (str name) "*reasoning*") " var-row-value-thinking"))}
         preview]
        [:pre.var-row-value preview])]]))

(defn- render-vars-table
  "Portal-style compact table listing vars (re)defined in this iteration.
   Columns: name · type · preview. Each row is a <details> → click to
   reveal the `(def …)` source + full pr-str preview.

   SYSTEM vars (`*query*`, `*reasoning*`, `*answer*`) are rebound by the
   agent loop every iteration and would otherwise dominate the table.
   They render inside a second `<details>` that stays closed by default."
  [vars]
  (let [user-vars (remove #(system-var? (:name %)) vars)]
    (when (seq user-vars)
      [:div.var-table-wrap
       [:div.var-table-section
        [:div.var-table-header
         [:span.var-th-name "var"]
         [:span.var-th-type "type"]
         [:span.var-th-preview "preview"]]
        [:div.var-table-body
         (for [v user-vars] (render-one-var-row v))]]])))

(defn- render-thinking
  "Render per-iteration reasoning inline (always visible) at the top of
   the iteration card when present."
  [thinking]
  (let [t (some-> thinking str/trim)]
    (when-not (str/blank? t)
      [:div.iter-thinking
       [:div.iter-thinking-summary
        [:span.iter-thinking-label "THINKING"]]
       [:div.iter-thinking-body.md-content t]])))

(defn- render-iteration
  "Render one iteration card.

   Header + reasoning (collapsible) + per-exec cards (each with an
   expandable `source` toggle) + var-writes table (each row expandable
   to show `(def …)` source + full value; system vars hidden in a
   secondary collapsed group). The final answer still renders once
   below the trace via `render-msg`.

   `solo?` unwraps visual chrome for the single-iteration case: no
   iteration wrapper card, no ITERATION header — the contents flow
   directly inside the assistant bubble, because an explicit
   “Iteration 1” badge for a turn that only ever had one iteration
   is just noise."
  ([iter] (render-iteration iter false))
  ([{:keys [iteration thinking executions final? error vars]} solo?]
  (let [has-execs?    (seq executions)
        has-vars?     (seq vars)
        has-thinking? (and thinking (not (str/blank? (str thinking))))]
    (when (or has-execs? error has-vars? has-thinking?)
      [(if solo? :div.iteration-solo :div.iteration)
       {:class (str/join " "
                 (cond-> []
                   error  (conj "iteration-error")
                   final? (conj "iteration-final-ok")))}
       (when-not solo?
         [:div.iter-header
          [:span.iter-title (str "ITERATION " (inc iteration))]
          (when final? [:span.iter-final-tag " FINAL"])
          (when error [:span.iter-error-tag " ERROR"])])
       (when (and solo? error)
         [:div.iter-header
          [:span.iter-error-tag "ERROR"]])
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
       (render-thinking thinking)
       (when has-execs?
         [:div
          [:div.iter-section-title
           (str "EXECUTION" (when (> (count executions) 1) "S"))]
          (keep render-exec executions)])
       (render-vars-table vars)]))))

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
  "Short summary for collapsed trace under a final answer."
  [trace]
  (let [steps (count trace)
        calls (reduce + 0 (map (fn [t] (count (:executions t))) trace))]
    (str "Execution trace · "
      steps " iteration" (when (not= 1 steps) "s")
      (when (pos? calls)
        (str " · " calls " code call" (when (not= 1 calls) "s"))))))

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
    :interrupted
    [:div.status-banner.status-warn
     [:i {:data-lucide "plug-zap"}]
     [:span "Server restarted mid-turn — this answer never finalized. Re-send the message to retry."]]
    :running
    [:div.status-banner.status-warn
     [:i {:data-lucide "loader"}]
     [:span "Turn still in flight — refresh to see updates. If the page hasn't moved in a while, the server may have crashed; re-send to retry."]]
    nil))

(defn render-msg [_idx {:keys [role text result]}]
  (case role
    :user [:div.msg.user-msg [:div.bubble.user-bubble [:span text]]]
    :assistant
    (let [trace (:trace result)]
      [:div.msg.ai-msg
       [:div.bubble.ai-bubble
        (status-banner (:status result))
        ;; Always render full iteration cards inline (no collapsed summary,
        ;; no chevron gate) so users see the whole execution trace directly.
        (when (seq trace)
          (let [solo? (= 1 (count trace))]
            (map #(render-iteration % solo?) trace)))
        ;; Final answer — unchanged prose rendering path.
        (when-let [a (:answer result)]
          (let [v (if (map? a) (:result a) a)
                raw (cond (string? v)     v
                      (sequential? v) (str/join "\n" (map str v))
                      :else           (pr-str v))
                cleaned (-> raw str/trim (str/replace #"\n{3,}" "\n\n"))]
             (when-not (str/blank? cleaned)
               [:div.final-answer
                [:div.final-answer-head
                 [:span.final-answer-marker "final"]
                 [:span.final-answer-label "answer"]]
                [:div.answer.md-content cleaned]])))
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
