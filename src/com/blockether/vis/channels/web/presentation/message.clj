(ns com.blockether.vis.channels.web.presentation.message
  "Message rendering — user/assistant bubbles, iterations, executions."
  (:require [com.blockether.vis.loop.runtime.shared :as rt-shared]
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


(defn- sci-var?
  "True when v is a SCI or Clojure var. Detected by class name to avoid
   coupling this rendering ns to `sci.core`'s class-loading order
   (same reason as in storage/sqlite/conversations/edn-safe)."
  [v]
  (and (some? v)
    (or (instance? clojure.lang.Var v)
      (= "sci.lang.Var" (.getName (class v))))))

(defn- unwrap-var-surrogate
  "Strip var wrapping so the renderer sees the bound VALUE, not the
   var reference itself.

   Two shapes can arrive here, depending on whether the result came
   from the live in-memory path or via the DB roundtrip:

     - Live (executor cache): an actual `clojure.lang.Var` /
       `sci.lang.Var` produced by `(def foo 42)`. We deref it to its
       raw root value.
     - Persisted (DB load): the storage layer's `edn-safe` already
       replaced the var with `{:rlm/var-id name :rlm/var-value bound}`
       so EDN can roundtrip it. We pull `:rlm/var-value` out.

   The var name itself is carried separately in the iteration's vars
   table, so the exec card's value+shape lines should focus on the
   bound value alone. Pass-through for anything else."
  [v]
  (cond
    (sci-var? v)
    (try
      (.getRawRoot v)
      (catch Throwable _ v))

    (and (map? v) (contains? v :rlm/var-id))
    (:rlm/var-value v)

    :else v))

(defn- render-exec [{:keys [code result error timeout?] :as exec}]
  (let [clean     (-> result clean-result unwrap-var-surrogate)
        is-final? (and (map? result) (:rlm/final result))]
    (when-not is-final?
      [:div.exec
       (render-exec-meta exec)
       (when code [:pre.exec-code code])
       (render-exec-streams exec)
       (when error
         [:div.exec-error {:class (when timeout? "exec-error-timeout")} (str error)])
       (when (and (nil? error) (some? clean))
         [:div.exec-result
          [:pre.exec-data
           (if (string? clean)
             (-> clean rt-shared/strip-sandbox-ns
               (rt-shared/truncate-with-marker rt-shared/MAX_RESULT_DISPLAY_CHARS))
             (rt-shared/result->display clean :full))]])])))

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
      [:div.iter-content
       (when error
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
