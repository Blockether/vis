(ns com.blockether.vis.trace
  "Beautiful ANSI trace rendering for RLM execution traces.

   Renders iteration-by-iteration execution traces with color-coded
   sections for LLM responses, code executions, tool calls, and errors.
   Designed for CLI output; the data model is reusable by the TUI."
  (:require [clojure.string :as str])
  (:import [java.util Locale]))

;;; ── ANSI escape codes ──────────────────────────────────────────────────

(def ^:private ESC "\033[")

(defn- sgr [& codes] (str ESC (str/join ";" codes) "m"))

(def ^:private RESET     (sgr 0))
(def ^:private BOLD      (sgr 1))
(def ^:private DIM       (sgr 2))

;; Foreground colors
(def ^:private FG-RED     (sgr 31))
(def ^:private FG-GREEN   (sgr 32))
(def ^:private FG-YELLOW  (sgr 33))
(def ^:private FG-CYAN    (sgr 36))
(def ^:private FG-WHITE   (sgr 37))

;; Bright foreground
(def ^:private FG-BRIGHT-GREEN  (sgr 92))

;; 256-color
(defn- fg256 [n] (str ESC "38;5;" n "m"))

;; Color constants
(def ^:private C-BORDER    (fg256 242))
(def ^:private C-ITER-NUM  (str BOLD FG-YELLOW))
(def ^:private C-FINAL-TAG (str BOLD FG-BRIGHT-GREEN))
(def ^:private C-LABEL     (str BOLD (fg256 180)))
(def ^:private C-CODE      FG-CYAN)
(def ^:private C-RESULT    FG-GREEN)
(def ^:private C-ERROR     (str BOLD FG-RED))
(def ^:private C-THINKING  (str DIM FG-YELLOW))
(def ^:private C-STDOUT    (fg256 244))
(def ^:private C-TIMING    (fg256 240))
(def ^:private C-ANSWER    (str BOLD FG-WHITE))
(def ^:private C-STAT-VAL  (str BOLD FG-WHITE))
(def ^:private C-MODEL     (fg256 180))
(def ^:private C-DIM       DIM)

;;; ── ANSI-aware text utilities ──────────────────────────────────────────

(def ^:private ansi-re #"\033\[[0-9;]*m")

(defn- visible-length
  "String length excluding ANSI escape sequences."
  [s]
  (count (str/replace (str s) ansi-re "")))

(defn- strip-ansi [s]
  (str/replace (str s) ansi-re ""))

(defn- pad-right
  "Pad string to target visible width with spaces."
  [s target-w]
  (let [vis (visible-length s)
        pad (- target-w vis)]
    (if (pos? pad)
      (str s (apply str (repeat pad " ")))
      s)))

(defn- expand-tabs
  "Replace tabs with spaces (2-space tabs for compact display)."
  [s]
  (str/replace s "\t" "  "))

(defn- word-wrap
  "Wrap plain text at width, breaking at spaces. Returns vec of lines."
  [text width]
  (let [text (expand-tabs text)]
    (if (<= (count text) width)
      [text]
      (loop [remaining text
             lines []]
        (if (<= (count remaining) width)
          (conj lines remaining)
          (let [break-at (or (str/last-index-of remaining " " width)
                             width)
                break-at (if (zero? break-at) width break-at)]
            (recur (str/trim (subs remaining break-at))
                   (conj lines (subs remaining 0 break-at)))))))))

;;; ── Box drawing ────────────────────────────────────────────────────────

(def ^:private W 80)       ;; inner width (between │ and │)
(def ^:private CW (- W 2)) ;; content width (after "│ " and before " │")

(defn- hrule
  "Horizontal rule: ┌────────────────────────────────────┐"
  ([left mid right]
   (str C-BORDER left (apply str (repeat W mid)) right RESET))
  ([left mid right label]
   (let [vis-w (visible-length label)
         fill  (- W 2 vis-w)]
     (str C-BORDER left " " label " " RESET
          C-BORDER (apply str (repeat (max 0 fill) mid)) right RESET))))

(defn- bar
  "Content line with left and right borders: │ content   │"
  [& parts]
  (let [content (apply str parts)]
    (str C-BORDER "│" RESET " " (pad-right content CW) " " C-BORDER "│" RESET)))

(defn- blank-bar []
  (bar ""))

(defn- separator []
  "Mid-box separator spanning full width: ├────────────────────┤"
  (str C-BORDER "├" (apply str (repeat W "─")) "┤" RESET))

;;; ── Content formatters ─────────────────────────────────────────────────

(defn- truncate [s n]
  (let [s (str s)]
    (if (> (count s) n)
      (str (subs s 0 (max 0 n)) C-DIM "…" RESET)
      s)))

(defn- format-ms [ms]
  (cond
    (nil? ms)       ""
    (< ms 1000)     (str ms "ms")
    (< ms 60000)    (String/format Locale/US "%.1fs" (into-array Object [(double (/ ms 1000.0))]))
    :else           (String/format Locale/US "%.1fm" (into-array Object [(double (/ ms 60000.0))]))))

(defn- format-tokens [tokens]
  (when tokens
    (let [{:keys [input output reasoning cached]} tokens
          parts (cond-> []
                  input  (conj (str C-STAT-VAL input RESET " input"))
                  output (conj (str C-STAT-VAL output RESET " output"))
                  (and reasoning (pos? reasoning)) (conj (str C-STAT-VAL reasoning RESET " reasoning"))
                  (and cached (pos? cached))       (conj (str C-STAT-VAL cached RESET " cached")))]
      (when (seq parts)
        (str C-DIM "tokens: " RESET (str/join (str C-DIM " / " RESET) parts))))))

(defn- format-cost [cost]
  (when-let [total (:total-cost cost)]
    (if (< total 0.01)
      (String/format Locale/US "$%.4f" (into-array Object [(double total)]))
      (String/format Locale/US "$%.2f" (into-array Object [(double total)])))))

;;; ── Result cleaning ────────────────────────────────────────────────────

(defn clean-result
  "Extract the meaningful value from RLM internal result maps.
   Strips :rlm/final, :rlm/answer wrappers to show just the value."
  [result]
  (cond
    ;; RLM FINAL result map — extract the answer
    (and (map? result) (:rlm/final result))
    (let [answer (:rlm/answer result)]
      (if (and (map? answer) (:result answer))
        (:result answer)
        (or answer result)))

    ;; Namespaced rlm map with :answer key
    (and (map? result) (contains? result :rlm/answer))
    (let [answer (:rlm/answer result)]
      (if (and (map? answer) (:result answer))
        (:result answer)
        answer))

    :else result))

;;; ── Wrapped text rendering ─────────────────────────────────────────────

(defn- render-wrapped-lines
  "Render text wrapped to fit inside the box, with a prefix on each line."
  [text prefix color]
  (let [prefix-w  (visible-length prefix)
        wrap-w    (- CW prefix-w)
        plain     (strip-ansi text)
        lines     (str/split-lines plain)
        wrapped   (mapcat #(if (str/blank? %) [""] (word-wrap % wrap-w)) lines)]
    (->> wrapped
         (map #(bar prefix (if color (str color % RESET) %)))
         (str/join "\n"))))

;;; ── Section renderers ──────────────────────────────────────────────────

(defn- render-thinking [thinking]
  (when (and thinking (not (str/blank? thinking)))
    (let [lines   (str/split-lines (str/trim thinking))
          preview (take 6 lines)
          text    (str/join "\n" preview)
          suffix  (when (> (count lines) 6)
                    (str "\n" (bar "   " C-DIM "… " (- (count lines) 6) " more lines" RESET)))]
      (str (bar C-LABEL "◇ Thinking" RESET)
           "\n"
           (render-wrapped-lines text " " C-THINKING)
           (or suffix "")
           "\n"))))

(defn- structured-json-response?
  "True if response is just the structured JSON wrapper with thinking+code
   that the RLM spec produces. Redundant when thinking is shown separately."
  [response]
  (let [trimmed (str/trim response)
        ;; Strip markdown code fences if present
        body (if (str/starts-with? trimmed "```")
               (-> trimmed
                   (str/replace #"^```(?:json)?\s*\n?" "")
                   (str/replace #"\n?```\s*$" "")
                   str/trim)
               trimmed)]
    (and (str/starts-with? body "{")
         (str/ends-with? body "}")
         (or (str/includes? body "\"thinking\"")
             (str/includes? body "\"code\"")))))

(defn- render-response [response thinking]
  (when (and response (not (str/blank? response)))
    ;; Skip redundant JSON response when thinking is already shown
    (when-not (and thinking (structured-json-response? response))
      (let [text    (str/trim response)
            lines   (str/split-lines text)
            preview (take 12 lines)
            text    (str/join "\n" preview)
            suffix  (when (> (count lines) 12)
                      (str "\n" (bar "   " C-DIM "… " (- (count lines) 12) " more lines" RESET)))]
        (str (bar C-LABEL "◆ Response" RESET)
             "\n"
             (render-wrapped-lines text " " nil)
             (or suffix "")
             "\n")))))

(def ^:private max-result-lines 6)
(def ^:private max-value-len 120)

(defn- truncate-val
  "Truncate a string value for display."
  [s]
  (let [s (str s)]
    (if (> (count s) max-value-len)
      (str (subs s 0 max-value-len) "…")
      s)))

(defn- prettify-value
  "Format a value for display. Strings truncated, maps as key: value lines, else pr-str.
   Caps output to max-result-lines."
  [v]
  (let [raw (cond
              (string? v) v
              (nil? v)    "nil"
              (and (coll? v) (empty? v)) (pr-str v)
              (map? v)    (->> v
                               (map (fn [[k val]]
                                      (str (if (keyword? k) (name k) (pr-str k)) ": "
                                           (truncate-val (if (string? val) val (pr-str val))))))
                               (str/join "\n"))
              (coll? v)   (->> v
                               (take (inc max-result-lines))
                               (map #(truncate-val (if (string? %) % (pr-str %))))
                               (str/join "\n"))
              :else       (pr-str v))
        lines (str/split-lines raw)]
    (if (> (count lines) max-result-lines)
      (str (str/join "\n" (take max-result-lines lines))
           "\n… " (- (count lines) max-result-lines) " more lines")
      raw)))

(defn- render-final-execution
  "Render a FINAL execution as clean answer text."
  [{:keys [result execution-time-ms]}]
  (let [time-str (when execution-time-ms
                   (str C-TIMING " " (format-ms execution-time-ms) RESET))
        answer   (clean-result result)
        answer-s (prettify-value answer)]
    (str (bar C-FINAL-TAG "✓ Answer" RESET time-str)
         "\n"
         (render-wrapped-lines answer-s " " nil)
         "\n")))

(defn- render-execution [{:keys [id code result stdout error execution-time-ms timeout?]}]
  (let [is-final? (and (map? result) (:rlm/final result))]
    (if is-final?
      (render-final-execution {:result result :execution-time-ms execution-time-ms})
      (let [time-str (when execution-time-ms
                       (str C-TIMING " " (format-ms execution-time-ms) RESET))
            err?     (or error timeout?)
            icon     (if err? (str C-ERROR "✗" RESET) (str C-RESULT "▸" RESET))
            clean-r  (clean-result result)
            header   (str icon " " C-LABEL "Exec [" id "]" RESET time-str)
            header-w (visible-length header)
            inline-w (- CW header-w 1)
            code-indent "       "
            code-cont-w (- CW (count code-indent))
            code-lines (when code
                         (let [trimmed   (str/trim code)
                               src-lines (str/split-lines trimmed)]
                           (if (and (= 1 (count src-lines))
                                    (<= (count trimmed) inline-w))
                             {:inline (str C-CODE trimmed RESET)}
                             {:lines (->> src-lines
                                          (mapcat #(word-wrap % code-cont-w))
                                          (mapv #(str C-CODE % RESET)))})))
            result-w   (- CW 7) ;; "     ⇒ " prefix
            result-pretty (prettify-value clean-r)
            result-oneline? (and (<= (count result-pretty) result-w)
                                 (not (str/includes? result-pretty "\n")))
            result-str (cond
                         timeout? (str C-ERROR "TIMEOUT" RESET)
                         error    (str C-ERROR error RESET)
                         result-oneline? (str C-RESULT result-pretty RESET)
                         :else nil) ;; multi-line handled below
            stdout-str (when (and stdout (not (str/blank? stdout)))
                         (str (bar "     " C-STDOUT "stdout: " (truncate (str/trim stdout) (- CW 14)) RESET)
                              "\n"))]
        (str
         (if (:inline code-lines)
           (str (bar header " " (:inline code-lines)) "\n")
           (str (bar header) "\n"
                (when-let [lines (:lines code-lines)]
                  (str (->> lines
                            (map #(bar code-indent %))
                            (str/join "\n"))
                       "\n"))))
         (if result-str
           ;; Single-line result
           (str (bar "     " "⇒ " result-str) "\n")
           ;; Multi-line result: header then wrapped lines
           (str (bar "     " "⇒")
                "\n"
                (render-wrapped-lines result-pretty "       " C-RESULT)
                "\n"))
         (or stdout-str ""))))))

;;; ── Iteration renderer ─────────────────────────────────────────────────

(defn- iteration-has-errors?
  [{:keys [executions]}]
  (some (fn [{:keys [error timeout?]}] (or error timeout?)) executions))

(defn- extract-call-name
  [code]
  (when code
    (let [trimmed (str/trim code)
          body (if (str/starts-with? trimmed "(") (subs trimmed 1) trimmed)]
      (first (str/split body #"[\s\)\(\"']" 2)))))

(defn- render-iteration-compact
  [{:keys [iteration executions]}]
  (let [n     (count executions)
        names (->> executions (keep :code) (keep extract-call-name)
                   (remove str/blank?) distinct (take 4) vec)
        preview (if (seq names)
                  (str (str/join ", " names)
                       (when (> n (count names))
                         (str " +" (- n (count names)) " more")))
                  (str n " exec" (when (not= n 1) "s")))]
    (str C-BORDER "─── " RESET
         C-ITER-NUM "Iteration " (inc iteration) RESET
         C-DIM " · " preview RESET "\n")))

(defn- render-iteration
  [{:keys [iteration response thinking executions final?]}]
  (let [label (str C-ITER-NUM "Iteration " (inc iteration) RESET
                   (when final? (str " " C-FINAL-TAG "FINAL" RESET)))]
    (str (hrule "┌" "─" "┐" label)
         "\n"
         (or (render-thinking thinking) "")
         (or (render-response response thinking) "")
         (when (seq executions)
           (str (separator)
                "\n"
                (str/join "" (map render-execution executions))))
         (hrule "└" "─" "┘")
         "\n")))

;;; ── Summary ────────────────────────────────────────────────────────────

(defn- render-summary [{:keys [iterations duration-ms tokens cost model actual-model
                               requested-model provider-id]}]
  (let [tok-str   (format-tokens tokens)
        cost-str  (format-cost cost)
        ;; Show routing with labels: provider → model (requested: X)
        actual    (or actual-model model (:model cost))
        requested (or requested-model model)
        route-str (when actual
                    (let [base (if provider-id
                                 (str C-DIM "provider: " RESET C-MODEL (name provider-id) RESET
                                      C-DIM " → model: " RESET C-MODEL actual RESET)
                                 (str C-DIM "model: " RESET C-MODEL actual RESET))]
                      (if (and requested (not= requested actual))
                        (str base C-DIM "  (requested: " requested ")" RESET)
                        base)))
        ;; Build compact stat line: ✓ 9.4s · $0.0043 · 1 iteration
        stat-parts (cond-> []
                     duration-ms (conj (str C-STAT-VAL (format-ms duration-ms) RESET))
                     cost-str    (conj (str C-STAT-VAL cost-str RESET))
                     iterations  (conj (str C-STAT-VAL iterations RESET
                                        (if (= 1 iterations) " iteration" " iterations"))))]
    (str (hrule "┌" "─" "┐" (str C-LABEL "Summary" RESET))
         "\n"
         (bar C-RESULT "✓" RESET " " (str/join (str C-DIM " · " RESET) stat-parts))
         "\n"
         (when tok-str
           (str (bar "  " tok-str) "\n"))
         (when route-str
           (str (bar "  " route-str) "\n"))
         (hrule "└" "─" "┘")
         "\n")))

;;; ── Answer ─────────────────────────────────────────────────────────────

(defn- render-answer [answer]
  (when answer
    (str "\n"
         C-ANSWER (str/trim (str answer)) RESET
         "\n")))

;;; ── Public API ─────────────────────────────────────────────────────────

(defn format-trace
  "Format a full trace + result into a colored ANSI string.

   `result` is the map returned by `agent/run!`:
     {:answer :trace :iterations :duration-ms :tokens :cost ...}

   Options:
     :show-answer?  - Show the final answer after the trace (default false for trace mode)
     :show-summary? - Show stats summary (default true)
     :compact?      - Collapse uneventful iterations to one-line summaries (default false)
                      Iterations with errors, thinking, or FINAL are always shown in full."
  ([result] (format-trace result {}))
  ([result {:keys [show-answer? show-summary? compact?]
            :or {show-answer? false show-summary? true compact? false}}]
   (let [trace (:trace result)]
     (if (or (nil? trace) (empty? trace))
       (str C-DIM "No trace entries." RESET "\n")
       (str "\n"
            (str/join "\n"
                      (map (fn [it]
                             (if (and compact?
                                      (not (:final? it))
                                      (not (:thinking it))
                                      (not (iteration-has-errors? it)))
                               (render-iteration-compact it)
                               (render-iteration it)))
                           trace))
            (when show-summary?
              (render-summary result))
            (when show-answer?
              (render-answer (:answer result))))))))

(defn print-trace!
  "Print a formatted trace to a PrintStream (defaults to System/out).

   `result` - Map from `agent/run!`
   `opts`   - Same as `format-trace` options
   `out`    - PrintStream to write to (optional)"
  ([result] (print-trace! result {} nil))
  ([result opts] (print-trace! result opts nil))
  ([result opts ^java.io.PrintStream out]
   (let [s (format-trace result opts)
         ^java.io.PrintStream w (or out System/out)]
     (.print w s)
     (.flush w)
     s)))

;;; ── Live trace hooks (real-time streaming) ─────────────────────────

(defn- emit!
  "Print string to a PrintStream and flush."
  [^java.io.PrintStream out s]
  (.print out (str s))
  (.flush out))

(defn live-trace-hooks
  "Create svar hooks map that streams trace to terminal in real-time.
   Each iteration renders as it completes. Summary printed after.

   Returns {:hooks map :state atom} — pass :hooks to agent/run! opts,
   read :state after for summary data."
  [^java.io.PrintStream out]
  (let [state    (atom {:start-ms (System/currentTimeMillis)})
        started? (atom false)]
    {:state state
     :hooks
     {:llm-call
      {:pre  (fn [{:keys [opts]}]
               (when-let [model (or (:model opts) (:model (:config opts)))]
                 (swap! state assoc :model model)))
       :post (fn [{:keys [provider-id model base-url]}]
               (when provider-id
                 (let [requested (:model @state)]
                   (swap! state assoc
                          :provider-id provider-id
                          :actual-model model
                          :base-url base-url)
                   ;; Flash routing info — highlight when actual differs from requested
                   (if (and requested (not= requested model))
                     (emit! out (str C-DIM "    ⤷ " (name provider-id) "/" model
                                     " (requested: " requested ")" RESET "\r"))
                     (emit! out (str C-DIM "    ⤷ " (name provider-id) "/" model RESET "\r"))))))}

      :iteration
      {:pre  (fn [{:keys [iteration]}]
               (when-not @started?
                 (emit! out "\n")
                 (reset! started? true))
               (emit! out (str C-DIM "  ⟳ Iteration " (inc iteration) "…" RESET "\r")))
       :post (fn [trace-entry]
               ;; Clear the progress line and render full iteration
               (emit! out (str "\r" (apply str (repeat 40 " ")) "\r"))
               (emit! out (render-iteration trace-entry))
               (emit! out "\n"))}

      :code-exec
      {:pre  (fn [{:keys [code]}]
               (when code
                 (let [name (extract-call-name code)]
                   (when (and name (not= name "FINAL"))
                     (emit! out (str C-DIM "    ▸ " name "…" RESET "\r"))))))
       :post (fn [_data]
               (emit! out (str "\r" (apply str (repeat 40 " ")) "\r")))}}}))

(defn print-live-summary!
  "Print the summary + answer after live trace completes.
   Uses the result from agent/run! plus routing info from live state."
  [^java.io.PrintStream out result live-state]
  (let [st      @live-state
        actual  (or (:actual-model st) (get-in result [:cost :model]))
        request (:model st)
        pid     (:provider-id st)
        ;; Always ensure at least one model shows
        model   (or actual request)
        result  (cond-> (assoc result :model model)
                  actual  (assoc :actual-model actual)
                  request (assoc :requested-model request)
                  pid     (assoc :provider-id pid))]
    (emit! out (render-summary result))
    (when (:error result)
      (emit! out (str "\n" C-ERROR "Error [" (:type result) "]: " (:error result) RESET "\n")))))

;;; ── Structured trace data (for TUI consumption) ───────────────────────

(defn trace->sections
  "Transform a trace into a flat vector of renderable sections.
   Each section is a map with :type and type-specific keys.
   The TUI can map these to Lanterna drawing calls.

   Section types:
     :iteration-start  {:iteration N :final? bool}
     :thinking         {:text str}
     :response         {:text str}
     :execution        {:id N :code str :result any :error str :time-ms N :stdout str}
     :iteration-end    {}
     :summary          {:iterations N :duration-ms N :tokens map :cost map}
     :answer           {:text str}"
  [result]
  (let [trace (:trace result)]
    (when (seq trace)
      (into
       (vec
        (mapcat
         (fn [{:keys [iteration response thinking executions final?]}]
           (cond-> [{:type :iteration-start :iteration iteration :final? (boolean final?)}]
             (and thinking (not (str/blank? thinking)))
             (conj {:type :thinking :text (str/trim thinking)})

             (and response (not (str/blank? response)))
             (conj {:type :response :text (str/trim response)})

             (seq executions)
             (into (mapv (fn [{:keys [id code result stdout error execution-time-ms timeout?]}]
                           {:type     :execution
                            :id       id
                            :code     code
                            :result   (clean-result result)
                            :error    (or error (when timeout? "TIMEOUT"))
                            :time-ms  execution-time-ms
                            :stdout   stdout})
                         executions))

             true
             (conj {:type :iteration-end})))
         trace))
       (cond-> [{:type         :summary
                 :iterations   (:iterations result)
                 :duration-ms  (:duration-ms result)
                 :tokens       (:tokens result)
                 :cost         (:cost result)}]
         (:answer result)
         (conj {:type :answer :text (str (:answer result))}))))))
