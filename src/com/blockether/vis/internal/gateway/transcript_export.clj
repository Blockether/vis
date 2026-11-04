(ns com.blockether.vis.internal.gateway.transcript-export
  "Render a session as a standalone HTML file that draws its transcript with the SAME
   components as the Companion session screen.

   `npm run build:web` writes `transcript-export.html` into the web bundle. That page holds
   all its code, styles and fonts, and one empty data element. This namespace copies the page
   and writes the session's JSON into that element: the gateway's own transcript rows
   (`state/transcript-page`), the session title, agent name and workspace, the ids of the
   session's subagents, the team rows of the turns that started them and the bytes of each
   artifact that the steps name. The page needs no gateway and no network when it opens.

   The bundle is found through `VIS_WEB_DIR` ([[web/configured-root]]), the folder the
   gateway serves the web app from. Without it, an export fails with a message that says how
   to build it; there is no second renderer to fall back to.

   [[md]] writes the same document as Markdown, as the Compact mode of the session screen shows
   it. [[digest-summary]] writes each digest row with the words and counts of that screen."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.server.web :as web]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io File]
           [java.math BigDecimal RoundingMode]
           [java.util Base64]))

(def page-name "The export page inside the web bundle." "transcript-export.html")

(def data-element
  "The empty data element of the export page. The page reads its JSON from this element."
  "<script id=\"vis-transcript-data\" type=\"application/json\">null</script>")

(defn template-file
  "The export page of the web bundle in `dir` (`VIS_WEB_DIR` by default), or nil."
  (^File [] (template-file (web/configured-dir)))
  (^File [dir]
   (when-let [root (web/configured-root dir)]
     (let [file (io/file root page-name)]
       (when (.isFile file) file)))))

(defn- script-json
  "`value` as JSON that is safe inside a `<script>` element. `\u003c` is the JSON escape of
   `<`, so no `</script>` or `<!--` in a transcript can end the element."
  ^String [value]
  (str/replace (wire/json-str value) "<" "\\u003c"))

(defn fill
  "The export page `template` with `data` written into its data element. Throws when the
   page has no data element or more than one, because then the page is not an export page."
  ^String [^String template data]
  (let [at (str/index-of template data-element)]
    (when (or (nil? at) (not= at (str/last-index-of template data-element)))
      (throw (ex-info (str "The HTML export page has no single transcript data element: " page-name)
                      {:type :transcript-export/bad-template})))
    (str (subs template 0 at)
         "<script id=\"vis-transcript-data\" type=\"application/json\">" (script-json data)
         "</script>" (subs template (+ (long at) (count data-element))))))

(defn- attachment-files
  "The bytes of each artifact that the steps of `turns` name, in base64, keyed by
   `<iteration id>/<index>`. The key is the address of the gateway's attachment route, which the
   web app reads, so the page answers that read. An artifact without stored bytes is left out,
   and the page answers it with a 404 as the gateway does."
  [turns]
  (let [rows (memoize state/user-iteration-attachments)]
    (into {}
          (keep (fn [[iid idx]]
                  (when-let [row (nth (rows iid) idx nil)]
                    (when-let [^bytes bs (state/attachment-bytes row)]
                      [(str iid "/" idx)
                       {"media_type" (or (not-empty (str (:media-type row)))
                                         "application/octet-stream")
                        "base64" (.encodeToString (Base64/getEncoder) bs)}]))))
          (distinct (for [turn turns
                          iteration (get turn "iterations")
                          attachment (get iteration "attachments")
                          :let [iid (get attachment "iteration_id")
                                idx (get attachment "index")]
                          :when (and (string? iid) (nat-int? idx))]

                      [iid idx])))))

(defn export-data
  "The JSON document of the export page for session `sid`, or nil when no session has that id.
   The turns are the complete rows of the session screen: no window and no step limit."
  [sid]
  (when-let [soul (state/soul sid)]
    (let [turns (:turns (state/transcript-page sid {}))]
      {"session" (select-keys soul ["id" "title" "agent_name" "workspace" "model" "model_pref"])
       "usage" (state/session-usage-info sid)
       "turns" turns
       "child_session_ids" (into []
                                 (keep #(some-> (or (:session-id %) (:id %))
                                                str))
                                 (persistance/db-agent-list (lp/db-info) sid))
       "agents" (wire/canonical (state/agent-rows sid))
       "attachments" (attachment-files turns)})))

(defn html
  "The standalone HTML export of session `sid`, or nil when no session has that id. Throws
   with a build hint when the web bundle holds no export page."
  ^String [sid]
  (let [file (or (template-file)
                 (throw (ex-info (str "The HTML export needs the Vis web app, and "
                                      (or (web/configured-dir) "VIS_WEB_DIR")
                                      " holds no "
                                      page-name
                                      ". Build it in apps/vis-companion with: npm run build:web")
                                 {:type :transcript-export/no-template})))]
    (when-let [data (export-data sid)]
      (fill (slurp file) data))))

(defn- strip-ansi
  "`s` without terminal escape sequences, which tool output can carry."
  [s]
  (when (some? s) (str/replace (str s) #"\u001b\[[0-9;?]*[ -/]*[@-~]" "")))

(defn- md-escape
  "`s` with `&`, `<` and `>` escaped, for text inside literal HTML in Markdown."
  [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

;; Markdown export: the same document, as the Compact mode of the session screen shows it.

(defn- web-duration
  "A duration as the session screen prints it: `<1ms`, `840ms`, `2.3s`, `1m 15s`, else nil."
  [ms]
  (when (and (number? ms) (not (neg? (double ms))))
    (let [ms (long ms)]
      (cond (< ms 1) "<1ms"
            (< ms 1000) (str ms "ms")
            ;; `toFixed(1)` rounds the exact binary value of the double, as `BigDecimal` does.
            (< ms 60000) (str (.setScale (BigDecimal. (/ ms 1000.0)) 1 RoundingMode/HALF_UP) "s")
            :else (str (quot ms 60000) "m " (quot (rem ms 60000) 1000) "s")))))

(defn- fixed
  "`x` with `digits` decimals, as JavaScript `toFixed` writes it."
  [x digits]
  (str (.setScale (BigDecimal. (double x)) (int digits) RoundingMode/HALF_UP)))

(defn- metric-number
  "`x` as a double, as JavaScript `Number(x ?? 0)` reads a metric."
  ^double [x]
  (double (or x 0)))

(defn- compact-count
  "A count as the metrics panel writes it: `840`, `8.4k`, `85k`, `2.1M`, `1.2B`."
  [x]
  (let [n (metric-number x)]
    (cond (not (Double/isFinite n)) "—"
          (< n 1e3) (if (== n (Math/floor n)) (str (long n)) (str n))
          (< n 1e6) (str (fixed (/ n 1e3) (if (< n 1e4) 1 0)) "k")
          (< n 1e9) (str (fixed (/ n 1e6) (if (< n 1e7) 1 0)) "M")
          :else (str (fixed (/ n 1e9) 1) "B"))))

(defn- usd
  "A cost as the metrics panel writes it: `$0`, `<$0.01`, `$4.20`, `$1,250`."
  [x]
  (let [n (metric-number x)]
    (cond (or (not (Double/isFinite n)) (<= n 0.0)) "$0"
          (< n 0.01) "<$0.01"
          (< n 1000.0) (str "$" (fixed n 2))
          :else (str "$"
                     (String/format java.util.Locale/US "%,d" (object-array [(Math/round n)]))))))

(defn- active-duration
  "A session's active time as the metrics panel writes it: `45s`, `12m`, `1.5h`, `2.0d`."
  [x]
  (let [ms (metric-number x)]
    (if (or (not (Double/isFinite ms)) (<= ms 0.0))
      "0s"
      (let [seconds (Math/round (/ ms 1000.0))
            minutes (/ (double seconds) 60.0)
            hours (/ minutes 60.0)]

        (cond (< seconds 60) (str seconds "s")
              (< minutes 60.0) (str (Math/round minutes) "m")
              (< hours 24.0) (str (fixed hours 1) "h")
              :else (str (fixed (/ hours 24.0) 1) "d"))))))

(defn metrics-markdown
  "The closed Metrics section of a Markdown export: the numbers and labels of the metrics panel
   under the METRICS row of the HTML export. `usage` is the session's usage rollup, or nil."
  [session usage]
  (if-not usage
    "<details>\n<summary>Metrics</summary>\n\n_No measured calls yet._\n\n</details>\n\n"
    (let [subagents
          (get usage "subagents")

          pct
          (fn [k]
            (if (number? (get usage k)) (str (get usage k) "%") "—"))

          rows
          (concat
            [["Total input" (compact-count (get usage "input_tokens"))]
             ["Total output" (compact-count (get usage "output_tokens"))]
             [(if subagents "Session cost" "Cost") (usd (get usage "cost_usd"))]
             ["Folds" (compact-count (get usage "fold_count"))]
             ["Turns" (compact-count (get usage "turn_count"))]
             ["Calls" (compact-count (get usage "iteration_count"))]
             ["Tools" (compact-count (get usage "tool_call_count"))]
             ["Tool failures" (compact-count (get usage "tool_failure_count"))]]
            (when subagents
              [["Subagents"
                (str (compact-count (get subagents "agent_count"))
                     " · "
                     (compact-count (get subagents "active_count"))
                     " active")]
               ["Subagent calls" (compact-count (get subagents "iteration_count"))]
               ["Subagent input" (compact-count (get subagents "input_tokens"))]
               ["Subagent output" (compact-count (get subagents "output_tokens"))]
               ["Subagent cost" (usd (get subagents "cost_usd"))]
               ["Total cost" (usd (get usage "total_cost_usd"))]])
            [["Cached input" (pct "cache_read_share_percent")]
             ["Reuse coverage"
              (let [v (pct "reusable_prefix_coverage_percent")]
                (if (and (get usage "reusable_prefix_estimated") (not= "—" v)) (str "≈" v) v))]
             ["Model"
              (or (not-empty (str (get usage "model")))
                  (not-empty (str (get-in session ["model_pref" "model"])))
                  (not-empty (str (get session "model")))
                  "—")] ["Active" (active-duration (get usage "duration_ms"))]])]

      (str "<details>\n<summary>Metrics</summary>\n\n| Metric | Value |\n| --- | --- |\n"
           (apply str
             (map (fn [[k v]]
                    (str "| " k " | " (str/replace v "|" "\\|") " |\n"))
                  rows))
           "\n</details>\n\n"))))

(defn- failure-line
  "The first line of the error of a failed tool call, as the Failed tools list shows it."
  [error]
  (let [text (cond (string? error) error
                   (string? (get error "message")) (get error "message")
                   :else (wire/json-str error))]
    (or (some #(not-empty (str/trim %)) (str/split-lines text)) "")))

(defn- form-code
  "The code of `form` as the session screen shows it under CODE."
  [form]
  (str/trim (str (some #(when (string? %) %)
                       (map #(get form %) ["display_code" "code" "source" "src"])))))

(defn failed-tools-markdown
  "The closed Failed tools section of a Markdown export, or an empty string when no call failed.
   Each failed call shows its step, the first line of its error and its code. A call fails when it
   names a tool and returns an error, which is the rule of `tool_failure_count`, so the list and
   the count agree."
  [turns]
  (let [rows
        (for [[index turn] (map-indexed vector turns)
              iteration (get turn "iterations")
              form (get iteration "forms")
              :when (and (not (str/blank? (str (get form "tool_name")))) (some? (get form "error")))]

          {:scope (or (not-empty (str (get form "scope"))) (str "t" (inc (long index))))
           :message (failure-line (get form "error"))
           :code (form-code form)
           :language (or (not-empty (str/trim (str (get form "display_language")))) "python")})]
    (if (empty? rows)
      ""
      (str "<details>\n<summary>Failed tools ("
           (count rows)
           ")</summary>\n\n"
           (apply str
             (map (fn [{:keys [scope message code language]}]
                    (str "**" scope
                         "** · " (md-escape message)
                         "\n\n" (when-not (str/blank? code)
                                  (str (util/fenced code language) "\n\n"))))
                  rows))
           "</details>\n\n"))))

(defn- noun [n word] (str n " " word (when (not= 1 n) "s")))

(defn- interrupted?
  "True when the error of `form` is an interruption, which a step counts as cancelled."
  [form]
  (let [error (get form "error")]
    (if (map? error)
      (or (contains? #{"vis/interrupted" ":vis/interrupted"} (get error "type"))
          (= "java.lang.InterruptedException" (get error "message")))
      (= "java.lang.InterruptedException" error))))

(defn- settled-state
  "The state of an Activity row of a finished turn. A row still running takes the outcome of
   its form: failed after an error, else cancelled."
  [form row]
  (let [state (get row "state")]
    (if (= "running" state)
      (if (and (get form "error") (not (interrupted? form))) "failed" "cancelled")
      state)))

(defn digest-summary
  "The digest row of the steps of one note, as the session screen's Compact mode writes it:
   the Activity tally (`3 mutations · 5 observations`), then the failed and cancelled counts.
   Without Activity it says `RAW EXECUTION`, or `THINKING` when no step ran code. The measured
   time of the forms follows."
  [forms]
  (let [rows
        (mapcat #(get-in % ["activity" "rows"]) forms)

        tally
        (frequencies (keep #(get % "signal") rows))

        states
        (frequencies (for [form
                           forms

                           row
                           (get-in form ["activity" "rows"])]

                       (settled-state form row)))

        unreported
        (count (filter (fn [form]
                         (and (get form "error")
                              (not-any? #(#{"failed" "cancelled"} (settled-state form %))
                                        (get-in form ["activity" "rows"]))))
                       forms))

        states
        (cond-> states
          (pos? unreported)
          (update "failed" (fnil + 0) unreported))

        parts
        (concat (for [[signal word]
                      [["mutation" "mutation"] ["observation" "observation"]
                       ["verification" "verification"] ["external" "external action"]]

                      :let [n
                            (get tally signal 0)]
                      :when (pos? (long n))]

                  (noun n word))
                (for [state
                      ["failed" "cancelled"]

                      :let [n
                            (get states state 0)]
                      :when (pos? (long n))]

                  (str n " " state)))

        measured
        (reduce +
                0
                (keep #(let [ms (get % "duration_ms")] (when (and (number? ms)
                                                                  (not (neg? (double ms))))
                                                         (long ms)))
                      forms))]

    (str/join " · "
              (cons (cond (seq parts) (str/join " · " parts)
                          (seq forms) "RAW EXECUTION"
                          :else "THINKING")
                    (when (pos? measured) [(web-duration measured)])))))

(defn- answered
  "The trimmed prose of the answer blocks of `turn`. A note that repeats it stays out of the steps."
  [turn]
  (into #{}
        (keep #(when (= "prose" (get % "type")) (not-empty (str/trim (str (get % "markdown"))))))
        (get turn "content")))

(defn- step-segments
  "The steps of `turn` in runs, as Compact mode groups them: a note or a message sent into the
   turn opens a run, and the steps without one join the open run."
  [turn]
  (let [said (answered turn)]
    (reduce (fn [runs iteration]
              (let [prose (str/trim (str (get iteration "assistant_prose")))
                    step {:prose (when-not (or (str/blank? prose) (said prose)) prose)
                          :thinking (not-empty (str/trim (str (get iteration "thinking"))))
                          :forms (vec (get iteration "forms"))
                          :attachments (vec (get iteration "attachments"))
                          :inputs (vec (get iteration "user_input"))}]

                (cond (not (some step [:prose :thinking]))
                      (if (some seq ((juxt :forms :attachments :inputs) step))
                        (if (or (empty? runs) (seq (:inputs step)))
                          (conj runs [step])
                          (update runs (dec (count runs)) conj step))
                        runs)
                      (or (empty? runs) (:prose step) (seq (:inputs step))) (conj runs [step])
                      :else (update runs (dec (count runs)) conj step))))
            []
            (get turn "iterations"))))

(defn- quoted [text] (str/join "\n" (map #(str "> " %) (str/split-lines text))))

(defn- activity-line
  "One Activity row as a list item: its headline, its result and a state that is not success."
  [form row]
  (let [presentation
        (get row "presentation")

        headline
        (or (not-empty (get presentation "headline")) (get row "summary") (get row "operation"))

        detail
        (or (not-empty (get row "error_summary")) (get presentation "summary"))

        state
        (settled-state form row)]

    (str "- "
         headline
         (when (and (not (str/blank? detail)) (not= detail headline)) (str " — " detail))
         (when (#{"failed" "cancelled"} state) (str " _(" state ")_")))))

(defn- error-text
  [error]
  (if (map? error) (or (get error "message") (wire/json-str error)) (str error)))

(defn- form-markdown
  "One form of an open digest: its source, then its Activity rows, or its output when it has
   none, then its error."
  [form]
  (let [rows
        (get-in form ["activity" "rows"])

        out
        (strip-ansi (get form "stdout"))]

    (str (when-not (str/blank? (get form "src"))
           (str (util/fenced (str/trimr (get form "src")) "python") "\n\n"))
         (when (seq rows) (str (str/join "\n" (map #(activity-line form %) rows)) "\n\n"))
         (when (and (empty? rows) (not (str/blank? out)))
           (str (util/fenced (str/trimr out) "text") "\n\n"))
         (when-let [error (get form "error")]
           (str "**Error**\n\n" (util/fenced (error-text error) "text") "\n\n")))))

(defn- attachment-line
  [attachment]
  (str "- Attachment: `" (or (get attachment "filename") (get attachment "kind") "file")
       "`" (when-let [type (get attachment "media_type")]
             (str " · " type))))

(defn- run-markdown
  "One run of steps: its note, the messages sent into it, one closed digest, then its artifacts."
  [run]
  (let [{:keys [prose inputs]}
        (first run)

        forms
        (vec (mapcat :forms run))

        thinking
        (keep :thinking run)

        attachments
        (mapcat :attachments run)]

    (str (apply str
           (for [input inputs]
             (str "**You →** "
                  (or (not-empty (get input "display_request")) (get input "request"))
                  "\n\n")))
         (when prose (str prose "\n\n"))
         (when (or (seq forms) (seq thinking))
           (str "<details>\n<summary>▸ "
                (md-escape (digest-summary forms))
                "</summary>\n\n"
                (when (seq thinking) (str (quoted (str/join "\n\n" thinking)) "\n\n"))
                (apply str (map form-markdown forms))
                "</details>\n\n"))
         (when (seq attachments) (str (str/join "\n" (map attachment-line attachments)) "\n\n")))))

(defn- turn-markdown
  [{:keys [agent-name children]} turn]
  (let [request
        (str/trim (str (or (get turn "display_request") (get turn "request"))))

        hidden?
        (and (= "council" (get turn "request_kind"))
             (contains? children (get-in turn ["council" "author_session_id"])))

        answer
        (str/join "\n\n"
                  (keep #(when (= "prose" (get % "type"))
                           (not-empty (str/trim (str (get % "markdown")))))
                        (get turn "content")))

        footer
        (str/join " · "
                  (remove str/blank?
                    [(get turn "meta_summary") (web-duration (get turn "duration_ms"))]))]

    (str (when-not hidden?
           (str "## You\n\n" (if (str/blank? request) "_(empty)_" request)
                "\n\n" (when-let [attachments (seq (get turn "attachments"))]
                         (str (str/join "\n" (map attachment-line attachments)) "\n\n"))))
         "## "
         agent-name
         "\n\n"
         (apply str (map run-markdown (step-segments turn)))
         (when-not (str/blank? answer) (str answer "\n\n"))
         (when-not (str/blank? footer) (str "_" footer "_\n\n")))))

(defn markdown
  "The Markdown export of the export document `data`: each turn as Compact mode shows it. A
   run of steps is its note, then one closed digest that holds the thinking, code and
   Activity of its steps. Artifacts are named, not inlined."
  ^String [data]
  (let [{:strs [session turns]}
        data

        title
        (or (not-empty (get session "title")) "Session")

        context
        {:agent-name (or (not-empty (get session "agent_name")) "Vis")
         :children (set (get data "child_session_ids"))}]

    (str "# "
         title
         "\n\n"
         "_Session `"
         (get session "id")
         "` · "
         (noun (count turns) "turn")
         "_\n\n"
         (metrics-markdown session (get data "usage"))
         (failed-tools-markdown turns)
         (if (seq turns)
           (str/join "---\n\n" (map #(turn-markdown context %) turns))
           "_This session has no turns._\n"))))

(defn md
  "The Markdown export of session `sid`, or nil when no session has that id."
  ^String [sid]
  (some-> (export-data sid)
          markdown))
