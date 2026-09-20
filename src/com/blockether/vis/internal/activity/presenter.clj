(ns com.blockether.vis.internal.activity.presenter
  "Closed semantic presenter registry for Activity rows. Presenters return data,
   never channel markup, and never inspect Python source."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.activity :as contract]))

(def presenters contract/presenters)

(defn presenter-for
  "The explicitly declared presenter, or the bounded generic fallback."
  [_operation declared]
  (if (contains? presenters declared) declared :generic))

(defn classification
  "The symbol entry's explicit tag, or generic when no declaration exists."
  [event]
  (or (:classification event) :generic))

(defn row-summary
  "Bounded already-redacted summary selected from semantic event fields. A shell
   spawn is durable command evidence, not a live-tense status: its ticker phrase
   `running: <command>` becomes the command itself in Activity."
  [{:keys [presenter phrase] :as event}]
  (or (when (and (= :shell presenter) (string? phrase) (str/starts-with? phrase "running: "))
        (subs phrase (count "running: ")))
      phrase
      (:label event)
      (:result-summary event)
      (:error-summary event)
      (some-> (:operation event)
              name)))

;; Presentation primitives shared with compiled-in packs. `vis.core` re-exports
;; them under `activity-*` names so a library that owns a tool binding builds the
;; same evidence shapes as a built-in, without reaching into this namespace.
(defn field [m k] (when (map? m) (get m k (get m (keyword k)))))

(defn label
  [value]
  (-> (if (keyword? value) (name value) (str value))
      (str/replace #"[_-]" " ")
      str/capitalize))

(defn scalar
  [value]
  (cond (nil? value) "None"
        (keyword? value) (name value)
        :else (str value)))

(defn- text-block
  [key text]
  {"type"
   (cond (contains? #{"content" "body" "description" "documentation" "message"} key) "markdown"
         (contains? #{"out" "err" "stdout" "stderr" "output" "code" "source" "log_tail"} key) "code"
         :else "text")
   "text" text})

(defn visible-result
  [value]
  (cond (map? value) (into {}
                           (keep (fn [[k v]]
                                   (let [key-name
                                         (str/replace (scalar k) "-" "_")

                                         clean
                                         (visible-result v)]

                                     (when-not (or (contains? #{"op" "id" "is_pass" "log_path"
                                                                "argument_key" "source_ref"
                                                                "idempotency_key"}
                                                              key-name)
                                                   (str/ends-with? key-name "_id")
                                                   (str/ends-with? key-name "_ids")
                                                   (nil? clean)
                                                   (and (coll? clean) (empty? clean)))
                                       [k clean]))))
                           value)
        (sequential? value) (vec (keep visible-result value))
        (and (string? value) (re-matches #"(?i)[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}" value))
        nil
        :else value))

(defn- shell-presentation
  [value]
  (let [command
        (field value "command")

        exit
        (field value "exit")

        running?
        (= "running" (field value "status"))]

    {"headline" (if running? "Running command" "Command finished")
     "summary" (or command "")
     "content" (vec (concat (when (seq command)
                              [{"type" "heading" "text" "Command"}
                               {"type" "code" "language" "bash" "text" command}])
                            (mapcat (fn [[key title]]
                                      (when-let [text (not-empty (field value key))]
                                        [{"type" "heading" "text" title}
                                         {"type" "code" "text" text}]))
                                    [["out" "Output"] ["stdout" "Output"] ["err" "Stderr"]
                                     ["stderr" "Stderr"]])
                            [{"type" "text"
                              "text" (cond (some? exit) (str "Exit code: " exit)
                                           running? "Running"
                                           :else "Exit code unavailable")}]))}))

(defn result-blocks
  "Keep metadata in readable text; tables are reserved for comparable records."
  [value]
  (cond (or (nil? value) (and (coll? value) (empty? value))) []
        (map? value)
        (let [entries
              (sort-by (comp str key) (dissoc value :op "op"))

              short?
              (fn [[k v]]
                (and (not (contains? #{"content" "body" "description" "documentation" "message"
                                       "out" "err" "stdout" "stderr" "output" "code" "source"
                                       "log_tail"}
                                     (scalar k)))
                     (not (coll? v))
                     (<= (count (scalar v)) 256)
                     (not (str/includes? (scalar v) "\n"))))

              fields
              (filter short? entries)

              bodies
              (remove short? entries)]

          (into (if (seq fields)
                  [{"type" "text"
                    "text" (str/join " · "
                                     (map (fn [[k v]]
                                            (str (label k) ": " (scalar v)))
                                          fields))}]
                  [])
                (mapcat (fn [[k v]]
                          (cons {"type" "heading" "text" (label k)}
                                (if (string? v) [(text-block (scalar k) v)] (result-blocks v))))
                        bodies)))
        (sequential? value) (if (every? #(not (coll? %)) value)
                              [{"type" "text" "text" (str/join "\n" (map scalar value))}]
                              (vec (mapcat result-blocks value)))
        (string? value) [{"type" "text" "text" value}]
        :else [{"type" "text" "text" (scalar value)}]))

(defn- code-language
  [path]
  (get {"clj" "clojure"
        "cljc" "clojure"
        "cljs" "clojure"
        "edn" "clojure"
        "py" "python"
        "ts" "typescript"
        "tsx" "tsx"
        "js" "javascript"
        "jsx" "jsx"
        "json" "json"
        "yaml" "yaml"
        "yml" "yaml"
        "toml" "toml"
        "sh" "bash"
        "md" "markdown"
        "css" "css"
        "html" "html"
        "rs" "rust"
        "java" "java"}
       (last (str/split (str path) #"\."))
       "text"))

(defn- read-content [text] (str/replace text #"(?m)^(\d+):[0-9a-f]+│ ?" "$1 │ "))

(def ^:private tool-headlines
  "Each built-in owns its start headline, settled headline and start visibility."
  {"cat" ["Read file" "Read" false]
   "patch" ["Patch file" "Patched" false]
   "grep" ["Search files" "Searched" true]
   "doc" ["Read documentation" "Read documentation" false]
   "apropos" ["Find symbols" "Found symbols" false]
   "defs" ["Read definitions" "Read definitions" false]
   "shell" ["Run command" "Started command" true]
   "_shell-logs" ["Read command output" "Read command output" false]
   "_shell-wait" ["Wait for command" "Command finished" true]
   "_shell-type" ["Send command input" "Sent command input" false]
   "_shell-stop" ["Stop command" "Stopped command" true]
   "council.publish" ["Publish Council message" "Published Council message" false]
   "council.read" ["Read thread" "Read thread" false]
   "council.get" ["Read Council message" "Read Council message" false]
   "council.threads" ["List threads" "Listed threads" false]
   "council.members" ["List members" "Listed members" false]
   "council.publish_spawn" ["Spawn subagent" "Spawned subagent" true]
   "council.subagents" ["List subagents" "Listed subagents" false]
   "council.cancel" ["Cancel subagent" "Cancelled subagent" false]
   "council.route" ["Choose agent model" "Chose agent model" false]
   "read_session" ["Read session" "Read session" true]
   "get_session" ["Inspect session" "Inspected session" true]
   "list_sessions" ["Find sessions" "Found sessions" true]
   "draft_status" ["Check draft status" "Checked draft status" false]
   "draft_diff" ["Capture draft diff" "Captured draft diff" true]
   "draft_create" ["Create draft" "Created draft" true]
   "draft_sync" ["Synchronize draft" "Draft synchronization updated" true]
   "draft_approve" ["Approve draft" "Approved draft" true]
   "draft_discard" ["Discard draft" "Discarded draft" true]
   "main_agent_instructions" ["Read agent instructions" "Read agent instructions" false]
   "update_goal" ["Update goal" "Updated goal" false]
   "mcp__call" ["Call MCP tool" "Called MCP tool" true]})

(def ^:private result-fields
  {"get_session" ["title" "goal" "turn_count" "model" "last_turn"]
   "list_sessions" ["title" "goal" "turn_count" "request_snippet" "reply_snippet"]
   "draft_approve" ["files"]
   "update_goal" ["objective" "reason"]
   "mcp__call" ["content" "tools"]})

(defn select-result
  [fields value]
  (cond (map? value) (into {}
                           (keep (fn [key]
                                   (when-some [v (field value key)]
                                     [key v])))
                           fields)
        (sequential? value) (mapv #(select-result fields %) value)
        :else value))

(defn counted-label [n singular] (str n " " singular (when (not= n 1) "s")))

(defn session-preview
  [value limit]
  (let [limit
        (long limit)

        lines
        (str/split (str/trim (str value)) #"\R" 2)

        ^String text
        (str/replace (or (first lines) "") #"\s+" " ")

        characters
        (.codePointCount text 0 (.length text))]

    (str (subs text 0 (.offsetByCodePoints text 0 (int (min characters limit))))
         (when (or (> characters limit) (next lines)) "…"))))

(defn summary-line [parts] (str/join " · " (remove str/blank? parts)))

(defn- draft-summary
  [op value]
  (let [branch
        (field value "branch")

        target
        (field value "target_branch")

        pending
        (field value "pending")

        ahead
        (field value "ahead")

        kept
        (field value "approved_ahead")

        files
        (field value "files")]

    (case op
      "draft_status"
      (cond (true? (field value "recovery_required")) (summary-line ["Draft recovery required"
                                                                     (field value "recovery_hint")])
            (false? (field value "in_draft")) "No active draft"
            (true? (field value "in_draft")) (summary-line
                                               [(str branch (when target (str " → " target)))
                                                (when (number? pending)
                                                  (if (zero? (long pending))
                                                    "No pending changes"
                                                    (counted-label pending "pending file")))
                                                (when (and (number? ahead) (pos? (long ahead)))
                                                  (str (counted-label ahead "commit") " ahead"))])
            :else "No draft status")

      "draft_create"
      (if branch
        (summary-line [(str branch (when target (str " → " target)))
                       (case (field value "clean")
                         true
                         "Clean snapshot"

                         false
                         "Includes pending changes"

                         nil)])
        "No draft result")

      "draft_approve"
      (if target
        (summary-line
          [(when (= "nothing-to-approve" (scalar (field value "status"))) "Nothing to approve")
           (str (if (true? (field value "published")) "Published to " "Approved locally on ")
                target) (when (seq files) (counted-label (count files) "file"))])
        "No draft result")

      "draft_sync"
      (if-let [status (field value "status")]
        (let [repositories (field value "repositories")
              conflicts (reduce + 0 (map #(count (field % "conflicts")) repositories))]

          (summary-line [(case (scalar status)
                           "synced"
                           "Synchronized"

                           "conflicts"
                           "Resolve conflicts"

                           "aborted"
                           "Synchronization aborted"

                           "partial"
                           "Partially synchronized"

                           (scalar status))
                         (when (seq repositories)
                           (str (count repositories)
                                (if (= 1 (count repositories)) " repository" " repositories")))
                         (when (pos? (long conflicts)) (counted-label conflicts "conflict path"))]))
        "No draft result")

      "draft_diff"
      (if-let [filename (field value "filename")]
        (summary-line [filename
                       (when-let [n (field value "repository_count")]
                         (str n (if (= 1 n) " repository" " repositories")))
                       (if (true? (field value "empty")) "No changes" "Diff attached")])
        "No draft result")

      "draft_discard"
      (if-let [target (field value "root")]
        (summary-line [(field value "label") (str "Returned to " target)
                       (when (and (number? kept) (pos? (long kept)))
                         (str (counted-label kept "approved commit") " kept"))
                       (when-let [preserved (field value "preserved_root")]
                         (str "Preserved " preserved))])
        "No draft result"))))

(defn- council-body
  [value key]
  (if-let [text (not-empty (field value key))]
    [{"type" "markdown" "text" text}]
    []))

(defn- council-replies
  [value]
  (when-let [states (seq (frequencies (keep #(field % "state") (field value "replies"))))]
    (str "Replies: "
         (str/join " · "
                   (map (fn [[state n]]
                          (str n
                               " "
                               (if (= "replied" (scalar state))
                                 "received"
                                 (str/replace (scalar state) "_" " "))))
                        (sort-by (comp scalar key) states))))))

(defn- council-message
  [value]
  {"headline" (session-preview (or (not-empty (field value "title"))
                                   (when (field value "reply_to") "Reply")
                                   "Message")
                               96)
   "summary" (or (council-replies value) "")
   "content" (council-body value "content")})

(defn- council-agent
  [value]
  {"headline" (session-preview (or (not-empty (field value "task")) "Subagent") 96)
   "summary" (str/join " · "
                       (remove str/blank?
                         [(label (field value "status")) (field value "model")
                          (when-let [budget (field value "iteration_budget")]
                            (if-let [used (field value "iterations_used")]
                              (str "Iterations: " used "/" budget)
                              (str "Up to " (counted-label budget "iteration"))))
                          (when (field value "pending_input") "Input pending")]))
   "content" (council-body value "task")})

(defn- council-presentation
  "Show messages and outcomes, not Council's storage and delivery envelopes."
  [op value]
  (let [headline (second (get tool-headlines op))]
    (case op
      ("council.publish" "council.get")
      {"headline" headline
       "summary" (if (not-empty (field value "content")) "" "No message content")
       "content" (council-body value "content")}

      ("council.read" "council.threads" "council.members" "council.subagents")
      (let [items
            (if (contains? #{"council.read" "council.threads"} op) (field value "entries") value)
            noun ({"council.read" "message"
                   "council.threads" "thread"
                   "council.members" "member"
                   "council.subagents" "subagent"}
                  op)
            sections? (contains? #{"council.read" "council.subagents"} op)]

        (cond-> {"headline" headline
                 "summary" (str (if (seq items)
                                  (counted-label (count items) noun)
                                  (str "No " (when (= op "council.members") "active ") noun "s"))
                                (when (field value "has_more") " · more available"))
                 "content" (if (and (seq items) (not sections?))
                             [{"type" "table"
                               "columns" (if (= op "council.members") ["Member" "State"] ["Thread"])
                               "rows" (mapv (fn [item]
                                              (cond-> [(or (not-empty (field item "title"))
                                                           (label noun))]
                                                (= op "council.members")
                                                (conj (label (field item "state")))))
                                            items)}]
                             [])}
          sections?
          (assoc "sections" (mapv (if (= op "council.read") council-message council-agent) items))))

      "council.publish_spawn"
      (let [agent (council-agent value)]
        (assoc agent
          "headline" headline
          "summary" (str/join " · "
                              (remove str/blank? [(get agent "headline") (get agent "summary")]))))

      "council.cancel"
      {"headline" headline
       "summary" (str (label (field value "status"))
                      (when-let [cancelled (field value "cancelled")]
                        (str " · " (counted-label (count cancelled) "subagent"))))
       "content" []}

      "council.route"
      {"headline" headline
       "summary" (str (field value "provider")
                      "/"
                      (field value "model")
                      (when-let [effective (field value "effective")]
                        (str " · " (str/replace (scalar effective) "_" " "))))
       "content" []})))

(defn- read-session-presentation
  "A compact overview with complete requests and unique failures behind section disclosure."
  [value]
  (let [session
        (field value "session")

        current
        (field value "current_turn")

        diagnosis
        (field value "diagnosis")

        totals
        (field (field value "usage") "totals")

        tokens
        (field totals "tokens")

        cost
        (field current "cost")

        turns
        (vec (field session "turns"))

        turns
        (cond-> turns
          (and (field current "user_request")
               (not-any? #(= (field current "id") (field % "id")) turns))
          (conj (assoc current "outcome" (field current "status"))))

        numbered
        (mapv vector (range 1 (inc (count turns))) turns)

        turn-numbers
        (into {}
              (map (fn [[n turn]]
                     [(field turn "id") n])
                   numbered))

        failures
        (vec (distinct (or (field value "failures")
                           (field diagnosis "failures")
                           (field current "failures"))))

        display
        (fn [v]
          (if (number? v) (str v) "Unavailable"))

        metric
        (fn [k]
          (display (if (= k "regular")
                     (let [uncached
                           (field tokens "uncached")

                           writes
                           (field tokens "cache_created")]

                       (when (and (number? uncached) (number? writes))
                         (max 0 (- (long uncached) (long writes)))))
                     (field tokens k))))

        metrics
        [["Input tokens" "input" "input_tokens"]
         ["Cache read tokens" "cached" "input_cache_read_tokens"]
         ["Cache write tokens" "cache_created" "input_cache_write_tokens"]
         ["Regular input tokens" "regular" "input_regular_tokens"]
         ["Output tokens" "output" "output_tokens"]
         ["Reasoning tokens" "reasoning" "output_reasoning_tokens"]]

        heading
        (fn [text]
          {"type" "heading" "text" text})

        text
        (fn [body]
          {"type" "text" "text" body})

        status
        (fn [turn]
          (label (or (field turn "outcome") (field turn "status") "unknown")))

        diagnosis-summary
        (str (counted-label (count failures) "failure")
             (when (field diagnosis "repetition_loop") " · repeated failures detected"))

        present?
        (or session current totals (seq failures))]

    {"headline" "Read session"
     "summary" (if present?
                 (str (session-preview (or (field session "title") "Session") 80)
                      " · " (counted-label (count turns) "turn")
                      " · " diagnosis-summary)
                 "No session data")
     "content" (vec
                 (concat (when current
                           [(heading "Current turn")
                            (text (str (status current)
                                       (when-let [n (get turn-numbers (field current "id"))]
                                         (str " · Turn " n))
                                       (when-let [n (field (field current "iteration") "current")]
                                         (str " · iteration " n))
                                       (when-some [elapsed (field current "elapsed_ms")]
                                         (str " · " elapsed " ms"))))])
                         (when (or totals cost)
                           [(heading "Usage")
                            (text (if (map? tokens)
                                    (str "Session · input " (metric "input")
                                         " · output " (metric "output")
                                         " · cost $" (display (field totals "cost_usd")))
                                    "Session usage unavailable"))
                            (text (if (map? tokens)
                                    (str "Cache read " (metric "cached")
                                         " · cache write " (metric "cache_created")
                                         " · uncached input " (metric "uncached")
                                         " · reasoning " (metric "reasoning"))
                                    "Open Session details for current-turn usage."))])
                         (when present? [(heading "Diagnosis") (text diagnosis-summary)])
                         (when (seq turns)
                           (concat [(heading "Turns")]
                                   (when (> (count turns) 6)
                                     [(text (str "Showing the latest 6 of "
                                                 (count turns)
                                                 " turns. Open Turn details for all turns."))])
                                   [{"type" "table"
                                     "columns" ["Turn" "Outcome" "Request"]
                                     "rows" (mapv (fn [[n turn]]
                                                    [(str "Turn " n) (status turn)
                                                     (session-preview (field turn "user_request")
                                                                      48)])
                                                  (take-last 6 numbered))}]))))
     "sections"
     (vec
       (concat
         (when present?
           [{"headline" "Session details"
             "summary" "Metadata and complete usage"
             "content"
             (vec (concat (result-blocks (visible-result (select-result ["title" "goal" "model"
                                                                         "provider" "created_at"]
                                                                        session)))
                          (when (or totals cost)
                            [(heading "Usage breakdown")
                             {"type" "table"
                              "columns" ["Metric" "Session" "Current turn"]
                              "rows" (conj (mapv (fn [[title total-key current-key]]
                                                   [title (metric total-key)
                                                    (display (field cost current-key))])
                                                 metrics)
                                           ["Cost (USD)" (display (field totals "cost_usd"))
                                            (display (field cost "total_cost"))])}])
                          (when (seq (field diagnosis "next_actions"))
                            [(heading "Next actions")
                             (text (str/join "\n" (field diagnosis "next_actions")))])))}])
         (when (seq turns)
           [{"headline" "Turn details"
             "summary" (str (counted-label (count turns) "turn") " · full requests and answers")
             "content" (vec (mapcat (fn [[n turn]]
                                      (concat [(heading (str "Turn " n " · " (status turn)))]
                                              (when-let [request (field turn "user_request")]
                                                [(text request)])
                                              (when-let [answer (field turn "answer")]
                                                [(heading "Answer")
                                                 {"type" "markdown" "text" answer}])))
                                    numbered))}])
         (when (seq failures)
           [{"headline" "Failure details"
             "summary" (str diagnosis-summary " · message and code")
             "content"
             (vec (mapcat (fn [n failure]
                            (let [turn
                                  (get turn-numbers (field failure "turn_id"))

                                  shared-request?
                                  (and turn
                                       (= (field failure "user_request")
                                          (field (get turns (dec (long turn))) "user_request")))]

                              (cons (heading (str "Failure " n (when turn (str " · Turn " turn))))
                                    (result-blocks (visible-result (cond-> failure
                                                                     shared-request?
                                                                     (dissoc "user_request"
                                                                       :user_request)))))))
                          (range 1 (inc (count failures)))
                          failures))}])))}))

(defn result-presentation
  "Result view selected explicitly by a built-in binding. Unknown tools have no
   default view. Evidence is public, redacted and bounded by the event owner.
   Displayed paths and all body text come from the bounded public value."
  [{:keys [operation] :as details} value]
  (when (contains? tool-headlines (name operation))
    (let [op
          (name operation)

          text
          (when (string? value) value)

          path
          (or (:label details) "")

          read-lines
          (when (and (= op "cat") text) (map second (re-seq #"(?m)^(\d+):[0-9a-f]+│" text)))

          headline
          (second (get tool-headlines op))

          summary
          (cond (= op "cat") (str path
                                  (when (seq read-lines)
                                    (str " · lines " (first read-lines) "–" (last read-lines))))
                (contains? #{"doc" "defs" "patch" "shell"} op) path
                (= op "grep") (or (first (str/split-lines (or text ""))) "")
                (str/starts-with? op "draft_") (draft-summary op value)
                (= op "update_goal")
                (summary-line [(if-let [status (field value "status")]
                                 (label status)
                                 "No goal result") (session-preview (field value "objective") 120)])
                (= op "list_sessions")
                (if (seq value) (counted-label (count value) "session") "No sessions found")
                (= op "mcp__call") (summary-line [(field value "server") (field value "tool")
                                                  (when (true? (field value "is_error"))
                                                    "Tool reported an error")])
                :else (str (or (field value "summary") (field value "title") "")))

          content
          (cond (contains? #{"patch" "read_session"} op) []
                (str/starts-with? op "council.") []
                (and (= op "cat") text)
                [{"type" "code" "language" (code-language path) "text" (read-content text)}]
                (and (contains? #{"doc" "main_agent_instructions"} op) text) [{"type" "markdown"
                                                                               "text" text}]
                (and (= op "defs") text) [{"type" "code" "language" "python" "text" text}]
                (and (= op "grep") text)
                [{"type" "code" "text" (str/replace text #"(?m)^(\s*\d+):[0-9a-f]+│ ?" "$1 │ ")}]
                (contains? #{"draft_status" "draft_create" "draft_discard" "draft_diff"} op) []
                :else (result-blocks (visible-result
                                       (let [public
                                             (if (map? value)
                                               (dissoc value :title "title" :summary "summary")
                                               value)]
                                         (if-let [fields (get result-fields op)]
                                           (select-result fields public)
                                           public)))))]

      (cond (= op "read_session") (read-session-presentation value)
            (str/starts-with? op "council.") (council-presentation op value)
            (or (= op "shell") (str/starts-with? op "_shell-")) (shell-presentation value)
            :else {"headline" headline "summary" summary "content" content}))))

(defn for-tool
  "Declare a built-in's presentation at its binding. Unknown operations are refused."
  [operation]
  (let [op (name operation)]
    (when-not (contains? tool-headlines op)
      (throw (ex-info "Missing built-in Activity presentation" {:operation operation})))
    {:headline (first (get tool-headlines op))
     :show-start (nth (get tool-headlines op) 2)
     :render (fn [details value]
               (result-presentation (assoc details :operation operation) value))}))
