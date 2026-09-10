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
   `running: <command>` becomes `cmd: <command>` in Activity."
  [{:keys [presenter phrase] :as event}]
  (or (when (and (= :shell presenter) (string? phrase) (str/starts-with? phrase "running: "))
        (str "cmd: " (subs phrase (count "running: "))))
      phrase
      (:label event)
      (:result-summary event)
      (:error-summary event)
      (some-> (:operation event)
              name)))

(defn- field [m k] (when (map? m) (get m k (get m (keyword k)))))

(defn- label
  [value]
  (-> (if (keyword? value) (name value) (str value))
      (str/replace #"[_-]" " ")
      str/capitalize))

(defn- scalar
  [value]
  (cond (nil? value) "None"
        (keyword? value) (name value)
        :else (str value)))

(defn- text-block
  [key text]
  {"type" (cond (contains? #{"content" "body" "description" "documentation" "message"} key)
                "markdown"
                (contains? #{"out" "err" "stdout" "stderr" "output" "code" "source"} key) "code"
                :else "text")
   "text" text})

(defn- visible-result
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

(defn- result-blocks
  "Render bounded public data with contextual scalar-table headings. Nested maps
   use generic detail headings rather than inheriting top-level metric labels."
  [value table-label]
  (cond (nil? value) []
        (map? value) (let [entries
                           (sort-by (comp str key) (dissoc value :op "op"))

                           short?
                           (fn [[k v]]
                             (and (not (contains? #{"content" "body" "description" "documentation"
                                                    "message" "out" "err" "stdout" "stderr" "output"
                                                    "code" "source"}
                                                  (scalar k)))
                                  (not (coll? v))
                                  (<= (count (scalar v)) 256)
                                  (not (str/includes? (scalar v) "\n"))))

                           fields
                           (filter short? entries)

                           bodies
                           (remove short? entries)]

                       (into (if (seq fields)
                               [{"type" "table"
                                 "columns" [table-label "Result"]
                                 "rows" (mapv (fn [[k v]]
                                                [(label k) (scalar v)])
                                              fields)}]
                               [])
                             (mapcat (fn [[k v]]
                                       (cons {"type" "heading" "text" (label k)}
                                             (if (string? v)
                                               [(text-block (scalar k) v)]
                                               (result-blocks v "Detail"))))
                                     bodies)))
        (sequential? value) (if (every? #(not (coll? %)) value)
                              [{"type" "text" "text" (str/join "\n" (map scalar value))}]
                              (vec (mapcat #(result-blocks % table-label) value)))
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

(defn- repl-presentation
  [value]
  (let [language
        (or (field value "language") "text")

        code
        (field value "code")

        status
        (field value "status")

        statuses
        (if (coll? status) (set status) #{status})

        timeout?
        (or (true? (field value "timed_out")) (contains? statuses "timeout"))

        error-text
        (some #(let [v (field value %)] (when (and (string? v) (not (str/blank? v))) v))
              ["exc" "error_message" "ex" "root_ex"])

        error?
        (or error-text (false? (field value "ok")) (contains? statuses "eval-error"))

        values
        (field value "values")

        result
        (if (seq values) (str/join "\n" values) (field value "value"))

        result
        (if (nil? result) (if (= language "clojure") "nil" "None") (scalar result))

        section
        (fn [title text syntax]
          (when (and (string? text) (not (str/blank? text)))
            [{"type" "heading" "text" title}
             (cond-> {"type" "code" "text" text}
               syntax
               (assoc "language" syntax))]))

        trace
        (field value "trace")

        error-body
        (str/join "\n"
                  (remove str/blank?
                    [(or error-text "Evaluation failed")
                     (when (seq trace) (if (string? trace) trace (str/join "\n" trace)))
                     (when-let [data (field value "error_data")]
                       (str "ex-data: " data))]))]

    {"headline" (cond timeout? "Evaluation timed out"
                      error? "Evaluation failed"
                      :else "Evaluated")
     "summary" (str (case language
                      "clojure"
                      "Clojure"

                      "python"
                      "Python"

                      language)
                    " REPL")
     "content" (vec (concat (section "Program" code language)
                            (section "Stdout" (field value "out") nil)
                            (section "Stderr" (field value "err") nil)
                            (cond timeout? (section "Timeout"
                                                    (str "Evaluation timed out"
                                                         (when-let [ms (field value "ms")]
                                                           (str " after " ms "ms"))
                                                         ".")
                                                    nil)
                                  error? (section "Error" error-body nil)
                                  :else (section "Result" result language))))}))

(defn result-presentation
  "Default result view for an invocation without an authored presentation.
   Input is public, redacted and node-bounded by the event owner. The owner also
   bounds and validates the returned blocks before retention. Never reads files."
  [{:keys [operation label]} value]
  (let [op
        (name operation)

        text
        (when (string? value) value)

        path
        (or label "")

        read-lines
        (when (and (= op "cat") text) (map second (re-seq #"(?m)^(\d+):[0-9a-f]+│" text)))

        headline
        (get {"cat" "Read"
              "patch" "Patched"
              "grep" "Searched"
              "doc" "Read documentation"
              "apropos" "Found symbols"
              "defs" "Read definitions"
              "run_tests" "Ran tests"
              "lint_code" "Linted"
              "format_code" "Formatted"
              "repl_eval" "Evaluated"
              "shell" "Started command"
              "council.publish" "Published message"
              "council.read" "Read thread"
              "council.get" "Read message"
              "council.threads" "Listed threads"
              "council.members" "Listed members"}
             op
             (str/capitalize (str/trim (str/replace op #"[_.-]" " "))))

        summary
        (cond (= op "cat") (str path
                                (when (seq read-lines)
                                  (str " · lines " (first read-lines) "–" (last read-lines))))
              (contains? #{"doc" "defs" "patch" "shell"} op) path
              (= op "grep") (or (first (str/split-lines (or text ""))) "")
              (and (= op "run_tests") (number? (field value "total")))
              (str (or (field value "total") 0) " tests · " (or (field value "fail") 0) " failed")
              :else (str (or (field value "summary") (field value "title") "")))

        content
        (cond (= op "patch") []
              (and (= op "council.publish") (number? value)) []
              (and (= op "cat") text)
              [{"type" "code" "language" (code-language path) "text" (read-content text)}]
              (and (= op "doc") text) [{"type" "markdown" "text" text}]
              (and (= op "defs") text) [{"type" "code" "language" "python" "text" text}]
              (and (= op "grep") text)
              [{"type" "code" "text" (str/replace text #"(?m)^(\s*\d+):[0-9a-f]+│ ?" "$1 │ ")}]
              :else (result-blocks
                      (visible-result
                        (if (map? value) (dissoc value :title "title" :summary "summary") value))
                      (case op
                        ("run_tests" "lint_code")
                        "Metric"

                        ("council.publish" "council.get")
                        "Message"

                        ("council.read" "council.threads")
                        "Thread"

                        "council.members"
                        "Member"

                        "Detail")))]

    (cond (= op "repl_eval") (repl-presentation value)
          (or (= op "shell") (str/starts-with? op "_shell-")) (shell-presentation value)
          :else {"headline" headline "summary" summary "content" content})))
